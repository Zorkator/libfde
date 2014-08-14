
"""Usage: refgen FILE [-o OUTFILE]

Preprocess FILE and expand type reference declarations.
The result is output to stdout.

Arguments:
  FILE        input file
  OUTFILE     output file

Options:
  -o OUTFILE --output=OUTFILE   the output file

"""

from docopt import docopt
import sys, re


class ReferenceType(object):

  Scope = dict()
  
  _ident     = '\s*(\w+)\s*'      #< some type identifier
  _baseType  = '\s*([\w *()]+)'   #< e.g. integer*4, type(Struct), character(*), <interfaceId>, ...
  _dimType   = '\s*([\w ,:()]+)'  #< e.g. scalar, dimension(:,:), procedure
  _keyAssign = '\s*(\w+)\s*=\s*(\w+)\s*'
  _procItf   = '\s*procedure\s*\(\s*\w*\s*\)\s*'            #< procedure interface
  _dimSpec   = '\s*dimension\s*\(\s*:(?:\s*,\s*:)*\s*\)\s*' #< dimension specification
  _keySpecs  = '((?:,\s*\w+\s*=\s*\w+\s*)*)'
  _typeDecl  = '^\s*!\s*_TypeReference_declare\(%s,%s,%s,%s%s\)' % (_ident, _ident, _baseType, _dimType, _keySpecs)
  _typeImpl  = '^\s*!\s*_TypeReference_implement\(%s\)' % _ident
  _typeImplA = '^\s*!\s*_TypeReference_implementAll\(\)'
  
  typeDeclMatch    = re.compile( _typeDecl ).match
  typeImplMatch    = re.compile( _typeImpl ).match
  typeImplAllMatch = re.compile( _typeImplA ).match
  procItfMatch     = re.compile( _procItf ).match
  dimSpecMatch     = re.compile( _dimSpec ).match

  _template  = dict(
    info = """
    !@ _TypeReference_declare( {access}, {typeId}, {baseType}{dimType}{keySpecs} )""",

    header = """
    !#################################
    !# {typeId}
    !#################################
    """,

    # parameters:
    #   typeId:    type identifier
    #   baseType:  fortran base type | type(...) | procedure(...) [needs baseExtra: , nopass]
    #   baseExtra: ('', ', nopass')[is_procedure] 
    #   dimType:   ('', ', dimension(:,...)')[has_dimension]
    #
    type = """
    type, private :: {typeId}_wrap_t
       {baseType}{baseExtra}{dimType}, pointer :: ptr
    end type
    type(TypeInfo_t), target :: type_{typeId}
    """,

    # parameters:
    #   access: private | public
    #
    gen_itf = """
    {access} :: ref
    {access} :: static_type
    """,
  
    # parameters:
    #   access:    private | public
    #   typeId:    type identifier
    #   derefName: identifier for dereferencing type
    #
    ref_itf = """
    interface ref        ; module procedure {typeId}_encode_ref_ ; end interface
    interface {derefName}; module procedure {typeId}_decode_ref_ ; end interface
    interface is_{typeId}; module procedure {typeId}_in_ref_     ; end interface
    interface static_type; module procedure {typeId}_typeinfo_   ; end interface
    {access} :: {derefName}
    {access} :: is_{typeId}
    """,
  
    # parameters:
    #   access: private | public
    #   typeId: type identifier
    #
    # NOTE: we can't create operator interfaces for procedure encoders/decoders.
    #   For the encoder, this is, because fortran can't distinguish different procedure types.
    #   For the decoder, it's obviously due to various compiler bugs ...
    #
    proc_itf = """
    interface ref_from_{typeId}; module procedure {typeId}_encode_ref_ ; end interface
    interface {typeId}_from_ref; module procedure {typeId}_decode_ref_ ; end interface
    interface is_{typeId}      ; module procedure {typeId}_in_ref_     ; end interface
    interface static_type      ; module procedure {typeId}_typeinfo_   ; end interface
    {access} :: ref_from_{typeId}
    {access} :: {typeId}_from_ref
    {access} :: is_{typeId}
    """,
  
    # parameters:
    #   typeId:     type identifier
    #   baseType:   fortran base type | type(...) | procedure(...)
    #   dimType:    ('', ', dimension(:,...)')[has_dimension]
    #
    ref_encoder = """
    function {typeId}_encode_ref_( val ) result(res)
      use iso_c_binding
      {baseType}{dimType}, target, intent(in) :: val
      type(GenericRef_t)                      :: res
      type({typeId}_wrap_t),           target :: wrap
    
      wrap%ptr => val
      call gr_set_TypeReference( res, c_loc(wrap), int(storage_size(wrap),4), static_type(val) )
    end function
    """,
    
    # parameters:
    #   typeId:     type identifier
    #   baseType:   fortran base type | type(...) | procedure(...)
    #   dimType:    ('', ', dimension(:,...)')[has_dimension]
    #
    proc_encoder = """
    function {typeId}_encode_ref_( val ) result(res)
      use iso_c_binding
      {baseType}{dimType}           :: val
      type(GenericRef_t)            :: res
      type({typeId}_wrap_t), target :: wrap
    
      wrap%ptr => val
      call gr_set_TypeReference( res, c_loc(wrap), int(storage_size(wrap),4), static_type(val) )
    end function
    """,
    
    # parameters:
    #   typeId:    type identifier
    #   baseType:  fortran base type | type(...) | procedure(...)
    #   dimType:   ('', ', dimension(:,...)')[has_dimension]
    #
    decoder = """
    function {typeId}_decode_ref_( val ) result(res)
      use iso_c_binding
      type(GenericRef_t), intent(in) :: val
      {baseType}{dimType},   pointer :: res
      type({typeId}_wrap_t), pointer :: wrap
      
      call c_f_pointer( gr_get_TypeReference(val), wrap )
      res => wrap%ptr
    end function
    """,
    
    # parameters:
    #   typeId:    type identifier
    #   baseType:  fortran base type | type(...) | procedure(...)
    #   dimType:   ('', ', dimension(:,...)')[has_dimension]
    #   cloneBy:   <cloneCode> [alloc_clone | proc_clone]
    #
    ref_cloner = """
    subroutine {typeId}_clone_ref_( tgt_ref, src_ref )
      use iso_c_binding
      type(GenericRef_t)                      :: tgt_ref
      type(GenericRef_t),          intent(in) :: src_ref
      {baseType}{dimType},            pointer :: src, tgt
    
      src => {typeId}_decode_ref_( src_ref )
      {code_clonePtr}
      tgt_ref =  {typeId}_encode_ref_( tgt )
    end subroutine
    """,

    ptr_cloner = dict(
      # parameters:
      #   typeId:   type identifier
      #   baseType: fortran base type | type(...)
      #   dimType:  ('', ', dimension(:,...)')[has_dimension]
      #   shapeArg: (", shape(src)" | "")[is_scalar]
      _shallow = """
      subroutine {typeId}_clone_ptr_( tgt, src )
        use iso_c_binding
        {baseType}{dimType}, pointer, intent(out) :: tgt
        {baseType}{dimType},           intent(in) :: src
        character(len=1), dimension(:),   pointer :: tmp
        allocate( tmp( product(shape(src)) * storage_size(src)/8 ) )
        call c_f_pointer( c_loc(tmp(1)), tgt{shapeArg} )
        tgt = src
      end subroutine
      """,

      # parameters:
      #   typeId:      type identifier
      #   baseType:    fortran base type | type(...)
      _type = """
      subroutine {typeId}_clone_ptr_( tgt, src )
        {baseType}, pointer, intent(out) :: tgt
        {baseType}, optional, intent(in) :: src
        type(TypeInfo_t),        pointer :: ti

        allocate( tgt ) !< initializes res as default {typeId}
        ti => static_type( tgt )
        if (associated( ti%initProc )) &
          call ti%initProc( tgt, 1 )
        if (present(src)) tgt = src
      end subroutine
      """
    ),

    # parameters:
    #   typeId:    type identifier
    #   baseType:  fortran base type | type(...) | procedure(...)
    #   dimType:   ('', ', dimension(:,...)')[has_dimension]
    c2f_caster = """
    subroutine {typeId}_c2f_cast_( ptr_c, ptr_f )
      use iso_c_binding
      type(c_ptr),                   intent(in) :: ptr_c
      {baseType}{dimType}, pointer, intent(out) :: ptr_f
      type({typeId}_wrap_t),            pointer :: wrap
      call c_f_pointer( ptr_c, wrap )
      ptr_f => wrap%ptr
    end subroutine
    """,

    # parameters:
    #   typeId: type identifier
    #
    ref_inspector = """
    subroutine {typeId}_inspect_( val, res, n )
      type(GenericRef_t), intent(in) :: val
      integer                        :: n
      integer                        :: res(n)
      res(:n) = shape( {typeId}_decode_ref_( val ) )
    end subroutine
    """,

    # parameters:
    ref_typechecker = """
    function {typeId}_in_ref_( self ) result(res)
      type(GenericRef_t), intent(in) :: self
      logical                        :: res
      res = associated( dynamic_type(self), type_{typeId} )
    end function
    """,

    # parameters:
    #   typeId:     type identifier
    #   baseType:   fortran base type | type(...) | procedure(...)
    #   dimType:    ('', ', dimension(:,...)')[has_dimension]
    #   initProc:   ', initProc = <funcId>'
    #   assignProc: ', assignProc = <funcId>'
    #   deleteProc: ', deleteProc = <funcId>'
    #   shapeProc:  ', shapeProc = <funcId>'
    ref_typeinfo = """
    function {typeId}_typeinfo_( self ) result(res)
      {baseType}{dimType}       :: self
      type(TypeInfo_t), pointer :: res

      res => type_{typeId}
      if (.not. res%initialized) &
        call init_TypeInfo( res, '{typeId}', '{baseType}' &
                            , int(storage_size(self),4) &
                            , size(shape(self)){initProc}{assignProc}{deleteProc}{shapeProc}{cloneProc} &
                            , castProc = {typeId}_c2f_cast_ &
                            , cloneRefProc = {typeId}_clone_ref_ )
    end function
    """,

    # parameters:
    #   typeId:     type identifier
    #   baseType:   fortran base type | type(...) | procedure(...)
    #   dimType:    ('', ', dimension(:,...)')[has_dimension]
    proc_typeinfo = """
    function {typeId}_typeinfo_( self ) result(res)
      {baseType}{dimType}       :: self
      type(TypeInfo_t), pointer :: res

      res => type_{typeId}
      if (.not. res%initialized) &
        call init_TypeInfo( res, '{typeId}', '{baseType}', 0, 0 )
    end function
    """
  )


  def __init__( self, access, typeId, baseType, dimType, keySpecs ):
    self._isProc    = bool(self.procItfMatch( baseType ))
    self._isScalar  = dimType == 'scalar'
    self._isArray   = bool(self.dimSpecMatch( dimType ))
    self._keySpecs  = dict( re.findall( self._keyAssign, keySpecs ) )
    self._typeProcs = dict( (k,v) for k,v in self._keySpecs.items() if k.endswith('Proc') )

    # sanity check
    if not (self._isScalar ^ self._isArray):
      raise TypeError('ERROR at processing type "%s": invalid dimension specification "%s"' % (typeId, dimType))

    if access not in ('public', 'private'):
      raise ValueError('ERROR at processing type "%s": invalid access specification "%s"' % (typeId, access))

    self.access    = access
    self.typeId    = typeId
    self.derefName = self._keySpecs.get('derefName', typeId)
    self.baseType  = baseType
    self.baseExtra = ('', ', nopass')[self._isProc]
    self.valTarget = (', target, intent(in)', '')[self._isProc]
    self.dimType   = ('', ', %s' % dimType)[self._isArray]
    self.shapeArg  = ('', ', shape(src)')[self._isArray]
    self.keySpecs  = ''

    if self._typeProcs:
      self.keySpecs = ', ' + ', '.join( '%s = %s' % i for i in self._keySpecs.items() )
      if self._isProc:
        print 'WARNING: given type procs %s are ignored for procedure type "%s"' % (self._typeProcs, typeId)

    if self._isArray:
      self._typeProcs.setdefault( 'shapeProc', typeId + '_inspect_' )

    if self._isProc:
      self._typeProcs.setdefault( 'cloneProc', '_none' ) #< disable cloning for procedure types

    for procId in ('initProc', 'assignProc', 'deleteProc', 'shapeProc'):
      procArg  = ''
      procName = self._typeProcs.get( procId, '' )
      if procName not in ('', '_none'):
        procArg = ',%s = %s' % (procId, procName)
      setattr( self, procId, procArg )

    # handle type cloning ...
    ptrClonerId     = self._typeProcs.get('cloneProc', '_shallow')        #< by default clone via shallow copy
    self._ptrCloner = self._template['ptr_cloner'].get( ptrClonerId, '' ) #< get code for default implementation
    
    if self._ptrCloner:
      # we implement a default cloner (_shallow or _type) - so update ptrClonerId
      ptrClonerId = typeId + '_clone_ptr_'
    elif ptrClonerId == '_none':
      # cloning disabled via _none - so there's no ptrClonerId
      ptrClonerId = ''
    
    if ptrClonerId:
      self.code_clonePtr = 'call %s( tgt, src )' % ptrClonerId
      self.cloneProc     = ', cloneObjProc = %s' % ptrClonerId
    else:
      self.code_clonePtr = 'tgt => src'
      self.cloneProc  = ''

    self._info      = self._template['info'] 
    self._type      = self._template['type']
    self._itf       = self._template[('ref_itf', 'proc_itf')[self._isProc]]
    self._encoder   = self._template[('ref_encoder', 'proc_encoder')[self._isProc]]
    self._decoder   = self._template['decoder']
    self._c2fCast   = self._template['c2f_caster']
    self._refCloner = (self._template['ref_cloner'], '')[self._isProc]
    self._inspector = (self._template['ref_inspector'], '')[self._isProc]
    self._typecheck = self._template['ref_typechecker']
    self._typeinfo  = self._template[('ref_typeinfo', 'proc_typeinfo')[self._isProc]]

    self._declared    = False
    self._implemented = False

    ReferenceType.Scope[typeId] = self


  def declare( self, out ):
    if not self._declared:
      out( self._info.format( **self.__dict__ ) )
      out( self._type.format( **self.__dict__ ) )
      out( self._itf.format( **self.__dict__ ) )
      if len(ReferenceType.Scope) == 1:
        out( self._template['gen_itf'].format( **self.__dict__ ) )

      self._declared = True


  def implement( self, out ):
    if not self._implemented:
      out( self._template['header'].format( **self.__dict__ ) )
      out( self._encoder.format( **self.__dict__ ) )
      out( self._decoder.format( **self.__dict__ ) )
      out( self._c2fCast.format( **self.__dict__ ) )
      out( self._refCloner.format( **self.__dict__ ) )
      out( self._ptrCloner.format( **self.__dict__ ) )
      out( self._inspector.format( **self.__dict__ ) )
      out( self._typecheck.format( **self.__dict__ ) )
      out( self._typeinfo.format( **self.__dict__ ) )
      self._implemented = True


  @staticmethod
  def _purgeLines( buf ):
    for l in buf:
      l = l.strip()
      a = (0,1)[l.startswith('!')]
      b = (None,-1)[l.endswith('\\')]
      yield l[a:b]


  @classmethod
  def convert( _class, options ):

    with open(options['FILE']) as f:

      outFile = options.get('--output')
      outChnl = outFile and open( outFile, 'w' ) or sys.stdout
      lineBuf = []

      for line in f.readlines():
        lineBuf.append( line.rstrip() )
        if lineBuf[-1].endswith('\\'):
          continue
        
        lines = '!' + ''.join( _class._purgeLines( lineBuf ) )
        match = _class.typeDeclMatch( lines )
        if match:
          _class( *map( str.strip, match.groups() ) ).declare( outChnl.write )

        else:
          match = _class.typeImplMatch( lines )
          if match:
            _class.Scope[ match.groups()[0] ].implement( outChnl.write )

          elif _class.typeImplAllMatch( lines ):
            for decl in sorted( _class.Scope.items() ):
              decl[1].implement( outChnl.write )
          
          else:
            outChnl.write( '\n'.join( lineBuf ) + '\n' )
        lineBuf = []

      outChnl.close()


if __name__ == '__main__':
  ReferenceType.convert( docopt( __doc__ ) )


