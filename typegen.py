
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
  _procItf   = '\s*procedure\s*\(%s\)\s*' % _ident          #< procedure interface
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
    {access} :: typeinfo
    """,
  
    # parameters:
    #   access:    private | public
    #   typeId:    type identifier
    #   derefName: identifier for dereferencing type
    #
    ref_itf = """
    interface ref        ; module procedure RefType_encode_{typeId}  ; end interface
    interface {derefName}; module procedure RefType_decode_{typeId}  ; end interface
    interface is_{typeId}; module procedure RefType_is_{typeId}      ; end interface
    interface typeinfo      ; module procedure RefType_typeinfo_{typeId}; end interface
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
    interface {typeId}_from_ref; module procedure RefType_decode_{typeId}  ; end interface
    interface is_{typeId}      ; module procedure RefType_is_{typeId}      ; end interface
    interface typeinfo         ; module procedure RefType_typeinfo_{typeId}; end interface
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
    function RefType_encode_{typeId}( val ) result(res)
      use iso_c_binding
      {baseType}{dimType}, target, intent(in) :: val
      type(GenericRef_t)                      :: res
      type({typeId}_wrap_t),           target :: wrap
    
      wrap%ptr => val
      call gr_set_TypeReference( res, c_loc(wrap), int(storage_size(wrap),4), typeinfo(val) )
    end function
    """,
    
    # parameters:
    #   typeId:     type identifier
    #   baseType:   fortran base type | type(...) | procedure(...)
    #   dimType:    ('', ', dimension(:,...)')[has_dimension]
    #
    proc_encoder = """
    function ref_from_{typeId}( val ) result(res)
      use iso_c_binding
      {baseType}{dimType}           :: val
      type(GenericRef_t)            :: res
      type({typeId}_wrap_t), target :: wrap
    
      wrap%ptr => val
      call gr_set_TypeReference( res, c_loc(wrap), int(storage_size(wrap),4), typeinfo(val) )
    end function
    """,
    
    # parameters:
    #   typeId:    type identifier
    #   baseType:  fortran base type | type(...) | procedure(...)
    #   dimType:   ('', ', dimension(:,...)')[has_dimension]
    #
    decoder = """
    function RefType_decode_{typeId}( val ) result(res)
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
    #   cloneBy:   <cloneCode> [alloc_clone | func_clone]
    #
    cloner = """
    subroutine RefType_clone_{typeId}( val, res )
      use iso_c_binding
      type(GenericRef_t),          intent(in) :: val
      type(GenericRef_t)                      :: res
      {baseType}{dimType},            pointer :: src, tgt => null()
      character(len=1), dimension(:), pointer :: tmp
    
      src => RefType_decode_{typeId}( val ){cloneBy}
      res =  RefType_encode_{typeId}( tgt )
    end subroutine
    """,
    
    # parameters:
    #   shapeArg: (", shape(src)" | "")[is_scalar]
    alloc_clone = """
      allocate( tmp( product(shape(src)) * storage_size(src)/8 ) )
      call c_f_pointer( c_loc(tmp(1)), tgt{shapeArg} )
      tgt = src
    """,
    
    # parameters:
    #   clonerFunc: id of clone function
    func_clone = """
      tgt => {clonerFunc}( src )
    """,

    # parameters:
    #   typeId:    type identifier
    #   baseType:  fortran base type | type(...) | procedure(...)
    #   dimType:   ('', ', dimension(:,...)')[has_dimension]
    c2fCast = """
    subroutine RefType_c2f_cast_{typeId}( ptr_c, ptr_f )
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
    inspector = """
    subroutine RefType_inspect_{typeId}( val, res, n )
      type(GenericRef_t), intent(in) :: val
      integer                        :: n
      integer                        :: res(n)
      res(:n) = shape( RefType_decode_{typeId}( val ) )
    end subroutine
    """,

    # parameters:
    typecheck = """
    function RefType_is_{typeId}( self ) result(res)
      type(GenericRef_t), intent(in) :: self
      logical                        :: res
      res = associated( typeOf(self), type_{typeId} )
    end function
    """,

    # parameters:
    #   typeId:     type identifier
    #   baseType:   fortran base type | type(...) | procedure(...)
    #   dimType:    ('', ', dimension(:,...)')[has_dimension]
    #   assignProc: ', assignProc = <funcId> | None'
    #   deleteProc: ', deleteProc = <funcId> | None'
    #   shapeProc:  ', shapeProc = <funcId> | None'
    ref_typeinfo = """
    function RefType_typeinfo_{typeId}( self ) result(res)
      {baseType}{dimType}       :: self
      type(TypeInfo_t), pointer :: res
      procedure(),      pointer :: None => null()

      res => type_{typeId}
      if (.not. res%initialized) &
        call init_TypeInfo( res, '{typeId}', '{baseType}' &
                            , int(storage_size(self),4) &
                            , size(shape(self)){assignProc}{deleteProc}{shapeProc} &
                            , castProc = RefType_c2f_cast_{typeId} &
                            , cloneProc = RefType_clone_{typeId} )
    end function
    """,

    # parameters:
    #   typeId:     type identifier
    #   baseType:   fortran base type | type(...) | procedure(...)
    #   dimType:    ('', ', dimension(:,...)')[has_dimension]
    proc_typeinfo = """
    function RefType_typeinfo_{typeId}( self ) result(res)
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
      raise TypeError('invalid dimension specification "%s"' % dimType)

    if access not in ('public', 'private'):
      raise ValueError('invalid access specification "%s"' % access)

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

    self.assignProc = ', assignProc = %s' % self._typeProcs.get('assignProc')
    self.deleteProc = ', deleteProc = %s' % self._typeProcs.get('deleteProc')
    self.shapeProc  = ('', ', shapeProc  = RefType_inspect_%s' % typeId)[self._isArray]
    self.clonerFunc = self._typeProcs.get('cloneProc')
    self.cloneType  = ('alloc_clone', 'func_clone')[bool(self.clonerFunc)]
    self.cloneBy    = self._template[self.cloneType].format( **self.__dict__ )
   
    self._info     = self._template['info'] 
    self._type     = self._template['type']
    self._itf      = self._template[('ref_itf', 'proc_itf')[self._isProc]]
    self._encoder  = self._template[('ref_encoder', 'proc_encoder')[self._isProc]]
    self._decoder  = self._template['decoder']
    self._c2fCast  = self._template['c2fCast']
    self._cloner   = (self._template['cloner'], '')[self._isProc]
    self._inspect  = (self._template['inspector'], '')[self._isProc]
    self._typeChk  = self._template['typecheck']
    self._typeinfo = self._template[('ref_typeinfo', 'proc_typeinfo')[self._isProc]]

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
      out( self._cloner.format( **self.__dict__ ) )
      out( self._inspect.format( **self.__dict__ ) )
      out( self._typeChk.format( **self.__dict__ ) )
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
        lineBuf.append( line )
        if line.rstrip().endswith('\\'):
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
            outChnl.write( ''.join( lineBuf ) )
        lineBuf = []

      outChnl.close()


if __name__ == '__main__':
  ReferenceType.convert( docopt( __doc__ ) )


