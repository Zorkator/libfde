
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
    !@ _TypeReference_declare( {access}, {typeId}, {baseType}{dimType}{typeProcs} )""",

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
    type :: {typeId}_t
       {baseType}{baseExtra}{dimType}, pointer :: ptr
    end type
    type(TypeInfo_t), target :: TypeInfo_{typeId}
    """,

    # parameters:
    #   access: private | public
    #
    gen_itf = """
    {access} :: ref
    """,
  
    # parameters:
    #   access: private | public
    #   typeId: type identifier
    #
    ref_itf = """
    interface ref        ; module procedure GenericRef_encode_{typeId}; end interface
    interface {typeId}   ; module procedure GenericRef_decode_{typeId}; end interface
    interface is_{typeId}; module procedure GenericRef_is_{typeId}    ; end interface
    {access} :: {typeId}
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
    interface {typeId}_from_ref; module procedure GenericRef_decode_{typeId}; end interface
    interface is_{typeId}      ; module procedure GenericRef_is_{typeId}    ; end interface
    {access} :: ref_from_{typeId}
    {access} :: {typeId}_from_ref
    {access} :: is_{typeId}
    """,
  
    # parameters:
    #   typeId:     type identifier
    #   baseType:   fortran base type | type(...) | procedure(...)
    #   dimType:    ('', ', dimension(:,...)')[has_dimension]
    #   assignProc: ', assignProc = <funcId> | None'
    #   deleteProc: ', deleteProc = <funcId> | None'
    #   shapeProc:  ', shapeProc = <funcId> | None'
    #
    ref_encoder = """
    function GenericRef_encode_{typeId}( val ) result(res)
      use iso_c_binding
      {baseType}{dimType}, target, intent(in) :: val
      type(GenericRef)                        :: res
      type({typeId}_t),                target :: wrap
      procedure(),                    pointer :: None => null()
    
      wrap%ptr => val
      if (gr_set_TypeReference( res, c_loc(wrap), int(storage_size(wrap),4), TypeInfo_{typeId} )) &
        call init_TypeInfo( TypeInfo_{typeId}, '{typeId}', '{baseType}' &
                            , int(storage_size(val),4) &
                            , size(shape(val)){assignProc}{deleteProc}{shapeProc} &
                            , castProc = GenericRef_c2f_cast_{typeId} &
                            , cloneProc = GenericRef_clone_{typeId} )
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
      {baseType}{dimType}      :: val
      type(GenericRef)         :: res
      type({typeId}_t), target :: wrap
    
      wrap%ptr => val
      if (gr_set_TypeReference( res, c_loc(wrap), int(storage_size(wrap),4), TypeInfo_{typeId} )) &
        call init_TypeInfo( TypeInfo_{typeId}, '{typeId}', '{baseType}', 0, 0 )
    end function
    """,
    
    # parameters:
    #   typeId:    type identifier
    #   baseType:  fortran base type | type(...) | procedure(...)
    #   dimType:   ('', ', dimension(:,...)')[has_dimension]
    #
    decoder = """
    function GenericRef_decode_{typeId}( val ) result(res)
      use iso_c_binding
      type(GenericRef), intent(in) :: val
      {baseType}{dimType}, pointer :: res
      type({typeId}_t),    pointer :: wrap
      
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
    subroutine GenericRef_clone_{typeId}( val, res )
      use iso_c_binding
      type(GenericRef),            intent(in) :: val
      type(GenericRef)                        :: res
      {baseType}{dimType},            pointer :: src, tgt => null()
      character(len=1), dimension(:), pointer :: tmp
    
      src => GenericRef_decode_{typeId}( val ){cloneBy}
      res =  GenericRef_encode_{typeId}( tgt )
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
    subroutine GenericRef_c2f_cast_{typeId}( ptr_c, ptr_f )
      use iso_c_binding
      type(c_ptr),                   intent(in) :: ptr_c
      {baseType}{dimType}, pointer, intent(out) :: ptr_f
      type({typeId}_t),                 pointer :: wrap
      call c_f_pointer( ptr_c, wrap )
      ptr_f => wrap%ptr
    end subroutine
    """,

    # parameters:
    #   typeId: type identifier
    #
    inspector = """
    subroutine GenericRef_inspect_{typeId}( val, res, n )
      type(GenericRef), intent(in) :: val
      integer                      :: n
      integer                      :: res(n)
      res(:n) = shape( GenericRef_decode_{typeId}( val ) )
    end subroutine
    """,

    # parameters:
    typecheck = """
    function GenericRef_is_{typeId}( self ) result(res)
      type(GenericRef), intent(in) :: self
      logical                      :: res
      res = associated( typeinfo_of(self), TypeInfo_{typeId} )
    end function
    """
  )


  def __init__( self, access, typeId, baseType, dimType, typeProcedures ):
    self._isProc    = bool(self.procItfMatch( baseType ))
    self._isScalar  = dimType == 'scalar'
    self._isArray   = bool(self.dimSpecMatch( dimType ))
    self._typeProcs = dict( re.findall( self._keyAssign, typeProcedures ) )

    # sanity check
    if not (self._isScalar ^ self._isArray):
      raise TypeError('invalid dimension specification "%s"' % dimType)

    if access not in ('public', 'private'):
      raise ValueError('invalid access specification "%s"' % access)

    self.access    = access
    self.typeId    = typeId
    self.baseType  = baseType
    self.baseExtra = ('', ', nopass')[self._isProc]
    self.valTarget = (', target, intent(in)', '')[self._isProc]
    self.dimType   = ('', ', %s' % dimType)[self._isArray]
    self.shapeArg  = ('', ', shape(src)')[self._isArray]
    self.typeProcs = ''

    if self._typeProcs:
      self.typeProcs = ', ' + ', '.join( '%s = %s' % i for i in self._typeProcs.items() )

    self.assignProc = ', assignProc = %s' % self._typeProcs.get('assignProc')
    self.deleteProc = ', deleteProc = %s' % self._typeProcs.get('deleteProc')
    self.shapeProc  = ('', ', shapeProc  = GenericRef_inspect_%s' % typeId)[self._isArray]
    self.clonerFunc = self._typeProcs.get('cloneProc')
    self.cloneType  = ('alloc_clone', 'func_clone')[bool(self.clonerFunc)]
    self.cloneBy    = self._template[self.cloneType].format( **self.__dict__ )
   
    self._info    = self._template['info'] 
    self._type    = self._template['type']
    self._itf     = self._template[('ref_itf', 'proc_itf')[self._isProc]]
    self._encoder = self._template[('ref_encoder', 'proc_encoder')[self._isProc]]
    self._decoder = self._template['decoder']
    self._c2fCast = self._template['c2fCast']
    self._cloner  = (self._template['cloner'], '')[self._isProc]
    self._inspect = (self._template['inspector'], '')[self._isProc]
    self._typeChk = self._template['typecheck']

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
      self._implemented = True


  @classmethod
  def convert( _class, options ):

    with open(options['FILE']) as f:

      outFile = options.get('--output')
      outChnl = outFile and open( outFile, 'w' ) or sys.stdout

      for line in f.readlines():

        match = _class.typeDeclMatch( line )
        if match:
          _class( *map( str.strip, match.groups() ) ).declare( outChnl.write )
          continue

        match = _class.typeImplMatch( line )
        if match:
          _class.Scope[ match.groups()[0] ].implement( outChnl.write )
          continue

        if _class.typeImplAllMatch( line ):
          for decl in sorted( _class.Scope.items() ):
            decl[1].implement( outChnl.write )
          continue

        outChnl.write( line )
      outChnl.close()



if __name__ == '__main__':
  ReferenceType.convert( docopt( __doc__ ) )


