
"""Usage: typegen FILE [-o OUTFILE] [--debug]

Preprocess FILE and expand type reference declarations.
The result is output to stdout.

Arguments:
  FILE        input file
  OUTFILE     output file

Options:
  -o OUTFILE --output=OUTFILE   the output file
  --debug                       enable debugging
"""

import sys, re


class TypeSpec(object):

  _procItf     = r'\s*procedure\s*\(\s*\w*\s*\)\s*'
  _baseTypeId  = r'\s*type\s*\(\s*(\w+)\s*\)\s*'
  _dimSize     = r'(?::|\d+)'
  _dimSpec     = r'\s*dimension\s*\(\s*%s(?:\s*,\s*%s)*\s*\)\s*' % (_dimSize, _dimSize)
  _keyAssign   = r'\s*(\w+)\s*=\s*(\w+|(?:"[^"]*?"))'
  _varLength   = r'.*\((?:len=)?[:*]\).*'

  procItfMatch    = re.compile( _procItf ).match
  dimSpecMatch    = re.compile( _dimSpec ).match
  varBaseMatch    = re.compile( _varLength ).match
  baseTypeIdMatch = re.compile( _baseTypeId ).match
  declWatcher     = dict()

  _template = dict(
    header = """
    !#################################
    !# {typeId} - {typegenId}
    !#################################
    """,

    access_decl = """
    {access} :: {ident}
    """,

    empty = ""
  )

  @staticmethod
  def peelString( string ):
    return re.sub( r'^\s*"|"\s*$', '', string )


  def __init__( self, access, typeId, baseType, dimType ):
    self._isProc    = bool(self.procItfMatch( baseType ))
    self._isScalar  = dimType == 'scalar'
    self._isArray   = bool(self.dimSpecMatch( dimType ))
    self._varBase   = bool(self.varBaseMatch( baseType ))

    # sanity checks
    if not (self._isScalar ^ self._isArray):
      raise TypeError('ERROR at processing type "%s": invalid dimension specification "%s"' % (typeId, dimType))

    if access not in ('public', 'private'):
      raise ValueError('ERROR at processing type "%s": invalid access specification "%s"' % (typeId, access))

    if self._varBase:
      baseType = baseType.replace('len=*', 'len=:')

    isDerived  = self.baseTypeIdMatch( baseType )
    isItemType = not (self._isArray or self._isProc)

    self.access       = access
    self.typeId       = typeId
    self.baseType     = baseType
    self.baseTypeId   = isDerived and isDerived.groups()[0] or ''
    self.baseType_arg = baseType.replace('len=:', 'len=*')
    self.subType_decl = ('', '\n      %s :: sub' % baseType)[self._isArray]
    self.baseSizeExpr = ('storage_size(self)', '0')[self._varBase]
    self.dimType      = dimType
    self.dimCount     = dimType.count(',')+1
    self.dimSize      = ('', ', %s' % dimType)[self._isArray]
    self.dimSpec      = ('', ', dimension(%s)' % ','.join( [':'] * self.dimCount ))[self._isArray]
    self.baseExtra    = ('', ', nopass')[self._isProc]
    self.valAttrib    = (', target, intent(in)', '')[self._isProc]
    self.itemCast     = ('!case(2); type not supported by Item_t', 'case (2); call c_f_pointer( cp, tgt ); res = .true.')[isItemType]
    self.shapeArg     = ('', ', shape(src)')[self._isArray]
    self.rank         = ('0', 'size(shape(self))')[self._isArray] #< NOTE: we ask for object shape only in case of arrays ...
                                                                  # This is for two reasons:
                                                                  #  1) for scalars rank is always 0 - so it's more straight forward
                                                                  #  2) asking a derived type object (which is a scalar too!) for its shape might have nasty side effects!
                                                                  #      e.g. in case of Ref_t shape(obj) triggers target resolving - even on WEAK references (!!)
                                                                  #           ... which get freed (to prevent memory leaks!).
    self.typegenId    = type(self).__name__

    self._isDerived   = bool(isDerived)
    self._declared    = False
    self._implemented = False


  def expand( self, out, *args, **kwArgs ):
    for what in filter( None, args ):
      out( self._template[what].format( **dict( self.__dict__, **kwArgs ) ) )


  def expandAccess( self, out, access, *identList ):
    for ident in filter( None, identList ):
      if ident not in self.declWatcher:
        out( self._template['access_decl'].format( access = access, ident = ident ) )
        self.declWatcher[ident] = access


  def expandAccessString( self, out, access, accessString ):
    self.expandAccess( out, access, *accessString.format( **self.__dict__ ).split(', ') )



class RefType(TypeSpec):

  _template  = dict( TypeSpec._template,
    info = """
    !@ _TypeGen_declare_RefType( {access}, {typeId}, {baseType}, {dimType}{keySpecStr} )""",

    # parameters:
    #   typeId:    type identifier
    #   baseType:  fortran base type | type(...) | procedure(...) [needs baseExtra: , nopass]
    #   baseExtra: ('', ', nopass')[is_procedure]
    #   dimSpec:   ('', ', dimension(:,...)')[has_dimension]
    #
    type = """
    type, public :: {typeId}Ptr_t
       {baseType}{baseExtra}{dimSpec}, pointer :: ptr
    end type

    type, private :: {typeId}_encoder_t
      type(TypeInfo_ptr_t) :: typeInfo(2)
      type({typeId}Ptr_t)  :: ref_wrap
    end type

    type(TypeInfo_t), target :: type_{typeId}
    """,

    # parameters:
    #   access:     private | public
    #   typeId:     type identifier
    #
    common_itf = """
    interface is_{typeId} ; module procedure {typeId}_in_ref_         ; end interface
    interface c_f_unwrap  ; module procedure {typeId}_unwrap_         ; end interface
    interface dynamic_cast; module procedure {typeId}_dynamic_cast_r_ ; end interface
    interface type_of     ; module procedure {typeId}_typeinfo_       ; end interface
    """,

    # parameters:
    #   access:     private | public
    #   typeId:     type identifier
    #
    item_itf = """
    interface dynamic_cast; module procedure {typeId}_dynamic_cast_i_ ; end interface
    """,

    # parameters:
    #   access:     private | public
    #   typeId:     type identifier
    #
    ref_itf = """
    interface ref_of      ; module procedure {typeId}_encode_ref_     ; end interface
    interface {typeId}    ; module procedure {typeId}_decode_ref_     ; end interface
    """,

    access_ref = "{typeId}, is_{typeId}",

    # parameters:
    #   access:     private | public
    #   typeId:     type identifier
    #
    # NOTE: we can't create operator interfaces for procedure encoders/decoders.
    #   For the encoder, this is, because fortran can't distinguish different procedure types.
    #   For the decoder, it's obviously due to various compiler bugs ...
    #
    proc_itf = """
    interface ref_from_{typeId}; module procedure {typeId}_encode_ref_     ; end interface
    interface {typeId}_from_ref; module procedure {typeId}_decode_ref_     ; end interface
    """,

    access_proc = "ref_from_{typeId}, {typeId}_from_ref, is_{typeId}",

    # parameters:
    #   typeId:     type identifier
    ref_streamItf = """
    interface stream           ; module procedure {typeId}_stream_; end interface
    """,

    # parameters:
    #   typeId:     type identifier
    #   baseType:   fortran base type | type(...) | procedure(...)
    #   dimSpec:    ('', ', dimension(:,...)')[has_dimension]
    #   writeBuf:   size of char buffer for writing type representaion
    #   writeFmt:   fortran format string to write type to buffer
    #   writeExpr:  fortran expression to write type to buffer
    #   formatSpec: fortran write format statement or empty
    ref_stream = """
!_PROC_EXPORT({typeId}_stream_)
!_ARG_REFERENCE1(self)
    subroutine {typeId}_stream_( self, outs )
      {baseType}{dimSpec}{valAttrib} :: self
      type(ostream_t)                :: outs
      type(TypeInfo_t),      pointer :: ti
      type({typeId}Ptr_t)            :: wrap
      ti => type_of(self)
      wrap%ptr => self
      call ti%streamProc( wrap, ti, outs )
    end subroutine
    """,

    ref_streaming = dict(
      buffered = """\n
        character(len={writeBuf}) :: buff
        integer                   :: st
        write(buff, {writeFmt}, iostat=st) {writeExpr}
        if (st == 0) then; call stream( outs, buff )
                     else; call error( outs, ti )
        end if
      """,

      direct = """\n
        call stream( outs, {writeExpr} )
      """,

      array = """\n
        integer :: i
        {baseType}, dimension(:), pointer :: linear
        type wrap_t
          {baseType}, pointer :: ptr
        end type
        type(wrap_t)          :: wrap
        call c_f_pointer( c_loc(self%ptr), linear, [size(self%ptr)] )
        do i = 1, size(linear)
          wrap%ptr => linear(i)
          call ti%subtype%streamProc( wrap, ti%subtype, outs )
        end do
      """
    ),

    ref_streamer = """
    recursive &
    subroutine {typeId}_stream_wrap( self, ti, outs )
      type({typeId}Ptr_t)               :: self
      type(TypeInfo_t)                  :: ti
      type(ostream_t)                   :: outs{streamWriting}{formatSpec}
    end subroutine
    """,

    ref_try_streaming = """
    subroutine {typeId}_stream_try_( buffer, status )
      character(len=*)            :: buffer
      integer                     :: status
      {baseType}{dimSpec}, target :: obj
      type({typeId}Ptr_t)         :: self
      self%ptr => obj
      write(buffer, {writeFmt}, iostat=status) {writeExpr},'#'{formatSpec}
    end subroutine
    """,

    ref_try_streaming_zero = """
    subroutine {typeId}_stream_try_( buffer, status )
      character(len=*)            :: buffer
      integer                     :: status
      {baseType}{dimSpec}, target :: obj
      type({typeId}Ptr_t)         :: self
      integer*1, parameter        :: zero(storage_size(obj)/8) = 0
      obj = transfer( zero, obj )
      self%ptr => obj
      write(buffer, {writeFmt}, iostat=status) {writeExpr},'#'{formatSpec}
    end subroutine
    """,

    # parameters:
    #   typeId:     type identifier
    ref_streamerItf = """
    interface
      recursive &
      subroutine {streamProcId}( wrap, ti, outs )
        import {typeId}Ptr_t, TypeInfo_t, ostream_t
        type({typeId}Ptr_t)   :: wrap
        type(TypeInfo_t)      :: ti
        type(ostream_t)       :: outs
      end subroutine
    end interface
    """,

    # parameters:
    #   typeId:     type identifier
    ref_acceptItf = """
    interface accept ; module procedure {typeId}_accept_ ; end interface
    """,

    ref_accept = """
!_PROC_EXPORT({typeId}_accept_)
!_ARG_REFERENCE1(self)
    subroutine {typeId}_accept_( self, vstr )
      use fde_visitor
      {baseType}{dimSpec}{valAttrib} :: self
      type(Visitor_t)                :: vstr
      type(TypeInfo_t),      pointer :: ti
      type({typeId}Ptr_t)            :: wrap
      ti       => type_of(self)
      wrap%ptr => self
      call ti%acceptProc( wrap, ti, vstr )
    end subroutine
    """,

    ref_acceptor = """
!_PROC_EXPORT({typeId}_accept_wrap)
!_ARG_REFERENCE1(wrap)
    recursive &
    subroutine {typeId}_accept_wrap( wrap, ti, vstr )
      use fde_visitor
      type({typeId}Ptr_t)       :: wrap
      type(TypeInfo_t)          :: ti
      type(Visitor_t)           :: vstr{visitorGroup_beg}
      call vstr%visit( vstr, wrap, ti ){visitorGroup_end}
    end subroutine
    """,

    ref_acceptorItf = """
    interface
      recursive &
      subroutine {acceptProcId}( wrap, ti, vstr )
        import {typeId}Ptr_t, TypeInfo_t, Visitor_t
        type({typeId}Ptr_t)       :: wrap
        type(TypeInfo_t)          :: ti
        type(Visitor_t)           :: vstr
      end subroutine
    end interface
    """,

    ref_encoder_mk_ptr = """
      contains
      function mk_ptr( b, tgt ) result(ptr)
        integer, dimension(:,:)                                    :: b
        {baseType_arg}, dimension({dimBounds}), target, intent(in) :: tgt
        {baseType_arg}{dimSpec},                           pointer :: ptr
        ! CAUTION: here Fortran might have transformed tgt into a temporary copy of val!!
        !          e.g. if val is part of multi-dimensional array ...
        ptr => tgt
        if (.not. associated( ptr, val )) then
          ! ... in that case we'd better take a pointer without bounds.
          ptr => val
        end if
      end function
    """,

    # parameters:
    #   typeId:           type identifier
    #   baseType_arg:     fortran base type | type(...) | procedure(...)
    #   dimSpec:          ('', ', dimension(:,...)')[has_dimension]
    #   encoder_ptr_tgt:  ('val', 'mk_ptr(val)')[is_array)]
    #   encoder_ptr_func: ('', $ref_encoder_mk_ptr)[is_array]
    #
    ref_encoder = """
!_PROC_EXPORT({typeId}_encode_ref_)
!_ARG_REFERENCE1(val)
    function {typeId}_encode_ref_( val, bind, lb, ub ) result(res)
      use iso_c_binding
      {baseType_arg}{dimSpec}{valAttrib} :: val
      logical,               optional    :: bind
      integer, dimension(:), optional    :: lb, ub
      type({typeId}_encoder_t),   target :: encoder
      type(RefEncoding_t)                :: dummy
      type(RefEncoding_t)                :: res( ceiling( storage_size(encoder) / real(storage_size(dummy)) ) )

      encoder%ref_wrap%ptr    => {encoder_ptr_tgt}
      encoder%typeInfo(1)%ptr => type_of(val)
      if (present(bind)) then
        if (bind) &
          encoder%typeInfo(2)%ptr => encoder%typeInfo(1)%ptr
      end if
      res = transfer( encoder, res )
      {encoder_ptr_func}
    end function
    """,

    # parameters:
    #   typeId:    type identifier
    #   baseType:  fortran base type | type(...) | procedure(...)
    #   dimSpec:   ('', ', dimension(:,...)')[has_dimension]
    #
    ref_decoder = """
!_PROC_EXPORT({typeId}_decode_ref_)
!_ARG_REFERENCE1(val)
    function {typeId}_decode_ref_( val ) result(res)
      use iso_c_binding
      type(Ref_t),        intent(in) :: val
      {baseType}{dimSpec},   pointer :: res
      type({typeId}Ptr_t),   pointer :: wrap

      call c_f_pointer( ref_get_typereference(val), wrap )
      if (associated(wrap)) then; res => wrap%ptr
                            else; res => null()
      end if
    end function
    """,

    # parameters:
    #   typeId:        type identifier
    #   baseType:      fortran base type | type(...) | procedure(...)
    #   dimSpec:       ('', ', dimension(:,...)')[has_dimension]
    #   code_clonePtr: 'tgt => src' | 'call <typeId>_clone_ptr_( tgt, src )'
    ref_cloner = """
    subroutine {typeId}_clone_ref_( tgt_ref, src_ref )
      use iso_c_binding
      type(Ref_t)                  :: tgt_ref
      type(Ref_t),      intent(in) :: src_ref
      {baseType}{dimSpec}, pointer :: src, tgt => null()

      src => {typeId}_decode_ref_( src_ref )
      {code_clonePtr}
      tgt_ref =  {typeId}_encode_ref_( tgt, .true.{cloneSrcBounds} )
    end subroutine
    """,

    ptr_cloner = dict(
      # parameters:
      #   typeId:   type identifier
      #   baseType: fortran base type | type(...)
      #   dimSpec:  ('', ', dimension(:,...)')[has_dimension]
      #   shapeArg: (", shape(src)" | "")[is_scalar]
      _shallow = """
      subroutine {typeId}_clone_ptr_( tgt, src )
        use iso_c_binding
        {baseType}{dimSpec}, pointer, intent(out) :: tgt
        {baseType}{dimSpec}, pointer,  intent(in) :: src
        character(len=1), dimension(:),   pointer :: tmp
        allocate( tmp( product(shape(src)) * storage_size(src)/8 ) )
        call c_f_pointer( c_loc(tmp(1)), tgt{shapeArg} )
        tgt = src
      end subroutine
      """,

      # parameters:
      #   typeId:   type identifier
      #   baseType: fortran base type | type(...)
      _type = """
      subroutine {typeId}_clone_ptr_( tgt, src )
        {baseType}, pointer, intent(out) :: tgt
        {baseType}, pointer,  intent(in) :: src
        type(TypeInfo_t),        pointer :: ti

        allocate( tgt ) !< initializes res as default {typeId}
        ti => type_of( tgt )
        if (associated( ti%initProc )) then
          call ti%initProc( tgt, 1, src )
        else
          tgt = src
        end if
      end subroutine
      """
    ),

    # parameters:
    #   typeId: type identifier
    #
    ref_inspector = """
    subroutine {typeId}_inspect_( val, res, n )
      type(Ref_t),      intent(in) :: val
      integer                      :: n
      integer                      :: res(n)
      {baseType}{dimSpec}, pointer :: ptr
      ! . o O (ifort appears to need an auxiliary pointer on inspecting large arrays ... >:-( got stackoverflow otherwise!)
      ptr => {typeId}_decode_ref_( val )
      res = shape( ptr )
    end subroutine
    """,

    # parameters:
    #   typeId: type identifier
    ref_typechecker = """
!_PROC_EXPORT({typeId}_in_ref_)
!_ARG_REFERENCE1(self)
    function {typeId}_in_ref_( self ) result(res)
      type(Ref_t), intent(in) :: self
      logical                 :: res
      call as_target_( self )

      contains

      subroutine as_target_( ref )
        type(Ref_t), target :: ref
        type(Ref_t), pointer :: refPtr
        refPtr => ref
        res = associated( content_type(refPtr), type_{typeId} )
      end subroutine
    end function
    """,

    # parameters:
    #   typeId: type identifier
    ref_unwrap = """
!_PROC_EXPORT({typeId}_unwrap_)
!_ARG_REFERENCE2(cp, tgt)
    subroutine {typeId}_unwrap_( cp, tgt )
      use iso_c_binding
      type(c_ptr),                  intent(in)  :: cp
      {baseType}{dimSpec}, pointer, intent(out) :: tgt
      type({typeId}Ptr_t),              pointer :: wrap

      if (c_associated( cp )) then
        call c_f_pointer( cp, wrap )
        tgt => wrap%ptr
      else
        tgt => null()
      endif
    end subroutine
    """,

    # parameters:
    #   typeId: type identifier
    ref_dynamic_cast = """
!_PROC_EXPORT({typeId}_dynamic_cast_r_)
!_ARG_REFERENCE2(tgt, self)
    logical &
    function {typeId}_dynamic_cast_r_( tgt, self ) result(res)
      use iso_c_binding
      {baseType}{dimSpec}, pointer, intent(out) :: tgt
      type(Ref_t), pointer,         intent(in)  :: self
      type(c_ptr)                               :: cp

      interface
        integer function item_resolve_data( ctgt, ti, ref, item )
          import
          type(c_ptr),        intent(out) :: ctgt
          type(TypeInfo_t)                :: ti
          type(Ref_t),   optional, target :: ref
          type(Ref_t),   optional, target :: item !< we cannot use Item_t here, but its ignored anyways!
        end function
      end interface

      select case (item_resolve_data( cp, type_{typeId}, ref=self ))
        case (1); call c_f_unwrap( cp, tgt );  res = .true.
        {itemCast}
        case default;           tgt => null(); res = .false.
      end select
    end function
    """,

    # parameters:
    #   typeId: type identifier
    item_dynamic_cast = """
!_PROC_EXPORT({typeId}_dynamic_cast_i_)
!_ARG_REFERENCE2(tgt, self)
    logical &
    function {typeId}_dynamic_cast_i_( tgt, self ) result(res)
      use iso_c_binding
      {baseType}{dimSpec}, pointer, intent(out) :: tgt
      type(Item_t), pointer,        intent(in)  :: self
      type(c_ptr)                               :: cp

      interface
        integer function item_resolve_data( ctgt, ti, ref, item )
          import
          type(c_ptr),        intent(out) :: ctgt
          type(TypeInfo_t)                :: ti
          type(Ref_t),   optional, target :: ref
          type(Item_t),  optional, target :: item
        end function
      end interface

      select case (item_resolve_data( cp, type_{typeId}, item=self ))
        case (1); call c_f_unwrap( cp, tgt );  res = .true.
        {itemCast}
        case default;           tgt => null(); res = .false.
      end select
    end function
    """,

    # parameters:
    #   typeId:       type identifier
    #   baseType:     fortran base type | type(...) | procedure(...)
    #   baseType_arg: fortran argument base type | type(...) | procedure(...)
    #   baseSizeExpr: fortran expression to determine byte size of baseType
    #   dimSpec:      ('', ', dimension(:,...)')[has_dimension]
    #   initProc:     ', initProc = <funcId>'
    #   assignProc:   ', assignProc = <funcId>'
    #   deleteProc:   ', deleteProc = <funcId>'
    #   shapeProc:    ', shapeProc = <funcId>'
    #   streamProc:   ', streamProc = <funcId>'
    ref_typeinfo = """
!_PROC_EXPORT({typeId}_typeinfo_)
!_ARG_REFERENCE1(self)
    function {typeId}_typeinfo_( self ) result(res)
      {baseType_arg}{dimSpec}   :: self{subType_decl}
      type(TypeInfo_t), pointer :: res

      res => type_{typeId}
      if (.not. res%initialized) &
        call typeinfo_init( res, '{typeId}', '{baseType}' &
                          , int({baseSizeExpr},4), {rank} &
                          {lookupSubtype}{initProc}{acceptProc}{assignProc}{deleteProc}{shapeProc}{cloneProc}{streamProc} &
                          , cloneRefProc = {typeId}_clone_ref_ {tryStreamProc} )
    end function
    """,

    # parameters:
    #   typeId:       type identifier
    #   baseType_arg: fortran argument base type | type(...) | procedure(...)
    #   dimSpec:      ('', ', dimension(:,...)')[has_dimension]
    proc_typeinfo = """
!_PROC_EXPORT({typeId}_typeinfo_)
!_ARG_REFERENCE1(self)
    function {typeId}_typeinfo_( self ) result(res)
      {baseType_arg}{dimSpec}   :: self{subType_decl}
      type(TypeInfo_t), pointer :: res

      res => type_{typeId}
      if (.not. res%initialized) &
        call typeinfo_init( res, '{typeId}', '{baseType}', 0, 0 {lookupSubtype}{acceptProc}{streamProc}{tryStreamProc} )
    end function
    """
  )


  def __init__( self, access, typeId, baseType, dimType, keySpecs ):
    super(RefType, self).__init__( access, typeId, baseType, dimType )

    keySpecs  = dict( re.findall( self._keyAssign, keySpecs ) )
    typeProcs = dict( (k,v) for k,v in keySpecs.items() if k.endswith('Proc') )

    if keySpecs: self.keySpecStr = ', ' + ', '.join( '%s = %s' % i for i in keySpecs.items() )
    else       : self.keySpecStr = ''

    if self._isArray:
      typeProcs.setdefault( 'shapeProc', typeId + '_inspect_' )

    if self._isProc:
      keySpecs.setdefault( 'cloneMode', '_none' ) #< if not set explicitly, we disable cloning for procedure types
      keySpecs.setdefault( 'writeSize', '32' )    #< IMPORTANT: fortran might freak-out at determining the size of proc pointers!
      if typeProcs:
        sys.stderr.write( 'WARNING: given type procs {0} are ignored for procedure type "{1}"\n'.format(typeProcs, typeId) )

    typeProcs.setdefault( 'acceptProc', typeId + '_accept_wrap' )
    typeProcs.setdefault( 'streamProc', typeId + '_stream_wrap' )

    for procId in ('initProc', 'acceptProc', 'assignProc', 'deleteProc', 'shapeProc', 'streamProc'):
      procArg  = ''
      procName = typeProcs.get( procId, '' )
      if procName not in ('', '_none'):
        procArg = ', %s = %s' % (procId, procName)
      setattr( self, procId, procArg )
      setattr( self, '%sId' % procId, procName )

    # handle type cloning ...
    ptrClonerId     = keySpecs.get('cloneMode', '_shallow')               #< by default clone via shallow copy
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

    streamFmt  = self.peelString( keySpecs.setdefault( 'streamFmt', '' ) )
    streamType = keySpecs.get('streaming', 'buffered')
    streamTpl  = self._template['ref_streaming'][streamType]
    writeSize  = keySpecs.get( 'writeSize', 'ti%typeSpecs%streamLen' ) #< FORTRAN-COMPILERBUG: can't use storage_size on proc pointers!
    writeExpr  = ('self%ptr', "'%s'" % self.baseTypeId)[bool(self._isDerived)]
    doItemCast = keySpecs.get('itemCast', 'true').lower() in '1 true yes'.split()

    keySpecs.update(
      writeBuf  = ('{0}', 'size(self%ptr)*{0}')[self._isArray].format( writeSize ),
      writeExpr = self.peelString( keySpecs.get( 'writeExpr', writeExpr ) )
    )

    if streamFmt:
      keySpecs.update( writeFmt = 100, formatSpec = '\n100   format({0})'.format( streamFmt ) )
    else:
      keySpecs.update( writeFmt = '*', formatSpec = '' )
    keySpecs.update( streamWriting = streamTpl.format( **dict( self.__dict__, **keySpecs ) ) )

    self._itf          = ('ref_itf',           'proc_itf'         )[self._isProc]
    self._itemCastItf  = ('',                  'item_itf',        )[doItemCast]
    self._access       = ('access_ref',        'access_proc'      )[self._isProc]
    self._refCloner    = ('ref_cloner',        ''                 )[self._isProc]
    self._inspector    = ('ref_inspector',     ''                 )[self._isProc]
    self._typeinfo     = ('ref_typeinfo',      'proc_typeinfo'    )[self._isProc]
    self._acceptorItf  = ('',                  'ref_acceptorItf'  )['acceptProc' in keySpecs]
    self._acceptor     = ('ref_acceptor',      ''                 )['acceptProc' in keySpecs]
    self._streamerItf  = ('',                  'ref_streamerItf'  )['streamProc' in keySpecs]
    self._streamer     = ('ref_streamer',      ''                 )['streamProc' in keySpecs]
    self._itemcaster   = ('',                  'item_dynamic_cast')[doItemCast]

    if streamType == 'array':
      self.visitorGroup_beg = '\n      call group( vstr, size(wrap%ptr) )'
      self.visitorGroup_end = '\n      call group( vstr, -size(wrap%ptr) )'
    else:
      self.visitorGroup_beg = ''
      self.visitorGroup_end = ''

    if (streamType == 'buffered' and not (self._isArray or self._isProc)):
      self._tryStreamer  = ('ref_try_streaming_zero', 'ref_try_streaming')[self._isDerived]
      self.tryStreamProc = ', tryStreamProc = {0}_stream_try_'.format( typeId )
    else:
      self._tryStreamer  = ''
      self.tryStreamProc = ''

    self.lookupSubtype = ('', ', subtype = type_of(sub)')[self._isArray]

    self._kwArgs = dict( (k, self.peelString(v)) for k,v in keySpecs.items() )
    self.import_baseType = ('', 'import %s' % self.baseTypeId)[bool(self._isDerived)]

    # prepare ref-encoding and cloning
    if self._isArray:
      self.cloneSrcBounds   = ', lbound(src), ubound(src)'
      self.dimBounds        = ','.join( map( 'b(1,{0}):b(2,{0})'.format, range( 1, self.dimCount+1 ) ) )
      self.encoder_ptr_tgt  = 'mk_ptr( mk_bounds( shape(val), lb, ub ), val )'
      self.encoder_ptr_func = self._template['ref_encoder_mk_ptr'].format( **self.__dict__ )
    else:
      self.cloneSrcBounds   = ''
      self.encoder_ptr_tgt  = 'val'
      self.encoder_ptr_func = ''


  def declare( self, out ):
    if not self._declared:
      self.expand( out, 'info', 'type', 'common_itf', self._itf, self._itemCastItf, 'ref_streamItf', self._streamerItf,
                   'ref_acceptItf', self._acceptorItf )
      self.expandAccess( out, self.access, 'ref_of', 'type_of', 'dynamic_cast', 'stream', 'accept', 'c_f_unwrap' )
      self.expandAccessString( out, self.access, self._template[self._access] )
      TypeGenerator.setDeclaration( self.typeId, self )
      self._declared = True


  def implement( self, out ):
    if not self._implemented:
      self.expand( out, 'header', 'ref_encoder', 'ref_decoder', 'ref_typechecker', 'ref_unwrap', 'ref_dynamic_cast', self._itemcaster,
                   self._refCloner, self._inspector, self._typeinfo, 'ref_accept', self._acceptor )
      self.expand( out, 'ref_stream', self._tryStreamer, self._streamer, **self._kwArgs )
      out( self._ptrCloner.format( **self.__dict__ ) )
      self._implemented = True



class ListNode(TypeSpec):

  _template  = dict( TypeSpec._template,
    info = """
    !@ _TypeGen_declare_ListNode( {access}, {typeId}, {baseType}, {dimType} )""",

    type = """
    type, private :: {typeId}_node_t
      type(ListNode_t)    :: super
      {baseType}{dimSize} :: value
    end type
    type(TypeInfo_t), target :: type_{typeId}_node
    """,

    node_itf = """
    interface new_ListNode    ; module procedure {typeId}_new_node_    ; end interface
    interface new_ListNode_of ; module procedure {typeId}_new_node_of_ ; end interface
    interface node_type       ; module procedure {typeId}_nodetype_    ; end interface
    interface {typeId}        ; module procedure {typeId}_node_value_  ; end interface
    """,

    alias_itf = """
    interface new_ListNode_of ; module procedure {typeId}{aliasId}_new_node_of_ ; end interface
    """,

    access_itf = "new_ListNode, new_ListNode_of, node_type, {typeId}",

    node_type = """
!_PROC_EXPORT({typeId}_nodetype_)
!_ARG_REFERENCE1(val)
    function {typeId}_nodetype_( val ) result(res)
      {baseType_arg}{dimSpec}, intent(in) :: val
      type(TypeInfo_t),           pointer :: res
      type({typeId}_node_t)               :: node
      res => type_{typeId}_node
      if (.not. res%initialized) &
        call typeinfo_init( res, '{typeId}_node', 'type({typeId}_node_t)' &
                          , int(storage_size(node),4), 0, subtype = type_of(val) &
                          , cloneObjProc = {typeId}_clone_node_ )
    end function
    """,

    new_node = """
!_PROC_EXPORT({typeId}_new_node_)
!_ARG_REFERENCE1(valPtr)
    function {typeId}_new_node_( valPtr ) result(res)
      {baseType}{dimSpec}, pointer, intent(out) :: valPtr
      type(ListNode_t),                 pointer :: res
      type({typeId}_node_t),            pointer :: node => null()
      type(TypeInfo_t),                 pointer :: ti

      allocate( node )
      ti => type_of( node%value )
      if (associated( ti%initProc )) &
        call ti%initProc( node%value, 0 ) !< 0 => init value as default instance - no prototype!
      valPtr => node%value
      res    => node%super
      res%typeInfo => node_type(valPtr)
    end function
    """,

    new_node_of = """
!_PROC_EXPORT({typeId}_new_node_of_)
!_ARG_REFERENCE1(val)
    function {typeId}_new_node_of_( val ) result(res)
      {baseType_arg}{dimSpec}, intent(in) :: val
      type(ListNode_t),           pointer :: res
      {baseType}{dimSpec},        pointer :: val_ptr
      res     => {typeId}_new_node_( val_ptr )
      val_ptr =  val
    end function
    """,

    new_node_of_alias = """
!_PROC_EXPORT({typeId}{aliasId}_new_node_of_)
!_ARG_REFERENCE1(val)
    function {typeId}{aliasId}_new_node_of_( val ) result(res)
      {baseType_arg}{dimSpec},    intent(in) :: val
      type(ListNode_t),              pointer :: res
      {aliasBaseType}{aliasDimSpec}, pointer :: val_ptr
      res     => {typeId}_new_node_( val_ptr )
      val_ptr =  val
    end function
    """,

    clone_node = """
    subroutine {typeId}_clone_node_( tgt, src )
      type(ListNode_t), pointer, intent(out) :: tgt
      type({typeId}_node_t),      intent(in) :: src
      type({typeId}_node_t),         pointer :: node => null()
      type(TypeInfo_t),              pointer :: ti

      allocate( node )
      ti => type_of( node%value )
      if (associated( ti%initProc )) &
        call ti%initProc( node%value, 0 ) !< init value as default instance!
      node%value = src%value
      tgt => node%super
      tgt%typeInfo => node_type(src%value)
    end subroutine
    """,

    node_value = """
!_PROC_EXPORT({typeId}_node_value_)
!_ARG_REFERENCE1(idx)
    function {typeId}_node_value_( idx ) result(res)
      use iso_c_binding
      type(ListIndex_t)              :: idx
      {baseType}{dimSpec},   pointer :: res
      type({typeId}_node_t), pointer :: ptr
      call c_f_pointer( cptr(idx), ptr )
      res => ptr%value
    end function
    """
  )

  def __init__( self, access, typeId, baseType, dimType ):
    if access != 'alias':
      self.aliasId = ''
    else:
      self.aliasId = '_alias%d' % TypeGenerator.getAliasCount( typeId, self )
      access = 'private'
    super(ListNode, self).__init__( access, typeId, baseType, dimType )


  def declare( self, out ):
    if not self._declared:
      if self.aliasId: self.expand( out, 'info', 'alias_itf' )
      else           : self.expand( out, 'info', 'type', 'node_itf' )
      self.expandAccessString( out, self.access, self._template['access_itf'] )
      TypeGenerator.setDeclaration( self.typeId, self )
      self._declared = True


  def implement( self, out ):
    if not self._implemented:
      self.expand( out, 'header' )
      if self.aliasId:
        alias = TypeGenerator.getBaseDeclaration( self.typeId, self )
        self.expand( out, 'new_node_of_alias', aliasBaseType = alias.baseType, aliasDimSpec = alias.dimSpec )
      else:
        self.expand( out, 'new_node', 'new_node_of', 'node_type', 'clone_node', 'node_value' )
      self._implemented = True



class TypeGenerator(object):

  scope = dict()
  count = dict()

  _ident     = r'\s*(\w+)\s*'      #< some type identifier
  _baseType  = r'\s*([\w :*=()]+)' #< e.g. integer(kind=4), type(Struct), character(len=*), <interfaceId>, ...
  _dimType   = r'\s*([\w ,:()]+)'  #< e.g. scalar, dimension(:,:), procedure
  _keySpecs  = r'((?:,\s*\w+\s*=\s*.+\s*)*)'
  _typeDecl  = r'^\s*!\s*_TypeGen_declare_RefType\(%s,%s,%s,%s%s\)' % (_ident, _ident, _baseType, _dimType, _keySpecs)
  _nodeDecl  = r'^\s*!\s*_TypeGen_declare_ListNode\(%s,%s,%s,%s\)' % (_ident, _ident, _baseType, _dimType)
  _typeImpl  = r'^\s*!\s*_TypeGen_implement\(%s\)' % _ident
  _typeImplA = r'^\s*!\s*_TypeGen_implementAll\(\)'

  typeDeclMatch    = re.compile( _typeDecl ).match
  nodeDeclMatch    = re.compile( _nodeDecl ).match
  typeImplMatch    = re.compile( _typeImpl ).match
  typeImplAllMatch = re.compile( _typeImplA ).match


  @classmethod
  def setDeclaration( _class, key, decl ):
    key = '%s.%s' % (key, type(decl).__name__)
    _class.count[key] = idx = _class.count.get( key, 0 ) + 1
    if not getattr( decl, 'aliasId', None ):
      idx = 'base'
    _class.scope['%s.%s' % (key, idx)] = decl


  @classmethod
  def getBaseDeclaration( _class, key, decl ):
    return _class.scope[ '%s.%s.base' % (key, type(decl).__name__) ]


  @classmethod
  def getAliasCount( _class, key, decl ):
    return _class.count.get( '%s.%s' % (key, type(decl).__name__), 0 )


  @staticmethod
  def _purgeLines( buf ):
    for l in buf:
      l = l.strip()
      a = (0,1)[l.startswith('!')]
      b = (None,-1)[l.endswith('\\')]
      yield l[a:b]


  @classmethod
  def convert( _class, options ):
    import pdb

    if options.get('--debug'):
      pdb.set_trace()

    with open(options['FILE']) as f:

      outFile = options.get('--output')
      outChnl = outFile and open( outFile, 'w' ) or sys.stdout
      lineBuf = []

      for line in f.readlines():
        lineBuf.append( line.rstrip() )
        if lineBuf[-1].endswith('\\'):
          continue

        bufStr  = '\n'.join( lineBuf ) + '\n'
        lines   = '!' + ''.join( _class._purgeLines( lineBuf ) )
        lineBuf = []

        match = _class.typeDeclMatch( lines )
        if match:
          RefType( *map( str.strip, match.groups() ) ).declare( outChnl.write )
          continue

        match = _class.nodeDeclMatch( lines )
        if match:
          ListNode( *map( str.strip, match.groups() ) ).declare( outChnl.write )
          continue

        match = _class.typeImplMatch( lines )
        if match:
          _class.scope[ match.groups()[0] ].implement( outChnl.write )
          continue

        if _class.typeImplAllMatch( lines ):
          for decl in sorted( _class.scope.items() ):
            decl[1].implement( outChnl.write )
          continue

        outChnl.write( bufStr )

      outChnl.close()



if __name__ == '__main__':
  try:
    from docopt import docopt
  except ImportError:
    def docopt( *ignored ): #< define stupid emergency mockup
        return { 'FILE'    : sys.argv[1:2][0],
                 '--output': sys.argv[3:4][0],
                 '--debug' : sys.argv[4:5] }
  TypeGenerator.convert( docopt( __doc__ ) )


