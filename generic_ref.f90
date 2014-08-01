
module generic_ref
  use dynamic_string
  use iso_c_binding
  implicit none
  private


  type, public :: TypeInfo
    character(32)                :: typeId;     integer*2, private :: typeId_term   = 0
    character(32)                :: baseType;   integer*2, private :: baseType_term = 0
    integer*4                    :: byteSize    =  0
    integer*4                    :: rank        =  0
    logical                      :: initialized = .false.

    ! type specific subroutines called by generic interfaces 
    procedure(), nopass, pointer :: assignProc  => null()
    procedure(), nopass, pointer :: shapeProc   => null()
    procedure(), nopass, pointer :: cloneProc   => null()
    procedure(), nopass, pointer :: deleteProc  => null()
  end type


  type(TypeInfo), target :: type_void = TypeInfo( "void", 0, "", 0, 0, 0, .true., &
                                                   null(), null(), null(), null() )


  type, public :: GenericRef
    private
    type(DynamicString_t)   :: ref_str
    type(TypeInfo), pointer :: typeInfo => null()
  end type


  type :: deref_t
    private
    type(GenericRef), pointer :: ptr
  end type
  type(TypeInfo), target :: TypeInfo_deref

  
  ! interface definitions

  interface assignment(=)
    module procedure gr_assign_gr
  end interface

  interface rank       ; module procedure gr_rank        ; end interface
  interface shape      ; module procedure gr_shape       ; end interface
  interface clone      ; module procedure gr_clone       ; end interface
  interface cptr       ; module procedure gr_cptr        ; end interface
  interface delete     ; module procedure gr_delete      ; end interface
  interface free       ; module procedure gr_free        ; end interface
  interface ref        ; module procedure gr_encode_deref; end interface
  interface deref      ; module procedure gr_decode_deref; end interface
  interface is_ref     ; module procedure gr_is_ref      ; end interface
  interface typeinfo_of; module procedure gr_typeinfo_of ; end interface

  ! declare public interfaces 

  public :: assignment(=)
  public :: gr_init_TypeInfo      !< needed by generated code
  public :: gr_set_TypeReference  !<            "
  public :: gr_get_TypeReference  !<            "
  public :: rank
  public :: shape
  public :: clone
  public :: cptr
  public :: delete
  public :: free
  public :: type_void
  public :: gr_assign_gr, gr_delete
  public :: ref
  public :: deref
  public :: is_ref
  public :: typeinfo_of

  type :: voidRef ; integer, pointer :: ptr; end type
  
!-----------------
  contains
!-----------------

  !**
  ! gr_init_TypeInfo initializes TypeInfo structure.
  ! @param self        - the TypeInfo to initialize
  ! @param typeId      - the type's id string (e.g. double)
  ! @param baseType    - the type's base string (e.g. real*8)
  ! @param bitSize     - the storage size of the type in bytes (=> storage_size(type))
  ! @param rank        - the rank of the type
  ! @param assignProc  - the subroutine to assign a variable: subroutine assign( lhs, rhs )
  ! @param deleteProc  - the subroutine to delete a variable: subroutine delete( var )
  ! @param shapeProc   - the function to inspect the shape  : function getShape( var ) return(res)
  ! @param cloneProc   - the function to clone a variable   : function getClone( var ) return(res)
  !*
  !PROC_EXPORT_1REF( gr_init_TypeInfo, self )
  subroutine gr_init_TypeInfo( self, typeId, baseType, bitSize, rank, &
                               assignProc, deleteProc, shapeProc, cloneProc )
    type(TypeInfo),    intent(inout) :: self
    character(len=*),     intent(in) :: typeId, baseType
    integer*4,            intent(in) :: bitSize
    integer*4,            intent(in) :: rank
    procedure(),            optional :: assignProc, deleteProc, shapeProc, cloneProc

    self%typeId   = adjustl(typeId);   self%typeId_term   = 0
    self%baseType = adjustl(baseType); self%baseType_term = 0
    self%byteSize = bitSize/8
    self%rank     = rank

    ! pre-initialize optional arguments
    self%assignProc => null()
    self%deleteProc => null()
    self%shapeProc  => null()
    self%cloneProc  => null()

    if (present(assignProc)) self%assignProc => assignProc
    if (present(deleteProc)) self%deleteProc => deleteProc
    if (present(shapeProc))  self%shapeProc  => shapeProc
    if (present(cloneProc))  self%cloneProc  => cloneProc
    self%initialized = .true.
  end subroutine


!--------------------------------------------------------------
!   generic_ref
!--------------------------------------------------------------

  subroutine gr_assign_gr( lhs, rhs )
    type(GenericRef), intent(inout) :: lhs
    type(GenericRef),    intent(in) :: rhs

    lhs%ref_str  =  rhs%ref_str
    lhs%typeInfo => rhs%typeInfo
  end subroutine


  function gr_set_TypeReference( self, cptr, bits, ti ) result(needInit)
    type(GenericRef), intent(inout) :: self
    type(c_ptr),         intent(in) :: cptr
    integer*4,           intent(in) :: bits
    type(TypeInfo),          target :: ti
    character(len=bits/8),  pointer :: ptr
    logical                         :: needInit

    call c_f_pointer( cptr, ptr )
    self%ref_str  = attrib_volatile
    self%ref_str  = ptr
    needInit      = .not. ti%initialized
    self%typeInfo => ti
  end function


  function gr_get_TypeReference( self ) result(res)
    type(GenericRef), intent(in) :: self
    type(c_ptr)                  :: res
    res = cptr(self%ref_str)
  end function


  pure function gr_rank( self ) result(res)
    type(GenericRef), intent(in) :: self
    integer                      :: res

    if (associated( self%typeInfo )) then
      res = self%typeInfo%rank
    else
      res = 0
    end if
  end function


  pure function gr_shape( self ) result(res)
    type(GenericRef), intent(in) :: self
    integer                      :: res(rank(self))

    if (associated( self%typeInfo )) then
      if (associated( self%typeInfo%shapeProc )) then
        call self%typeInfo%shapeProc( self, res, self%typeInfo%rank )
        return
      end if
    end if
    res = 0
  end function


  function gr_clone( self ) result(res)
    type(GenericRef), intent(in) :: self
    type(GenericRef)             :: res

    res%ref_str = attrib_volatile
    if (associated( self%typeInfo )) then
      if (associated( self%typeInfo%cloneProc )) then
        call self%typeInfo%cloneProc( self, res )
        return
      end if
    end if
    res = self
  end function


  function gr_cptr( self ) result(res)
    type(GenericRef), intent(in) :: self
    type(c_ptr)                  :: res
    type(voidRef),       pointer :: wrap

    res = gr_get_TypeReference(self)
    if (c_associated(res)) then
      call c_f_pointer( res, wrap )
      res = c_loc(wrap%ptr)
    end if
  end function


  subroutine gr_delete( self )
    type(GenericRef) :: self

    call delete( self%ref_str )
    self%typeInfo => null()
  end subroutine


  subroutine gr_free( self )
    type(GenericRef)       :: self
    type(voidRef), pointer :: wrap
    type(c_ptr)            :: cptr

    cptr = gr_get_TypeReference(self)
    if (c_associated( cptr )) then
      call c_f_pointer( cptr, wrap )

      if (associated( wrap%ptr )) then
        if (associated( self%typeInfo )) then
          if (associated( self%typeInfo%deleteProc )) &
            call self%typeInfo%deleteProc( wrap%ptr )
        end if
        deallocate( wrap%ptr )
      end if
    end if
    call delete( self )
  end subroutine


!--------------------------------------------------------------
!   deref
!--------------------------------------------------------------
  
  function gr_encode_deref( val ) result(res)
    use iso_c_binding
    type(GenericRef), target, intent(in) :: val
    type(GenericRef)                     :: res
    type(deref_t),                target :: wrap
    procedure(),                  pointer :: None => null()
  
    wrap%ptr => val
    if (gr_set_TypeReference( res, c_loc(wrap), int(storage_size(wrap),4), TypeInfo_deref )) &
      call gr_init_TypeInfo( TypeInfo_deref, 'deref', 'type(GenericRef)' &
                             , int(storage_size(val),4) &
                             , size(shape(val)), assignProc = None, deleteProc = deref_deleter &
                             , cloneProc = gr_clone_deref )
  end function
  

  function gr_decode_deref( val ) result(res)
    use iso_c_binding
    type(GenericRef), intent(in) :: val
    type(GenericRef),    pointer :: res
    type(deref_t),       pointer :: wrap
    
    call c_f_pointer( gr_get_TypeReference(val), wrap )
    res => wrap%ptr
  end function
  

  subroutine gr_clone_deref( val, res )
    use iso_c_binding
    type(GenericRef),            intent(in) :: val
    type(GenericRef)                        :: res
    type(GenericRef),               pointer :: src, tgt => null()
    character(len=1), dimension(:), pointer :: tmp
  
    src => gr_decode_deref( val )
    tgt => deref_cloner( src )
  
    res =  gr_encode_deref( tgt )
  end subroutine
  

  subroutine gr_inspect_deref( val, res, n )
    type(GenericRef), intent(in) :: val
    integer                      :: n
    integer                      :: res(n)
    res(:n) = shape( gr_decode_deref( val ) )
  end subroutine
  

  function deref_cloner( val ) result(res)
    type(GenericRef), intent(in) :: val
    type(GenericRef),    pointer :: res
    allocate( res ) !< initializes res as default GenericRef
    res = val
  end function


  subroutine deref_deleter( val )
    type(GenericRef) :: val
    call delete( val )
  end subroutine

  
  function gr_is_ref( self ) result(res)
    type(GenericRef), intent(in) :: self
    logical                       :: res
    res = associated( self%typeInfo, TypeInfo_deref )
  end function

  
  function gr_typeinfo_of( self ) result(res)
    type(GenericRef), intent(in) :: self
    type(TypeInfo),      pointer :: res
    res => self%typeInfo
  end function

end module

