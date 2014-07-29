
#include "adt/itfUtil.fpp"

module generic_list
  implicit none

  type, public :: List
    private
    type (List), pointer :: prev => null(), next => null()
  end type


  type, public :: ListIterator
    private
    type (List), public, pointer :: node => null()
    type (List),         pointer :: head => null()
    integer                      :: step = 1
  end type


  interface operator (+); module procedure gli_increment            ; end interface
  interface operator (-); module procedure gli_decrement            ; end interface

  interface initialize  ; module procedure gl_initialize            ; end interface
  interface is_valid    ; module procedure gl_is_valid, gli_is_valid; end interface
  interface is_empty    ; module procedure gl_is_empty              ; end interface
  interface appendList  ; module procedure gl_append_list           ; end interface
  interface front       ; module procedure gl_front                 ; end interface
  interface back        ; module procedure gl_back                  ; end interface
  interface iterate     ; module procedure gl_iterate               ; end interface
  interface reverse     ; module procedure gl_reverse               ; end interface
  interface frontInsert ; module procedure gl_front_insert          ; end interface
  interface backInsert  ; module procedure gl_back_insert           ; end interface
  interface insert      ; module procedure gli_insert               ; end interface
  interface unlink      ; module procedure gli_unlink               ; end interface

  public :: initialize
  public :: is_valid
  public :: is_empty   
  public :: front      
  public :: back       
  public :: iterate    
  public :: reverse    
  public :: frontInsert
  public :: backInsert 
  public :: insert
  public :: unlink

  contains

  !PROC_EXPORT_1REF( gl_initialize, self )
  subroutine gl_initialize( self )
    type (List), target :: self
    self%prev => self
    self%next => self
  end subroutine


  !PROC_EXPORT_1REF( gl_try_initialize, self )
  subroutine gl_try_initialize( self )
    type (List), target :: self

    if (.not. (associated(self%prev) .and. associated(self%next))) then
      self%prev => self
      self%next => self
    end if
  end subroutine


  !PROC_EXPORT_1REF( gl_is_valid, self )
  logical function gl_is_valid( self ) result(res)
    type (List), intent(in) :: self
    res = associated(self%prev) .and. associated(self%next)
  end function


  !PROC_EXPORT_1REF( gl_is_empty, self )
  logical function gl_is_empty( self ) result(res)
    type (List), target, intent(in) :: self
    res = associated( self%prev, self ) .and. associated( self%next, self )
  end function


  !PROC_EXPORT_1REF( gl_front, self )
  function gl_front( self ) result(res)
    type (List), intent(in) :: self
    type (List),    pointer :: res
    res => self%next
  end function

    
  !PROC_EXPORT_1REF( gl_back, self )
  function gl_back( self ) result(res)
    type (List), intent(in) :: self
    type (List),    pointer :: res
    res => self%prev
  end function

    
  subroutine gl_link_node( node, prev, next )
    type (List), target :: node, prev, next
    next%prev => node
    node%next => next
    node%prev => prev
    prev%next => node
  end subroutine


  subroutine gl_unlink_node( prev, next )
    type (List), target :: prev, next
    prev%next => next
    next%prev => prev
  end subroutine


  subroutine gl_prepend_node( self, node )
    type (List), target :: self, node
    call gl_link_node( node, self, self%next )
  end subroutine


  subroutine gl_append_node( self, node )
    type (List), target :: self, node
    call gl_link_node( node, self%prev, self )
  end subroutine


  subroutine gl_remove_node( node )
    type (List), target :: node
    call gl_unlink_node( node%prev, node%next )
    node%prev => null()
    node%next => null()
  end subroutine


  subroutine gl_replace_node( old, new )
    type (List), target :: old, new
    call gl_unlink_node( old%prev, old%next )
    call gl_link_node( new, old%prev, old%next )
    old%prev => null()
    old%next => null()
  end subroutine


  subroutine gl_move_node( node, trgt )
    type (List), target :: node, trgt
    call gl_unlink_node( node%prev, node%next )
    call gl_link_node( node, trgt, trgt%next )
  end subroutine


  subroutine gl_tail_move_node( node, trgt )
    type (List), target :: node, trgt
    call gl_unlink_node( node%prev, node%next )
    call gl_link_node( node, trgt%prev, trgt )
  end subroutine


  !PROC_EXPORT_2REF( gl_append_list, self, other )
  subroutine gl_append_list( self, other )
    type (List), target :: self
    type (List)         :: other
    ! link list items of other ...
    ! it's ok if other is empty
    self%prev%next  => other%next
    other%next%prev => self%prev
    self%prev       => other%prev
    other%prev%next => self
    ! and clear other by initializing it
    call gl_initialize( other )
  end subroutine


  !PROC_EXPORT_1REF( gl_iterate, self )
  function gl_iterate( self ) result(res)
    type (List), target, intent(in) :: self
    type (ListIterator)             :: res
    res%head => self
    res%node => self%next
    res%step  = 1
  end function


  !PROC_EXPORT_1REF( gl_reverse, self )
  function gl_reverse( self ) result(res)
    type (List), target, intent(in) :: self
    type (ListIterator)             :: res
    res%head => self
    res%node => self%prev
    res%step = -1
  end function


  !PROC_EXPORT_1REF( gl_front_insert, self )
  function gl_front_insert( self ) result(res)
    type (List), target, intent(in) :: self
    type (ListIterator)             :: res
    res%head => self
    res%node => self
    res%step = -1
  end function


  !PROC_EXPORT_1REF( gl_back_insert, self )
  function gl_back_insert( self ) result(res)
    type (List), target, intent(in) :: self
    type (ListIterator)             :: res
    res%head => self
    res%node => self
    res%step = 1
  end function


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! ListIterator routines
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  !PROC_EXPORT_1REF( gli_is_valid, self )
  logical function gli_is_valid( self ) result(res)
    type (ListIterator), intent(in) :: self
    res = associated( self%node ) .and. .not. associated( self%node, self%head )
  end function


  !PROC_EXPORT_1REF( gli_increment, self )
  function gli_increment( self, val ) result(res)
    type (ListIterator), intent(in) :: self
    integer,             intent(in) :: val
    type (ListIterator)             :: res
    integer                         :: cnt
    res = self
    cnt = gli_advance( res, val )
  end function


  !PROC_EXPORT_1REF( gli_decrement, self )
  function gli_decrement( self, val ) result(res)
    type (ListIterator), intent(in) :: self
    integer,             intent(in) :: val
    type (ListIterator)             :: res
    integer                         :: cnt
    res = self
    cnt = gli_advance( res, -val )
  end function

 
  !PROC_EXPORT_1REF( gli_advance, self )
  integer function gli_advance( self, val ) result(stepsOk)
    type (ListIterator), intent(inout) :: self
    integer                            :: val, steps
    
    steps = self%step * val
    if (steps >= 0) then
      do stepsOk = 0, steps-1
        if (associated( self%node, self%head )) then
          exit
        end if
        self%node => self%node%next
      end do
    else
      do stepsOk = 0, -steps-1
        if (associated( self%node, self%head )) then
          exit
        end if
        self%node => self%node%prev
      end do
    end if
    if (.not. is_valid(self)) &
      stepsOk = stepsOk - 1
  end function


  !PROC_EXPORT_2REF( gli_insert, self, node )
  subroutine gli_insert( self, node )
    type (ListIterator), intent(inout) :: self
    type (List)                        :: node
    if (self%step >= 0) then
      call gl_link_node( node, self%node%prev, self%node )
    else
      call gl_link_node( node, self%node, self%node%next )
    end if
  end subroutine


  !PROC_EXPORT_1REF( gli_unlink, self )
  function gli_unlink( self ) result(res)
    type (ListIterator), intent(inout) :: self
    type (ListIterator)                :: res
    res = self
    if (gli_advance( self, 1 ) > -1) then
      call gl_remove_node( res%node )
    end if
  end function

end module


!##################################################################################################
#ifdef TEST

# define _TYPE  type(VarItem)

program testinger
  use generic_list
  use dynamic_string
  use var_item
  implicit none

  type :: ValList
    type (List) :: super
    _TYPE   :: val
  end type

  interface ValListItem; procedure vallist_new      ; end interface
  interface ValList    ; procedure vallist_downcast ; end interface
  interface deref      ; procedure vallist_deref_itr; end interface
  interface delete     ; procedure vallist_delete   ; end interface

  type (ValList)          :: vlist
  type (ValList), pointer :: item => null()
  type (ListIterator)     :: itr
  integer :: i
  character(len=5)           :: buff

  call initialize( vlist%super )

  itr = frontInsert(vlist%super)
  do i = 1, 20
    write(buff, '(I1)') i
    call insert( itr, ValListItem( VarItem( trim(buff) )) )
  end do

  itr = iterate( vlist%super )
  do while (is_valid(itr))
    print *, ref(string(deref(itr)))
    itr = itr + 1
  end do
  
  call delete( vlist )

  contains

  ! create new item
  function vallist_new( val ) result(res)
    _TYPE,   intent(in) :: val
    type (List),    pointer :: res
    type (ValList), pointer :: ptr
    allocate( ptr )
    ptr%val = val
    res => ptr%super
  end function

  ! downcast
  function vallist_downcast( obj ) result(res)
    use iso_c_binding
    type (List), target, intent(in) :: obj
    type (ValList),         pointer :: res
    call c_f_pointer( c_loc(obj), res )
  end function

  ! deref value from iterator
  function vallist_deref_itr( itr ) result(res)
    use iso_c_binding
    type (ListIterator), intent(in) :: itr
    _TYPE,                  pointer :: res
    type (ValList),         pointer :: node
    call c_f_pointer( c_loc(itr%node), node )
    res  => node%val
  end function

  ! clear list
  subroutine vallist_delete( self )
    type (ValList), intent(inout) :: self
    type (ListIterator)           :: itr, delItr
    itr = iterate( self%super )
    do while (is_valid(itr))
      delItr = unlink( itr )
      call delete( deref(delItr) )
      deallocate( delItr%node )
    end do
  end subroutine

end program

#endif 

