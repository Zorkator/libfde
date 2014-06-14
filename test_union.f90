
module f_union
end module




program main
  use iso_c_binding
  implicit none

#if defined __GFORTRAN__
# define _paste(a,b)  a/**/b
#else
# define _paste(a,b)  a ## b
#endif

# define XtableItem(id)   _paste(foreach_,id)
# define Xtable_VarType \
    XtableItem(VarType)(logical,bool,none,none ) \
    XtableItem(VarType)(integer*1,byte,none, none ) \
    XtableItem(VarType)(integer*2,shortInt,none, none ) \
    XtableItem(VarType)(integer*4,int,none, none ) \
    XtableItem(VarType)(integer*8,longInt,none, none ) \
    XtableItem(VarType)(real*4,float,none,none ) \
    XtableItem(VarType)(real*8,double,none,none ) \
    XtableItem(VarType)(real*16,longDbl,none,none ) \
    XtableItem(VarType)(complex*8,cplx,none,none ) \
    XtableItem(VarType)(complex*16,dblCplx,none,none ) \
    XtableItem(VarType)(complex*32,quadCplx,none,none ) \
    XtableItem(VarType)(type(c_ptr),cptr,none,none )

  type VarItem
    integer*4 :: dat(8)
  end type

  interface VarItem
# define foreach_VarType(_type,_id,x,y) \
    _paste(VarItem_of_,_id),
    procedure Xtable_VarType &
              VarItem_of_VarItem
# undef foreach_VarType
  end interface

# define foreach_VarType(_type,_id,x,y) \
    interface _id; procedure _paste(VarItem_get_,_id); end interface;
    Xtable_VarType
# undef foreach_VarType

  interface assignment (=)
# define foreach_VarType(_type,_id,x,y) \
    _paste(VarItem_assign_,_id),
    procedure Xtable_VarType &
              VarItem_assign_VarItem
# undef foreach_VarType
  end interface



  type (VarItem) :: item
  complex        :: c

  c    = (2,4)
  item = VarItem(42)
  print *, item%dat
  print *, int(item)
  item = VarItem(42.0)
  print *, item%dat
  print *, float(item)
  print *, int(item)

  item = 17; print *, int(item)
  item = c; print *, cplx(item)
  item = 4.2334; print *, float(item)
  item = item; print *, float(item)

  contains

# define implementConstructor(_type,_id)                 \
    function _paste(VarItem_of_,_id)( val ) result(res) ;\
      _type,      intent(in) :: val                     ;\
      _type,         pointer :: ptr                     ;\
      type (VarItem), target :: res                     ;\
      call c_f_pointer( c_loc(res%dat(1)), ptr )        ;\
      ptr = val                                         ;\
    end function

  implementConstructor(logical,bool)
  implementConstructor(integer*1,byte)
  implementConstructor(integer*2,shortInt)
  implementConstructor(integer*4,int)
  implementConstructor(integer*8,longInt)
  implementConstructor(real*4,float)
  implementConstructor(real*8,double)
  implementConstructor(real*16,longDbl)
  implementConstructor(complex*8,cplx)
  implementConstructor(complex*16,dblCplx)
  implementConstructor(complex*32,quadCplx)
  implementConstructor(type(c_ptr),cptr)
  implementConstructor(type(VarItem),VarItem)


# define implementGetter(_type,_id)                        \
    function _paste(VarItem_get_,_id)( item ) result(res) ;\
      type (VarItem), target :: item                      ;\
      _type,         pointer :: res                       ;\
      call c_f_pointer( c_loc(item%dat(1)), res )         ;\
    end function

  implementGetter(logical,bool)
  implementGetter(integer*1,byte)
  implementGetter(integer*2,shortInt)
  implementGetter(integer*4,int)
  implementGetter(integer*8,longInt)
  implementGetter(real*4,float)
  implementGetter(real*8,double)
  implementGetter(real*16,longDbl)
  implementGetter(complex*8,cplx)
  implementGetter(complex*16,dblCplx)
  implementGetter(complex*32,quadCplx)
  implementGetter(type(c_ptr),cptr)


# define implementAssignment(_type,_id)                 \
    subroutine _paste(VarItem_assign_,_id)( lhs, rhs ) ;\
      type (VarItem), target, intent(inout) :: lhs     ;\
      _type,                  intent(in)    :: rhs     ;\
      _type,                  pointer       :: ptr     ;\
      call c_f_pointer( c_loc(lhs%dat(1)), ptr )       ;\
      ptr = rhs                                        ;\
    end subroutine

  implementAssignment(logical,bool)
  implementAssignment(integer*1,byte)
  implementAssignment(integer*2,shortInt)
  implementAssignment(integer*4,int)
  implementAssignment(integer*8,longInt)
  implementAssignment(real*4,float)
  implementAssignment(real*8,double)
  implementAssignment(real*16,longDbl)
  implementAssignment(complex*8,cplx)
  implementAssignment(complex*16,dblCplx)
  implementAssignment(complex*32,quadCplx)
  implementAssignment(type(c_ptr),cptr)

  subroutine VarItem_assign_VarItem( lhs, rhs )
    type (VarItem), target, intent(inout) :: lhs
    type (VarItem),         intent(in)    :: rhs
    lhs%dat = rhs%dat
  end subroutine


end

