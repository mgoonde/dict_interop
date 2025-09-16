#define ASSERT( rhsdt, dt, rhsdr, dr, me ) \
block;\
character(len=8) :: w;\
character(len=256) :: loc;\
write(loc,"(a,'at: ',a,'::',i0)") new_line('a'),__FILE__,__LINE__;\
if( rhsdt /= dt ) call assignment_error("dtype/="//get_dtype_str(dt)//" to "//me//trim(loc));\
write(w,"(i0)") dr;\
if( rhsdr /= dr ) call assignment_error("drank/="//trim(w)//" to "//me//trim(loc));\
end block


submodule(m_dbval) m_dbval_assignments

  implicit none

contains

  subroutine assignment_error( msg )
    character(*), intent(in) :: msg
    write(*,"(a)") ">> db assignment error:"//msg
    error stop 1
  end subroutine assignment_error



  ! hard-copy


  ! The below are assingments for:
  ! ```
  ! type( dbval ) :: t
  ! integer, allocatable :: i1(:)
  ! t = db_get_cpy( db, "key", reshape=[n] )
  ! i1 = t
  ! ! instead do directly:
  ! i1 = db_get_cpy( db, "key", reshape=[n] )
  ! ```
  ! i
  module procedure assign_dbval_i0
    integer(c_int), pointer :: valptr
    ASSERT( rhs%dtype, DTYPE_INT, rhs%drank, 0, "i0" )
    call c_f_pointer( rhs%cptr, valptr )
    lhs = int( valptr, kind(lhs) )
  end procedure
  module procedure assign_dbval_i1
    integer(c_int), pointer :: valptr(:)
    ASSERT( rhs%dtype, DTYPE_INT, rhs%drank, 1, "i1")
    call c_f_pointer( rhs%cptr, valptr, shape=rhs%dsize )
    allocate( lhs, source=int(valptr, kind(lhs)) )
  end procedure
  module procedure assign_dbval_i2
    integer(c_int), pointer :: valptr(:,:)
    ASSERT( rhs%dtype, DTYPE_INT, rhs%drank, 2, "i2" )
    call c_f_pointer( rhs%cptr, valptr, shape=rhs%dsize )
    allocate( lhs, source=int(valptr, kind(lhs)) )
  end procedure
  module procedure assign_dbval_i3
    integer(c_int), pointer :: valptr(:,:,:)
    ASSERT( rhs%dtype, DTYPE_INT, rhs%drank, 3, "i3")
    call c_f_pointer( rhs%cptr, valptr, shape=rhs%dsize )
    allocate( lhs, source=int(valptr, kind(lhs)) )
  end procedure
  module procedure assign_dbval_i4
    integer(c_int), pointer :: valptr(:,:,:,:)
    ASSERT(rhs%dtype, DTYPE_INT, rhs%drank, 4, "i4")
    call c_f_pointer( rhs%cptr, valptr, shape=rhs%dsize )
    allocate( lhs, source=int(valptr, kind(lhs)) )
  end procedure
  ! rf
  module procedure assign_dbval_rf0
    real(c_float), pointer :: valptr
    ASSERT(rhs%dtype, DTYPE_REAL32, rhs%drank, 0, "rf0")
    call c_f_pointer( rhs%cptr, valptr )
    lhs = real( valptr, kind(lhs) )
  end procedure
  module procedure assign_dbval_rf1
    real(c_float), pointer :: valptr(:)
    ASSERT(rhs%dtype, DTYPE_REAL32, rhs%drank, 1, "rf1")
    call c_f_pointer( rhs%cptr, valptr, shape=rhs%dsize )
    allocate( lhs, source=real(valptr, kind(lhs)) )
    ! call rhs%destroy()
  end procedure
  module procedure assign_dbval_rf2
    real(c_float), pointer :: valptr(:,:)
    ASSERT(rhs%dtype, DTYPE_REAL32, rhs%drank, 2, "rf2")
    call c_f_pointer( rhs%cptr, valptr, shape=rhs%dsize )
    allocate( lhs, source=real(valptr, kind(lhs)) )
  end procedure
  module procedure assign_dbval_rf3
    real(c_float), pointer :: valptr(:,:,:)
    ASSERT( rhs%dtype, DTYPE_REAL32, rhs%drank, 3, "rf3")
    call c_f_pointer( rhs%cptr, valptr, shape=rhs%dsize )
    allocate( lhs, source=real(valptr, kind(lhs)) )
  end procedure
  module procedure assign_dbval_rf4
    real(c_float), pointer :: valptr(:,:,:,:)
    ASSERT( rhs%dtype, DTYPE_REAL32, rhs%drank, 4, "rf4")
    call c_f_pointer( rhs%cptr, valptr, shape=rhs%dsize )
    allocate( lhs, source=real(valptr, kind(lhs)) )
  end procedure
  ! rd
  module procedure assign_dbval_rd0
    real(c_double), pointer :: valptr
    ASSERT( rhs%dtype, DTYPE_REAL64, rhs%drank, 0, "rd0")
    call c_f_pointer( rhs%cptr, valptr )
    lhs = real( valptr, kind(lhs) )
  end procedure
  module procedure assign_dbval_rd1
    real(c_double), pointer :: valptr(:)
    ASSERT(rhs%dtype, DTYPE_REAL64, rhs%drank, 1, "rd1")
    call c_f_pointer( rhs%cptr, valptr, shape=rhs%dsize )
    allocate( lhs, source=real(valptr, kind(lhs)) )
  end procedure
  module procedure assign_dbval_rd2
    real(c_double), pointer :: valptr(:,:)
    ASSERT(rhs%dtype, DTYPE_REAL64, rhs%drank, 2, "rd2")
    call c_f_pointer( rhs%cptr, valptr, shape=rhs%dsize )
    allocate( lhs, source=real(valptr, kind(lhs)) )
  end procedure
  module procedure assign_dbval_rd3
    real(c_double), pointer :: valptr(:,:,:)
    ASSERT(rhs%dtype, DTYPE_REAL64, rhs%drank, 3, "rd3")
    call c_f_pointer( rhs%cptr, valptr, shape=rhs%dsize )
    allocate( lhs, source=real(valptr, kind(lhs)) )
  end procedure
  module procedure assign_dbval_rd4
    real(c_double), pointer :: valptr(:,:,:,:)
    ASSERT(rhs%dtype, DTYPE_REAL64, rhs%drank, 4, "rd4")
    call c_f_pointer( rhs%cptr, valptr, shape=rhs%dsize )
    allocate( lhs, source=real(valptr, kind(lhs)) )
  end procedure
  ! b
  module procedure assign_dbval_b0
    logical(c_bool), pointer :: valptr
    ASSERT( rhs%dtype, DTYPE_BOOL, rhs%drank, 0, "b0" )
    call c_f_pointer( rhs%cptr, valptr )
    lhs = logical(valptr, kind(lhs))
  end procedure
  module procedure assign_dbval_b1
    logical(c_bool), pointer :: valptr(:)
    ASSERT( rhs%dtype, DTYPE_BOOL, rhs%drank, 1, "b1" )
    call c_f_pointer( rhs%cptr, valptr, shape=rhs%dsize )
    allocate( lhs, source=logical(valptr, kind(lhs)) )
  end procedure
  module procedure assign_dbval_b2
    logical(c_bool), pointer :: valptr(:,:)
    ASSERT( rhs%dtype, DTYPE_BOOL, rhs%drank, 2, "b2" )
    call c_f_pointer( rhs%cptr, valptr, shape=rhs%dsize )
    allocate( lhs, source=logical(valptr, kind(lhs)) )
  end procedure
  module procedure assign_dbval_b3
    logical(c_bool), pointer :: valptr(:,:,:)
    ASSERT( rhs%dtype, DTYPE_BOOL, rhs%drank, 3, "b3" )
    call c_f_pointer( rhs%cptr, valptr, shape=rhs%dsize )
    allocate( lhs, source=logical(valptr, kind(lhs)) )
  end procedure
  module procedure assign_dbval_b4
    logical(c_bool), pointer :: valptr(:,:,:,:)
    ASSERT( rhs%dtype, DTYPE_BOOL, rhs%drank, 4, "b4" )
    call c_f_pointer( rhs%cptr, valptr, shape=rhs%dsize )
    allocate( lhs, source=logical(valptr, kind(lhs)) )
  end procedure
  ! str
  module procedure assign_dbval_str
    ASSERT( rhs%dtype, DTYPE_STR, rhs%drank, 0, "str" )
    lhs = rhs%str
  end procedure




  ! The below are assingments for:
  ! ```
  ! type( dbval_ptr ) :: t
  ! integer(c_int), pointer :: i1(:)
  ! t = db_get_ptr( db, "key", reshape=[n] )
  ! i1 = t
  ! ! instead do directly:
  ! i1 = db_get_ptr( db, "key", reshape=[n] )
  ! ```
  ! In order to differentiate the normal assignment from pointer assingment,
  ! an auxiliary type is created `dbval_ptr`. The function returning pointer to
  ! `dbval` actually returns type `dbval_ptr`, which is then used in assignment
  ! overloading.
  ! The argument `reshape` is saved into `dtype_ptr`, and used at assignment
  ! to reshape the data.

  ! ptr: the dsize is read from dbval_ptr, since it could be overwritten by assign
  module procedure assign_dbval_ptr_i0
    ASSERT( rhs%dbval%dtype, DTYPE_INT, rhs%drank, 0, "ptr_i0")
    call c_f_pointer( rhs%dbval%cptr, lhs )
  end procedure
  module procedure assign_dbval_ptr_i1
    ASSERT( rhs%dbval%dtype, DTYPE_INT, rhs%drank, 1, "ptr_i1")
    call c_f_pointer( rhs%dbval%cptr, lhs, shape=rhs%dsize )
  end procedure
  module procedure assign_dbval_ptr_i2
    ASSERT( rhs%dbval%dtype, DTYPE_INT, rhs%drank, 2, "ptr_i2")
    call c_f_pointer( rhs%dbval%cptr, lhs, shape=rhs%dsize )
  end procedure
  module procedure assign_dbval_ptr_i3
    ASSERT( rhs%dbval%dtype, DTYPE_INT, rhs%drank, 3, "ptr_i3")
    call c_f_pointer( rhs%dbval%cptr, lhs, shape=rhs%dsize )
  end procedure
  module procedure assign_dbval_ptr_i4
    ASSERT( rhs%dbval%dtype, DTYPE_INT, rhs%drank, 4, "ptr_i4")
    call c_f_pointer( rhs%dbval%cptr, lhs, shape=rhs%dsize )
  end procedure
  ! rf
  module procedure assign_dbval_ptr_rf0
    ASSERT( rhs%dbval%dtype, DTYPE_REAL32, rhs%drank, 0, "ptr_rf0")
    call c_f_pointer( rhs%dbval%cptr, lhs )
  end procedure
  module procedure assign_dbval_ptr_rf1
    ASSERT( rhs%dbval%dtype, DTYPE_REAL32, rhs%drank, 1, "ptr_rf1")
    call c_f_pointer( rhs%dbval%cptr, lhs, shape=rhs%dsize )
  end procedure
  module procedure assign_dbval_ptr_rf2
    ASSERT( rhs%dbval%dtype, DTYPE_REAL32, rhs%drank, 2, "ptr_rf2")
    call c_f_pointer( rhs%dbval%cptr, lhs, shape=rhs%dsize )
  end procedure
  module procedure assign_dbval_ptr_rf3
    ASSERT( rhs%dbval%dtype, DTYPE_REAL32, rhs%drank, 3, "ptr_rf3")
    call c_f_pointer( rhs%dbval%cptr, lhs, shape=rhs%dsize )
  end procedure
  module procedure assign_dbval_ptr_rf4
    ASSERT( rhs%dbval%dtype, DTYPE_REAL32, rhs%drank, 4, "ptr_rf4")
    call c_f_pointer( rhs%dbval%cptr, lhs, shape=rhs%dsize )
  end procedure
  ! rd
  module procedure assign_dbval_ptr_rd0
    ASSERT( rhs%dbval%dtype, DTYPE_REAL64, rhs%drank, 0, "ptr_rd0")
    call c_f_pointer( rhs%dbval%cptr, lhs )
  end procedure
  module procedure assign_dbval_ptr_rd1
    ASSERT( rhs%dbval%dtype, DTYPE_REAL64, rhs%drank, 1, "ptr_rd1")
    call c_f_pointer( rhs%dbval%cptr, lhs, shape=rhs%dsize )
  end procedure
  module procedure assign_dbval_ptr_rd2
    ASSERT( rhs%dbval%dtype, DTYPE_REAL64, rhs%drank, 2, "ptr_rd2")
    call c_f_pointer( rhs%dbval%cptr, lhs, shape=rhs%dsize )
  end procedure
  module procedure assign_dbval_ptr_rd3
    ASSERT( rhs%dbval%dtype, DTYPE_REAL64, rhs%drank, 3, "ptr_rd3")
    call c_f_pointer( rhs%dbval%cptr, lhs, shape=rhs%dsize )
  end procedure
  module procedure assign_dbval_ptr_rd4
    ASSERT( rhs%dbval%dtype, DTYPE_REAL64, rhs%drank, 4, "ptr_rd4")
    call c_f_pointer( rhs%dbval%cptr, lhs, shape=rhs%dsize )
  end procedure
  ! b
  module procedure assign_dbval_ptr_b0
    ASSERT( rhs%dbval%dtype, DTYPE_BOOL, rhs%drank, 0, "ptr_b0" )
    call c_f_pointer( rhs%dbval%cptr, lhs )
  end procedure
  module procedure assign_dbval_ptr_b1
    ASSERT( rhs%dbval%dtype, DTYPE_BOOL, rhs%drank, 1, "ptr_b1" )
    call c_f_pointer( rhs%dbval%cptr, lhs, shape=rhs%dsize  )
  end procedure
  module procedure assign_dbval_ptr_b2
    ASSERT( rhs%dbval%dtype, DTYPE_BOOL, rhs%drank, 2, "ptr_b2" )
    call c_f_pointer( rhs%dbval%cptr, lhs, shape=rhs%dsize )
  end procedure
  module procedure assign_dbval_ptr_b3
    ASSERT( rhs%dbval%dtype, DTYPE_BOOL, rhs%drank, 3, "ptr_b3" )
    call c_f_pointer( rhs%dbval%cptr, lhs, shape=rhs%dsize )
  end procedure
  module procedure assign_dbval_ptr_b4
    ASSERT( rhs%dbval%dtype, DTYPE_BOOL, rhs%drank, 4, "ptr_b4" )
    call c_f_pointer( rhs%dbval%cptr, lhs, shape=rhs%dsize )
  end procedure

end submodule m_dbval_assignments

