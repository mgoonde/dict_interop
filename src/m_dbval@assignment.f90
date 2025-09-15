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


  ! i
  module procedure assign_dbval_i0
    ASSERT( rhs%dtype, DTYPE_INT, rhs%drank, 0, "i0" )
    lhs = int( rhs%i(1), kind(lhs) )
  end procedure
  module procedure assign_dbval_i1
    ASSERT( rhs%dtype, DTYPE_INT, rhs%drank, 1, "i1")
    allocate( lhs, source=int(rhs%i, kind(lhs)) )
  end procedure
  module procedure assign_dbval_i2
    ASSERT( rhs%dtype, DTYPE_INT, rhs%drank, 2, "i2" )
    allocate( lhs(1:rhs%dsize(1),1:rhs%dsize(2)) )
    lhs = int( reshape( rhs%i, shape=[rhs%dsize(1), rhs%dsize(2)] ), kind(lhs) )
  end procedure
  module procedure assign_dbval_i3
    ASSERT( rhs%dtype, DTYPE_INT, rhs%drank, 3, "i3")
    allocate( lhs(1:rhs%dsize(1),1:rhs%dsize(2),1:rhs%dsize(3)) )
    lhs = int( reshape( rhs%i, shape=[rhs%dsize(1), rhs%dsize(2), rhs%dsize(3)] ), kind(lhs) )
  end procedure
  module procedure assign_dbval_i4
    ASSERT(rhs%dtype, DTYPE_INT, rhs%drank, 4, "i4")
    allocate( lhs(1:rhs%dsize(1),1:rhs%dsize(2),1:rhs%dsize(3),1:rhs%dsize(4)) )
    lhs = int( reshape( rhs%i, shape=[rhs%dsize(1), rhs%dsize(2), rhs%dsize(3), rhs%dsize(4)] ), kind(lhs) )
  end procedure
  ! rf
  module procedure assign_dbval_rf0
    ASSERT(rhs%dtype, DTYPE_REAL32, rhs%drank, 0, "rf0")
    lhs = real( rhs%rf(1), kind(lhs) )
  end procedure
  module procedure assign_dbval_rf1
    ASSERT(rhs%dtype, DTYPE_REAL32, rhs%drank, 1, "rf1")
    allocate( lhs, source=real(rhs%rf, kind(lhs)) )
  end procedure
  module procedure assign_dbval_rf2
    ASSERT(rhs%dtype, DTYPE_REAL32, rhs%drank, 2, "rf2")
    allocate( lhs(1:rhs%dsize(1),1:rhs%dsize(2)) )
    lhs = real( reshape( rhs%rf, shape=[rhs%dsize(1), rhs%dsize(2)] ), kind(lhs) )
  end procedure
  module procedure assign_dbval_rf3
    ASSERT( rhs%dtype, DTYPE_REAL32, rhs%drank, 3, "rf3")
    allocate( lhs(1:rhs%dsize(1),1:rhs%dsize(2),1:rhs%dsize(3)) )
    lhs = real( reshape( rhs%rf, shape=[rhs%dsize(1), rhs%dsize(2), rhs%dsize(3)] ), kind(lhs) )
  end procedure
  module procedure assign_dbval_rf4
    ASSERT( rhs%dtype, DTYPE_REAL32, rhs%drank, 4, "rf4")
    allocate( lhs(1:rhs%dsize(1),1:rhs%dsize(2),1:rhs%dsize(3),1:rhs%dsize(4)) )
    lhs = real( reshape( rhs%rf, shape=[rhs%dsize(1), rhs%dsize(2), rhs%dsize(3), rhs%dsize(4)] ), kind(lhs) )
  end procedure
  ! rd
  module procedure assign_dbval_rd0
    ASSERT( rhs%dtype, DTYPE_REAL64, rhs%drank, 0, "rd0")
    lhs = real( rhs%rf(1), kind(lhs) )
  end procedure
  module procedure assign_dbval_rd1
    ASSERT(rhs%dtype, DTYPE_REAL64, rhs%drank, 1, "rd1")
    allocate( lhs, source=real(rhs%rf, kind(lhs)) )
  end procedure
  module procedure assign_dbval_rd2
    ASSERT(rhs%dtype, DTYPE_REAL64, rhs%drank, 2, "rd2")
    allocate( lhs(1:rhs%dsize(1),1:rhs%dsize(2)) )
    lhs = real( reshape( rhs%rf, shape=[rhs%dsize(1), rhs%dsize(2)] ), kind(lhs) )
  end procedure
  module procedure assign_dbval_rd3
    ASSERT(rhs%dtype, DTYPE_REAL64, rhs%drank, 3, "rd3")
    allocate( lhs(1:rhs%dsize(1),1:rhs%dsize(2),1:rhs%dsize(3)) )
    lhs = real( reshape( rhs%rf, shape=[rhs%dsize(1), rhs%dsize(2), rhs%dsize(3)] ), kind(lhs) )
  end procedure
  module procedure assign_dbval_rd4
    ASSERT(rhs%dtype, DTYPE_REAL64, rhs%drank, 4, "rd4")
    allocate( lhs(1:rhs%dsize(1),1:rhs%dsize(2),1:rhs%dsize(3),1:rhs%dsize(4)) )
    lhs = real( reshape( rhs%rf, shape=[rhs%dsize(1), rhs%dsize(2), rhs%dsize(3), rhs%dsize(4)] ), kind(lhs) )
  end procedure
  ! b
  module procedure assign_dbval_b0
    ASSERT( rhs%dtype, DTYPE_BOOL, rhs%drank, 0, "b0" )
    lhs = logical(rhs%b(1), kind(lhs))
  end procedure
  module procedure assign_dbval_b1
    ASSERT( rhs%dtype, DTYPE_BOOL, rhs%drank, 1, "b1" )
    allocate( lhs(1:rhs%dsize(1)) )
    lhs = logical( rhs%b, kind(lhs))
  end procedure
  module procedure assign_dbval_b2
    ASSERT( rhs%dtype, DTYPE_BOOL, rhs%drank, 2, "b2" )
    allocate( lhs(1:rhs%dsize(1),1:rhs%dsize(2)) )
    lhs = logical( reshape( rhs%b, shape=[rhs%dsize(1), rhs%dsize(2)] ), kind(lhs) )
  end procedure
  module procedure assign_dbval_b3
    ASSERT( rhs%dtype, DTYPE_BOOL, rhs%drank, 3, "b3" )
    allocate( lhs(1:rhs%dsize(1),1:rhs%dsize(2),1:rhs%dsize(3)) )
    lhs = logical( reshape( rhs%b, shape=[rhs%dsize(1), rhs%dsize(2), rhs%dsize(3)] ), kind(lhs) )
  end procedure
  module procedure assign_dbval_b4
    ASSERT( rhs%dtype, DTYPE_BOOL, rhs%drank, 4, "b4" )
    allocate( lhs(1:rhs%dsize(1),1:rhs%dsize(2),1:rhs%dsize(3),1:rhs%dsize(4)) )
    lhs = logical( reshape( rhs%b, shape=[rhs%dsize(1), rhs%dsize(2), rhs%dsize(3), rhs%dsize(4)] ), kind(lhs) )
  end procedure




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
  module proceduree assign_dbval_ptr_b1
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

