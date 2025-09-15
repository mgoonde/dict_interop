program test_dbval
  use m_dbval
  use, intrinsic :: iso_c_binding
  implicit none

  integer :: ierr
  type( dbval ) :: t, t_cp

  integer( c_int ), pointer :: cval(:,:,:,:)
  type( c_ptr ) :: cptr
  integer, allocatable :: dsize(:)
  integer :: i
  integer(c_intptr_t) :: pval
  integer( c_int ), pointer :: b

  allocate( cval(1:3,1:3,1:4,1:5), source=3)
  cptr = c_loc(cval)

  allocate(dsize(1:rank(cval)))
  do i = 1, rank(cval)
     dsize(i) = size(cval,i)
  end do

  t = dbval( cptr, DTYPE_INT, rank(cval), dsize , ierr )

  write(*,*) "ierr", ierr
  if( ierr /= 0 ) write(*,*) dbval_errmsg(t)
  write(*,*) t


  block
    character(len=*), parameter :: ss="some_string here"
    character(len=1,kind=c_char), pointer :: cstr(:)
    allocate( cstr(1:len(ss)+1) )
    do i = 1, len(ss)
       cstr(i) = ss(i:i)
    end do
    cstr(len(ss)+1)=c_null_char

    cptr = c_loc(cstr(1))

    t = dbval( cptr, DTYPE_STR, 0, [0], ierr)
    write(*,*) t

  end block

  allocate(b, source=18 )
  cptr = c_loc(b)
  t = dbval( cptr, DTYPE_INT, 0, [0], ierr )
  write(*,*) t
  i=t
  write(*,*) i
end program test_dbval
