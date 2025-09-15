
! external stuff for bind(c)
function db_create()result(cptr)bind(C,name="db_create")
  use, intrinsic :: iso_c_binding, only: c_ptr
  use m_db, only: db_create_x => db_create
  implicit none
  type( c_ptr ) :: cptr
  cptr = db_create_x()
end function db_create


subroutine db_destroy(cptr)bind(C,name="db_destroy")
  use, intrinsic :: iso_c_binding, only: c_ptr
  use m_db, only: db_destroy_x => db_destroy
  type(c_ptr), intent(in), value :: cptr
  call db_destroy_x(cptr)
end subroutine db_destroy

subroutine db_print(cptr)bind(C,name="db_print")
  use, intrinsic :: iso_c_binding, only: c_ptr
  use m_db, only: db_print_x => db_print
  type(c_ptr), intent(in), value :: cptr
  call db_print_x(cptr)
end subroutine db_print

function db_add(cptr, key, val, dtype, drank, store_shape, overwrite )result(ierr)bind(C,name="db_add")
  use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_char, c_size_t, c_null_char
  use m_db, only: db_add_x => db_add
  implicit none
  interface
     function c_strlen(str) bind(c, name='strlen')
       import :: c_ptr, c_size_t, c_char
       implicit none
       character(c_char), dimension(*), intent(in) :: str
       integer(c_size_t) :: c_strlen
     end function c_strlen
  end interface
  type( c_ptr ), intent(in), value :: cptr
  character(len=1, kind=c_char), intent(in) :: key(*)
  type( c_ptr ), intent(in) :: val
  integer(c_int), intent(in), value :: dtype
  integer(c_int), intent(in), value :: drank
  integer(c_int), intent(in) :: store_shape(drank)
  integer(c_int), intent(in), value :: overwrite ! 0 for no overwrite, /=0 for overwrite
  integer(c_int) :: ierr

  integer(c_size_t) :: i, w
  character(:), allocatable :: fkey
  logical :: ovr
  ! transfor key into f string
  w = c_strlen(key)
  allocate( character(len=w) :: fkey )
  i = 1
  do while( key(i) .ne. c_null_char .and. (int(i,c_size_t) .le. w) )
     fkey(i:i) = key(i)
     i = i + 1
  end do
  ovr = .false.
  if( overwrite /= 0_c_int )ovr=.true.
  ierr = int( db_add_x(cptr, fkey, val, dtype, store_shape, ovr), kind(ierr))
end function db_add
