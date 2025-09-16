module db_c_tools

  use, intrinsic :: iso_c_binding
  implicit none

  interface
     function c_strlen(str) bind(c, name='strlen')
       import :: c_ptr, c_size_t, c_char
       implicit none
       character(c_char), dimension(*), intent(in) :: str
       integer(c_size_t) :: c_strlen
     end function c_strlen

     function c_malloc(size) bind(C, name="malloc")
       use, intrinsic :: iso_c_binding, only: c_size_t, c_ptr
       integer(c_size_t), intent(in), value :: size
       type(c_ptr) :: c_malloc
     end function c_malloc

     subroutine c_free(ptr) bind(c, name='free')
       use, intrinsic :: iso_c_binding, only: c_ptr
       implicit none
       type(c_ptr), value :: ptr
     end subroutine c_free

  end interface

contains

  !> copy c char into fortran string
  function c2f_char( cstring )result(fstring)
    implicit none
    character(len=1,kind=c_char), intent(in) :: cstring(*)
    character(:), allocatable :: fstring

    integer(c_size_t) :: len
    integer(c_size_t) :: i
    len = c_strlen( cstring )
    allocate( character(len=len) :: fstring )
    i = 1
    do while( cstring(i) .ne. c_null_char .and. (int(i,c_size_t) .le. len) )
       fstring(i:i) = cstring(i)
       i = i + 1
    end do
  end function c2f_char

end module db_c_tools



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
  use db_c_tools
  implicit none
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
  fkey = c2f_char(key)
  ovr = .false.
  if( overwrite /= 0_c_int )ovr=.true.
  ierr = int( db_add_x(cptr, fkey, val, dtype, store_shape, ovr), kind(ierr))
end function db_add


function db_get_cpy( cptr, key )result(val)bind(C,name="db_get_cpy")
  use m_db, only: db_get_cpy_x => db_get_cpy
  use m_dbval, only: dbval_copy, dbval
  use, intrinsic :: iso_c_binding, only: c_ptr, c_char
  use db_c_tools, only: c2f_char
  implicit none
  type( c_ptr ), intent(in), value :: cptr
  character(len=1, kind=c_char), intent(in) :: key(*)
  type(c_ptr) :: val

  type( dbval ) :: dbv, dbv1
  ! dbv here is just a reference to dbval:
  dbv = db_get_cpy_x( cptr, c2f_char(key) )
  ! make a hard-copy:
  call dbval_copy( dbv, dbv1 )
  ! return cptr of the copy
  val = dbv1%cptr
  ! proof: write out the values of cptr, compare to wiritng same from db_destroy()
  ! the value of dbv%cptr (dbv is from db_get_cpy), should be same as from db_destroy,
  ! while the value from dbv1 (hard-copy of dbv) is unique::
  ! write(*,*) transfer( dbv%cptr, c_intptr_t )
  ! write(*,*) transfer( dbv1%cptr, c_intptr_t )
end function db_get_cpy


function db_get_ptr( cptr, key )result(val)bind(C,name="db_get_ptr")
  use, intrinsic :: iso_c_binding, only: c_ptr, c_char
  use m_db, only: db_get_ptr_x => db_get_ptr
  use m_dbval, only: dbval_ptr, dbval
  use db_c_tools, only: c2f_char
  implicit none
  type( c_ptr ), intent(in), value :: cptr
  character(len=1, kind=c_char), intent(in) :: key(*)
  type(c_ptr) :: val
  type(dbval_ptr) :: dbv
  dbv = db_get_ptr_x( cptr, c2f_char(key) )
  val = dbv%dbval%cptr
end function db_get_ptr


subroutine db_free( val )bind(C,name="db_free")
  !! equivalent to `free` of <stdlib.h>
  use, intrinsic :: iso_c_binding, only: c_ptr
  use db_c_tools, only: c_free
  type( c_ptr ), value :: val
  ! this works, it seems to deallocate the full object!
  call c_free( val )
end subroutine db_free
