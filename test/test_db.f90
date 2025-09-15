program test_db
  use m_db
  implicit none

  type( c_ptr ) :: db
  integer :: ierr
  integer, allocatable :: i1(:)
  integer :: m
  type( dbval_ptr ), pointer :: tptr
  integer(c_int), pointer :: ti

  db = db_create()

  ierr = db_add( db, "key1", [3,4,5], DTYPE_INT )

  call db_print(db)


  ierr = db_add( db, "key1", [3,4,6], DTYPE_INT, overwrite=.true. )

  ierr = db_add( db, "key_int", 18, DTYPE_INT )

  m = db_get_cpy( db, "key1" )
  write(*,*) "m:",m

  ti = db_get_ptr( db, "key_int" )
  write(*,*) "ti:",ti
  ti = 4

  call db_print(db)

  nullify(ti)
  call db_destroy(db)

end program test_db
