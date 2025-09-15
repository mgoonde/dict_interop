program test_db
  use m_db
  implicit none

  type( c_ptr ) :: db
  integer :: ierr
  integer, allocatable :: i1(:), ii2(:,:)
  integer :: m
  ! type( dbval_ptr ), pointer :: tptr
  type( dbval_ptr ) :: tptr
  integer(c_int), pointer :: ti, t1(:), t2(:,:)

  integer :: i2(3,4)

  db = db_create()

  ierr = db_add( db, "key1", [3,4,5], DTYPE_INT )

  call db_print(db)


  ierr = db_add( db, "key1", [3,4,6], DTYPE_INT, overwrite=.true. )

  ierr = db_add( db, "key_int", 18, DTYPE_INT )

  m = db_get_cpy( db, "key_int" )
  write(*,*) "m:",m

  ti = db_get_ptr( db, "key_int" )
  write(*,*) "ti:",ti
  ti = 4

  t1 = db_get_ptr( db, "key1" )
  write(*,*) "t1:",t1
  t1 = [4,5,7]



  i2(:,1) = [11,12,13]
  i2(:,2) = [21,22,23]
  i2(:,3) = [31,32,33]
  i2(:,4) = [41,42,43]
  ierr = db_add( db, "i2", i2, DTYPE_INT )

  call db_print(db)

  ii2 = db_get_cpy( db, "i2", reshape=[2,6] )
  do m = 1, 6
     write(*,*) m, ii2(:,m)
  end do

  write(*,*) ii2
  deallocate(ii2)

  t1 = db_get_ptr( db, "i2", reshape=[12] )
  write(*,*) "hre"
  write(*,"(*(i0,1x))") t1
  ! write(*,*) t2

  nullify(t2)
  call db_destroy(db)

end program test_db
