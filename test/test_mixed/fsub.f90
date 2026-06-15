module savethis

  use, intrinsic :: iso_c_binding, only: c_double

  real(c_double), pointer :: r0 => null()
contains

end module savethis



subroutine pass_db2f90( db )bind(c)
  use db_mod
  use savethis
  type( c_ptr ), intent(in) :: db
  integer :: ierr


  write(*,*) "print from fortran routine"
  call db_print(db)

  write(*,*) "adding a value from fortran"
  ierr = db_add( db, "fortran_value", 66.6 )

  r0 = db_get_ptr( db, "some_val" )

  write(*,*) "r0=",r0

  write(*,*) "exit fortran routine"
end subroutine pass_db2f90


subroutine print_r( )bind(c)
  use savethis
  if(associated(r0)) then
     write(*,*) "print r0=",r0
  else
     write(*,*) "r0 not associated!"
  end if
end subroutine print_r

