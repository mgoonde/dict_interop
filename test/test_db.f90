program test_db
  use m_db
  implicit none

  type( c_ptr ) :: db

  db = t_db()

  call t_db_add( db, "key1", [3,4,5], DTYPE_INT )

  call t_db_print(db)


  call t_db_add( db, "key1", [3,4,6], DTYPE_INT, overwrite=.true. )


  call t_db_print(db)
  call t_db_destroy(db)

end program test_db
