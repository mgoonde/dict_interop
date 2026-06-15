
add_executable( test_dbval test_dbval.f90 )
set_target_properties( test_dbval
  PROPERTIES
  Fortran_PREPROCESS ON
  Fortran_FORMAT FREE )
target_compile_options( test_dbval
  PRIVATE
  -ffree-line-length-none
  )
target_link_libraries( test_dbval
  PRIVATE
  "${PROJECT_NAME}"
  )
add_test( NAME test_dbval COMMAND test_dbval )


add_executable( test_db test_db.f90 )
set_target_properties( test_db
  PROPERTIES
  Fortran_PREPROCESS ON
  Fortran_FORMAT FREE )
target_compile_options( test_db
  PRIVATE
  -ffree-line-length-none
  )
target_link_libraries( test_db
  PRIVATE
  "${PROJECT_NAME}"
  )
add_test( NAME test_db COMMAND test_db )



add_executable( test_c test_db.c )
target_link_libraries( test_c
  PRIVATE
  "${PROJECT_NAME}"
  )
add_test( NAME test_c COMMAND test_c )



add_library( pass2f90 OBJECT test_mixed/fsub.f90)
target_link_libraries( pass2f90 PUBLIC "${PROJECT_NAME}" )
add_executable( test_mixed test_mixed/cprog.c )
target_link_libraries( test_mixed
  PRIVATE
  pass2f90
  )
add_test( NAME test_mixed COMMAND test_mixed)
