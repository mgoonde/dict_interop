
set(libdir "${CMAKE_SOURCE_DIR}/lib")
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_SOURCE_DIR}/include)

add_compile_options(-ffree-line-length-none)


set( MODFILES
  ${CMAKE_CURRENT_SOURCE_DIR}/m_dbval.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/m_dbval@assignment.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/m_db.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/db.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/c_db.f90
  )

add_library(db "${MODFILES}")
set_target_properties(db PROPERTIES
  Fortran_PREPROCESS ON
  Fortran_FORMAT FREE
  LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
  )

## make dir /lib and copy the library there
add_custom_command(
  TARGET db
  POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E make_directory "${libdir}"
  COMMAND
  ${CMAKE_COMMAND} -E copy "$<TARGET_FILE:db>"
  "${libdir}/$<TARGET_FILE_NAME:db>"
  )
