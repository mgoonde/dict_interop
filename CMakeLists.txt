cmake_minimum_required(VERSION 3.21)
project( db
  DESCRIPTION "dictionary structure with C interop"
  LANGUAGES Fortran C CXX
  VERSION 0.0.1
  )




set( srcs
  src/m_dbval.f90
  src/m_dbval@assignment.f90
  src/m_db.f90
  src/db.f90
  src/c_db.f90
  )
add_library(${PROJECT_NAME} "${srcs}")
set_target_properties(${PROJECT_NAME}
  PROPERTIES
  Fortran_PREPROCESS ON
  Fortran_FORMAT FREE
  POSITION_INDEPENDENT_CODE ON
  OUTPUT_NAME "${PROJECT_NAME}"
  VERSION "${PROJECT_VERSION}"
  SOVERSION "${PROJECT_VERSION_MAJOR}"
  Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/include"
  LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/lib"
  ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/lib"
  )


include(CheckFortranCompilerFlag)
set( flagstr "" )
check_fortran_compiler_flag( "-ffree-line-length-0" ff0 )
if(ff0)
  list(APPEND flagstr "-ffree-line-length-0")
endif()



target_compile_options( "${PROJECT_NAME}"
  PRIVATE
  "${flagstr}"
  )

target_include_directories( "${PROJECT_NAME}"
  PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
  )

enable_testing()
add_subdirectory(test)
