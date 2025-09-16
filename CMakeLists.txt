cmake_minimum_required(VERSION 3.5)
project( dict_interop
  DESCRIPTION "dictionary structure with C interop"
  LANGUAGES Fortran C)
enable_language(Fortran)
enable_language(C)


add_subdirectory(src)
