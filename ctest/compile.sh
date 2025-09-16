cd ../src && sh compile.sh && cd -
gcc -I../src -g -o ctest.x ctest.c ../src/libdb.a -lgfortran
