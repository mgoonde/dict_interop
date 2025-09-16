cd ../ && rm -rf bld && cmake -B bld && cmake --build bld && cd -
gcc -I../include -I../src -g -o ctest.x ctest.c -L../lib -ldb -lgfortran
