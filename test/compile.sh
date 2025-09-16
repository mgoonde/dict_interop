cd ../ && rm -rf bld && cmake -B bld && cmake --build bld && cd -
gfortran -g -I../include -o test_dbval.x test_dbval.f90 -L../lib -ldb
gfortran -g -I../include -o test_db.x test_db.f90 -L../lib -ldb
