cd ../src && sh compile.sh && cd -
gfortran -g -I../src -o test_dbval.x test_dbval.f90 ../src/m_dbval.o
gfortran -g -I../src -o test_db.x test_db.f90 ../src/m_db.o ../src/m_dbval.o
