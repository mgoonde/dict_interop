gfortran -cpp -g -c m_dbval.f90
gfortran -cpp -g -c -ffree-line-length-none m_dbval@assignment.f90
gfortran -cpp -g -c m_db.f90
gfortran -cpp -g -c db.f90
gfortran -cpp -g -c c_db.f90
ar -rcv libdb.a m_dbval.o m_dbval@assignment.o m_db.o db.o

