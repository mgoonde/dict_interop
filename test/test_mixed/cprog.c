#include "db.h"
#include <stdio.h>
#include <stdlib.h>


void pass_db2f90( void* db );
void print_r( );


int main(){
  t_db* db;
  int ierr;

  db = db_create();

  double rd=4.0;
  ierr = db_add( db, "some_val", &rd, DTYPE_REAL64, 0, NULL, 0);

  pass_db2f90( &db );

  db_print(db);

  // overwrite 'some_val'
  rd = 4.5;
  ierr = db_add( db, "some_val", &rd, DTYPE_REAL64, 0, NULL, 1);

  // print r0 from f90
  print_r();

  db_destroy(db);
}
