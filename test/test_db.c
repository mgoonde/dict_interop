#include "db.h"
#include <stdio.h>
#include <stdlib.h>

int main(){
  t_db* db;
  int ierr;

  db = db_create();


  double rd=4.0;
  ierr = db_add( db, "real_double", &rd, DTYPE_REAL64, 0, NULL, 0);

  db_print(db);

  double* rd_ptr = (double *) db_get_ptr( db, "real_double");
  printf( "rd_ptr == %f\n",*rd_ptr);
  rd_ptr[0]=4.1;

  float x[4];
  x[0] = 0.1;
  x[1] = 0.2;
  x[2] = 0.3;
  x[3] = 0.4;

  int shape[2];
  shape[0]=2;
  shape[1]=2;

  ierr = db_add( db, "smt", &x, DTYPE_REAL32, 2, shape, 0);
  /* void* cval = db_get_cpy( db, "smt"); */
  /* float* rv=(float *) cval; */
  float* rv=(float *)db_get_cpy( db, "smt" );

  db_print(db);
  db_print(db);
  db_destroy( db );


  for( int i = 0; i<4; i++){
    printf( "%d %f\n",i,rv[i]);
  }
  free(rv);

  /* db_free( cval ); */

}
