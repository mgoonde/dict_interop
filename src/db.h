int DTYPE_UNKNOWN = -1;
int DTYPE_INT     = 1;
int DTYPE_REAL32  = 2;
int DTYPE_REAL64  = 3;
int DTYPE_STR     = 4;
int DTYPE_BOOL    = 5;

typedef struct{}t_db;

t_db * db_create();
void db_destroy( t_db* db );
void db_print( t_db* db );
int db_add( t_db* db, char* key, void* val, int dtype, int drank, int* store_shape, int overwrite );
void* db_get_cpy( t_db* db, char* key );
void* db_get_ptr( t_db* db, char* key );

void db_free( void* val );
