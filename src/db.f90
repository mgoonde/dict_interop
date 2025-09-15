module db

  use m_db
  use m_dbval

  private

  public :: DTYPE_UNKNOWN
  public :: DTYPE_INT
  public :: DTYPE_REAL32
  public :: DTYPE_REAL64
  public :: DTYPE_STR
  public :: DTYPE_BOOL

  public :: c_ptr
  public :: db_create
  public :: db_destroy
  public :: db_print
  public :: db_add
  public :: db_exist
  public :: db_get_cpy
  public :: db_get_ptr
  public :: db_get_dtype
  public :: db_get_drank
  public :: db_get_dsize
  public :: assignment(=)


end module db
