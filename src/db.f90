module db_mod

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


contains


  !! external management of db:
  function db_create()result(cptr)
    !! Create db instance
    implicit none
    type( c_ptr ) :: cptr
    type( t_db ), pointer :: me
    write(*,*) "create me"
    allocate( t_db :: me )
    me% n_size = batch_size
    me% n_occupied = 0
    allocate( me%keys(1:batch_size) )
    me%keys(:)=""
    allocate( me%vals(1:batch_size) )
    allocate( me%flags(1:batch_size), source="e" )
    ! me%bugs => t_error()
    cptr = c_loc(me)
  end function db_create

  subroutine db_destroy( db )
    !! Destroy db instance
    implicit none
    type( c_ptr ), intent(in) :: db
    type( t_db ), pointer :: me
    integer :: i
    write(*,*) "destroy me"
    call c_f_pointer(db, me)
    do i = 1, me% n_occupied
       !! skip empty
       if( me%flags(i) == "e" ) cycle
       write(*,*) "idx loc:",i, transfer(me%vals(i)%cptr, c_intptr_t )
       call dbval_destroy( me% vals(i) )
    end do
    deallocate( me% keys )
    deallocate( me% vals )   ! deallocate happens where? in data%destroy?
    deallocate( me% flags )
    deallocate(me)
  end subroutine db_destroy


  subroutine db_print(db)
    !! Output the current contents of `db`
    type( c_ptr ), intent(in) :: db
    type( t_db ), pointer :: me
    call c_f_pointer( db, me)
    call me%print(print_vals=.true.)
  end subroutine db_print


  ! db_add, call from fortran
  function db_add( db, key, val, dtype, store_shape, overwrite )result(ierr)
    !! Add a key-value pair to db.
    !! By default, the shape of `val` is preserved.
    !! if `store_shape` is specified, then the value is reshaped
    !! as specified by `store_shape`.
    use, intrinsic :: iso_c_binding, only: c_int, c_float, c_double, c_bool
    implicit none
    type( c_ptr ), intent(in) :: db
    character(*), intent(in) :: key
    class(*), intent(in) :: val(..)
    integer, intent(in), optional :: dtype
    integer, intent(in), optional :: store_shape(:)
    logical, intent(in), optional :: overwrite
    integer :: ierr

    type( t_db ), pointer :: me
    logical :: ovr
    integer :: srank, i, dd
    integer, allocatable :: ssize(:)
    integer(c_intptr_t) :: ptrval
    type( c_ptr ) :: cval


    srank = rank(val)
    allocate( ssize(1:srank) )
    do i = 1, srank
       ssize(i) = size(val,i)
    end do
    if(present(store_shape)) then
       srank = size(store_shape)
       ssize=store_shape
    end if

    ! create cptr from class(*) val
    select rank(val)
    rank(0); ptrval=LOC(val); dd=val_dtype(val)
    rank(1); ptrval=LOC(val); dd=val_dtype(val(1))
    rank(2); ptrval=LOC(val); dd=val_dtype(val(1,1))
    rank(3); ptrval=LOC(val); dd=val_dtype(val(1,1,1))
    rank(4); ptrval=LOC(val); dd=val_dtype(val(1,1,1,1))
    end select
    cval = transfer(ptrval, cval)

    if(present(dtype)) dd = dtype
    write(*,*) "HERE",dd
    call c_f_pointer( db, me )
    ovr=.false.
    if(present(overwrite))ovr=overwrite
    ierr = me%add( key, cval, dd, srank, ssize, ovr )
  end function db_add

  function val_dtype(val) result(dtype)
    class(*), intent(in) :: val
    integer :: dtype
    dtype=DTYPE_UNKNOWN
    select type(val)
    type is( integer(c_int)  ); dtype=DTYPE_INT
    type is( real(c_float)   ); dtype=DTYPE_REAL32
    type is( real(c_double)  ); dtype=DTYPE_REAL64
    type is( logical(c_bool) ); dtype=DTYPE_BOOL
    type is( character(*)    ); dtype=DTYPE_STR
    end select
  end function val_dtype


  function db_get_cpy( db, key, reshape )result(val)
    !! Get a hard-copy (allocated) of `key`.
    !! Optionally reshape.
    implicit none
    type(c_ptr), intent(in) :: db
    character(*), intent(in) :: key
    integer, intent(in), optional :: reshape(:)
    type( dbval ) :: val
    type( t_db ), pointer :: me
    integer :: idx
    call c_f_pointer(db, me)
    idx = me%get_index(key)
    if( idx < 1 ) return
    ! this is not a hard copy....
    ! the actual copying happens in assignment overload
    val = me%vals(idx)
    ! if we do the below, it shoudl dbval%destroy() in assignment;
    ! but this would mean to effectively make 2 copies, one here and one is assignment
    ! call dbval_copy( me%vals(idx), val )
    if(present(reshape)) then
       val%drank = size(reshape)
       val%dsize = reshape
    end if
  end function db_get_cpy

  function db_get_ptr( db, key, reshape )result(val_ptr)
    !! Obtain pointer to `key`.
    !! Optionally reshape.
    ! NOTE: pass through dbval_ptr type (to avoid ambiguous assignment)
    implicit none
    type(c_ptr), intent(in) :: db
    character(*), intent(in) :: key
    integer, intent(in), optional :: reshape(:)
    type( dbval_ptr ) :: val_ptr
    type( t_db ), pointer :: me
    integer :: idx
    call c_f_pointer(db, me)
    idx = me%get_index(key)
    if( idx < 1 ) return
    nullify(val_ptr%dbval)
    val_ptr%dbval => me%vals(idx)
    val_ptr%dtype = me%vals(idx)%dtype
    val_ptr%drank = me%vals(idx)%drank
    val_ptr%dsize = me%vals(idx)%dsize
    if(present(reshape)) then
       val_ptr%drank = size(reshape)
       val_ptr%dsize = reshape
    end if
  end function db_get_ptr


  function db_exist( db, key )result(exist)
    !! Check if `key` exists in db.
    !! Return positive value if yes, negative otherwise
    type(c_ptr), intent(in) :: db
    character(*), intent(in) :: key
    integer :: exist
    type( t_db ), pointer :: me
    call c_f_pointer( db, me )
    exist = me%get_index(key)
  end function db_exist

  function db_get_dtype( db, key )result(dtype)
    !! Return DTYPE encoder of `key`
    type(c_ptr), intent(in) :: db
    character(*), intent(in) :: key
    integer :: dtype
    type( t_db ), pointer :: me
    integer :: idx
    dtype = DTYPE_UNKNOWN
    call c_f_pointer(db,me)
    idx = me%get_index(key)
    if(idx < 1) return
    dtype = me%vals(idx)%dtype
  end function db_get_dtype

  function db_get_drank( db, key )result(drank)
    !! Return rank of `key`
    type(c_ptr), intent(in) :: db
    character(*), intent(in) :: key
    integer :: drank
    type( t_db ), pointer :: me
    integer :: idx
    drank = -1
    call c_f_pointer(db,me)
    idx = me%get_index(key)
    if(idx < 1) return
    drank = me%vals(idx)%drank
  end function db_get_drank

  function db_get_dsize( db, key )result(dsize)
    !! Return the size (shape) of `key`
    type(c_ptr), intent(in) :: db
    character(*), intent(in) :: key
    integer, allocatable :: dsize(:)
    type( t_db ), pointer :: me
    integer :: idx
    call c_f_pointer(db,me)
    idx = me%get_index(key)
    if(idx < 1) return
    dsize = me%vals(idx)%dsize
  end function db_get_dsize


end module db_mod
