module m_db

  ! use m_error
  use m_dbval
  use, intrinsic :: iso_c_binding
  implicit none



  !> initial allocation size, reallocate when exceed
  !! this is allocated without sourcing, so actually just "reserve the place in memory",
  !! not actually allocating anything until values are passed.
  integer, parameter :: batch_size = 30

  !> maximal length for key string
  integer, parameter :: max_keylen = 64


  type :: t_db
     !> current total size
     integer :: n_size

     !> how many are occupied
     integer :: n_occupied

     !> store keys
     character(len=max_keylen), allocatable :: keys(:)
     ! character(len=max_keylen), pointer :: keys(:)

     !> store values
     ! type( dbval ), allocatable :: vals(:)
     type( dbval ), pointer :: vals(:)

     !> flag for empty or deleted
     !! 'e' -> empty
     !! 'o' -> occupied
     character(len=1), allocatable :: flags(:)

     !> error reporting instance
     ! type( t_error ), pointer :: bugs => null()

   contains
     procedure, private :: get_index
     procedure, private :: realloc
     procedure, private :: print
     procedure, private :: add => t_db_add
  end type t_db


contains

  function get_index( me, key )result(idx)
    !! get index of the key in me%keys
    !! if key does not exist return -1
    implicit none
    class( t_db ), intent(in) :: me
    character(*), intent(in) :: key   !! keyword of wanted data
    integer :: idx                    !! index of data under `key`
    integer :: i
    !! loop over all
    do idx = 1, me% n_occupied
       if( me%keys(idx) == key ) return
    end do
    !! key not found
    ! idx = ERR_NODATA
    idx = -3
  end function get_index


  subroutine realloc( me )
    !! test if arrays need to be reallocated:
    !! based on n_occupied.
    !! NOTE: if reallocation happens, it will move things to a different location in memory,
    !! meaning all pointers to data elements are invalid after realloc!
    implicit none
    class( t_db ), intent(inout) :: me
    integer :: cur_size, new_size
    character(len=max_keylen), allocatable :: keys_tmp(:)
    type( dbval ), allocatable :: vals_tmp(:)
    character(len=1), allocatable :: flags_tmp(:)
    integer :: i
    cur_size = me%n_occupied
    ! write(*,*) "cur size",cur_size
    ! write(*,*) "me% n_size",me% n_size
    ! write(*,*) size(me%vals)
    !! if number of occupied is equal to total size
    if( cur_size == me%n_size ) then
       ! write(*,*) "doing realloc"
       new_size = cur_size + batch_size
       !! copy to tmp, this deallocates the me%..
       call move_alloc( me%keys, keys_tmp )
       call move_alloc( me%flags, flags_tmp )
       allocate( vals_tmp(1:cur_size) )
       do i = 1, cur_size
          call dbval_copy( me%vals(i), vals_tmp(i) )
          call dbval_destroy( me%vals(i) )
       end do
       deallocate( me%vals )

       !! allocate me%.. to new_size
       ! write(*,*) "allocating new_size=", new_size
       allocate(me%keys(1:new_size))
       me%keys=""
       allocate(me%flags(1:new_size))
       me%flags="e"
       allocate(me%vals(1:new_size))

       !! copy the old elements
       ! write(*,*) "copying old elements"
       me%keys(1:cur_size) = keys_tmp
       me%flags(1:cur_size) = flags_tmp

       ! me%vals(1:cur_size) = vals_tmp
       do i = 1,  cur_size
          call dbval_copy( vals_tmp(i), me%vals(i) )
          call dbval_destroy( vals_tmp(i) )
       end do
       deallocate( vals_tmp )

       !! deallocate tmp
       ! deallocate( keys_tmp, vals_tmp, flags_tmp )
       me% n_size = new_size
    endif
  end subroutine realloc

  ! internal print
  subroutine print(me, print_vals)
    implicit none
    class( t_db ), intent(in) :: me
    logical, intent(in), optional :: print_vals
    integer :: i
    logical :: print_v

    print_v = .false.
    if(present(print_vals))print_v=print_vals

    do i = 1, me% n_occupied
       write(*,"('>key::',a,2x,'>index::',i0)") &
            trim( me%keys(i) ), i
       if(print_v)write(*,*) me%vals(i)
    end do
  end subroutine print

  ! internal add from cptr
  function t_db_add( me, key, val, dtype, drank, dsize, overwrite )result(ierr)
    implicit none
    class( t_db ), intent(inout) :: me
    character(*), intent(in) :: key
    type(c_ptr), intent(in) :: val
    integer, intent(in) :: dtype, drank
    integer, intent(in) :: dsize(drank)
    logical, intent(in) :: overwrite
    integer :: ierr
    integer :: idx, strlen
    ierr=0
    idx = me%get_index(key)
    if( idx > 0 .and. .not.overwrite) then
       ! data exists, will not overwrite
       ierr = -5 !ERR_OVERWRITE
       ! if(associated(me%bugs))call me%bugs%err_set(ier,__FILE__,__LINE__, &
       !      msg=err(ier)//": "//key )
       return
    elseif( idx > 0 .and. overwrite ) then
       ! overwrite existing key-val on same idx
       call me%vals(idx)%destroy()
    elseif( idx < 0 ) then
       ! key is new
       call me% realloc()
       idx = me% n_occupied + 1
    endif
    me%vals(idx) = dbval( val, dtype, drank, dsize, ierr )
    if( ierr/= 0 ) then
       ! get errmsg
       return
    end if
    ! set metadata
    strlen = min( len_trim(key), max_keylen )
    me%keys(idx) = key( 1:strlen )
    me%flags(idx) = "o"
    if(.not.overwrite) me% n_occupied = me% n_occupied + 1
  end function t_db_add

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
    implicit none
    type( c_ptr ), intent(in) :: db
    character(*), intent(in) :: key
    class(*), intent(in) :: val(..)
    integer, intent(in) :: dtype
    integer, intent(in), optional :: store_shape(:)
    logical, intent(in), optional :: overwrite
    integer :: ierr

    type( t_db ), pointer :: me
    logical :: ovr
    integer :: srank, i
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
    rank(0); ptrval=LOC(val)
    rank(1); ptrval=LOC(val)
    rank(2); ptrval=LOC(val)
    rank(3); ptrval=LOC(val)
    rank(4); ptrval=LOC(val)
    end select
    cval = transfer(ptrval, cval)

    call c_f_pointer( db, me )
    ovr=.false.
    if(present(overwrite))ovr=overwrite
    ierr = me%add( key, cval, dtype, srank, ssize, ovr )
  end function db_add


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
    val = me%vals(idx)
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



end module m_db


