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
     type( dbval ), allocatable :: vals(:)

     !> flag for empty or deleted
     !! 'e' -> empty
     !! 'o' -> occupied
     character(len=1), allocatable :: flags(:)

     !> error reporting instance
     ! type( t_error ), pointer :: bugs => null()

   contains
     procedure, private :: get_index
     procedure, private :: t_db_realloc
     procedure, private :: db_print
  end type t_db


  interface t_db
     procedure :: t_db_constructor
  end interface t_db

contains

  !> get index of the key in me%keys
  !! if key does not exist return -1
  function get_index( me, key )result(idx)
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


  subroutine t_db_realloc( me )
    !! test if arrays need to be reallocated:
    !! based on n_occupied.
    !! NOTE: this will move things to a different location in memory,
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
  end subroutine t_db_realloc


  subroutine db_print(me, print_vals)
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
  end subroutine db_print



  function t_db_constructor()result(cptr)
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
  end function t_db_constructor

  subroutine t_db_destroy( db )
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
  end subroutine t_db_destroy



  subroutine t_db_add( db, key, val, dtype, overwrite, store_shape, ierr )
    implicit none
    type( c_ptr ), intent(in) :: db
    character(*), intent(in) :: key
    class(*), intent(in) :: val(..)
    integer, intent(in) :: dtype
    logical, intent(in), optional :: overwrite
    integer, intent(in), optional :: store_shape(:)
    integer, intent(out), optional :: ierr

    type( t_db ), pointer :: me
    logical :: ovr
    integer :: ier, idx, i, strlen
    integer :: srank
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
    rank(5); ptrval=LOC(val)
    rank(6); ptrval=LOC(val)
    end select
    cval = transfer(ptrval, cval)

    call c_f_pointer( db, me )
    ovr=.false.
    ier=0
    if(present(overwrite))ovr=overwrite
    idx = me%get_index(key)
    if( idx > 0 .and. .not.ovr) then
       ! data exists, will not overwrite
       ier = -5 !ERR_OVERWRITE
       ! if(associated(me%bugs))call me%bugs%err_set(ier,__FILE__,__LINE__, &
       !      msg=err(ier)//": "//key )
       if(present(ierr))ierr=ier
       return
    elseif( idx > 0 .and. ovr ) then
       ! overwrite existing key-val on same idx
       call me%vals(idx)%destroy()
    elseif( idx < 0 ) then
       ! key is new
       call me% t_db_realloc()
       idx = me% n_occupied + 1
    endif
    me%vals(idx) = dbval( cval, dtype, srank, ssize, ierr )
    if(present(ierr))ierr=ier
    if( ier/= 0 ) return
    ! set metadata
    strlen = min( len_trim(key), max_keylen )
    me%keys(idx) = key( 1:strlen )
    me%flags(idx) = "o"
    if(.not.ovr) me% n_occupied = me% n_occupied + 1
  end subroutine t_db_add

  subroutine t_db_print(db)
    type( c_ptr ), intent(in) :: db
    type( t_db ), pointer :: me
    call c_f_pointer( db, me)
    call me%db_print(print_vals=.true.)
  end subroutine t_db_print

end module m_db


function t_db()result(cptr)bind(C,name="t_db")
  use, intrinsic :: iso_c_binding, only: c_ptr
  use m_db, only: t_db_x => t_db
  implicit none
  type( c_ptr ) :: cptr
  cptr = t_db_x()
end function t_db

subroutine t_db_destroy(cptr)bind(C,name="t_db_destroy")
  use, intrinsic :: iso_c_binding, only: c_ptr
  use m_db, only: t_db_destroy_x => t_db_destroy
  type(c_ptr), intent(in) :: cptr
  call t_db_destroy_x(cptr)
end subroutine t_db_destroy
