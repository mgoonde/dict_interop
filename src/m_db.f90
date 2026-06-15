#define DBG(mmsg,vval) write(*,"(a,'::',i0,2x,g0,x,g0)")__FILE__,__LINE__,mmsg,vval
module m_db

  ! use m_error
  use m_dbval
  use, intrinsic :: iso_c_binding
  implicit none

  private
  public :: t_db
  public :: batch_size
  public :: max_keylen


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
     procedure :: get_index
     procedure, private :: realloc
     procedure :: print
     procedure :: add => t_db_add
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
    DBG( "key", key)
    DBG( "n_occupied", me%n_occupied )
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
    check_: if( idx > 0 .and. .not.overwrite) then
       ! data exists, will not overwrite
       ierr = -5 !ERR_OVERWRITE
       ! if(associated(me%bugs))call me%bugs%err_set(ier,__FILE__,__LINE__, &
       !      msg=err(ier)//": "//key )
       return
    elseif( idx > 0 .and. overwrite ) then
       ! overwrite existing key-val on same idx
       ! check metadata, if everything is identical, just modify value,
       ! if not, we destroy and create new value in memory.
       ! NOTE: string types are always recreated
       if(       me%vals(idx)%dtype == dtype &
           .and. me%vals(idx)%drank == drank &
           .and. all(me%vals(idx)%dsize == dsize) &
           .and. dtype /= DTYPE_STR ) then
          ! all metadata are the same, just modify the value
          ierr = me%vals(idx)% modif_val( val )
          return
       else
          ! there is some change in metadata, destroy the value at idx and create new
          call me%vals(idx)%destroy()
          exit check_
       end if
    elseif( idx < 0 ) then
       ! key is new
       call me% realloc()
       idx = me% n_occupied + 1
    endif check_
    if( ierr/= 0 ) then
       ! get errmsg
       return
    end if
    ! create new dbval
    me%vals(idx) = dbval( val, dtype, drank, dsize, ierr )
    ! set metadata
    strlen = min( len_trim(key), max_keylen )
    me%keys(idx) = key( 1:strlen )
    me%flags(idx) = "o"
    if( .not. overwrite ) me% n_occupied = me% n_occupied + 1

  end function t_db_add

end module m_db


