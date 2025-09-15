module m_dbval

  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64
  implicit none

  !! datatype encoders
  integer, parameter, public :: &
       DTYPE_UNKNOWN = -1, &
       DTYPE_INT     = 1, &
       DTYPE_REAL32  = 2, &
       DTYPE_REAL64  = 3, &
       DTYPE_STR     = 4, &
       DTYPE_BOOL    = 5


  type :: dbval
     !! store data in 1D arrays, but store also the original dsize
     ! private
     integer( c_int ), pointer, contiguous :: i(:) => null()
     real( c_float ), pointer, contiguous  :: rf(:) => null()
     real( c_double ), pointer, contiguous :: rd(:) => null()
     logical( c_bool ), pointer, contiguous :: b(:) =>null()
     character(:), allocatable :: str
     ! c_ptr to data
     type( c_ptr ) :: cptr = c_null_ptr
     ! meta
     integer :: dtype = DTYPE_UNKNOWN
     integer :: drank = -1
     integer, allocatable :: dsize(:)
     ! error message
     character(:), allocatable :: errstr
   contains
     procedure, public :: destroy => dbval_destroy !! explicit destructor
     procedure, public :: nullify => dbval_nullify
     procedure, public :: errmsg => dbval_errmsg

     procedure, private :: write_formatted
     generic, public :: write(formatted) => write_formatted

     procedure, private :: write_unformatted
     generic, public :: write(unformatted) => write_unformatted

     procedure, private :: read_unformatted
     generic, public :: read(unformatted) => read_unformatted
  end type dbval


  type :: dbval_ptr
     !! auxiliary type to help overloading pointer assignment
     type(dbval), pointer :: dbval => null()
     integer :: drank
     integer, allocatable :: dsize(:)
   contains
     final :: dbval_ptr_destroy
  end type dbval_ptr


  interface dbval
     procedure :: dbval_constructor_cptr
  end interface dbval

  !! declare the assingments from submodule
  interface
     ! i
     module subroutine assign_dbval_i0( lhs, rhs )
       integer, intent(out) :: lhs
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_i0
     module subroutine assign_dbval_i1( lhs, rhs )
       integer, intent(out), allocatable :: lhs(:)
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_i1
     module subroutine assign_dbval_i2( lhs, rhs )
       integer, intent(out), allocatable :: lhs(:,:)
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_i2
     module subroutine assign_dbval_i3( lhs, rhs )
       integer, intent(out), allocatable :: lhs(:,:,:)
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_i3
     module subroutine assign_dbval_i4( lhs, rhs )
       integer, intent(out), allocatable :: lhs(:,:,:,:)
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_i4
     ! rf
     module subroutine assign_dbval_rf0( lhs, rhs )
       real(sp), intent(out) :: lhs
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_rf0
     module subroutine assign_dbval_rf1( lhs, rhs )
       real(sp), intent(out), allocatable :: lhs(:)
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_rf1
     module subroutine assign_dbval_rf2( lhs, rhs )
       real(sp), intent(out), allocatable :: lhs(:,:)
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_rf2
     module subroutine assign_dbval_rf3( lhs, rhs )
       real(sp), intent(out), allocatable :: lhs(:,:,:)
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_rf3
     module subroutine assign_dbval_rf4( lhs, rhs )
       real(sp), intent(out), allocatable :: lhs(:,:,:,:)
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_rf4
     ! rd
     module subroutine assign_dbval_rd0( lhs, rhs )
       real(dp), intent(out) :: lhs
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_rd0
     module subroutine assign_dbval_rd1( lhs, rhs )
       real(dp), intent(out), allocatable :: lhs(:)
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_rd1
     module subroutine assign_dbval_rd2( lhs, rhs )
       real(dp), intent(out), allocatable :: lhs(:,:)
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_rd2
     module subroutine assign_dbval_rd3( lhs, rhs )
       real(dp), intent(out), allocatable :: lhs(:,:,:)
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_rd3
     module subroutine assign_dbval_rd4( lhs, rhs )
       real(dp), intent(out), allocatable :: lhs(:,:,:,:)
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_rd4
     ! b
     module subroutine assign_dbval_b0( lhs, rhs )
       logical, intent(out) :: lhs
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_b0
     module subroutine assign_dbval_b1( lhs, rhs )
       logical, intent(out), allocatable :: lhs(:)
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_b1
     module subroutine assign_dbval_b2( lhs, rhs )
       logical, intent(out), allocatable :: lhs(:,:)
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_b2
     module subroutine assign_dbval_b3( lhs, rhs )
       logical, intent(out), allocatable :: lhs(:,:,:)
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_b3
     module subroutine assign_dbval_b4( lhs, rhs )
       logical, intent(out), allocatable :: lhs(:,:,:,:)
       class( dbval ), intent(in) :: rhs
     end subroutine assign_dbval_b4



     ! ptrs
     ! i
     module subroutine assign_dbval_ptr_i0( lhs, rhs )
       integer(c_int), pointer, intent(out) :: lhs
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_i0
     module subroutine assign_dbval_ptr_i1( lhs, rhs )
       integer(c_int), pointer, intent(out) :: lhs(:)
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_i1
     module subroutine assign_dbval_ptr_i2( lhs, rhs )
       integer(c_int), pointer, intent(out) :: lhs(:,:)
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_i2
     module subroutine assign_dbval_ptr_i3( lhs, rhs )
       integer(c_int), pointer, intent(out) :: lhs(:,:,:)
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_i3
     module subroutine assign_dbval_ptr_i4( lhs, rhs )
       integer(c_int), pointer, intent(out) :: lhs(:,:,:,:)
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_i4
     ! rf
     module subroutine assign_dbval_ptr_rf0( lhs, rhs )
       real(c_float), pointer, intent(out) :: lhs
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_rf0
     module subroutine assign_dbval_ptr_rf1( lhs, rhs )
       real(c_float), pointer, intent(out) :: lhs(:)
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_rf1
     module subroutine assign_dbval_ptr_rf2( lhs, rhs )
       real(c_float), pointer, intent(out) :: lhs(:,:)
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_rf2
     module subroutine assign_dbval_ptr_rf3( lhs, rhs )
       real(c_float), pointer, intent(out) :: lhs(:,:,:)
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_rf3
     module subroutine assign_dbval_ptr_rf4( lhs, rhs )
       real(c_float), pointer, intent(out) :: lhs(:,:,:,:)
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_rf4
     ! rd
     module subroutine assign_dbval_ptr_rd0( lhs, rhs )
       real(c_double), pointer, intent(out) :: lhs
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_rd0
     module subroutine assign_dbval_ptr_rd1( lhs, rhs )
       real(c_double), pointer, intent(out) :: lhs(:)
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_rd1
     module subroutine assign_dbval_ptr_rd2( lhs, rhs )
       real(c_double), pointer, intent(out) :: lhs(:,:)
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_rd2
     module subroutine assign_dbval_ptr_rd3( lhs, rhs )
       real(c_double), pointer, intent(out) :: lhs(:,:,:)
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_rd3
     module subroutine assign_dbval_ptr_rd4( lhs, rhs )
       real(c_double), pointer, intent(out) :: lhs(:,:,:,:)
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_rd4
     ! b
     module subroutine assign_dbval_ptr_b0( lhs, rhs )
       logical(c_bool), pointer, intent(out) :: lhs
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_b0
     module subroutine assign_dbval_ptr_b1( lhs, rhs )
       logical(c_bool), pointer, intent(out) :: lhs
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_b1
     module subroutine assign_dbval_ptr_b2( lhs, rhs )
       logical(c_bool), pointer, intent(out) :: lhs
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_b2
     module subroutine assign_dbval_ptr_b3( lhs, rhs )
       logical(c_bool), pointer, intent(out) :: lhs
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_b3
     module subroutine assign_dbval_ptr_b4( lhs, rhs )
       logical(c_bool), pointer, intent(out) :: lhs
       class(dbval_ptr), intent(in) :: rhs
     end subroutine assign_dbval_ptr_b4

  end interface

  interface assignment(=)
     module procedure :: assign_dbval_i0
     module procedure :: assign_dbval_i1
     module procedure :: assign_dbval_i2
     module procedure :: assign_dbval_i3
     module procedure :: assign_dbval_i4

     module procedure :: assign_dbval_rf0
     module procedure :: assign_dbval_rf1
     module procedure :: assign_dbval_rf2
     module procedure :: assign_dbval_rf3
     module procedure :: assign_dbval_rf4

     module procedure :: assign_dbval_rd0
     module procedure :: assign_dbval_rd1
     module procedure :: assign_dbval_rd2
     module procedure :: assign_dbval_rd3
     module procedure :: assign_dbval_rd4

     module procedure :: assign_dbval_b0
     module procedure :: assign_dbval_b1
     module procedure :: assign_dbval_b2
     module procedure :: assign_dbval_b3
     module procedure :: assign_dbval_b4

     ! assign_str
  end interface assignment(=)

  interface assignment(=)
     module procedure :: assign_dbval_ptr_i0
     module procedure :: assign_dbval_ptr_i1
     module procedure :: assign_dbval_ptr_i2
     module procedure :: assign_dbval_ptr_i3
     module procedure :: assign_dbval_ptr_i4

     module procedure :: assign_dbval_ptr_rf0
     module procedure :: assign_dbval_ptr_rf1
     module procedure :: assign_dbval_ptr_rf2
     module procedure :: assign_dbval_ptr_rf3
     module procedure :: assign_dbval_ptr_rf4

     module procedure :: assign_dbval_ptr_rd0
     module procedure :: assign_dbval_ptr_rd1
     module procedure :: assign_dbval_ptr_rd2
     module procedure :: assign_dbval_ptr_rd3
     module procedure :: assign_dbval_ptr_rd4

     module procedure :: assign_dbval_ptr_b0
     module procedure :: assign_dbval_ptr_b1
     module procedure :: assign_dbval_ptr_b2
     module procedure :: assign_dbval_ptr_b3
     module procedure :: assign_dbval_ptr_b4
  end interface assignment(=)

contains

  subroutine dbval_destroy( me )
    !! deallocate/destroy
    implicit none
    class( dbval ), intent(inout) :: me
    if( associated(me%i )) deallocate(me%i )
    if( associated(me%rf)) deallocate(me%rf)
    if( associated(me%rd)) deallocate(me%rd)
    if( associated(me%b )) deallocate(me%b)
    if( allocated(me%str)) deallocate(me%str)
    me%dtype = DTYPE_UNKNOWN
    me%drank = -1
    if( allocated(me%dsize))deallocate(me%dsize)
    me%cptr = c_null_ptr
  end subroutine dbval_destroy

  subroutine dbval_ptr_destroy(me)
    type(dbval_ptr), intent(inout) :: me
    if(allocated(me%dsize))deallocate(me%dsize)
    if(associated(me%dbval))nullify(me%dbval)
  end subroutine dbval_ptr_destroy

  subroutine dbval_nullify(me)
    !! nullify
    implicit none
    class( dbval ), intent(inout) :: me
    nullify(me%i )
    nullify(me%rf)
    nullify(me%rd)
    nullify(me%b)
    if(allocated(me%str))deallocate(me%str)
    me%dtype = DTYPE_UNKNOWN
    me%drank = -1
    if(allocated(me%dsize))deallocate(me%dsize)
    if(allocated(me%errstr))deallocate(me%errstr)
    me%cptr = c_null_ptr
  end subroutine dbval_nullify


  function dbval_constructor_cptr( val, dtype, drank, dsize, ierr )result(me)
    !! constructor
    implicit none
    interface
       function c_strlen(str) bind(c, name='strlen')
         import :: c_ptr, c_size_t
         type(c_ptr), intent(in), value :: str
         integer(c_size_t) :: c_strlen
       end function c_strlen
    end interface
    type( c_ptr ), value :: val
    integer, intent(in) :: dtype, drank
    integer, intent(in) :: dsize(drank)
    integer, intent(out), optional :: ierr
    type( dbval ) :: me
    integer :: ier

    integer(c_int), pointer :: i1(:)
    real(c_float ), pointer :: rf(:)
    real(c_double), pointer :: rd(:)
    character(len=1, kind=c_char), pointer :: cstr(:)
    logical(c_bool), pointer :: b(:)
    integer :: savesize, i, wordsize
    character(len=256) :: msg
    ier = 0
    call dbval_nullify(me)
    ! save into 1d array, but keep ref of dsize for later
    savesize=1
    do i = 1, drank
       savesize = savesize*dsize(i)
    end do
    select case( dtype )
    case( DTYPE_INT )
       call c_f_pointer( val, i1, shape=[savesize] )
       allocate(me%i, source=int(i1, kind(me%i)), stat=ier, errmsg=msg )
    case( DTYPE_REAL32 )
       call c_f_pointer( val, rf, shape=[savesize] )
       allocate(me%rf, source=real(rf, kind(me%rf)), stat=ier, errmsg=msg )
    case( DTYPE_REAL64 )
       call c_f_pointer( val, rd, shape=[savesize] )
       allocate(me%rd, source=real(rd, kind(me%rd)), stat=ier, errmsg=msg )
    case( DTYPE_BOOL )
       call c_f_pointer( val, b, shape=[savesize] )
       allocate(me%b, source=logical(b, kind(me%b)), stat=ier, errmsg=msg )
    case( DTYPE_STR )
       ! only rank-0 for strings ...
       select case( drank )
       case( 0 )
          wordsize=c_strlen( val )
          call c_f_pointer( val, cstr, shape=[wordsize+1] )
          allocate( character(len=wordsize) :: me%str )
          do i = 1, wordsize
             me%str(i:i) = cstr(i)
          end do
       case default
          ier = -2
          write(msg, "('Error at :: ',a,'::',i0,1x,a)") __FILE__,__LINE__,&
               "rank>0 not supported for DTYPE_STR"
       end select
    case default
       ier = -1
       write(msg, "('Error at :: ',a,'::',i0,1x,a,i0)") __FILE__,__LINE__,&
            "unknown dtype value::", dtype
    end select
    if(present(ierr))ierr=ier
    if(ier/=0)then
       call dbval_nullify(me)
       me%errstr=trim(msg)
       return
    end if
    ! set the cptr here, because of ier checking
    if( associated(me%i )) me%cptr = c_loc(me%i(1))
    if( associated(me%rf)) me%cptr = c_loc(me%rf(1))
    if( associated(me%rd)) me%cptr = c_loc(me%rd(1))
    if( associated(me%b )) me%cptr = c_loc(me%b(1))
    me%drank = drank
    me%dtype = dtype
    me%dsize = dsize
  end function dbval_constructor_cptr


  subroutine write_formatted( me, unit, iotype, v_list, iostat, iomsg )
    !! overload:
    !! write(*,*) dbval
    implicit none
    class(dbval), intent(in) :: me
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in)  :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write(unit, "(a,a,1x)", iostat=iostat, iomsg=iomsg ) "dtype::",trim(get_dtype_str(me%dtype))
    if( iostat /= 0 ) return
    write(unit, "(a,i0,1x)", iostat=iostat, iomsg=iomsg ) "drank::", me%drank
    if( me%drank>0) write(unit, "(a,*(i0,1x))", iostat=iostat, iomsg=iomsg ) "dsize::", me%dsize
    write(unit,"(a)", iostat=iostat,iomsg=iomsg ) "vals::"
    select case( me%dtype )
    case( DTYPE_INT    ); write(unit, "(*(g0,:,1x))", iostat=iostat, iomsg=iomsg ) me%i
    case( DTYPE_REAL32 ); write(unit, "(*(g0,:,1x))", iostat=iostat, iomsg=iomsg ) me%rf
    case( DTYPE_REAL64 ); write(unit, "(*(g0,:,1x))", iostat=iostat, iomsg=iomsg ) me%rd
    case( DTYPE_BOOL   ); write(unit, "(*(g0,:,1x))", iostat=iostat, iomsg=iomsg ) me%b
    case( DTYPE_STR    ); write(unit, "(a)", iostat=iostat, iomsg=iomsg ) me%str
    end select
  end subroutine write_formatted

  subroutine write_unformatted(me, unit, iostat, iomsg)
    !! overload:
    !! write(u0) dbval
    implicit none
    class(dbval), intent(in) :: me
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    ! write meta
    write(unit, iostat=iostat, iomsg=iomsg) me%dtype; if(iostat/=0)return
    write(unit, iostat=iostat, iomsg=iomsg) me%drank; if(iostat/=0)return
    if( me%drank > 0 )then
       write(unit, iostat=iostat, iomsg=iomsg) me%dsize; if(iostat/=0)return
    end if
    select case( me%dtype )
    case( DTYPE_INT )
       write(unit) size(me%i)
       write(unit, iostat=iostat, iomsg=iomsg ) me%i
    case( DTYPE_REAL32 )
       write(unit) size(me%rf)
       write(unit, iostat=iostat, iomsg=iomsg ) me%rf
    case( DTYPE_REAL64 )
       write(unit) size(me%rd)
       write(unit, iostat=iostat, iomsg=iomsg ) me%rd
    case( DTYPE_BOOL )
       write(unit) size(me%b)
       write(unit, iostat=iostat, iomsg=iomsg ) me%b
    case( DTYPE_STR )
       write(unit) len(me%str)
       write(unit, iostat=iostat, iomsg=iomsg ) me%str
    end select
  end subroutine write_unformatted

  subroutine read_unformatted(me, unit, iostat, iomsg)
    !! overload:
    !! read(u0) dbval
    implicit none
    class(dbval), intent(inout) :: me
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    integer :: allocsize
    call dbval_nullify(me)
    read(unit, iostat=iostat, iomsg=iomsg) me%dtype; if(iostat/=0)return
    read(unit, iostat=iostat, iomsg=iomsg) me%drank; if(iostat/=0)return
    allocate( me%dsize(1:me%drank) )
    if( me%drank > 0 ) then
       read(unit, iostat=iostat, iomsg=iomsg) me%dsize; if(iostat/=0)return
    end if
    ! read actual allocation size
    read(unit, iostat=iostat, iomsg=iomsg) allocsize; if(iostat/=0)return
    select case( me%dtype )
    case( DTYPE_INT )
       allocate( me%i(1:allocsize) )
       read(unit, iostat=iostat, iomsg=iomsg) me%i; if(iostat/=0)return
       me%cptr = c_loc(me%i(1))
    case( DTYPE_REAL32 )
       allocate( me%rf(1:allocsize) )
       read(unit, iostat=iostat, iomsg=iomsg) me%rf; if(iostat/=0)return
       me%cptr = c_loc(me%rf(1))
    case( DTYPE_REAL64 )
       allocate( me%rd(1:allocsize) )
       read(unit, iostat=iostat, iomsg=iomsg) me%rd; if(iostat/=0)return
       me%cptr = c_loc(me%rd(1))
    case( DTYPE_BOOL )
       allocate( me%b(1:allocsize) )
       read(unit, iostat=iostat, iomsg=iomsg) me%b; if(iostat/=0)return
       me%cptr = c_loc(me%b(1))
    case( DTYPE_STR )
       allocate( character(len=allocsize)::me%str )
       read(unit, iostat=iostat, iomsg=iomsg) me%str; if(iostat/=0)return
    end select
  end subroutine read_unformatted


  !! return error string from dbval
  function dbval_errmsg(me)result(msg)
    class( dbval ), intent(inout) :: me
    character(:), allocatable :: msg
    allocate(msg, source = me%errstr )
  end function dbval_errmsg


  subroutine dbval_copy( from, to )
    !! copy a dbval instance
    implicit none
    type( dbval ), intent(in) :: from
    type( dbval ), intent(out) :: to
    type( c_ptr ) :: cptr
    call dbval_nullify(to)
    to = dbval( from%cptr, from%dtype, from%drank, from%dsize )
  end subroutine dbval_copy




  function get_dtype_str(val)result(str)
    !! return string of datatype depending on encoder value
    implicit none
    integer, intent(in) :: val
    character(len=10) :: str
    select case( val )
    case( DTYPE_UNKNOWN ); str = "unknown"
    case( DTYPE_INT     ); str = "integer"
    case( DTYPE_REAL32  ); str = "real32"
    case( DTYPE_REAL64  ); str = "real64"
    case( DTYPE_BOOL    ); str = "bool"
    case( DTYPE_STR     ); str = "string"
    case default         ; str = "invalid"
    end select
  end function get_dtype_str


  function get_dtype_val(name)result(val)
    !! return encoder value based on name
    implicit none
    character(*), intent(in) :: name
    integer :: val
    select case( name )
    case( "DTYPE_UNKNOWN" ); val = DTYPE_UNKNOWN
    case( "DTYPE_INT"     ); val = DTYPE_INT
    case( "DTYPE_REAL32"  ); val = DTYPE_REAL32
    case( "DTYPE_REAL64"  ); val = DTYPE_REAL64
    case( "DTYPE_BOOL"    ); val = DTYPE_BOOL
    case( "DTYPE_STR"     ); val = DTYPE_STR
    end select
  end function get_dtype_val

end module m_dbval
