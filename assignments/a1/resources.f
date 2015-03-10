! ###################### MODULE ##################
! Precision module
! 
! Contains:
!   Precision parameteres:
!     - sp: single precision
!     - dp: double precision
!     - wp: working precision
!   Math contants:
!     - PI
!   Physical constants:
!     - ELEMENTARY_CHARGE
! 
! ################################################
module precision_w

    implicit none

    private         ! Default to private

    public ::   sp, dp, wp, &
                PI,         &
                SPEED_OF_LIGHT, NEWTON_GRAVITY, PLANCK_CONSTANT, PLANCK_REDUCED, VACUUM_PERMEABILITY, &
                VACUUM_PERMITTIVITY, VACUUM_IMPEDANCE, ELEMENTARY_CHARGE


    ! ######## Precision ##############
    
    ! -- single
    integer, parameter :: sp = kind(0.0)

    ! -- double
    integer, parameter :: dp = kind(0.0d0)

    ! -- working
    integer, parameter :: wp = dp


    ! ######## Math constants #########

    ! -- PI
    real(wp), parameter :: PI           = 4.0_wp * DATAN(1.0_wp)


    ! ######## Physical constants #####
    ! Updated: 2015-03-06

    ! -- Universal constants
    real(wp), parameter :: SPEED_OF_LIGHT           = 299792458_wp                                          ! [m/s]
    real(wp), parameter :: NEWTON_GRAVITY           = 6.6738480E-11_wp                                      ! [m^3/kg.s^2]
    real(wp), parameter :: PLANCK_CONSTANT          = 6.6260695729E-34_wp                                   ! [J.s]
    real(wp), parameter :: PLANCK_REDUCED           = 1.05457172647E-34_wp                                  ! [J.s]

    ! -- Electromagnetic constants
    real(wp), parameter :: VACUUM_PERMEABILITY      = 4.0E-7_wp * PI                                        ! [N/A^2]
    real(wp), parameter :: VACUUM_PERMITTIVITY      = 1.0_wp / (VACUUM_PERMEABILITY * SPEED_OF_LIGHT ** 2)  ! [F/m]
    real(wp), parameter :: VACUUM_IMPEDANCE         = VACUUM_PERMEABILITY * SPEED_OF_LIGHT                  ! [ohm]
    real(wp), parameter :: ELEMENTARY_CHARGE        = 1.60217656535E-19_wp                                  ! [C]

end module

! ##################### MODULE #################
! Generic resources
! 
! Contains:
!   Subroutines
!       - init_random_seed()
! 
! ##############################################
module resources

    ! Use 64-bit int in init_random_seed
    use iso_fortran_env, only: int64

    implicit none

    private         ! Default to private

    public :: init_random_seed

contains

    ! 
    ! Initializes random_seed() : public
    ! 
    ! Provides initialization of the Fortran random number generator with 
    ! a good seed. First checks for system-provided random seed in specified 
    ! path (default is "/dev/urandom"). Otherwise, a pseudo-random seed is 
    ! generated from the value of the system clock.
    ! 
    subroutine init_random_seed(seed)
        ! ARGS
        integer, dimension(:), intent(out), allocatable :: seed

        ! Path to system random number generator
        character(len = *), parameter :: file_path_random = "/dev/urandom"

        ! ALLOC
        integer :: i, n, un, istat, dt(8), pid
        integer(int64) :: t

        ! Get random seed size
        call random_seed(size = n)
        allocate(seed(n))

        ! First try if the OS provides a random number generator.
        
        open(newunit = un, file=file_path_random, access="stream", &
            form="unformatted", action="read", status="old", iostat = istat)
        if (istat == 0) then
            read(un) seed
            close(un)
        else

            ! Fallback to XOR:ing the current time and pid. The PID is
            ! useful in case one launches multiple instances of the same
            ! program in parallel.
            
            call system_clock(t)
            if (t == 0) then
                call date_and_time(values = dt)

                ! Convert date and time to miliseconds
                t = (dt(1) - 1970) * 365_int64 * 24 * 60 * 60 * 1000 &
                    + dt(2) * 31_int64 * 24 * 60 * 60 * 1000 &
                    + dt(3) * 24_int64 * 60 * 60 * 1000 &
                    + dt(5) * 60 * 60 * 1000 &
                    + dt(6) * 60 * 1000 + dt(7) * 1000 &
                    + dt(8)
            end if
            pid = getpid()
            t = ieor(t, int(pid, kind(t)))

            ! Generate seed
            do i = 1, n
                seed(i) = lcg(t)
            end do
        end if

        ! Feed seed
        call random_seed(put = seed)

    end subroutine init_random_seed

    ! Simple PRNG
    ! Private
    function lcg(s)
        ! ALLOC
        integer :: lcg
        integer(int64) :: s

        if (s == 0) then
            s = 104729
        else
            s = mod(s, 4294967296_int64)
        end if
        
        s = mod(s * 279470273_int64, 4294967291_int64)
        lcg = int(mod(s, int(huge(0), int64)), kind(0))
    end function lcg

end module

