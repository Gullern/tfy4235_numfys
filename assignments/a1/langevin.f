! #################### PROGRAM ################
! Langevin
! Assignment 1
! 
! #############################################
program langvein

    use precision_w, only : wp, PI, ELEMENTARY_CHARGE
    use resources, only : init_random_seed

    implicit none

    ! ########### PARAM ############

    ! -- Number of particles
    integer, parameter      :: NUM_PAR = 10

    ! -- Time length
    integer, parameter      :: TIME = 500

    ! -- Acuracy
    ! 
    ! dt = dt0 * accuracy, where dt0 is the critical
    ! time step value. 
    real(wp), parameter     :: DT_ACCURACY     = 1E-2_wp

    ! -- Standard values
    real(wp), parameter     :: std_r1       = 12E-9_wp
    real(wp), parameter     :: std_alpha    = 0.2_wp
    real(wp), parameter     :: std_L        = 20E-6_wp
    real(wp), parameter     :: std_eta      = 1E-3_wp
    real(wp), parameter     :: std_kB_T     = 26E-3_wp
    real(wp), parameter     :: std_DU       = 80.0_wp
    real(wp), parameter     :: std_x0       = 0.0_wp
    real(wp), parameter     :: std_period   = 1.0_wp

    ! ########### VARS #############

    ! -- Parameters
    real(wp)                :: r1
    real(wp)                :: alpha
    real(wp)                :: L
    real(wp)                :: eta
    real(wp)                :: kB_T
    real(wp)                :: DU
    real(wp)                :: x0
    real(wp)                :: period

    ! -- Derived parameters
    real(wp)                :: gamma1
    real(wp)                :: omega
    real(wp)                :: diff
    real(wp)                :: dt

    ! -- Path
    ! 
    ! To be replaces by preprocessor
    character(len=*), parameter     :: PATH_FIG     =   PRE_PATH_FIG
    character(len=*), parameter     :: PATH_OUTPUT  =   PRE_PATH_OUTPUT

    ! -- Log file
    character(len=*), parameter     :: LOG_FILENAME             = PATH_OUTPUT // 'logfile.log'
    integer, parameter              :: LOG_OUTFID               = 10
    
    ! -- Output files
    character(len=*), parameter     :: TRAJECTORIES_FILENAME    = PATH_OUTPUT // 'biased_diffusion.dat'
    integer, parameter              :: TRAJECTORIES_OUTFID      = 20
    character(len=*), parameter     :: POTENTIAL_FILENAME       = PATH_OUTPUT // 'potential_energy.dat'
    integer, parameter              :: POTENTIAL_OUTFID         = 30

    ! -- Vars
    real(wp), dimension(NUM_PAR)        :: particles
    real(wp)                            :: max_force
    real(wp)                            :: K0
    real(wp)                            :: end_time
    integer, dimension(:), allocatable  :: rand_seed

    ! ########## END DECL ########

    ! -- Seed random
    call init_random_seed(rand_seed)

    ! -- Get input
    call input()

    ! -- Process parameters
    call process_parameters()

    ! -- Begin Euler scheme
    call euler()

    ! -- Calculate drift velocity
    call calc_drift_velocity()

    ! -- Write log file
    call write_logfile()

    deallocate(rand_seed)

contains

    ! 
    ! Euler scheme
    ! 
    ! Finds path determined by ODE by iterative Euler 
    ! scheme. Writes trajectories and potential energy
    ! to file. 
    ! 
    subroutine euler()
        ! -- PARAM
        integer, parameter  :: WRITE_MOD = 1

        ! -- VARS
        integer     :: i
        real(wp)    :: t

        ! -- Initiate in reduced units
        particles = x0 / L
        t = 0.0_wp

        ! -- Write to file
        !open(TRAJECTORIES_OUTFID, file=TRAJECTORIES_FILENAME)
        open(POTENTIAL_OUTFID, file=POTENTIAL_FILENAME)
        !write(TRAJECTORIES_OUTFID, *) particles

        ! -- Main loop
        do i = 1, FLOOR(time / DT_ACCURACY)
            particles = particles + force(particles, t) * dt + SQRT(2 * diff * dt) * rand_gauss(SIZE(particles, 1))
            t = t + dt

            ! -- Write to file
            if (MOD(i, WRITE_MOD) == 0) then
                !write(TRAJECTORIES_OUTFID, *) particles
                write(POTENTIAL_OUTFID, *) potential(particles, t)
            end if
        end do

        !close(TRAJECTORIES_OUTFID)
        close(POTENTIAL_OUTFID)

        end_time = t
    end subroutine

    !
    ! Calculate drift velocity
    ! 
    ! Uses the end positions of the particle to 
    ! calculate the average drift velocity of 
    ! each particle. 
    ! 
    subroutine calc_drift_velocity()
        ! -- VARS
        real(wp) :: drift_velocity

        drift_velocity = SUM(particles / end_time) / SIZE(particles)
        print *, drift_velocity
    end subroutine

    !
    ! Process parameters
    !
    ! Does initial parameter processing, including
    ! unit conversion and calculation of derived
    ! parameters. 
    ! 
    subroutine process_parameters()
        ! -- Convert to SI
        DU = DU * ELEMENTARY_CHARGE
        kB_T = kB_T * ELEMENTARY_CHARGE

        ! -- Calculate derived parameters
        gamma1  = 6.0_wp * PI * eta * r1
        omega   = DU / (gamma1 * L ** 2)
        diff    = kB_T / DU

        ! -- Find dt
        max_force = MAX(1.0_wp / alpha, 1.0_wp / ( 1.0_wp - alpha))
        K0 = (max_force * alpha / (64.0_wp * diff)) ** ( 1.0_wp / 2.0_wp)
        K0 = MAX(k0, 1.0_wp)
        dt = alpha ** 2 / (128.0_wp * diff * K0 ** 2)
        dt = dt * DT_ACCURACY
    end subroutine

    ! 
    ! Read input parameters
    !
    ! Reads all relevant input parameters 
    ! from terminal.
    ! 
    subroutine input()
        ! -- PARAM
        character(len=*), parameter :: FORMAT_0 = "(A)"
        character(len=*), parameter :: FORMAT_1 = "(A, F14.10, A)"

        write(*, FORMAT_0) 'Input parameters:'

        ! Gather input
        write(*, FORMAT_1, advance='no') 'Potential strength DeltaU (', std_DU, '):'
        call input_w_default(DU, std_DU)
        write(*, FORMAT_1, advance='no') 'Initial particle position x0 (', std_x0, '):'
        call input_w_default(x0, std_x0)
        write(*, FORMAT_1, advance='no') 'Time period tau (', std_period, '):'
        call input_w_default(period, std_period)
        write(*, FORMAT_1, advance='no') 'Particle radius r1 (', std_r1, '):'
        call input_w_default(r1, std_r1)
        write(*, FORMAT_1, advance='no') 'Potential asymmetry factor alpha (', std_alpha, '):'
        call input_w_default(alpha, std_alpha)
        write(*, FORMAT_1, advance='no') 'Potential periodic length L (', std_L, '):'
        call input_w_default(L, std_L)
        write(*, FORMAT_1, advance='no') 'Dynamic viscosity eta (', std_eta, '):'
        call input_w_default(eta, std_eta)
        write(*, FORMAT_1, advance='no') 'Temperature energy kB_T (', std_kB_T, '):'
        call input_w_default(kB_T, std_kB_T)

        ! Validation
        if (DU == 0 .OR. r1 == 0 .OR. L == 0 .OR. eta == 0 .OR. kB_T == 0 .OR. alpha == 0) then
            write(*, *) "Fatal error!"
            write(*, *) "DU, r1, L, eta, kB_T and alpha are not allowed to be 0."
            write(*, *) "Exiting"
            call EXIT(1)    ! Hard exit
        end if
    end subroutine

    ! 
    ! Inputs from console with default
    ! 
    ! Reads a single line from standard input. If 
    ! the input is a valid number, param is set to
    ! it. If not, param is set to default_param. 
    ! Specifically, this alows a blank line (ENTER)
    ! to be given, giving the parameters its 
    ! default value. 
    ! 
    subroutine input_w_default(param, default_param)
        ! -- ARGS
        real(wp), intent(out) :: param
        real(wp), intent(in)  :: default_param

        ! -- PARAM
        integer, parameter              :: INPUT_MAX_LENGTH   = 100
        character(len=*), parameter     :: FORMAT_0     = "(A)"

        ! -- VARS
        character(len=INPUT_MAX_LENGTH) :: input_str
        real(wp)                        :: input_real
        integer                         :: flag

        read(*, FORMAT_0) input_str

        read(input_str, *, iostat=flag) input_real
        if (flag == 0) then
            param = input_real
        else
            param = default_param
        end if
    end subroutine

    ! 
    ! Write log file
    ! 
    subroutine write_logfile()
        integer, dimension(8) :: values

        open(LOG_OUTFID, file=LOG_FILENAME)
        call date_and_time(VALUES=values)
        write(LOG_OUTFID, '(A)') '## Langevin log file'
        write(LOG_OUTFID, '(A, I4.4, A, I2.2, A, I2.2, A, I2.2, A, I2.2, A, I2.2)') & 
            '## Run: ', values(1), '-', values(2), '-', &
            values(3), ' ', values(5), ':', values(6), ':', values(7)

        close(LOG_OUTFID)
    end subroutine

    ! 
    ! Potential U(x, t)
    ! 
    ! Returns the potential U at position x and 
    ! time t, all in reduced units. 
    ! 
    function potential(x, t)
        ! -- ARGS
        real(wp), dimension(:), intent(in)  :: x
        real(wp), intent(in)                :: t
        real(wp), dimension(SIZE(x, 1))     :: potential

        ! -- VARS
        real(wp), dimension(SIZE(x, 1)) :: pos
        real(wp)                        :: time

        pos = x - FLOOR(x)
        time = t - FLOOR(t / (omega * period)) * omega * period

        ! period == 0 represents time-independent potential
        if (period /= 0 .AND. time < 3.0_wp / 4.0_wp * omega * period) then
            potential = 0.0_wp
        else
            where (pos < alpha)
                potential = pos / alpha
            elsewhere
                potential = (1.0_wp - pos) / (1.0_wp - alpha)
            end where
        end if
    end function 

    ! 
    ! Force F(x, t) = - dU/dx (x, t)
    ! 
    ! Returns the force equal to the negative
    ! potential gradiant at position x and time
    ! t. All in reduced units. 
    ! 
    function force(x, t)
        ! -- ARGS
        real(wp), dimension(:), intent(in)  :: x
        real(wp), intent(in)                :: t
        real(wp), dimension(SIZE(x, 1))     :: force

        ! -- VARS
        real(wp), dimension(SIZE(x, 1)) :: pos
        real(wp)                        :: time

        pos = x - FLOOR(x)
        time = t - FLOOR(t / (omega * period)) * omega * period

        ! period == 0 represents time-independent potential
        if (period /= 0 .AND. time < 3.0_wp / 4.0_wp * omega * period) then
            force = 0.0_wp
        else
            where (pos < alpha)
                force = - 1.0_wp / alpha
            elsewhere
                force = 1.0_wp / (1.0_wp - alpha)
            end where
        end if
    end function 

    ! 
    ! Gaussian random numbers
    ! 
    ! Draws Gaussian distributed random numbers. Normal 
    ! distribution is implemented by the polar Box-Müller 
    ! algorithm, using the uniform distribution from 
    ! random_number(). 
    ! 
    function rand_gauss(length)
        ! -- ARGS
        integer, intent(in)         :: length
        real(wp), dimension(length) :: rand_gauss

        ! -- VARS
        real(wp)    :: u1
        real(wp)    :: u2
        real(wp)    :: s
        integer     :: i

        do i = 1, length
            s = 0.0_wp
            do while(s == 0 .OR. s >= 1)
                call random_number(u1)
                call random_number(u2)
                u1 = u1 * 2 - 1
                u2 = u2 * 2 - 1
                s = u1 ** 2 + u2 ** 2
            end do

            rand_gauss(i) = u1 * SQRT( - 2.0_wp * LOG(s) / s)
        end do
    end function

end program

