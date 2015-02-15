! #################### PROGRAM ################
! Langevin
! Assignment 1
! 
! #############################################
program langvein

    use precision_w, only : wp, PI
    use resources, only : init_random_seed

    implicit none

    ! ########### PARAM ############

    ! -- Acuracy
    ! 
    ! dt = dt0 * accuracy, where dt0 is the critical
    ! time step value. 
    real(wp), parameter     :: accuracy     = 1E-4_wp

    ! -- Standard values
    real(wp), parameter     :: std_r1       = 12E-9_wp
    real(wp), parameter     :: std_alpha    = 0.2_wp
    real(wp), parameter     :: std_L        = 20E-6_wp
    real(wp), parameter     :: std_eta      = 1E-3_wp
    real(wp), parameter     :: std_kB_T     = 26E-3_wp
    real(wp), parameter     :: std_DU       = 80.0_wp
    real(wp), parameter     :: std_x0       = 0.0_wp
    real(wp), parameter     :: std_period   = 0.0_wp

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

    ! FILE
    character(len=*), parameter :: file_name    = 'diffusion.dat'
    integer, parameter          :: output_fid   = 20

    ! VARS
    real(wp) :: max_force
    real(wp) :: K0

    ! ########## END DECL ########

    ! Seed random
    call init_random_seed

    ! Get input
    call input()

    ! Calculate parameters
    gamma1  = 6.0_wp * PI * eta * r1
    omega   = DU / (gamma1 * L ** 2)
    diff    = kB_T / DU

    ! Find dt
    max_force = MAX(1.0_wp / alpha, 1.0_wp / ( 1.0_wp - alpha))
    K0 = (max_force * alpha ** 2 / (128.0_wp * sqrt(2.0_wp * diff) * diff)) ** ( 1.0_wp / 3.0_wp)
    K0 = MAX(k0, 1.0_wp)
    dt = alpha ** 2 / (128.0_wp * diff * K0 ** 2)
    dt = dt * accuracy
    !print *, dt

    ! Begin Euler scheme
    call euler()

contains

    ! 
    ! Euler scheme
    ! 
    ! Finds path determined by ODE
    ! by iteration. 
    ! 
    subroutine euler()
        ! ALLOC
        integer     :: i
        real(wp)    :: x
        real(wp)    :: t

        ! Initiate in reduced units
        x = x0 / L
        t = 0.0_wp

        open(output_fid, file=file_name)
        write(output_fid, *) x
        do i = 1, 10000
            !x = x - force(x, t) * dt + SQRT(2 * diff * dt) * rand_gauss()
            x = x + SQRT(2.0_wp * diff * dt) * rand_gauss()
            t = t + dt
            write(output_fid, *) x
        end do

        close(output_fid)
    end subroutine

    ! 
    ! Read input parameters
    !
    ! Reads all relevant input parameters 
    ! from terminal.
    ! 
    subroutine input()
        character(len=*), parameter :: format_0 = "(A)"
        character(len=*), parameter :: format_1 = "(A, F14.10, A)"

        write(*, format_0) 'Input parameters:'

        write(*, format_1, advance='no') 'Potential strength DeltaU (', std_DU, '):'
        call input_w_default(DU, std_DU)
        write(*, format_1, advance='no') 'Initial particle position x0 (', std_x0, '):'
        call input_w_default(x0, std_x0)
        write(*, format_1, advance='no') 'Time period tau (', std_period, '):'
        call input_w_default(period, std_period)
        write(*, format_1, advance='no') 'Particle radius r1 (', std_r1, '):'
        call input_w_default(r1, std_r1)
        write(*, format_1, advance='no') 'Potential asymmetry factor alpha (', std_alpha, '):'
        call input_w_default(alpha, std_alpha)
        write(*, format_1, advance='no') 'Potential periodic length L (', std_L, '):'
        call input_w_default(L, std_L)
        write(*, format_1, advance='no') 'Unknown constant eta (', std_eta, '):'
        call input_w_default(eta, std_eta)
        write(*, format_1, advance='no') 'Temperature energy kB_T (', std_kB_T, '):'
        call input_w_default(kB_T, std_kB_T)
    end subroutine

    ! 
    ! Inputs from console with default
    ! 
    ! Reads a single line from standard input. If 
    ! the input is a valid number, param is set to
    ! it. If not, param is set to default_param. 
    ! 
    subroutine input_w_default(param, default_param)
        real(wp), intent(out) :: param
        real(wp), intent(in)  :: default_param

        integer, parameter              :: MAX_LENGTH   = 100
        character(len=*), parameter     :: format_0     = "(A)"
        character(len=MAX_LENGTH)       :: input_str
        real(wp)                        :: input_real
        integer                         :: flag

        read(*, format_0) input_str

        read(input_str, *, iostat=flag) input_real
        if (flag == 0) then
            param = input_real
        else
            param = default_param
        end if
    end subroutine

    ! 
    ! Potential U(x, t)
    ! 
    ! Returns the potential U at position x and 
    ! time t, all in reduced units. 
    ! 
    function potential(x, t)

        ! ARGS
        real(wp)                :: potential
        real(wp), intent(in)    :: x
        real(wp), intent(in)    :: t

        if (t < 3.0_wp / 4.0_wp * omega * period) then
            potential = 0.0_wp
        else
            if (x < alpha) then
                potential = x / alpha
            else
                potential = (1.0_wp - x) / (1.0_wp - alpha)
            end if
        end if
    end function 

    ! 
    ! F(x, t) = - dU/dx (x, t)
    ! 
    ! Returns the force equal to the negative
    ! potential gradiant at position x and time
    ! t. All in reduced units. 
    ! 
    function force(x, t)

        ! ARGS
        real(wp)                :: force
        real(wp), intent(in)    :: x
        real(wp), intent(in)    :: t

        if (t < 3.0_wp / 4.0_wp * omega * period) then
            force = 0.0_wp
        else
            if (x < alpha) then
                force = - 1.0_wp / alpha
            else
                force = 1.0_wp / (1.0_wp - alpha)
            end if
        end if
    end function 

    ! 
    ! Gaussian random numbers
    ! 
    ! Draws Gaussian distributed random numbers. Normal 
    ! distribution is implemented by the Box-Müller 
    ! algorithm, using the uniform distribution from 
    ! random_number(). 
    ! 
    function rand_gauss()

        ! ARGS
        real(wp)    :: rand_gauss

        ! ALLOC
        real(wp)    :: u1
        real(wp)    :: u2
        real(wp)    :: s

        s = 0.0_wp
        do while(s == 0 .or. s >= 1)
            call random_number(u1)
            call random_number(u2)
            u1 = u1 * 2 - 1
            u2 = u2 * 2 - 1
            s = u1 ** 2 + u2 ** 2
        end do

        rand_gauss = u1 * SQRT( - 2.0_wp * LOG(s) / s)
    end function

end program

