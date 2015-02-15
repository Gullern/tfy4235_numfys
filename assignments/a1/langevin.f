! #################### PROGRAM ################
! Langevin
! Assignment 1
! 
! #############################################
program langvein

    use precision_w, only : wp, PI
    use resources, only : init_random_seed

    implicit none

    ! PARAMETERS
    real(wp), parameter     :: r1       = 0.000000012_wp
    real(wp), parameter     :: alpha    = 0.2_wp
    real(wp), parameter     :: L        = 0.000020_wp
    real(wp), parameter     :: eta      = 0.001_wp
    real(wp), parameter     :: kB_T     = 0.026_wp
    real(wp), parameter     :: DU       = 80.0_wp

    real(wp)                :: gamma1
    real(wp)                :: omega
    real(wp)                :: diff

    real(wp)                :: dt
    real(wp)                :: period

    ! FILE
    integer, parameter :: output_fid = 20

    ! ALLOC
    integer     :: i
    real(wp)    :: x
    real(wp)    :: t

    ! Seed random
    call init_random_seed

    gamma1  = 6 * PI * eta * r1
    omega   = DU / (gamma1 * L ** 2)
    diff    = kB_T / DU



    print*,PI
    print*,DACOS(-1.0_wp)
    print*,2.0_wp * DASIN(1.0_wp)



    open(output_fid, file='gauss.dat')
    do i = 1, 1000
        write(20, *) rand_gauss()
    end do

    close(output_fid)


    ! Euler
    do i = 1, 100
        x = x - 1.0_wp / gamma1 * force(x, t) * dt + SQRT(2 * kB_T * dt / gamma1)

    end do


contains

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

        s = 0
        do while(s == 0 .or. s >= 1)
            call random_number(u1)
            call random_number(u2)
            u1 = u1 * 2 - 1
            u2 = u2 * 2 - 1
            s = u1 ** 2 + u2 ** 2
        end do

        rand_gauss = u1 * SQRT( - 2.0_wp * LOG(s) / s)

        return
    end function

end program

