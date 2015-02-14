! #################### PROGRAM ################
! Langevin
! Assignment 1
! 
! #############################################
program langvein

    use precision_w, only : wp

    implicit none

    ! rand_gauss()

contains

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
        real :: rand_gauss

        ! ALLOC
        real(wp) :: u1
        real(wp) :: u2

        print*,SQRT(u1)
        print*,DSQRT(u1)

        return
    end function


end program

