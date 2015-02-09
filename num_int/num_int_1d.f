module newton_cotes_quad
    ! Module implementing small-order Newton-Cotes quadratures
    ! numerical integration. 
    ! 
    ! Public:
    !   - function: midpoint(func, lower, upper, N)
    !   - function: trapezoid(func, lower, upper, N)
    !   - function: simpson(func, lower, upper, N)
    ! --

    implicit none

    private        ! Default to private

    public midpoint, trapezoid, simpson

    ! Working precision
    integer, parameter :: wp = 8

contains

    ! 
    ! Numerical integration using the midpoint method
    ! 
    function midpoint(func, lower, upper, N)
        ! PARAMS
        real(wp), intent(in)    :: lower
        real(wp), intent(in)    :: upper
        real(wp), intent(in)    :: N
        real(wp), external      :: func

        ! OUTPUT
        real(wp) :: midpoint

        ! ALLOC
        real(wp) :: step

        ! BEGIN
        step = (upper - lower) / N

        midpoint = 1
        return
    end function

    real(wp) function trapezoid(func, lower, upper, N)
        real(wp), intent(in) :: lower
        real(wp), intent(in) :: upper
        real(wp), intent(in) :: N
        real(wp), external :: func
    end function

    real(wp) function simpson(func, lower, upper, N)
        real(wp), intent(in) :: lower
        real(wp), intent(in) :: upper
        real(wp), intent(in) :: N
        real(wp), external :: func
    end function

end module
