program tester
    use newton_cotes_quad

    implicit none

    print*, midpoint(a, 0d0, 0d0, 10d0)

contains

    function a()
        real(8) :: a
        a = 0
        return
    end function

end program
