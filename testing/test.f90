program evolution

	!  Test
	implicit none

	real :: d, r, rres
	integer :: j, k, ires

	complex, parameter :: i = (0, 1)   ! sqrt(-1)
	complex :: x, y

	d = 2.0; r = 3.0
	j = 2; k = 3

	rres = r / d
	write(*,*) 'rres = r / d : ', rres

	ires = k / j; write(*,*) 'ires = k / j : ', ires
	ires = r / d; write(*,*) 'ires = k / j : ', ires
	rres = r / j; write(*,*) 'ires = k / j : ', ires

	x = (1, 1); y = (1, -1)
	write(*,*) i * x * y


end program evolution
