program evolution
	use seed_init

	! Bak-Sneppen evolution model
	implicit none

	! Parameters
	integer, parameter :: length = 1000
	integer, parameter :: iterations = 100000

	! Vars
	real :: fitness_chain(length)
	real :: temp_chain(1)
	integer :: i
	integer :: j

	! Output file paramters
	integer, parameter :: chain_fid = 20
	integer, parameter :: mutations_fid = 30

	! Seed random
	call init_random_seed()
	
	! Init fitness chain
	call random_number(fitness_chain)

	! Open file
	open(chain_fid, file='chain.dat')
	open(mutations_fid, file='mutations.dat')

	! Write data header
	write(chain_fid, *) 'length=', length, ', iterations=', iterations
	write(mutations_fid, *) 'length=', length, ', iterations=', iterations	

	! Print initial chain
	write(*,*) fitness_chain
	write(*,*) char(13)

	! Begin algorithm
	do i = 1, iterations
		temp_chain = minloc(fitness_chain)
		j = temp_chain(1)
		call random_number(fitness_chain(j))
		call random_number(fitness_chain(mod(j - 1 + length - 1, length) + 1))
		call random_number(fitness_chain(mod(j + 1 - 1, length) + 1))
		
		! Write to files
		write(chain_fid, *) fitness_chain
		write(mutations_fid, *) j
	end do

	write(*,*) fitness_chain

	! Close files
	close(chain_fid)
	close(mutations_fid)

end program evolution
