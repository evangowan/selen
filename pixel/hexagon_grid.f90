module hexagon_grid

	implicit none
! this module contains the variables and subroutines that define the Tegmark grid

	double precision, save :: x_min, x_max, y_min, y_max, grid_radius
	integer, save :: number_hexagons
	integer, parameter :: hexagon_points = 6

	double precision, allocatable, dimension(:) :: hexagon_x, hexagon_y
	double precision, allocatable, dimension(:,:) :: hexagon_corner_x, hexagon_corner_y

	double precision, parameter :: pi = 3.14159265359

contains

subroutine definine_grid(x_min_in, x_max_in, y_min_in, y_max_in, grid_radius_in)
	implicit none

	double precision, intent(in) :: x_min_in, x_max_in, y_min_in, y_max_in, grid_radius_in
	integer :: large_hexagon_row, hexagon_column, number_est_hexagons, counter

	integer :: shift_direction
	double precision :: x_temp, y_temp, x_temp_hex(hexagon_points), y_temp_hex(hexagon_points), angle, x_last, y_last

	logical :: done, add_hex


	x_min = x_min_in
	x_max = x_max_in
	y_min = y_min_in
	y_max = y_max_in
	grid_radius = grid_radius_in

	! the grid is defined to start at the bottom left corner, centered on the hexagon, and designed to ensure that the entire input
	! regular grid is covered by hexagons

	large_hexagon_row = ceiling(((x_max - x_min)/grid_radius + grid_radius)/2.0) ! plus one radii to account for the fact we are starting at the center of a hexagon

!	small_hexagon_row = large_hexagon_row - 2 ! should be true, I think

	hexagon_column = ceiling((y_max-y_min) / (2.0 * grid_radius) + grid_radius)

	number_est_hexagons = large_hexagon_row * hexagon_column ! should be a maximum estimate, but not too far off

	allocate(hexagon_x(number_est_hexagons), hexagon_y(number_est_hexagons), hexagon_corner_x(number_est_hexagons, hexagon_points),&
	  hexagon_corner_y(number_est_hexagons, hexagon_points))

	number_hexagons = 1

	hexagon_x(number_hexagons) = x_min
	hexagon_y(number_hexagons) = y_min
	x_last = hexagon_x(number_hexagons)
	y_last = hexagon_y(number_hexagons)

	call hexagon_coordinates(hexagon_x(number_hexagons), hexagon_y(number_hexagons), grid_radius, &
        hexagon_corner_x(number_hexagons,:), hexagon_corner_y(number_hexagons,:))

	done = .false.

	shift_direction = 1.0

	do while (.not. done)

		if(shift_direction > 0) THEN
			x_temp = x_last + grid_radius * 2
			y_temp = y_last
			call hexagon_coordinates(x_temp, y_temp, grid_radius, x_temp_hex, y_temp_hex)

			if(x_temp_hex(3) > x_max .and. x_temp_hex(4) > x_max) THEN ! shift y up
				x_temp = x_temp + 2.0 * grid_radius * cos(pi / 3.0)
				y_temp = y_temp + 2.0 * grid_radius * sin(pi / 3.0)

				call hexagon_coordinates(x_temp, y_temp, grid_radius, x_temp_hex, y_temp_hex)
				shift_direction = -1.0
			endif



		else
			x_temp = x_last - grid_radius * 2
			y_temp = y_last
			call hexagon_coordinates(x_temp, y_temp, grid_radius, x_temp_hex, y_temp_hex)

			if(x_temp_hex(1) < x_min .and. x_temp_hex(6) < x_min) THEN ! shift y up
				x_temp = x_temp - 2.0 * grid_radius * cos(pi / 3.0)
				y_temp = y_temp + 2.0 * grid_radius * sin(pi / 3.0)

				call hexagon_coordinates(x_temp, y_temp, grid_radius, x_temp_hex, y_temp_hex)
				shift_direction = 1.0
			endif

		endif
!		write(6,*) x_temp, y_temp

		! check all corners to see if it is within range

		add_hex = .false.

		do counter = 1, hexagon_points, 1
			if (x_temp_hex(counter) > x_min .and. x_temp_hex(counter) < x_max) THEN
				add_hex = .true.
			endif
		end do

		! check if it goes above the y range

		if (y_temp_hex(5) >= y_max) THEN
			done = .true.
		endif
	

		if (add_hex .and. .not. done) THEN ! add the hexagon
			number_hexagons = number_hexagons + 1

			hexagon_x(number_hexagons) = x_temp
			hexagon_y(number_hexagons) = y_temp
			hexagon_corner_x(number_hexagons,:) = x_temp_hex(:)
			hexagon_corner_y(number_hexagons,:) = y_temp_hex(:)
		endif

		x_last = x_temp
		y_last = y_temp

	end do


end subroutine definine_grid

subroutine hexagon_grid_clear()

	deallocate(hexagon_x, hexagon_y, hexagon_corner_x, hexagon_corner_y)

end subroutine hexagon_grid_clear

subroutine hexagon_coordinates(x, y, radius, corners_x, corners_y)
	implicit none
	double precision, intent(in) :: x, y, radius
	double precision, dimension(hexagon_points), intent(out) :: corners_x, corners_y

	double precision :: angle, dx, dy, distance

	! some corners are easy

	angle = pi / 6.0
	dx = radius
	dy = tan(angle) * radius

!	write(6,*) "length: ", 2.0 * radius * tan(angle)

	corners_x(1) = x + dx
	corners_y(1) = y + dy

	corners_x(3) = x - dx
	corners_y(3) = y + dy

	corners_x(4) = x - dx
	corners_y(4) = y - dy

	corners_x(6) = x + dx
	corners_y(6) = y - dy

	! other two corners, I can just use the length from one of the previous corners

	distance = sqrt( (corners_x(1) - x)**2 + (corners_y(1) - y)**2)

	corners_x(2) = x
	corners_y(2) = y + distance

	corners_x(5) = x
	corners_y(5) = y - distance
	

end subroutine hexagon_coordinates

end module hexagon_grid
