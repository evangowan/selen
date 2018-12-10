module hexagon_grid

! this module contains the variables and subroutines that define the Tegmark grid

	double precision, save :: x_min, x_max, y_min, y_max, grid_radius
	integer, save :: number_hexagons
	integer, parameter :: hexagon_points = 6

	double precision, allocatable(:) :: hexagon_x, hexagon_y
	double precision, allocatable(:,:) :: hexagon_corner_x, hexagon_corner_y


subroutine definine grid(x_min_in, x_max_in, y_min_in, y_max_in, grid_radius_in)

	double precison, intent(in) :: x_min_in, x_max_in, y_min_in, y_max_in, grid_radius_in
	integer :: large_hexagon_row, hexagon_column, number_est_hexagons


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

	

end subroutine definine grid

subroutine hexagon_points(x, y, radius, corners_x, corners_y)
	double precision, intent(in) :: x, y, radius
	double precision, dimension(hexagon_points), intent(out) :: corners_x, corners_y

	double precision :: angle, dx, dy, distance

	! some corners are easy

	angle = atan2(radius/2.0,radius)
	dx = cos(angle)
	dy = cos(angle)

	corners_x(1) = x + dx
	corners_y(1) = y + dy

	corners_x(3) = x - dx
	corners_y(3) = y + dy

	corners_x(4) = x - dx
	corners_y(4) = y - dy

	corners_x(6) = x + dx
	corners_y(6) = y - dy

	! other two corners, I can just use the length from one of the previous corners

	distance = sqrt( (corner_x(1) - x)**2 + (corner_y(1) - y)**2

	corner_x(2) = x
	corner_y(2) = y + distance

	corner_x(5) = x
	corner_y(5) = y - distance
	

end subroutine hexagon_points

end module hexagon_grid
