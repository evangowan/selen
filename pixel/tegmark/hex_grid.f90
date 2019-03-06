program hex_grid

	! this program takes the output from Tegmark's program and produces the polygon representation of the pixels

	! requires the value of "n" which was used when running Tegmark's program

	implicit none

	type pixel

		double precision, dimension(3) :: coordinates
		double precision, dimension(6) :: distance_store
		integer, dimension(6) :: nearest_pixels
		integer, dimension(6) :: sorted_pixels
		double precision, dimension(6,3) :: polygon_corners
		double precision, dimension(6) :: polygon_corners_latitude
		double precision, dimension(6) :: polygon_corners_longitude
		integer :: found_points
		double precision :: latitude, longitude
		logical :: pole_point

	end type pixel


	integer :: n, resolution, counter, counter2, counter3, counter4, rot_pixel
	integer, parameter :: in_unit = 10, out_unit = 20, points_unit=30
	character(len=80) :: dummy
	character(len=80), parameter :: in_file="test.dat", out_file = "hexagons.gmt", points_file = "points.txt"
	type(pixel), allocatable, dimension(:) :: pixel_list
	integer :: pixel_number1, pixel_number2, closest_pixel
	double precision :: x, y, z, smallest
	double precision, allocatable, dimension(:) :: distance
	double precision, parameter :: pi = 3.14159265359
	logical, allocatable, dimension(:) :: distance_mask

	double precision :: rot_dot, length1, length2, lat1, lat2, lon2, lon1
	double precision, dimension(6) :: rotate_angle
	logical, dimension(6) :: pixel_mask
	integer :: number_pole_points
	integer, dimension(2) :: pole_points
	double precision, dimension(3) :: vec1, vec2

	call get_command_argument(1,dummy)
	read(dummy,*) resolution
	n = 20*(2*resolution*(resolution-1)) + 12 ! number of points

	write(6,*) n

	allocate(pixel_list(n),distance(n), distance_mask(n))

	open(unit=in_unit, file=in_file, access="sequential", form="formatted", status="old")

	open(unit=out_unit, file=out_file, access="sequential", form="formatted", status="replace")
	open(unit=points_unit, file=points_file, access="sequential", form="formatted", status="replace")

	! read in the pixel coordinates and initialize other variables
	number_pole_points = 0
	do counter = 1, n

		read(in_unit,*) pixel_number1, pixel_number2, x, y, z

		pixel_list(pixel_number1+1)%coordinates(1) = x
		pixel_list(pixel_number1+1)%coordinates(2) = y
		pixel_list(pixel_number1+1)%coordinates(3) = z

		pixel_list(pixel_number1+1)%nearest_pixels = 0
		pixel_list(pixel_number1+1)%found_points = 0

		pixel_list(pixel_number1+1)%latitude = 90.-acos(z)*180./pi
		pixel_list(pixel_number1+1)%longitude = atan2(y,x)*180./pi

		write(points_unit,*) pixel_list(pixel_number1+1)%longitude, pixel_list(pixel_number1+1)%latitude


	end do


	! find distances
	do counter = 1, n-1



		if(pixel_list(counter)%found_points < 6) THEN ! search for close points



			distance_mask = .false.

			do counter2 = counter+1, n

				distance(counter2) = acos(dot_product(pixel_list(counter)%coordinates,pixel_list(counter2)%coordinates))

				if (pixel_list(counter2)%found_points < 6) THEN
					distance_mask(counter2) = .true.
				endif
			end do

			fill_pixel_list: do counter3 = pixel_list(counter)%found_points + 1, 6

				

				closest_pixel = minloc(distance,1,distance_mask)

				if(counter3 > 1) THEN

					if(distance(closest_pixel) > 2. * pixel_list(counter)%distance_store(1)) THEN ! exit loop

						exit fill_pixel_list
					endif

				endif

				pixel_list(counter)%nearest_pixels(counter3) = closest_pixel
				pixel_list(counter)%distance_store(counter3) = distance(closest_pixel)
				pixel_list(counter)%found_points = pixel_list(counter)%found_points + 1
				distance_mask(closest_pixel) = .false.

				! also do the recipriocol pixel

				pixel_list(closest_pixel)%found_points = pixel_list(closest_pixel)%found_points + 1
				pixel_list(closest_pixel)%nearest_pixels(pixel_list(closest_pixel)%found_points) = counter
				pixel_list(closest_pixel)%distance_store(pixel_list(closest_pixel)%found_points) = distance(closest_pixel)

			end do fill_pixel_list

		end if



	end do


	close(in_unit)

	! reorder the nearest pixels so that it is clockwise


	do counter = 1, n

		lat1 = pixel_list(counter)%latitude * pi / 180.
		lon1 = pixel_list(counter)%longitude * pi / 180.

		pixel_mask = .false.
		pixel_mask(1:pixel_list(counter)%found_points) = .true.

		! find the bearing from the central point so that the clockwise order of the points can be determined
		do counter3 = 1, pixel_list(counter)%found_points


!https://www.movable-type.co.uk/scripts/latlong.html
!var y = Math.sin(λ2-λ1) * Math.cos(φ2);
!var x = Math.cos(φ1)*Math.sin(φ2) -
!        Math.sin(φ1)*Math.cos(φ2)*Math.cos(λ2-λ1);
!var brng = Math.atan2(y, x).toDegrees();


			lat2 = pixel_list(pixel_list(counter)%nearest_pixels(counter3))%latitude * pi / 180.
			lon2 = pixel_list(pixel_list(counter)%nearest_pixels(counter3))%longitude * pi / 180.

			y = sin(lon2-lon1) * cos(lat2)
			x = cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(lon2-lon1)



			rotate_angle(counter3) = atan2(y,x)

		end do

		do counter2 = 1, pixel_list(counter)%found_points

			rot_pixel = maxloc(rotate_angle,1,pixel_mask)
			pixel_list(counter)%sorted_pixels(counter2) = pixel_list(counter)%nearest_pixels(rot_pixel)
			pixel_mask(rot_pixel) = .false.

		end do



		! next find the corners of the hexagon (or whatever shape it is). Should be as simple as taking the average of the unit vectors

		write(out_unit,'(A1,1X,I7)') ">", pixel_list(counter)%found_points
		do counter3 = 1, pixel_list(counter)%found_points


			pixel_list(counter)%polygon_corners(counter3,:) = pixel_list(counter)%coordinates + &
			  pixel_list(pixel_list(counter)%sorted_pixels(counter3))%coordinates

			if(counter3 == pixel_list(counter)%found_points) THEN
				pixel_list(counter)%polygon_corners(counter3,:) = pixel_list(counter)%polygon_corners(counter3,:) + &
			 	    pixel_list(pixel_list(counter)%sorted_pixels(1))%coordinates
			else
				pixel_list(counter)%polygon_corners(counter3,:) = pixel_list(counter)%polygon_corners(counter3,:) + &
			 	    pixel_list(pixel_list(counter)%sorted_pixels(counter3+1))%coordinates
			endif

			pixel_list(counter)%polygon_corners(counter3,:) = pixel_list(counter)%polygon_corners(counter3,:) / 3.0

			x = pixel_list(counter)%polygon_corners(counter3,1)
			y = pixel_list(counter)%polygon_corners(counter3,2)
			z = pixel_list(counter)%polygon_corners(counter3,3)


			pixel_list(counter)%polygon_corners_latitude(counter3) = 90.-acos(z)*180./pi
			pixel_list(counter)%polygon_corners_longitude(counter3) = atan2(y,x)*180./pi


			write(out_unit,*) pixel_list(counter)%polygon_corners_longitude(counter3),  &
				pixel_list(counter)%polygon_corners_latitude(counter3)

		end do


	end do
	
	close(out_unit)
	close(points_unit)
	deallocate(pixel_list,distance)
contains


double precision function get_length(xyz)
	double precision, intent(in), dimension(3) :: xyz	

	get_length = sqrt(xyz(1)**2 + xyz(2)**2 + xyz(3)**2)


end function get_length

end program hex_grid
