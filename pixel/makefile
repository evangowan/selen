FC = gfortran

FCFLAGS = -g -fbacktrace -fbounds-check 
#FCFLAGS = -O2

# if compiling with the Intel Fortran compiler, you need to add an extra flag
#FCFLAGS = -O2 -assume byterecl

objfiles =  global_parameters.o grids.o read_icefile.o  find_flowline_fisher_adaptive_4.o flowline_location.o 



grid_creation: grid_creation.f90 hexagon_grid.o
	$(FC) -o grid_creation $(FCFLAGS) grid_creation.f90 hexagon_grid.o

hexagon_grid.o: hexagon_grid.f90
	$(FC) -o hexagon_grid.o $(FCFLAGS) -c hexagon_grid.f90

overlapping_polygon.o: overlapping_polygon.f90
	$(FC) -o overlapping_polygon.o $(FCFLAGS) -c overlapping_polygon.f90

test_overlap: test_overlap.f90 hexagon_grid.o overlapping_polygon.o
	$(FC) -o test_overlap $(FCFLAGS) test_overlap.f90 hexagon_grid.o overlapping_polygon.o