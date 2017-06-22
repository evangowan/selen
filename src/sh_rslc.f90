!
! SH_RSLC.F90 
!
! Last modified GS 04-11-2008 Intel port
! Re-touched August 2008 for v. 2.8 of SELEN
! Reviewed GS & FC July 2009 -  "Varying coastlines" (reprise!) 
! *** Reviewed GS & FC November 2009 - Porting under gfortran 
!
! +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! Copyright (C) 2008 Giorgio Spada, Florence Colleoni, and Paolo Stocchi 
!
! This file is part of SELEN. 
!  
! SELEN is free software: you can redistribute it and/or modify it under the 
! terms of the GNU General Public License as published by the Free Software 
! Foundation, either version 3 of the License, or at your option) any later 
! version. 
!
! SELEN is distributed in the hope that it will be useful, but WITHOUT ANY 
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
! FOR A PARTICULAR PURPOSE. See the GNU General Public License for more 
! details. 
! 
! You should have received a copy of the GNU General Public License along 
! with SELEN.  If not, see <http://www.gnu.org/licenses/>.
! +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!  Computes the 4pi-normalized, complex spherical harmonics at
!  VIRTUAL Relative Sea Level sites listed in file RSLC_FILE -
!
! Input files:
!	- "lonlat_rslc.dat"
! Output files: 
!	- shrslc.bin
!
!
! INCLUDE "harmonics.f90"

 PROGRAM S
 INCLUDE "data.inc"
 INTEGER :: I, J
 REAL*8, ALLOCATABLE :: LONS(:), LATS(:)
 COMPLEX*16, ALLOCATABLE :: Y_1(:) !Y(:,:)
 integer :: counter, counter2
 integer, parameter :: notification_count = 100
!
!
!------ sh_rslc.f: reading the coordinates of the RSL 
!       sites & computing the SH at these coordinates 
!  
!
!-- Allocate memory
!
   allocate( lons(nrslc), lats(nrslc) )
!   allocate( y(jmax,nrslc) )
   allocate( y_1(jmax) )
   
!
   write(*,*) '    - Number of virtual RSL sites:', nrslc
   write(6,*) "jmax: ", jmax
   write(6,*) "warning, will require: ", dble(jmax)  * ((dble(NRSLC) * 16.0) /(1024.0 * 1024.0 * 1024.0)), " GB of memory"
     
   OPEN(1,FILE=RSLC_FILE,STATUS='unknown')

  open(7,file='shrslc.bin',status='replace',form='unformatted', access="direct", recl = 16) 
   counter = 0
   counter2 = 1
   do i=1, NRSLC 

	if(counter2 == notification_count) then
	  write(6,*) "sh_rslc.exe: completed ", i, " out of ", NRSLC
	  write(6,*) "memory used: ", dble(counter) * 16. /(1024. * 1024. * 1024.), " GB"
	  counter2=1
	else
	  counter2 = counter2 + 1
	endif

   		read(1,*) lons(i), lats(i) 
!If(mod(i,500)==0) & 
!write(*,*) '    - sh_rslc.f:', i, '<<RSL sites>> of', nrslc
                !call harmo(lmax, lons(i), lats(i), y(:,i)) 

		call harmo(lmax, lons(i), lats(i), y_1(:)) 	

		do j = 1, jmax

			counter = counter + 1
			write(7,rec=counter) y_1(j)

		end do 
  
   enddo
!
!
!
!write(*,*) "    - sh_rslc.f: the harmonics are written on file 'shrslc.bin'"
!  open(7,file='shrslc.bin',status='unknown',form='unformatted') 
!  write(7) y
  close(7) 
!
  deallocate( lons, lats )
!  deallocate( y )
  deallocate( y_1 )
!
 END PROGRAM S
!
!
!
