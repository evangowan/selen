module SHTOOLS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	This module contains an interface block defining all the routines
!	used in the archive SHTOOLS. These are necessary in order to use
!	implicitly shaped arrays with most subroutines.
!
!	Copyright (c) 2005, Mark A. Wieczorek
!	All rights reserved.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	integer, parameter ::	CSPHASE_DEFAULT = 1	! The default is to EXCLUDE the 
							! CONDON-SHORTLEY phase of (-1)^m
							! in front of the Legendre functions.
							! To use this phase function, set
							! CSPHASE_DEFAULT = -1

	interface
	
		subroutine PlmBar(p, lmax, z, csphase)
			integer, intent(in) ::	lmax
			REAL*8, intent(out) ::	p(:)
       			REAL*8, intent(in) ::	z
       			integer, intent(in), optional :: csphase
       		end subroutine PlmBar
       		
		subroutine PlmBar_d1(p, dp, lmax, z, csphase)
			integer, intent(in) ::	lmax
			REAL*8, intent(out) ::	p(:), dp(:) 
       			REAL*8, intent(in) ::	z
       			integer, intent(in), optional :: csphase
		end subroutine PlmBar_d1
       		
       		subroutine PlBar(p, lmax, z)
			integer, intent(in) ::	lmax
			real*8, intent(out) ::	p(:)
       			real*8, intent(in) ::	z
       		end subroutine PlBar
       		
       	       	subroutine PlBar_d1(p, dp, lmax, z)
			integer, intent(in) ::	lmax
			real*8, intent(out) ::	p(:), dp(:)
       			real*8, intent(in) ::	z
       		end subroutine PlBar_d1
       		
       		subroutine PlmSchmidt(p,lmax,z, csphase)
			integer, intent(in) ::	lmax
			real*8, intent(out) ::	p(:)
      		 	real*8, intent(in) ::	z
      		 	integer, intent(in), optional :: csphase
      		end subroutine PlmSchmidt
      		
      		subroutine PlSchmidt(p,lmax,z)
			integer, intent(in) ::	lmax
			real*8, intent(out) ::	p(:)
      		 	real*8, intent(in) ::	z
      		end subroutine PlSchmidt
      		
      		subroutine PlmSchmidt_d1(p, dp, lmax, z, csphase)
			integer, intent(in) ::	lmax
			real*8, intent(out) ::	p(:), dp(:)
      		 	real*8, intent(in) ::	z
      		 	integer, intent(in), optional :: csphase
      		end subroutine PlmSchmidt_d1
      		
      		subroutine PlSchmidt_d1(p, dp, lmax, z)
			integer, intent(in) ::	lmax
			real*8, intent(out) ::	p(:), dp(:)
      		 	real*8, intent(in) ::	z
      		end subroutine PlSchmidt_d1
      		
      	       	subroutine PLegendre(p,lmax,z)
			integer, intent(in) ::	lmax
			REAL*8, intent(out) ::	p(:)
       			REAL*8, intent(in) ::	z
       		end subroutine PLegendre
       		
       	      	subroutine PLegendreA(p,lmax,z, csphase)
			integer, intent(in) ::	lmax
			real*8, intent(out) ::	p(:)
       			real*8, intent(in) ::	z
       			integer, intent(in), optional :: csphase
       		end subroutine PLegendreA
       		
      	       	subroutine PLegendre_d1(p, dp, lmax, z)
			integer, intent(in) ::	lmax
			real*8, intent(out) ::	p(:), dp(:)
       			real*8, intent(in) ::	z
       		end subroutine PLegendre_d1
       		
       	      	subroutine PLegendreA_d1(p, dp, lmax, z, csphase)
			integer, intent(in) ::	lmax
			real*8, intent(out) ::	p(:), dp(:)
       			real*8, intent(in) ::	z
			integer, intent(in), optional :: csphase
       		end subroutine PLegendreA_d1
       	
       		subroutine CilmPlus(cilm, gridglq, lmax, nmax, mass, d, rho, w, plx, zero)
			real*8, intent(in) :: 	gridglq(:,:), mass, rho, w(:)
			real*8, intent(in), optional :: plx(:,:), zero(:)
			real*8, intent(out) :: 	cilm(:,:,:), d
			integer, intent(in) :: 	lmax, nmax
		end subroutine CilmPlus
		
		subroutine Hilm(cilm, ba, gridglq, lmax, nmax, mass, r0, rho, w, plx, zero, filter_type, filter_deg)
			real*8, intent(out) :: 	cilm(:,:,:)
			real*8, intent(in) ::	ba(:,:,:), gridglq(:,:), mass, r0, rho, w(:)
			real*8, intent(in), optional ::	plx(:,:), zero(:)
			integer, intent(in) ::	lmax, nmax
			integer, intent(in), optional :: filter_type, filter_deg
		end subroutine Hilm
		
		subroutine MakeGrid2d(grid, cilm, lmax, interval, nlat, nlong, norm, csphase, f)
			real*8, intent(in) :: 	cilm(:,:,:), interval
			real*8, intent(out) :: 	grid(:,:)
			integer, intent(in) :: 	lmax
			integer, intent(out) :: nlat, nlong
			integer, intent(in), optional ::	norm, csphase
			real*8, intent(in), optional :: 	f
		end subroutine MakeGrid2D

		subroutine GLQGridCoord(latglq, longlq, lmax, nlat, nlong)
			integer, intent(in) ::	lmax
			integer, intent(out) ::	nlat, nlong
			real*8, intent(out) ::	latglq(:), longlq(:)
		end subroutine GLQGridCoord
		
		subroutine MakeGridGLQ(gridglq, cilm, lmax, plx, zero, norm, csphase)
			real*8, intent(in) ::	cilm(:,:,:)
			real*8, intent(in), optional :: plx(:,:), zero(:)
			real*8, intent(out) ::	gridglq(:,:)
			integer, intent(in) ::	lmax
			integer, intent(in), optional :: norm, csphase
		end subroutine MakeGridGLQ
		
		subroutine PreCompute(lmax, zero, w, plx, wisdom_file, norm, csphase)
			integer, intent(in) ::	lmax
			real*8, intent(out) ::	zero(:), w(:)
			real*8, intent(out), optional ::	plx(:,:)
			integer, intent(in), optional :: norm, csphase
			character(*), intent(in), optional ::	wisdom_file
		end subroutine PreCompute

		subroutine SHExpandGLQ(cilm, lmax, gridglq, w, plx, zero, norm, csphase)	
			real*8, intent(in) ::	w(:), gridglq(:,:)
			real*8, intent(in), optional ::	plx(:,:), zero(:)
			real*8, intent(out) :: 	cilm(:,:,:)
			integer, intent(in) ::	lmax
			integer, intent(in), optional :: norm, csphase
		end subroutine SHExpandGLQ
		
		subroutine PreGLQ(x1, x2, n, zero, w)
			real*8, intent(in) :: 	x1, x2
			real*8, intent(out) ::	zero(:), w(:)
			integer, intent(in) ::	n
		end subroutine PreGLQ
		
		integer function NGLQ(degree)
			integer, intent(in) ::	degree
		end function NGLQ

		integer function NGLQSH(degree)
			integer, intent(in) ::	degree
		end function NGLQSH

		integer function NGLQSHN(degree, n)
			integer, intent(in) ::	degree, n
		end function NGLQSHN

		subroutine SHRead(filename, cilm, lmax, skip, header, error)
			character(*), intent(in) ::		filename
			integer, intent(out) ::			lmax
			real*8, intent(out) ::			cilm(:,:,:)
			real*8, intent(out), optional ::	header(:), error(:,:,:)
			integer, intent(in), optional ::	skip
		end subroutine SHRead
		
		subroutine MakeMagGrid2D(rad, phi, theta, total, cilm, r0, r, f, lmax, interval, nlat, nlong)
			real*8, intent(in) :: 	cilm(:,:,:), interval, r0, r, f
			real*8, intent(out) :: 	rad(:,:), phi(:,:), theta(:,:), total(:,:)
			integer, intent(in) :: 	lmax
			integer, intent(out) :: nlat, nlong
		end subroutine MakeMagGrid2D
		
		real*8 function SHPowerL(c, l)
			real*8, intent(in) :: c(:,:,:)
			integer, intent(in) :: l
		end function SHPowerL
		
		real*8 function SHPowerDensityL(c, l)
			real*8, intent(in) :: 	c(:,:,:)
			integer, intent(in) :: 	l
		end function SHPowerDensityL
		
		real*8 function SHCrossPowerL(c1, c2, l)
			real*8, intent(in) :: 	c1(:,:,:), c2(:,:,:)
			integer, intent(in) :: 	l
		end function SHCrossPowerL
		
		real*8 function SHCrossPowerDensityL(c1, c2, l)
			real*8, intent(in) :: 	c1(:,:,:), c2(:,:,:)
			integer, intent(in) :: 	l
		end function SHCrossPowerDensityL
		
		subroutine SHPowerSpectra(c, lmax, spectra)
			real*8, intent(in) :: 	c(:,:,:)
			integer, intent(in) :: 	lmax
			real*8, intent(out) ::	spectra(:)
		end subroutine SHPowerSpectra
		
		subroutine SHPowerSpectraDensity(c, lmax, spectra)
			real*8, intent(in) :: 	c(:,:,:)
			integer, intent(in) :: 	lmax
			real*8, intent(out) ::	spectra(:)
		end subroutine SHPowerSpectraDensity
		
		subroutine SHCrossPowerSpectra(c1, c2, lmax, cspectra)
			real*8, intent(in) :: 	c1(:,:,:), c2(:,:,:)
			integer, intent(in) :: 	lmax
			real*8, intent(out) ::	cspectra(:)
		end subroutine SHCrossPowerSpectra
		
		subroutine SHCrossPowerSpectraDensity(c1, c2, lmax, cspectra)
			real*8, intent(in) :: 	c1(:,:,:), c2(:,:,:)
			integer, intent(in) :: 	lmax
			real*8, intent(out) ::	cspectra(:)
		end subroutine SHCrossPowerSpectraDensity
		
		subroutine djpi2(dj, lmax)
			integer, intent(in) ::	lmax
			real*8, intent(out) ::	dj(:,:,:)
		end subroutine djpi2
		
		subroutine SHrtoc(rcilm, ccilm, degmax, convention, switchcs)
			real*8, intent(in) :: 	rcilm(:,:,:)
			real*8, intent(out) :: 	ccilm(:,:,:) 
			integer, intent(in), optional ::	degmax, convention, switchcs
		end subroutine SHrtoc
		
		subroutine SHctor(ccilm, rcilm, degmax, convention, switchcs)
			real*8, intent(in) :: 	ccilm(:,:,:)
			real*8, intent(out) :: 	rcilm(:,:,:)
			integer, intent(in), optional ::	degmax, convention, switchcs
		end subroutine SHctor
		
		subroutine SHCilmToCindex(cilm, cindex, degmax)
			real*8, intent(in) :: 	cilm(:,:,:)
			real*8, intent(out) :: 	cindex(:,:)
			integer, intent(in), optional ::	degmax
		end subroutine SHCilmToCindex
		
		subroutine SHCindexToCilm(cindex, cilm, degmax)
			real*8, intent(out) :: 	cilm(:,:,:)
			real*8, intent(in) ::	cindex(:,:)
			integer, intent(in), optional ::	degmax
		end subroutine SHCindexToCilm
		
		subroutine SHRotateCoef(x, cof, rcof, dj, lmax)
			real*8, intent(in) :: 	cof(:,:), dj(:,:,:), x(3)
			real*8, intent(out) ::	rcof(:,:)
			integer, intent(in) :: 	lmax
		end subroutine SHRotateCoef
		
		subroutine SHRotateRealCoef(cilmrot, cilm, lmax, x, dj)
			real*8, intent(in) ::	cilm(:,:,:), x(:), dj(:,:,:)
			real*8, intent(out) ::	cilmrot(:,:,:)
			integer, intent(in) ::	lmax
		end subroutine SHRotateRealCoef
		
		subroutine DHaj(n, aj)
			integer, intent(in) ::	n
			real*8, intent(out) ::	aj(:)
		end subroutine DHaj

		subroutine SHExpandDH(grid, n, cilm, lmax, norm, sampling, csphase)
			real*8, intent(in) ::	grid(:,:)
			real*8, intent(out) ::	cilm(:,:,:)
			integer, intent(in) ::	n
			integer, intent(out) ::	lmax
			integer, intent(in), optional :: norm, sampling, csphase
		end subroutine SHExpandDH
		
		subroutine MakeGridDH(griddh, n, cilm, lmax, norm, sampling, csphase)
			real*8, intent(in) :: 	cilm(:,:,:)
			real*8, intent(out) ::	griddh(:,:)
			integer, intent(in) :: 	lmax
			integer, intent(out) ::	n
			integer, intent(in), optional :: norm, sampling, csphase
		end subroutine MakeGridDH
		
		real*8 function MakeGridPoint(cilm, lmax, lat, longitude, norm, csphase)
			real*8, intent(in) ::	cilm(:,:,:), lat, longitude
			integer, intent(in) ::	lmax
			integer, intent(in), optional ::	norm, csphase
		end function MakeGridPoint
		
		real*8 function Wl(l, half, r, d)
			integer, intent(in) ::	l, half
			real*8, intent(in) ::	r, d
		end function Wl
		
		real*8 function WlCurv(l, half, r, d)
			integer, intent(in) ::	l, half
			real*8, intent(in) ::	r, d
		end function WlCurv
		
		subroutine SHExpandLSQ(cilm, d, lat, lon, nmax, lmax, norm, chi2, csphase)
			real*8, intent(in) ::	d(:), lat(:), lon(:)
			real*8, intent(out) ::	cilm(:,:,:)
			integer, intent(in) ::	nmax, lmax
			integer, intent(in), optional ::	norm, csphase
			real*8, intent(out), optional ::	chi2
		end subroutine SHExpandLSQ

		subroutine SHMultiply(shout, sh1, lmax1, sh2, lmax2, precomp, norm, csphase)
			real*8, intent(out) ::	shout(:,:,:)
			real*8, intent(in) ::	sh1(:,:,:), sh2(:,:,:)
			integer, intent(in) ::	lmax1, lmax2
			integer, intent(in), optional ::	precomp, norm, csphase
		end subroutine SHMultiply
		
		subroutine ComputeD0(D0, lmax, theta0)
			real*8, intent(out) ::	D0(:,:)
			real*8, intent(in) ::	theta0
			integer, intent(in) :: 	lmax
		end subroutine ComputeD0
		
		subroutine ComputeDm(dllm, lmax, m, theta0)
			real*8, intent(out) ::	dllm(:,:)
			real*8, intent(in) ::	theta0
			integer, intent(in) :: 	lmax, m
		end subroutine ComputeDm
		
		subroutine SphericalCapCoef(coef, theta, lmax)
			real*8, intent(out) ::	coef(:)
			real*8, intent(in) ::	theta
			integer, intent(in), optional ::	lmax
		end subroutine SphericalCapCoef
		
		real*8 function SHDeltaL(coef, lmax)
			real*8, intent(in) ::	coef(:)
			integer, intent(in) :: 	lmax
		end function SHDeltaL
		
		real*8 function SHDeltaX(coef, lmax, m)
			real*8, intent(in) ::	coef(:)
			integer, intent(in) :: 	lmax
			integer, intent(in), optional :: m
		end function SHDeltaX

		subroutine EigValVecSym(ain, n, eig, evec, ul)
			real*8, intent(in) ::	ain(:,:)
			integer, intent(in) ::	n
			real*8, intent(out) ::	eig(:), evec(:,:)
			character, intent(in), optional ::	ul
		end subroutine EigValVecSym
		
		subroutine SHReturnTapersM(theta0, lmax, m, tapers, eigenvalues, shannon)
			real*8, intent(in) ::	theta0
			integer, intent(in) ::	lmax, m
			real*8, intent(out) ::	tapers(:,:), eigenvalues(:)
			real*8, intent(out), optional :: shannon
		end subroutine SHReturnTapersM
		
		subroutine EigValSym(ain, n, eval, ul)
			real*8, intent(in) ::	ain(:,:)
			integer, intent(in) ::	n
			real*8, intent(out) ::	eval(:)
			character, intent(in), optional :: ul
		end subroutine EigValSym
		
		integer function SHFindLWin(theta0, m, alpha, taper_number)
			real*8, intent(in) :: 	theta0, alpha
			integer, intent(in) :: 	m
			integer, intent(in), optional ::	taper_number
		end function SHFindLWin
		
		subroutine SHAdmitCorr(G, T, lmax, admit, corr, admit_error)
			real*8, intent(in) ::	G(:,:,:), T(:,:,:)
			integer, intent(in) ::	lmax
			real*8, intent(out) ::	admit(:), corr(:)
			real*8, intent(out), optional ::	admit_error(:)
		end subroutine SHAdmitCorr
				
		subroutine SHLocalizedAdmitCorr(tapers, taper_order, lwin, lat, lon, g, t, lmax, admit, corr, K, &
			admit_error, corr_error, taper_wt, csphase, k1linsig)
			real*8, intent(in) ::	tapers(:,:), lat, lon, g(:,:,:), t(:,:,:)
			integer, intent(in) ::	lwin, lmax, K, taper_order(:)
			real*8, intent(out) ::	admit(:), corr(:)
			real*8, intent(out), optional ::	admit_error(:), corr_error(:)
			integer, intent(in), optional ::	csphase, k1linsig
			real*8, intent(in), optional ::	taper_wt(:)
		end subroutine SHLocalizedAdmitCorr

		subroutine EigValVecSymTri(ain, n, eig, evec, ul)
			real*8, intent(in) ::	ain(:,:)
			integer, intent(in) ::	n
			real*8, intent(out) ::	eig(:), evec(:,:)
			character, intent(in), optional :: ul
		end subroutine EigValVecSymTri
		
		subroutine ComputeDG82(dG82, lmax, m, theta0)
			real*8, intent(out) ::	dG82(:,:)
			real*8, intent(in) ::	theta0
			integer, intent(in) :: 	lmax, m
		end subroutine ComputeDG82	
						
		integer function PlmIndex(l,m)
			integer, intent(in)	:: l, m
		end function PlmIndex
				
		real*8 function RandomN(idum)
			integer, parameter ::	K4B=selected_int_kind(9)
			integer(K4B), intent(inout) ::	idum
		end function RandomN
		
		real*8 function RandomGaussian(idum)
			integer, intent(inout) :: 	idum
		end function RandomGaussian
 	 	
 	 	subroutine Wigner3j(w3j, jmin, jmax, j2, j3, m1, m2, m3)
			integer, intent(in) ::	j2, j3, m1, m2, m3
			integer, intent(out) ::	jmin, jmax
			real*8, intent(out) ::	w3j(:)
		end subroutine Wigner3j
 	 	
 	 	subroutine SHBias(Shh, lwin, incspectra, ldata, outcspectra)
			real*8, intent(in) ::	Shh(:), incspectra(:)
			real*8, intent(out) ::	outcspectra(:)
			integer, intent(in) ::	lwin, ldata
		end subroutine SHBias
		
		subroutine SHBiasK(tapers, lwin, numk, incspectra, ldata, outcspectra, taper_wt)
			real*8, intent(in) ::	tapers(:,:), incspectra(:)
			real*8, intent(out) ::	outcspectra(:)
			integer, intent(in) ::	lwin, ldata, numk
			real*8, intent(in), optional :: taper_wt(:)
		end subroutine SHBiasK
		
		real*8 function SHSjkPG0(incspectra, j, k, l, m, evec, lwin)
			real*8, intent(in) ::	incspectra(:), evec(:,:)
			integer, intent(in) ::	lwin, l, m, j, k
		end function SHSjkPG0
		
		Subroutine SHMTVarOpt0(l, tapers, lwin, kmax, Sff, var_opt, var_unit, weight_opt, unweighted_covar, nocross)
			real*8, intent(in) ::	tapers(:,:), Sff(:)
			real*8, intent(out) ::	var_opt(:), var_unit(:)
			integer, intent(in) ::	l, lwin, kmax
			real*8, intent(out), optional ::	weight_opt(:,:), unweighted_covar(:,:)
			integer, intent(in), optional ::	nocross
		end subroutine SHMTVarOpt0
		
		subroutine SHMultiTaperSE(mtse, sd, sh, lmax, tapers, taper_order, lmaxt, K, alpha, &
			lat, lon, taper_wt, norm, csphase)
			real*8, intent(out) ::	mtse(:), sd(:)
			real*8, intent(in) ::	sh(:,:,:), tapers(:,:)
			integer, intent(in) ::	lmax, lmaxt, K, taper_order(:)
			real*8, intent(in), optional ::	alpha(:), lat, lon, taper_wt(:)
			integer, intent(in), optional :: csphase, norm
		end subroutine SHMultiTaperSE
		
		subroutine SHMultiTaperCSE(mtse, sd, sh1, lmax1, sh2, lmax2, tapers, taper_order, lmaxt, K, &
			alpha, lat, lon, taper_wt, norm, csphase)
			real*8, intent(out) ::	mtse(:), sd(:)
			real*8, intent(in) ::	sh1(:,:,:), sh2(:,:,:), tapers(:,:)
			integer, intent(in) ::	lmax1, lmax2, lmaxt, K, taper_order(:)
			real*8, intent(in), optional ::	alpha(:), lat, lon, taper_wt(:)
			integer, intent(in), optional ::	csphase, norm
		end subroutine SHMultiTaperCSE
		
		subroutine SHReadJPL(filename, cilm, lmax, error, gm, formatstring)
			character(*), intent(in) ::	filename
			integer, intent(in) ::	lmax
			real*8, intent(out) ::	cilm(:,:,:)
			real*8, intent(out), optional :: error(:,:,:), gm(2)
			character, intent(in), optional :: formatstring*6
		end subroutine SHReadJPL
		
		subroutine SHReadCHAMP(filename, cilm, lmax, gm, r0_pot, error)
			character(*), intent(in) ::	filename
			integer, intent(out) ::	lmax
			real*8, intent(out) ::	cilm(:,:,:), gm, r0_pot
			real*8, intent(out), optional :: error(:,:,:)
		end subroutine SHReadCHAMP
		
		subroutine SHReadGRACE(filename, cilm, lmax, gm, r0_pot, error)
			character(*), intent(in) ::	filename
			integer, intent(out) ::	lmax
			real*8, intent(out) ::	cilm(:,:,:), gm, r0_pot
			real*8, intent(out), optional :: error(:,:,:)
		end subroutine SHReadGRACE
		
		subroutine MakeGeoid2D(geoid, cilm, lmax, r0pot, GM, r, interval, nlat, nlong, omega)
			real*8, intent(out) ::	geoid(:,:)
			real*8, intent(in) ::	cilm(:,:,:), r0pot, GM, r, interval
			integer, intent(in) :: lmax
			integer, intent(out) :: nlat, nlong
			real*8, intent(in), optional :: omega
		end subroutine MakeGeoid2D
		
		subroutine PlmON(p, lmax, z, csphase)
			integer, intent(in) ::	lmax
			real*8, intent(out) ::	p(:)
       			real*8, intent(in) ::	z
       			integer, intent(in), optional :: csphase
       		end subroutine PlmON
       		
       		subroutine PlON(p, lmax, z)
			integer, intent(in) ::	lmax
			real*8, intent(out) ::	p(:)
       			real*8, intent(in) ::	z
       		end subroutine PlON

		subroutine PlmON_d1(p, dp, lmax, z, csphase)
			integer, intent(in) ::	lmax
			real*8, intent(out) ::	p(:), dp(:)
       			real*8, intent(in) ::	z
       			integer, intent(in), optional :: csphase
       		end subroutine PlmON_d1

		subroutine PlON_d1(p, dp, lmax, z)
			integer, intent(in) ::	lmax
			real*8, intent(out) ::	p(:), dp(:)
       			real*8, intent(in) ::	z
       		end subroutine PlON_d1
       		
       		subroutine MakeCircleCoord(coord, lat, lon, theta0, cinterval, cnum)
			real*8, intent(in) ::	lat, lon, theta0
			real*8, intent(out) :: coord(:,:)
			real*8, intent(in), optional ::	cinterval
			integer, intent(out), optional :: cnum
		end subroutine MakeCircleCoord
		
		subroutine SHReturnTapers(theta0, lmax, tapers, eigenvalues, taper_order)
			real*8, intent(in) ::	theta0
			integer, intent(in) ::	lmax
			real*8, intent(out) ::	tapers(:,:), eigenvalues(:)
			integer, intent(out) ::	taper_order(:)
		end subroutine SHReturnTapers
		
		complex*16 function SHSjkPG(incspectra, l, m, mprime, hj_real, hk_real, mj, mk, lwin, hkcc)
			real*8, intent(in) ::	incspectra(:), hj_real(:), hk_real(:)
			integer, intent(in) ::	lwin, l, m, mprime, mj, mk, hkcc
		end function SHSjkPG
		
		Subroutine SHMTVarOpt(l, tapers, taper_order, lwin, kmax, Sff, var_opt, var_unit, weight_opt, unweighted_covar, nocross)
			real*8, intent(in) ::	tapers(:,:), Sff(:)
			real*8, intent(out) ::	var_opt(:), var_unit(:)
			integer, intent(in) ::	l, lwin, kmax, taper_order(:)
			real*8, intent(out), optional ::	weight_opt(:,:), unweighted_covar(:,:)
			integer, intent(in), optional :: 	nocross
		end subroutine SHMTVarOpt
		
		subroutine SHMTDebias (mtdebias, mtspectra, lmax, tapers, lwin, K, nl, lmid, n, taper_wt)
			real*8, intent(out) ::	mtdebias(:,:), lmid(:)
			real*8, intent(in) :: mtspectra(:,:), tapers(:,:)
			real*8, intent(in), optional :: taper_wt(:)
			integer, intent(in) :: lmax, K, lwin, nl
			integer, intent(out) :: n
		end subroutine SHMTDebias
		
		subroutine MakeGravGrid2D(grid, cilm, lmax, r0, r, f, gm, gravpot, interval, nlat, nlong, omega, gref)
			real*8, intent(in) :: 	cilm(:,:,:), interval, f, r0, r, gm
			real*8, intent(out) :: 	grid(:,:)
			integer, intent(in) :: 	lmax
			integer, intent(out) :: nlat, nlong
			character*1, intent(in) ::	gravpot
			real*8, intent(in), optional :: omega
			real*8, intent(out), optional :: gref
		end subroutine MakeGravGrid2D
		
		real*8 function SHConfidence(l_conf, r)
			real*8, intent(in) :: r
			integer, intent(in) :: l_conf
		end function SHConfidence
	
	end interface
	
end module SHTOOLS

