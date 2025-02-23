! FILE: ngtdm.f90
	SUBROUTINE ngtdm2d(image, ngtdm, x, y, maxvalue)
!   Calculate NGTDM of 2D array

	integer :: x, y, i, j, i2, j2, maxvalue, gl
	real :: ngtdm(0:maxvalue), image(1:x,1:y), mysum 
 
!f2py intent(in) image, ngtdm, x, y
!f2py intent(out) ngtdm
!f2py depend(x, y, z) image
!f2py depend(maxvalue) ngtdm

	do i = 2, x-1
		do j = 2, y-1
			mysum = sum(image(i-1:i+1,j-1:j+1)) - image(i,j)
			gl = int(image(i,j))
			ngtdm(gl) = ngtdm(gl) + abs(image(i,j) - (mysum / 8.0))
		end do
	end do
	
	END SUBROUTINE
	
	
	SUBROUTINE ngtdm3d(image, ngtdm, x, y, z, maxvalue)
!   Calculate NGTDM of 3D array

	integer :: x, y, i, j, k, i2, j2, k2, maxvalue, gl
	real :: ngtdm(0:maxvalue), image(1:x,1:y, 1:z), mysum 
 
!f2py intent(in) image, ngtdm, x, y
!f2py intent(out) ngtdm
!f2py depend(x, y, z) image
!f2py depend(maxvalue) ngtdm

	do i = 2, x-1
		do j = 2, y-1
			do k = 2, z-1
				mysum = sum(image(i-1:i+1, j-1:j+1, k-1:k+1)) - image(i,j,k)
				gl = int(image(i,j,k))
				ngtdm(gl) = ngtdm(gl) + abs(image(i,j,k) - (mysum / 26.0))
			end do
		end do
	end do
	
	END SUBROUTINE
	
! END FILE ngtdm.f90
















