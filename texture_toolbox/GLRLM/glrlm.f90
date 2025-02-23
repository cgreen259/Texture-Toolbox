! FILE: glrlm.f90
	SUBROUTINE glrlm2d(image, glrlm, x, y, &
		maxvalue, minvalue, angle, s)
!   Calculate GLRLM of 2D array

	INTEGER :: x, y, val1, i, j, k, i2, j2, maxvalue, minvalue, rl
	REAL :: glrlm(0:maxvalue,0:x*y)
	INTEGER :: image(1:x,1:y), image_temp(1:x, 1:y), angle(1:2)
	REAL :: s

!f2py intent(in) image, glrlm, x, y, z, angle, s, minvalue, maxvalue
!f2py intent(out) glrlm
!f2py depend(x, y, z) image, image_temp
!f2py depend(maxvalue, x, y, z) glrlm
	
	image_temp = image
	minvalue = minvalue - 1
	
	do i2 = 1, x
		do j2 = 1, y
			val1 = image_temp(i2, j2)
			if (val1 .NE. minvalue) then
				rl = 0
				call calc_rl2d(image_temp, i2, j2, x, y, angle(1), angle(2), val1, rl, minvalue)
				glrlm(val1, rl) = glrlm(val1, rl) + s				
			end if
		end do
	end do

	end subroutine
	
	
	recursive subroutine calc_rl2d(image_temp, i, j, x, y, a1, a2, val1, rl, minvalue)
		INTEGER, INTENT(in) :: i, j, x, y, val1, a1, a2, minvalue
		INTEGER, INTENT(inout) :: image_temp(1:x, 1:y)
		INTEGER, INTENT(inout) :: rl
		
		if (i .LT. 1 .OR. i .GT. x .OR. j .LT. 1 .OR. j .GT. y  .OR. image_temp(i,j) .NE. val1) then
			return
		else
			rl = rl + 1
		end if
		
		image_temp(i,j) = minvalue
		call calc_rl2d(image_temp, i+a1, j+a2, x, y, a1, a2, val1, rl, minvalue)
	end subroutine
		
	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	
	SUBROUTINE glrlm3d(image, glrlm, x, y, z, maxvalue, minvalue, angle, s)
!   Calculate GLRLM of 3D array

	INTEGER :: x, y, z, val1, i, j, k, i2, j2, k2, maxvalue, minvalue, rl, a1, a2, a3
	REAL :: glrlm(0:maxvalue,0:x*y*z)
	INTEGER :: image(1:x,1:y,1:z), image_temp(1:x, 1:y, 1:z), angle(1:3)
	REAL :: s

!f2py intent(in) image, glrlm, x, y, z, angle, s, minvalue, maxvalue
!f2py intent(out) glrlm
!f2py depend(x, y, z) image, image_temp
!f2py depend(maxvalue, x, y, z) glrlm

	
	image_temp = image
	minvalue = minvalue - 1
	a1 = angle(1)
	a2 = angle(2)
	a3 = angle(3)

	do k2 = 1, z
		do j2 = 1, y
			do i2 = 1, x
				val1 = image_temp(i2, j2, k2)
				if (val1 .GT. minvalue) then
					rl = 0
					call calc_rl3d(image_temp, i2, j2, k2, x, y, z, a1, a2, a3, val1, rl, minvalue)
					glrlm(val1, rl) = glrlm(val1, rl) + s					
				end if
			end do
		end do
	end do

	end subroutine
	
	recursive subroutine calc_rl3d(image_temp, i, j, k, x, y, z, a1, a2, a3, val1, rl, minvalue)
		INTEGER, INTENT(in) :: i, j, k, x, y, z, val1, a1, a2, a3, minvalue
		INTEGER, INTENT(inout) :: image_temp(1:x, 1:y, 1:z)
		INTEGER, INTENT(inout) :: rl
		
		if (i .LT. 1 .OR. i .GT. x .OR. j .LT. 1 .OR. j .GT. y .OR. k .LT. 1 .OR. k .GT. z .OR. image_temp(i,j,k) .NE. val1) then
			return
		else
			rl = rl + 1
		end if
		
		image_temp(i,j,k) = minvalue
		call calc_rl3d(image_temp, i+a1, j+a2, k+a3, x, y, z, a1, a2, a3, val1, rl, minvalue)
	end subroutine
	
			
! END FILE glrlm.f90
