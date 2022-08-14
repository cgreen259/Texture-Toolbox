! FILE: glszm.f90
	SUBROUTINE glszm2d(image, glszm, x, y, maxvalue, minvalue)
!
!     Calculate GLSZM of 2D array
!
	integer :: x, y, val1, i, j, maxvalue, zone_size, minvalue
	integer :: glszm(0:maxvalue,0:x*y)
	integer :: image(1:x,1:y)
	  
!f2py intent(in) image, glszm, x, y, maxvalue, minvalue
!f2py intent(out) glszm
!f2py depend(x, y) image
!f2py depend(maxvalue, x, y) glszm

	minvalue = minvalue - 1

	do val1 = minvalue+1, maxvalue
		zone_size = 0
		do j2 = 1, y
			do i2 = 1, x
				if (image(i2,j2) .EQ. val1) then
					zone_size = 0
					call DFS(image, i2, j2, x, y, val1, zone_size, minvalue)
					if (zone_size .NE. 0) then
						glszm(val1, zone_size) = glszm(val1, zone_size) + 1
					end if
				end if
			end do
		end do
	end do	

	
	END
	
	RECURSIVE SUBROUTINE DFS(image, i, j, x, y, val1, zone_size, minvalue)
		integer, intent(in) :: i, j, x, y, val1, zone_size, minvalue
		integer, intent(in) :: image(1:x, 1:y)
		integer, intent(out) :: zone_size
		
		if (i .LT. 1 .OR. i .GT. x .OR. j .LT. 1 .OR. j .GT. y .OR. image(i,j) .NE. val1) then
			return
		else
			zone_size = zone_size + 1
		end if
		image(i,j) = minvalue
		
		call DFS(image, i-1, j-1, x, y, val1, zone_size, minvalue)
		call DFS(image, i-1, j, x, y, val1, zone_size, minvalue)
		call DFS(image, i-1, j+1, x, y, val1, zone_size, minvalue)
		call DFS(image, i, j-1, x, y, val1, zone_size, minvalue)
		call DFS(image, i, j+1, x, y, val1, zone_size, minvalue)
		call DFS(image, i+1, j-1, x, y, val1, zone_size, minvalue)
		call DFS(image, i+1, j, x, y, val1, zone_size, minvalue)
		call DFS(image, i+1, j+1, x, y, val1, zone_size, minvalue)
		
	end SUBROUTINE DFS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	
	recursive SUBROUTINE glszm3d(image, glszm, x, y, z, maxvalue, minvalue)
!
!     Calculate GLSZM of 3D array
!
	integer :: x, y, z, val1, i2, j2, k2, zone_size, maxvalue, minvalue
	integer :: glszm(0:maxvalue,0:x*y*z)
	integer :: image(1:x, 1:y, 1:z)
	  
!f2py intent(in) image, glszm, x, y, z, maxvalue, minvalue
!f2py intent(out) glszm
!f2py depend(x, y, z) image
!f2py depend(maxvalue, x, y, z) glszm

	minvalue = minvalue - 1
	
	do val1 = minvalue+1, maxvalue
		zone_size = 0
		do i2 = 1, x
			do j2 = 1, y
				do k2 = 1, z
					if (image(i2,j2,k2) .EQ. val1) then
						zone_size = 0
						call DFS3d(image, i2, j2, k2, x, y, z, val1, zone_size, minvalue)
						if (zone_size .NE. 0) then
							glszm(val1, zone_size) = glszm(val1, zone_size) + 1
						end if
					end if
				end do
			end do
		end do
	end do	

	
	END
	
	RECURSIVE SUBROUTINE DFS3d(image, i, j, k, x, y, z, val1, zone_size, minvalue)
		integer, intent(in) :: i, j, k, x, y, z, val1, zone_size, minvalue
		integer, intent(in) :: image(1:x, 1:y, 1:z)
		integer, intent(out) :: zone_size
		if (i .LT. 1 .OR. i .GT. x .OR. j .LT. 1 .OR. j .GT. y .OR. k .LT. 1 .OR. k .GT. z .OR. image(i,j,k) .NE. val1) then
			return
		else
			zone_size = zone_size + 1
		end if
		image(i,j,k) = minvalue
		
		call DFS3d(image, i-1, j-1, k-1, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i-1, j-1, k, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i-1, j-1, k+1, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i-1, j, k-1, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i-1, j, k, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i-1, j, k+1, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i-1, j+1, k-1, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i-1, j+1, k, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i-1, j+1, k+1, x, y, z, val1, zone_size, minvalue)
		
		call DFS3d(image, i, j-1, k-1, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i, j-1, k, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i, j-1, k+1, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i, j, k-1, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i, j, k+1, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i, j+1, k-1, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i, j+1, k, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i, j+1, k+1, x, y, z, val1, zone_size, minvalue)
		
		call DFS3d(image, i+1, j-1, k-1, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i+1, j-1, k, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i+1, j-1, k+1, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i+1, j, k-1, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i+1, j, k, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i+1, j, k+1, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i+1, j+1, k-1, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i+1, j+1, k, x, y, z, val1, zone_size, minvalue)
		call DFS3d(image, i+1, j+1, k+1, x, y, z, val1, zone_size, minvalue)
		

				
	end SUBROUTINE DFS3d	
	
	
! END FILE glszm.f90