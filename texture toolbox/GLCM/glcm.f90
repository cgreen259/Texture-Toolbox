! FILE: glcm.f90
	SUBROUTINE glcm2d(image, glcm, max_val, min_val, sizex, sizey)
!
!     Calculate GLCM of 2D array
!
	INTEGER :: sizex, sizey, max_val, min_val, i_min, i_max, j_min, j_max, val1, val2, i2, j2
	real*8 :: glcm(0:max_val,0:max_val)
	real*8 :: s1, s2
	integer :: image(sizex, sizey)
	
	s1 = 1.0
	s2 = sqrt(2.0)
	  
!f2py intent(in) image, glcm, max_val, min_val, sizex, sizey
!f2py intent(out) glcm
!f2py depend(max_val, max_val) glcm

	!loop over central region of array
	do j=2, sizey-1
		do i=2, sizex-1
			val1 = image(i,j)
			if (val1 >= min_val) then
				
				val2 = image(i-1, j-1)
				if (val2 >= min_val) then
					glcm(val1, val2) = glcm(val1, val2) + s2
				end if
				
				val2 = image(i, j-1)
				if (val2 >= min_val) then
					glcm(val1, val2) = glcm(val1, val2) + s1
				end if
				
				val2 = image(i+1, j-1)
				if (val2 >= min_val) then
					glcm(val1, val2) = glcm(val1, val2) + s2
				end if
				
				val2 = image(i-1, j)
				if (val2 >= min_val) then
					glcm(val1, val2) = glcm(val1, val2) + s1
				end if
				
				val2 = image(i+1, j)
				if (val2 >= min_val) then
					glcm(val1, val2) = glcm(val1, val2) + s1
				end if
				
				val2 = image(i-1, j+1)
				if (val2 >= min_val) then
					glcm(val1, val2) = glcm(val1, val2) + s2
				end if
				
				val2 = image(i, j+1)
				if (val2 >= min_val) then
					glcm(val1, val2) = glcm(val1, val2) + s1
				end if
				
				val2 = image(i+1, j+1)
				if (val2 >= min_val) then
					glcm(val1, val2) = glcm(val1, val2) + s2
				end if
			end if
		end do
	end do
	
	!loop over edges of array
	i = 1
	i_min = 1
	i_max = 2
	do j=1,sizey
		val1 = image(i,j)
		if (val1 >= min_val) then
			j_min = max(1, j-1)
			j_max = min(j+1, sizey)
			do i2 = i_min, i_max
				do j2 = j_min, j_max
					if (i .NE. i2 .OR. j .NE. j2) then
						val2 = image(i2, j2)
						if (val2 >= min_val) then 
	!						glcm(val1, val2) = glcm(val1, val2) + 1
							glcm(val1, val2) = glcm(val1, val2) + sqrt(abs(real(i2-i,8))+abs(real(j2-j,8)))
						end if
					end if
				end do
			end do
		end if
	end do	
	
	i = sizex
	i_min = sizex-1
	i_max = sizex
	do j=1,sizey
		val1 = image(i,j)
		if (val1 >= min_val) then
			j_min = max(1, j-1)
			j_max = min(j+1, sizey)
			do i2 = i_min, i_max
				do j2 = j_min, j_max
					if (i .NE. i2 .OR. j .NE. j2) then
						val2 = image(i2, j2)
						if (val2 >= min_val) then
		!					glcm(val1, val2) = glcm(val1, val2) + 1
							glcm(val1, val2) = glcm(val1, val2) + sqrt(abs(real(i2-i,8))+abs(real(j2-j,8)))
						end if
					end if
				end do
			end do
		end if
	end do
	
	j = 1
	j_min = 1
	j_max = 2
	do i=2, sizex-1
		val1 = image(i,j)
		if (val1 >= min_val) then
			i_min = max(1, i-1)
			i_max = min(i+1, sizex)
			do i2 = i_min, i_max
				do j2 = j_min, j_max
					if (i .NE. i2 .OR. j .NE. j2) then
						val2 = image(i2, j2)
						if (val2 >=min_val) then
		!					glcm(val1, val2) = glcm(val1, val2) + 1
							glcm(val1, val2) = glcm(val1, val2) + sqrt(abs(real(i2-i,8))+abs(real(j2-j,8)))
						end if
					end if
				end do
			end do
		end if
	end do	
	
	j = sizey
	j_min = sizey-1
	j_max = sizey
	do i=2,sizex-1
		val1 = image(i,j)
		if (val1 >= min_val) then
			i_min = max(1, i-1)
			i_max = min(i+1, sizex)
			do i2 = i_min, i_max
				do j2 = j_min, j_max
					if (i .NE. i2 .OR. j .NE. j2) then
						val2 = image(i2, j2)
						if (val2 >= min_val) then
		!					glcm(val1, val2) = glcm(val1, val2) + 1
							glcm(val1, val2) = glcm(val1, val2) + sqrt(abs(real(i2-i,8))+abs(real(j2-j,8)))
						end if
					end if
				end do
			end do
		end if
	end do		
	
	END SUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	SUBROUTINE glcm3d(image, glcm, max_val, min_val, sizex, sizey, sizez)
	
!	Calculate GLCM of 3D array
	
	integer :: sizex, sizey, sizez, max_val, min_val, i_min, i_max, j_min, j_max, k_min, k_max
	integer :: val1, val2, i, j, k, i2, j2, k2, idiff, jdiff, kdiff
	real*8 :: glcm(0:max_val,0:max_val)
	integer :: image(1:sizex, 1:sizey, 1:sizez)
	  
!f2py intent(in) image, glcm, max_val, min_val, sizex, sizey, sizez 
!f2py intent(out) glcm
!f2py depend(max_val, max_val) glcm	

	!loop over central region of array
	do k=2, sizez-1
		do j=2, sizey-1
			do i=2, sizex-1
				val1 = image(i,j,k)
				if (val1 >= min_val) then
					do k2 =k-1, k+1
						kdiff = abs(k2-k)
						do j2=j-1, j+1
							jdiff = abs(j2-j)
							do i2=i-1, i+1
								if (k2 .NE. k .OR. j2 .NE. j .OR. i2 .NE. i) then
									val2 = image(i2,j2,k2)
									if (val2 >= min_val) then
										idiff = abs(i2-i)
										!glcm(val1, val2) = glcm(val1, val2) + 1
										glcm(val1, val2) = glcm(val1, val2) + sqrt(real(idiff+jdiff+kdiff,8))
									end if
								end if
							end do
						end do
					end do
				end if
			end do
		end do
	end do
	
	
	!loop over edges of array
	i = 1
	i_min = 1
	i_max = 2
	do k=1,sizez
		k_min = max(1, k-1)
		k_max = min(k+1, sizez)
		do j=1,sizey
			val1 = image(i,j,k)
			if (val1 >= min_val) then
				j_min = max(1, j-1)
				j_max = min(j+1, sizey)
				do k2 = k_min, k_max
					kdiff = abs(k2-k)
					do j2 = j_min, j_max
						jdiff = abs(j2-j)
						do i2 = i_min, i_max
							if (i .NE. i2 .OR. j .NE. j2 .OR. k2 .NE. k) then
								val2 = image(i2, j2,k2)
								if (val2 >= min_val) then 
									idiff = abs(i2-i)
									!glcm(val1, val2) = glcm(val1, val2) + 1
									glcm(val1, val2) = glcm(val1, val2) + sqrt(real(idiff+jdiff+kdiff,8))
								end if
							end if
						end do
					end do
				end do
			end if
		end do
	end do	
	
	
	i = sizex
	i_min = sizex-1
	i_max = sizex
	do k=1,sizez
		k_min = max(1, k-1)
		k_max = min(k+1, sizez)
		do j=1,sizey
			val1 = image(i,j,k)
			if (val1 >= min_val) then
				j_min = max(1, j-1)
				j_max = min(j+1, sizey)
				do k2 = k_min, k_max
					kdiff = abs(k2-k)
					do j2 = j_min, j_max
						jdiff = abs(j2-j)
						do i2 = i_min, i_max
							if (i .NE. i2 .OR. j .NE. j2 .OR. k2 .NE. k) then
								val2 = image(i2, j2,k2)
								if (val2 >= min_val) then 
									idiff = abs(i2-i)
									!glcm(val1, val2) = glcm(val1, val2) + 1
									glcm(val1, val2) = glcm(val1, val2) + sqrt(real(idiff+jdiff+kdiff,8))
								end if
							end if
						end do
					end do
				end do
			end if
		end do
	end do


	j = 1
	j_min = 1
	j_max = 2
	do k=1,sizez
		k_min = max(1, k-1)
		k_max = min(k+1, sizez)
		do i=2,sizex-1
			val1 = image(i,j,k)
			if (val1 >= min_val) then
				i_min = max(1, i-1)
				i_max = min(i+1, sizex)
				do k2 = k_min, k_max
					kdiff = abs(k2-k)
					do j2 = j_min, j_max
						jdiff = abs(j2-j)
						do i2 = i_min, i_max
							if (i .NE. i2 .OR. j .NE. j2 .OR. k2 .NE. k) then
								val2 = image(i2, j2,k2)
								if (val2 >= min_val) then 
									idiff = abs(i2-i)
									!glcm(val1, val2) = glcm(val1, val2) + 1
									glcm(val1, val2) = glcm(val1, val2) + sqrt(real(idiff+jdiff+kdiff,8))
								end if
							end if
						end do
					end do
				end do
			end if
		end do
	end do	
	
	
	j = sizey
	j_min = sizey-1
	j_max = sizey
	do k=1,sizez
		k_min = max(1, k-1)
		k_max = min(k+1, sizez)
		do i=2,sizex-1
			val1 = image(i,j,k)
			if (val1 >= min_val) then
				i_min = max(1, i-1)
				i_max = min(i+1, sizex)
				do k2 = k_min, k_max
					kdiff = abs(k2-k)
					do j2 = j_min, j_max
						jdiff = abs(j2-j)
						do i2 = i_min, i_max
							if (i .NE. i2 .OR. j .NE. j2 .OR. k2 .NE. k) then
								val2 = image(i2, j2,k2)
								if (val2 >= min_val) then 
									idiff = abs(i2-i)
									!glcm(val1, val2) = glcm(val1, val2) + 1
									glcm(val1, val2) = glcm(val1, val2) + sqrt(real(idiff+jdiff+kdiff,8))
								end if
							end if
						end do
					end do
				end do
			end if
		end do
	end do	
	
	
	k = 1
	k_min = 1
	k_max = 2
	do j=2,sizey-1
		j_min = max(1, j-1)
		j_max = min(j+1, sizey)
		do i=2,sizex-1
			val1 = image(i,j,k)
			if (val1 >= min_val) then
				i_min = max(1, i-1)
				i_max = min(i+1, sizex)
				do k2 = k_min, k_max
					kdiff = abs(k2-k)
					do j2 = j_min, j_max
						jdiff = abs(j2-j)
						do i2 = i_min, i_max
							if (i .NE. i2 .OR. j .NE. j2 .OR. k2 .NE. k) then
								val2 = image(i2, j2,k2)
								if (val2 >= min_val) then
									idiff = abs(i2-i)
									!glcm(val1, val2) = glcm(val1, val2) + 1
									glcm(val1, val2) = glcm(val1, val2) + sqrt(real(idiff+jdiff+kdiff,8))
								end if
							end if
						end do
					end do
				end do
			end if
		end do
	end do	
	
	
	k = sizez
	k_min = sizez-1
	k_max = sizez
	do j=2,sizey-1
		j_min = max(1, j-1)
		j_max = min(j+1, sizey)
		do i=2,sizex-1
			val1 = image(i,j,k)
			if (val1 >= min_val) then
				i_min = max(1, i-1)
				i_max = min(i+1, sizex)
				do k2 = k_min, k_max
					kdiff = abs(k2-k)
					do j2 = j_min, j_max
						jdiff = abs(j2-j)
						do i2 = i_min, i_max
							if (i .NE. i2 .OR. j .NE. j2 .OR. k2 .NE. k) then
								val2 = image(i2, j2,k2)
								if (val2 >= min_val) then 
									idiff = abs(i2-i)
									!glcm(val1, val2) = glcm(val1, val2) + 1
									glcm(val1, val2) = glcm(val1, val2) + sqrt(real(idiff+jdiff+kdiff,8))
								end if
							end if
						end do
					end do
				end do
			end if
		end do
	end do
	END SUBROUTINE
	
! END FILE glcm.f90
















