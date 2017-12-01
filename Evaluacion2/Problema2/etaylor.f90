SUBROUTINE exptaylor (n, j, fi, fj, exptay)
	integer, intent (IN)      :: n
	double precision, intent (IN) :: fi
	integer :: j
	double precision, dimension (100), intent(OUT) :: exptay
	double precision :: fj, term, partial_sum
	
	
	
	term = 1
	partial_sum = term
	DO j = 1, n
	 fj = dble(j)
	 term = term * fi / fj
	 partial_sum = partial_sum + term
	 exptay(j) = partial_sum
	END DO

	 
END SUBROUTINE exptaylor
	 

PROGRAM exponencial
	double precision, dimension (15) :: f
	integer :: i, j, n
	double precision, dimension (100)   :: x
	double precision, dimension (100) :: exptay
	double precision, dimension (100) :: funcion
	double precision :: fi, fj, term, partial_sum

     OPEN (1, FILE = 'exp.dat', STATUS = 'unknown')
	
	DO n=1, 15, 2
	DO i=0, 100, 1
	  fi = dble(i)
	  fi = fi / 10.0d0
	CALL exptaylor (n, j, fi, fj, exptay)
	funcion(n) = exptay(n)
	WRITE(1,*) fi, funcion(n)

	END DO
	WRITE (1,*) ' '
	END DO
     CLOSE (1)

END PROGRAM exponencial
