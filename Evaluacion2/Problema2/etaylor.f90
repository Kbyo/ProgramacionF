SUBROUTINE EXPTAYLOR (n, j, fi, fj, expt)
	integer, intent (IN)      :: n
	double precision, intent (IN) :: fi
	integer :: j
	double precision, dimension (100), intent(OUT) :: expt
	double precision :: fj, termino, sumaparcial
	
	
	
	termino = 1
	sumaparcial = termino
	DO j = 1, n
	 fj = dble(j)
	 termino = termino * fi / fj
	 sumaparcial = sumaparcial + termino
	 expt(j) = sumaparcial
	END DO

	 
END SUBROUTINE EXPTAYLOR
	 

PROGRAM EXPONENCIAL
	double precision, dimension (15) :: f
	integer :: i, j, n
	double precision, dimension (100)   :: x
	double precision, dimension (100) :: expt
	double precision, dimension (100) :: funcion
	double precision :: fi, fj, termino, sumaparcial

     OPEN (1, FILE = 'exp.dat', STATUS = 'unknown')
	
	DO n=1, 15, 2
	DO i=0, 100, 1
	  fi = dble(i)
	  fi = fi / 10.0d0
	CALL EXPTAYLOR(n, j, fi, fj, expt)
	funcion(n) = expt(n)
	WRITE(1,*) fi, funcion(n)

	END DO
	WRITE (1,*) ' '
	END DO
     CLOSE (1)

END PROGRAM EXPONENCIAL
