SUBROUTINE FUNCIONSENO (n, j, fi, fj, seno, signo, potencia, factorial)
	integer, intent (IN)      :: n
	double precision, intent (IN) :: fi
	integer :: j
	double precision, dimension (10000), intent(OUT) :: seno
	double precision :: fj, termino, sumaparcial, signo, potencia, factorial

	
	signo = 1.0d0
	termino = fi
	sumaparcial = termino
	potencia = fi
	factorial = 1
	DO j = 1, n
	 fj = dble(j)
	 potencia = fi**(j + 2)
	 factorial = factorial * (j + 1) * (j + 2)
	 signo = signo * (-1.0d0)
	 termino = potencia / factorial
	 termino = termino * signo
	 sumaparcial = sumaparcial + termino
	 seno(j) = sumaparcial
	 
	END DO

	 
END SUBROUTINE FUNCIONSENO
	 

PROGRAM APROXIMACIONESSENO
	double precision, dimension (10000) :: f
	integer :: i, j, n
	double precision, dimension (10000)   :: x
	double precision, dimension (10000) :: seno
	double precision, dimension (10000) :: funcion
	double precision :: fi, fj, termino, sumaparcial, signo, potencia, factorial
	

     OPEN (1, FILE = 'funciones.dat', STATUS = 'unknown')
	fi = -3.1d0
	DO i=1, 60
	WRITE (1,*) fi, fi
	fi = fi + 0.1d0
	
	END DO
	WRITE (1,*) ' '
	DO n=1, 15, 2
	  fi = -3.1d0
	DO i=1, 60
	fi = fi + 0.1d0
	CALL FUNCIONSENO (n, j, fi, fj, seno, signo, potencia, factorial)
	funcion(n) = seno(n)
	WRITE (1,*) fi, funcion(n)

	END DO
	WRITE (1,*) ' '
	END DO
     CLOSE (1)

END PROGRAM APROXIMACIONESSENO
