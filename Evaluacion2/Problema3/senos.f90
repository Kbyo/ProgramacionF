SUBROUTINE seno (n, j, fi, fj, sen, signo, potencia, factorial)
	integer, intent (IN)      :: n
	double precision, intent (IN) :: fi
	integer :: j
	double precision, dimension (10000), intent(OUT) :: sen
	double precision :: fj, term, partial_sum, signo, potencia, factorial

	
	signo = 1.0d0
	term = fi
	partial_sum = term
	potencia = fi
	factorial = 1
	DO j = 1, n
	 fj = dble(j)
	 potencia = fi**(j + 2)
	 factorial = factorial * (j + 1) * (j + 2)
	 signo = signo * (-1.0d0)
	 term = potencia / factorial
	 term = term * signo
	 partial_sum = partial_sum + term
	 sen(j) = partial_sum
	 
	END DO

	 
END SUBROUTINE seno
	 

PROGRAM senos
	double precision, dimension (10000) :: f
	integer :: i, j, n
	double precision, dimension (10000)   :: x
	double precision, dimension (10000) :: sen
	double precision, dimension (10000) :: funcion
	double precision :: fi, fj, term, partial_sum, signo, potencia, factorial
	

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
	CALL seno (n, j, fi, fj, sen, signo, potencia, factorial)
	funcion(n) = sen(n)
	WRITE (1,*) fi, funcion(n)

	END DO
	WRITE (1,*) ' '
	END DO
     CLOSE (1)

END PROGRAM senos
