FUNCTION fx(g) RESULT (x)
	double precision, intent(IN) :: g
	double precision 	     :: x
	 x = 1.496d8 * dcos(g)
END FUNCTION fx

FUNCTION fy(g) RESULT (y)
	double precision, intent(IN) :: g
	double precision 	     :: y
	 y = 1.496d8 * dsin(g)
END FUNCTION fy

PROGRAM movt

	IMPLICIT NONE
	integer :: i
	double precision :: g, fx, fy
	double precision, parameter :: r = 1.496d8, pi=3.1416d0 !KILOMETROS
	double precision, dimension(1000) :: x, y

 
OPEN (1, FILE = 'datos.dat', STATUS = 'unknown')
 DO i=1, 360, 1
 g = dble(i)
 g = g * pi / 180.0d0
 x(i) = fx(g)
 y(i) = fy(g)
 WRITE (1,*) x(i), y(i)
 WRITE (1,*) ' '
 END DO
 CLOSE (1)


END PROGRAM movt

