FUNCTION solx(angsol) RESULT (x)
	double precision, intent(IN) :: angsol
	double precision 	     :: x
        double precision, parameter :: rsolar = 1.496d8
	 x = rsolar * dcos(angsol)
END FUNCTION solx

FUNCTION soly(angsol) RESULT (y)
	double precision, intent(IN) :: angsol
	double precision 	     :: y
	double precision, parameter :: rsolar = 1.496d8
	 y = rsolar * dsin(angsol)
END FUNCTION soly

SUBROUTINE moonearth(rsolar, rlunar, posx, posy, anglun, angsol)
   double precision, intent (IN) :: rsolar, anglun, angsol
   double precision, intent (OUT) :: posx, posy
   double precision :: rlunar
   rlunar = rsolar / 4.0d0
   posx = (rsolar * dcos(angsol)) +(rlunar * dcos(anglun))
   posy = (rsolar * dsin(angsol)) +(rlunar * dsin(anglun))
 
END SUBROUTINE moonearth 


PROGRAM moon
	IMPLICIT NONE
	integer :: i
	double precision :: g, dia, rsolar, rlunar, posx, posy, anglun
	double precision :: rad, velocidadlun, velocidadsol, solx, soly, angsol
	double precision, parameter :: pi=3.1416d0, mes = 27.3217d0, year = 365.26d0
	double precision, dimension(360) :: totalx,totaly
	double precision, dimension(360) :: x, y
  rsolar = 1.496d8
  rad = pi / 180.0d0
  dia = 365.26d0/(360.0d0*rad) 
  velocidadlun = 2.0d0 * (pi / mes) !Calcula el trayecto diariamente la luna en radianes
  velocidadsol = 2.0d0 * (pi / year)  
  
OPEN (1, FILE = 'MoonEarth.dat', STATUS = 'unknown')
OPEN (2, FILE = 'EarthSun.dat', STATUS = 'unknown')
 DO i=1, 360, 1
 g = dble(i)
 angsol = g * velocidadsol
 anglun = g * velocidadlun  !Obtiene la posicion actual en radianes
 x(i) = solx(angsol)
 y(i) = soly(angsol) 
 CALL moonearth(rsolar, rlunar, posx, posy, anglun, angsol)  !Calcula la posicion de la luna respecto a la tierra y el sol
 totalx(i) = posx
 totaly(i) = posy
 WRITE (1,*) totalx(i), totaly(i)
 WRITE (1,*) ' '
 WRITE (2,*) x(i), y(i)
 WRITE (2,*) ' '
 
 END DO
 CLOSE (1)
 CLOSE (2)
END PROGRAM moon

