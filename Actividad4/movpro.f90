program movimientoproyectil
  implicit none
  !Definimos las variables reales
   real :: vt, c, area, vter, fi, fj, t, a
  !Definimos las variables reales con parametros
   real, parameter :: deltat = .01, m = 0.250, r = 0.05, cd = 0.47
   real, parameter :: u = 2 , g = 9.81, pi = 3.1415927, Ro = 1.225
  !Definimos las variables 
   real, dimension(100000) :: x, y, vx, vy
  !Definimos las variables enteras
   integer :: i, j
   integer, parameter :: maxvel = 100, ntimes = 100000
   
!Calculamos el Area del circulo.
   area = pi * r * r
!Calculamos la velocidad terminal.
  vt = ((2 * m * g) / (Ro * area * cd))
  vter = SQRT(vt)
!Calculamos el coeficiente de fricción.
 c = (m * g) / (vt)
!Calculamos el angulo en radianes
a = 45 * pi / 180.0

   open(1, file='Graficas.dat', status='unknown')


 do j=10, maxvel, 10
    fj = float(j)
    
     do i=1, ntimes
       fi = float(i)
       t = fi * deltat
       
     if (i.LT.3) then
     
     vx(i) = fj * cos(a)
     vy(i) = fj * sin(a) - (g * t)

     x(i) = fj * t * cos(a)
     y(i) = fj * t * sin(a) - 0.5 * g * t * t

     end if

     if (i.GT.2) then
     vx(i) = vx(i-1) * (1 - deltat * c / m)
     x(i) = x(i-1) + deltat * vx(i-2) * (1 - deltat * c / m)

     vy(i) = vy(i-1) * (1-deltat * c / m) - deltat * g
     y(i) = y(i-1) + deltat * vy(i-2) - deltat * deltat * g + deltat * c / m * vy(i-2)
     

     if (y(i).LT.0) exit
     
     end if
     
       write(1,*) x(i), y(i)
       
    
     end do
       write(1,*) ' '
 end do
     
    close(1)
    
 

end program movimientoproyectil
