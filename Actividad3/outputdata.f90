

program outputdata
  implicit none
  !Definimos las variables reales
   real :: a, fi, fj, t
  !Definimos las variables reales con parametros
   real, parameter :: deltat = .01
   real, parameter :: u = 10.0, g = 10, pi = 3.1415927
  !Definimos las variables 
   real, dimension(1000) :: x, y
  !Definimos las variables enteras
   integer :: i, j
   integer, parameter :: maxangle = 90, ntimes = 1000
   
    open(1, file='Graficas.dat', status='unknown')
 
 do j=15, maxangle, 15
    fj = float(j)
    
     do i=1, ntimes
       fi = float(i)
       t = fi * deltat
       
       a = fj * pi / 180.0
       
     x(i) = u * t * cos(a)
     y(i) = u * t * sin(a) - 0.5 * g * t * t
     if (y(i).LT.0) exit

       
       write(1,*) x(i), y(i)
       
    
     end do
       write(1,*) ' '
 end do
     
    close(1)
    
 

end program outputdata
