program xmax
  implicit none

  ! definimos constantes
  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415927

  ! definimos las variables
  real :: a, v0, x
  
  write(*,*) 'Dame el ángulo y la velocidad inicial'
  read(*,*) a, v0

  ! convirtiendo angulo a radianes
  a = a * pi / 180.0

  ! ecuacion de la trayectoria maxima
  x = (v0**2 * (sin (2 * a)))/g

  ! escribiendo el resultado en la pantalla
  write(*,*) 'la trayectoria maxima en x es = ', x, 'metros'

end program xmax
