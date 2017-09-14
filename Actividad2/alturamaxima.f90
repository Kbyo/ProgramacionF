program alturamaxima
  implicit none

  ! definimos constantes
  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415927

  ! definimos variables
  real :: a, v0, h

  write(*,*) 'Dame el ángulo, y la velocidad inicial'
  read(*,*) a, v0

  ! convirtiendo angulo a radianes
  a = a * pi / 180.0

  ! ecuacion de la velocidad final para la altura maxima
  h= (v0**2 * (sin (a))**2/(2 * g))
  
  ! escribiendo el resultado en la pantalla
  write(*,*) 'la altura maxima es = ', h, ' metros'

end program alturamaxima
