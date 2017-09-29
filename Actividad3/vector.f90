program vector

  implicit none
  real, dimension(100) :: x, y
  real :: a, u, n
  integer :: i
  write(*,*) 'Dame el ángulo y la velocidad inicial'
  read(*,*) a, u

  ! setup x and y with some data

  do i=1,100
     n=i * 0.1
     x(i) = n * cos(a)
     y(i) = sin(x(i)) * (1-cos(x(i)/3.0))
  end do

  ! output data to a file

  open(1, file='Grafica1.dat', status='unknown')
  do i=1,100
     write(1,*) x(i), y(i)
  end do
  close(1)

end program vector
