program ESFERA

! Declaracion de tipo de las variables.
  IMPLICIT NONE   

  integer :: ierr
  character(1) :: yn
  real :: RADIO, AREA, VOLUMEN
  real, parameter :: pi = 3.141592653589793

  interactive_loop: DO

! Impresion en pantalla.
    WRITE (*,*) 'Enter radius of the sphere..'
    READ (*,*,IOSTAT=ierr) RADIO

! Ingreso de datos.
    IF (ierr /= 0) THEN
      WRITE(*,*) 'Error, invalid input.'
      CYCLE interactive_loop
    END IF

! Calculos.
    AREA = 4*pi * (RADIO**2)
    VOLUMEN= (4/3)*pi*(RADIO**3)

! Impresion del resultado.
    WRITE (*,'(1x,a7,f6.2,5x,a7,f6.2,5x,a5,f6.2)') &
      'RADIO=',RADIO,'AREA=',AREA,'VOLUMEN=',VOLUMEN

    yn = ' '
    yn_loop: DO
      WRITE(*,*) 'Perform another calculation? y[n]'
      READ(*,'(a1)') yn
      IF (yn=='y' .OR. yn=='Y') EXIT yn_loop
      IF (yn=='n' .OR. yn=='N' .OR. yn==' ') EXIT interactive_loop
    END DO yn_loop

  END DO interactive_loop

! Finalizacion del programa.
END PROGRAM ESFERA
