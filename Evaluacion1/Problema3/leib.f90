PROGRAM LEIBNIZ

!Declaracion de las variables
 IMPLICIT NONE
INTEGER :: i
REAL :: n, iteracion, pi

!Condiciones del programa
  pi = 1
  iteracion = 1
 WRITE(*,*) 'El valor de pi/4 segun las repeticiones:'

!Desarrollo del programa
     DO i=1, 50
     iteracion = iteracion * (-1)
     n = 2 * i + 1
     n = 1 / n
     n = n * (iteracion)
     pi = pi + n
	IF (i.EQ.10) THEN
	WRITE(*,*) ' '
	WRITE(*,*) '10:', pi
        END IF

	IF (i.EQ.20) THEN
	WRITE(*,*) ' '
	WRITE(*,*) '20:', pi
        END IF

	IF (i.EQ.30) THEN
	WRITE(*,*) ' '
	WRITE(*,*) '30:', pi
        END IF

	IF (i.EQ.40) THEN
	WRITE(*,*) ' '
	WRITE(*,*) '40:', pi
        END IF

	IF (i.EQ.50) THEN
	WRITE(*,*) ' '
	WRITE(*,*) '50:', pi
        END IF

END DO

! Finalizacion del programa.
END PROGRAM LEIBNIZ


