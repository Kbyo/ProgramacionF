PROGRAM MEDIAS

!Declaracion de las variables
IMPLICIT NONE
INTEGER :: sumatory, x, tell
REAL :: SUMATORIAARITMETICA, SUMATORIAARMONICA, harmony, fx, fa, fb

!Condiciones del programa
PRINT*, "Este programa realiza las medias de una sumatoria, presione 0 para finalizar el proceso"
OPEN(UNIT=10, FILE="SumData.DAT", STATUS='unknown')

sumatory = 0
tell = 0
harmony = 0

!Desarrollo del programa
 DO
  PRINT*, "Add:"
  READ*, x
  IF (x == 0) THEN
   EXIT
 ELSE
sumatory = sumatory + x
tell = tell + 1
fx = float(x)
fx = 1/fx
harmony = harmony + fx

 END IF
 WRITE(10,*) x
 END DO
fb = float(sumatory)
fa = float(tell)
SUMATORIAARITMETICA = fb / fa
SUMATORIAARMONICA = fa / harmony


PRINT*, "Sumatoria =", sumatory
WRITE(10,*) "Sumatoria =", sumatory
WRITE(10,*)' '
PRINT*, "Media aritmetica =", SUMATORIAARITMETICA
WRITE(10,*) "Media aritmetica =", SUMATORIAARITMETICA
WRITE(10,*) ' '
PRINT*, "Media armonica =", SUMATORIAARMONICA
WRITE(10,*) "Media armonica =", SUMATORIAARMONICA
WRITE(10,*) ' '


close(10)

! Finalizacion del programa.
END PROGRAM MEDIAS




