!get the unit
IF( PRESENT(file) )THEN
 OPEN(Unit,FILE=file,FORM="Unformatted",STATUS="Replace")
ELSE
 Unit=NewUnit()
 OPEN(Unit,FILE="temp."//STR(Unit),FORM="Unformatted",STATUS="Replace")
END IF

!read the info
READ(Unit)N

!allocate the array
ALLOCATE(x(N),y(N))

!read the array
READ(Unit)x,y

!close the unit
CLOSE(Unit)

