!get the unit
IF( PRESENT(file) )THEN
 OPEN(Unit,FILE=file,FORM="Unformatted",STATUS="Replace")
ELSE
 Unit=NewUnit()
 OPEN(Unit,FILE="temp."//STR(Unit),FORM="Unformatted",STATUS="Replace")
END IF

!write the info
WRITE(Unit)SIZE(x)
WRITE(Unit)x,y

!deallocate the array
DEALLOCATE(x,y)

!close the unit
CLOSE(Unit)
