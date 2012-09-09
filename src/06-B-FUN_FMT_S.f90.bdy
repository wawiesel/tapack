!set format specifier
VS = 'A'

!add field width
IF( PRESENT(y) )THEN
 VS = VS//TRIM(STR(y))

!default to trimmed length of string
ELSE
 VS = VS//TRIM(STR(LEN_TRIM(X)))
ENDIF