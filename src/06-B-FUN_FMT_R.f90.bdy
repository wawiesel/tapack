!add format specifier
IF( PRESENT(F) )THEN
 IF( (F .sEQ. 'En') .OR. &
     (F .sEQ. 'Es') .OR. &
     (F .sEQ. 'E' ) .OR. &
     (F .sEQ. 'F' ) .OR. &
     (F .sEQ. 'D' )  )THEN
  VS = F
 ELSE
  VS = 'G'
 ENDIF
!default to exponential notation
ELSE
 VS = 'E'
ENDIF


!add field width
IF( PRESENT(y) )THEN
 VS = VS//TRIM(STR(y))

!default to full field width
ELSE
 VS = VS//TRIM(STR(LEN_R))
ENDIF


!add significant digits
IF( PRESENT(z) )THEN
 VS = VS//'.'//TRIM(STR(z))

!default to precision
ELSE
 VS = VS//'.'//TRIM(STR(PRECISION(X)))
ENDIF