IF( PRESENT(F) )THEN
 IF( (F.sEQ.'B') .OR. &
     (F.sEQ.'I') .OR. &
     (F.sEQ.'O') .OR. &
     (F.sEQ.'Z') )THEN
 ELSE
  VS = 'G' 
 ENDIF

!default to 'I' format
ELSE
 VS = 'I'
ENDIF

!add field width
IF( PRESENT(y) )THEN
 VS = VS//TRIM(STR(y))

!default to full field width for hexadecimal
ELSE
 VS = VS//TRIM(STR(LEN_I))
ENDIF


!add forced width
IF( PRESENT(z) )THEN
 VS = VS//'.'//TRIM(STR(z))
ENDIF
!default to no forced width