IF( PRESENT(F) )THEN
 IF( F.sEQ.'L' )THEN
  VS='L'
 ELSE
  VS='G'
 ENDIF

ELSE
 VS = 'L'
ENDIF

!add field width
IF( PRESENT(y) )THEN
 VS = VS//TRIM(STR(y))

!default to 1 field width
ELSE
 VS = VS//'1'
ENDIF

IF( PRESENT(F) )THEN
 IF( F.sEQ.'G' )THEN
  !force width to 1
  VS = VS//'.1'
 ENDIF
ENDIF