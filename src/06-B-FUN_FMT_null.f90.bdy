!add format specifier
IF( PRESENT(F) )THEN
 VS = F

!default to general purpose 'G' specifier
ELSE
 VS = 'G'
ENDIF

!add field width
IF( PRESENT(y) )THEN
 VS = VS//TRIM(STR(y))
 
 !add additional specifier
 IF( PRESENT(z) )THEN
  VS = VS//'.'//TRIM(STR(z))
 ENDIF
 !default to no additional specifier

ENDIF
!default to no field width