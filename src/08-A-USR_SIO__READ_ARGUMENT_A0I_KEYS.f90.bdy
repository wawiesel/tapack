IF( Formatted(SIO) )THEN
 X  = INDEXa( KEYS , TRIM(STR(SIO%arg(SIO%currarg))) , CASESEN = .FALSE. )
ELSE
 READ(SIO%Unit,IOSTAT=SIO%IOSTAT)X
ENDIF