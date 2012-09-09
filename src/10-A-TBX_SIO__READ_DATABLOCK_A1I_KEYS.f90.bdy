IF( Formatted(SIO) )THEN
 
 !read in the elements
 DO j=1,SIZE(X)
  CALL READ_CHARS(SIO,VS,FdBk,proper_string=.TRUE. )
  X(j) = INDEXa( KEYS , TRIM(STR(VS)) , CASESEN=CaseSen )
  ![debug]
  !WRITE(*,*)"VS="//STR(VS)
 END DO
 CALL getNextLine(sio,fdbk)

ELSE
 DO j=1,SIZE(X)
  READ(SIO%Unit,IOSTAT=SIO%IOSTAT)X(j)
 END DO
END IF
