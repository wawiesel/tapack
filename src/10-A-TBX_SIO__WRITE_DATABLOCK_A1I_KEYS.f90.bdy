IF( Formatted(SIO) )THEN
 
 IF( PRESENT(Indent) )THEN
  CALL WRITE_CHARS(SIO,REPEAT(" ",Indent),FdBk,proper_string=.FALSE. )
 END IF
 
 !read in the elements and verify character sequence which separates them
 DO j=1,SIZE(X)
  VS = TRIM(KEYS(X(j)))
  CALL WRITE_CHARS(SIO,STR(VS),FdBk,proper_string=.TRUE. )
  CALL WRITE_CHARS(SIO," ",FdBk,proper_string=.FALSE. )
 END DO
 CALL putEndOfLine(SIO,fdbk)

ELSE
 DO j=1,SIZE(X)
  WRITE(SIO%Unit,IOSTAT=SIO%IOSTAT)X(j)
 END DO
ENDIF
