IF( Formatted(SIO) )THEN
 DO j=1,SIZE(X,2)
  DO i=1,SIZE(X,1)
   VS = X(i,j)
   CALL WRITE_CHARS(SIO,STR(VS),FdBk,proper_string=.TRUE.)
   IF( SIO%IOSTAT/=0 )GOTO 666
  END DO
 END DO
 666 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//&
    ' error writing formatted varying string data-block from file/line = '//&
    TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//'.' )
  VS = ""
  RETURN
 END IF 
  
ELSE
 DO k=1,SIZE(X,2)
  DO i=1,SIZE(X,1)
   VS = X(i,k)
   DO j=1,LEN(VS)
    SIO%char = EXTRACT(VS,j,j)
    WRITE(SIO%UNIT,IOSTAT=SIO%IOSTAT)SIO%char
    IF( SIO%IOSTAT/=0 )GOTO 667
   END DO
   WRITE(SIO%UNIT,IOSTAT=SIO%IOSTAT)SIO%stop
   IF( SIO%IOSTAT/=0 )GOTO 667
  END DO
 END DO

 667 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//&
    ' error writing unformatted varying string data-block from file/line = '//&
    TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//'.' )
  VS = ""
  RETURN
 END IF
END IF
