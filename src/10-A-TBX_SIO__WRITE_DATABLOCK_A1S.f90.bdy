!formatted output
IF( Formatted(SIO) )THEN
 
 IF( PRESENT(Indent) )THEN
  CALL WRITE_CHARS(SIO,REPEAT(" ",Indent),Fdbk,proper_string=.FALSE.)
 END IF

 DO i=1,SIZE(X,1)
  CALL WRITE_CHARS(SIO,X(i),FdBk,proper_string=.TRUE.)
  CALL WRITE_CHARS(SIO," ",FdBk,proper_string=.TRUE.)
  IF( SIO%IOSTAT/=0 )GOTO 666
 END DO
 CALL putEndOfLine(SIO,fdbk)

 666 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//&
    ' error writing formatted string data-block from file/line = '//&
    TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//'.' )
  VS = ""
  RETURN
 END IF 
  
!unformatted output
ELSE
 
 !start write loop
 DO i=1,SIZE(X,1) 
  DO j=1,LEN(X(i))
   
   !set the character
   SIO%char = X(i)(j:j) 

   !write a character
   WRITE(SIO%UNIT,IOSTAT=SIO%IOSTAT)SIO%char

   !error condition
   IF( SIO%IOSTAT/=0 )GOTO 667
  
  END DO
  
  !write a stop
  WRITE(SIO%UNIT,IOSTAT=SIO%IOSTAT)SIO%stop

 END DO

 667 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//&
    ' error writing unformatted string data-block from file/line = '//&
    TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//'.' )
  VS = ""
  RETURN
 END IF

END IF
