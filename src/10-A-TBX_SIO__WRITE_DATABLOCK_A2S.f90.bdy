!formatted input
IF( Formatted(SIO) )THEN
 
 DO k=1,SIZE(X,2)
  DO i=1,SIZE(X,1)
   CALL WRITE_CHARS(SIO,X(i,k),FdBk,proper_string=.TRUE.)
   IF( SIO%IOSTAT/=0 )GOTO 666
  END DO
 END DO

 666 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//&
    ' error writing formatted string data-block from file/line = '//&
    TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//'.' )
  VS = ""
  RETURN
 END IF 
  
!unformatted input
ELSE
 
 !start simple write loop
 DO k=1,SIZE(X,2)
  DO i=1,SIZE(X,1) 
   DO j=1,LEN(X(i,k))
    
    !set the character
    SIO%char = X(i,k)(j:j) 

    !write a character
    WRITE(SIO%UNIT,IOSTAT=SIO%IOSTAT)SIO%char

    !error condition
    IF( SIO%IOSTAT/=0 )GOTO 667
   
   END DO
   
   !write a stop
   WRITE(SIO%UNIT,IOSTAT=SIO%IOSTAT)SIO%stop

  END DO
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
