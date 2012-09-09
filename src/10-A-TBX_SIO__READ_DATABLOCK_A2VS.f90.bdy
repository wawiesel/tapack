!formatted input
IF( Formatted(SIO) )THEN

 DO j=1,SIZE(X,2)

  DO i=1,SIZE(X,1)

   CALL READ_CHARS(SIO,VS,proper_string=.TRUE.)

   IF( sio%end_of_line .AND. i/=SIZE(X,1) )THEN
    GOTO 666
   END IF

   X(i,j) = VS

  END DO

  CALL getNextLine(sio,fdbk)

 END DO

 666 CONTINUE
 
 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//&
    ' error reading formatted varying-string data-block from file/line = '//&
    TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//'.' )
  VS = ""
  RETURN
 END IF 
  
!unformatted input
ELSE
 DO j=1,SIZE(X,2)
  DO i=1,SIZE(X,1)
   !initialize
   VS = ''
   DO 
    READ(SIO%UNIT,IOSTAT=SIO%IOSTAT)char
    IF( SIO%IOSTAT/=0 )GOTO 667
    IF( char==SIO%stop )THEN
     EXIT
    ELSE
     VS = VS//char
    END IF
   END DO
   X(i,j) = VS
  END DO
 END DO

 667 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//&
    ' error reading unformatted varying-string data-block from file/line = '//&
    TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//'.' )
  VS = ""
  RETURN
 END IF

END IF
