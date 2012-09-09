!formatted input
IF( Formatted(SIO) )THEN
 DO i=1,SIZE(X,1)
  CALL READ_CHARS(SIO,VS,FdBk,proper_string=.TRUE.)
  X(i) = VS
  IF( SIO%IOSTAT/=0 )GOTO 666
 END DO

 666 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//&
    ' error reading formatted varying-string data-block from file/line = '//&
    TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//'.' )
  VS = ""
  RETURN
 END IF 
  
!unformatted input
ELSE
 DO i=1,SIZE(X,1)
  !initialize
  VS = ''
  DO 
   READ(SIO%UNIT,IOSTAT=SIO%IOSTAT)SIO%char
   IF( SIO%IOSTAT/=0 )GOTO 667
   IF( SIO%char==SIO%stop )THEN
    EXIT
   ELSE
    VS = VS//SIO%char
   ENDIF
  END DO
  X(i) = VS
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
