!formatted output
IF( Formatted(SIO) )THEN
 DO i=1,SIZE(X)-1
  CALL WRITE_CHARS(SIO,TRIM(KEYS(X(i))),FdBk,end=SIO%subargdelim,proper_string=.TRUE.)
  IF( SIO%iostat/=0 )GOTO 668
 END DO
 CALL WRITE_CHARS(SIO,TRIM(KEYS(X(i))),FdBk,proper_string=.TRUE.)
 
 !error condition
 668 IF( SIO%iostat/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE(fdbk_error,FdBk,s=STR(VS)//&
    'an error occurred while writing formatted &
    &integers-from-keys output.')
  VS = ""
  RETURN
 END IF
  
 !output argument delimeter
 IF( SIO%currarg<=SIZE(SIO%argnm) )THEN
  WRITE(SIO%UNIT,'(a1)',IOSTAT=SIO%IOSTAT)SIO%argdelim(1:SIO%LEN_argdelim)
  IF( SIO%IOSTAT/=0 )GOTO 668
 END IF

!unformatted output
ELSE
 DO i=1,SIZE(X)
  WRITE(SIO%Unit,IOSTAT=SIO%IOSTAT)X(i)
  IF( SIO%IOSTAT/=0 )GOTO 669
 END DO
 
 !error condition
 669 IF( SIO%iostat/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE(fdbk_error,FdBk,s=STR(VS)//&
    'an error occurred while writing unformatted &
    &integers-from-keys output.')
  VS = ""
  RETURN
 END IF

END IF
