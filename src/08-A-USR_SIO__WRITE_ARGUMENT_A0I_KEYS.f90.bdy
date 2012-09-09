!formatted output
IF( Formatted(SIO) )THEN
 CALL WRITE_CHARS(SIO,TRIM(KEYS(X)),FdBk,proper_string=.TRUE.)
 
 !error condition
 668 IF( SIO%iostat/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE(fdbk_error,FdBk,s=STR(VS)//&
    'an error occurred while writing formatted integers-from-keys output.')
  VS = ""
  RETURN
 ENDIF
  
 !output argument delimeter
 IF( SIO%currarg<SIZE(SIO%argnm) )THEN
  WRITE(SIO%UNIT,'(a)',IOSTAT=SIO%IOSTAT,ADVANCE="no")SIO%argdelim(1:SIO%LEN_argdelim)
  IF( SIO%IOSTAT/=0 )GOTO 668
 ENDIF

!unformatted output
ELSE
 WRITE(SIO%Unit,IOSTAT=SIO%IOSTAT)X
 
 !error condition
 669 IF( SIO%iostat/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE(fdbk_error,FdBk,s=STR(VS)//&
    'an error occurred while writing unformatted integers-from-keys output.')
  VS = ""
  RETURN
 ENDIF

ENDIF
