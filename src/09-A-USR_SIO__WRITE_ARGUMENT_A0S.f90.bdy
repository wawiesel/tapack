IF( Formatted(SIO) )THEN
 CALL WRITE_CHARS(SIO,TRIM(X),FdBk,proper_string=.TRUE.)
 
 666 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE(fdbk_error,FdBk,s=STR(VS)//&
  'an error occurred while writing formatted output.')
  VS = ""
  RETURN
 END IF

 !output argument delimeter
 IF( SIO%currarg<SIZE(SIO%argnm) )THEN
  WRITE(SIO%UNIT,'(a)',ADVANCE='no')SIO%argdelim(1:SIO%LEN_argdelim)
  IF( SIO%IOSTAT/=0 )GOTO 666
 END IF

ELSE
 DO j=1,LEN_TRIM(X)
  WRITE(SIO%UNIT,IOSTAT=SIO%IOSTAT)X(j:j)
 END DO
 WRITE(SIO%UNIT,IOSTAT=SIO%IOSTAT)SIO%stop
ENDIF