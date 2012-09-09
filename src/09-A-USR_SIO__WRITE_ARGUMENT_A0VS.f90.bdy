IF( Formatted(SIO) )THEN
 !USE WRITE_CHARS for formatted output
 CALL WRITE_CHARS(SIO,STR(X),proper_string=.TRUE.)

ELSE
 !USE this simple loop for unformatted output
 DO j=1,LEN(X)
  VS = EXTRACT(X,j,j)
  WRITE(SIO%UNIT,IOSTAT=SIO%IOSTAT)STR(VS)
 END DO
 WRITE(SIO%UNIT,IOSTAT=SIO%IOSTAT)SIO%stop
END IF

IF( SIO%IOSTAT/=0 )THEN
 VS = MODPROC(mod_,proc_)
 CALL UPDATE(fdbk_error,FdBk,s=STR(VS)//&
   'an error occurred while writing a varying string &
   & argument.')
END IF