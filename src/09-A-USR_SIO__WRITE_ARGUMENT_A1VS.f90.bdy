IF( Formatted(SIO) )THEN
 !formatted output using write_chars
 DO i=1,SIO%nsubarg(SIO%currarg)-1
  VS = X(i)
  CALL WRITE_CHARS(SIO,STR(VS),FdBk,end=SIO%subargdelim,proper_string=.TRUE.)
  IF( SIO%IOSTAT/=0 )GOTO 666
 END DO
 VS = X(i)
 CALL WRITE_CHARS(SIO,STR(VS),FdBk,proper_string=.TRUE.)
 VS = ""

 !error condition
 666 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE(fdbk_error,FdBk,s=STR(VS)//&
  'an error occurred while writing formatted varying string rank-1 array&
  & arguments.  The error occurred while processing argument number&
  & '//TRIM(STR(i))//', named '//TRIM(SIO%argnm(i))//'.')
  VS = ""
 END IF
 
 !output argument delimeter
 IF( SIO%currarg<=SIZE(SIO%argnm) )THEN
  WRITE(SIO%UNIT,'(a1)',IOSTAT=SIO%IOSTAT)SIO%argdelim(1:SIO%LEN_argdelim)
  IF( SIO%IOSTAT/=0 )GOTO 666
 END IF

ELSE
 !unformatted output using this simple loop
 DO i=1,SIO%nsubarg(SIO%currarg)
  DO j=1,LEN(X(i))
   VS = X(i)
   VS = EXTRACT(VS,j,j)
   WRITE(SIO%UNIT,IOSTAT=SIO%IOSTAT)STR(VS)
   VS=""
   IF( SIO%IOSTAT/=0 )GOTO 667
  END DO
  WRITE(SIO%UNIT,IOSTAT=SIO%IOSTAT)SIO%stop
 END DO
 
 !error condition
 667 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//&
    'an error occurred while writing unformatted varying-string output.')
  VS = ""
 END IF

END IF
