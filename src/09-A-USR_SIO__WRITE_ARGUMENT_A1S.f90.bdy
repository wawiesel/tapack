IF( Formatted(SIO) )THEN
 
 !now write the data
 DO i=1,SIO%nsubarg(SIO%currarg)-1
  CALL WRITE_CHARS(SIO,X(i),FdBk,end=SIO%subargdelim,proper_string=.TRUE.)
  IF( SIO%IOSTAT/=0 )GOTO 666
 END DO
 CALL WRITE_CHARS(SIO,X(i),FdBk,proper_string=.TRUE.)
 
 !error condition
 666 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE(fdbk_error,FdBk,s=STR(VS)//&
    'an error occurred while writing formatted string output.')
  VS = ""
  RETURN
 ENDIF
  
 !output argument delimeter
 IF( SIO%currarg<=SIZE(SIO%argnm) )THEN
  WRITE(SIO%UNIT,'(a1)',IOSTAT=SIO%IOSTAT)SIO%argdelim(1:SIO%LEN_argdelim)
  IF( SIO%IOSTAT/=0 )GOTO 666
 ENDIF

ELSE

 !unformatted output using this simple loop
 DO i=1,SIO%nsubarg(SIO%currarg)
  DO j=1,LEN(X(i))
   WRITE(SIO%UNIT,IOSTAT=SIO%IOSTAT)X(i)(j:j)
   IF( SIO%IOSTAT/=0 )GOTO 667
  END DO
  WRITE(SIO%UNIT,IOSTAT=SIO%IOSTAT)SIO%stop
 END DO
  
 !error condition
 667 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE(fdbk_error,FdBk,s=STR(VS)//&
    'an error occurred while writing unformatted string output.')
  VS = ""
 END IF
END IF
