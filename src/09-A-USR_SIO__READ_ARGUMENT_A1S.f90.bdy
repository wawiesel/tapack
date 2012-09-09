IF( Formatted(SIO) )THEN
 !For a list of strings we can use this special subroutine to
 !extract the subarguments---it separates a string into
 !pieces (words) based on the delimeter.
 CALL Words(X,STR(SIO%arg(SIO%currarg)),delim=SIO%subargdelim)
 
ELSE

 READ(UNIT=SIO%unit,IOSTAT=SIO%IOSTAT) X
 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE(fdbk_error,FdBk,s=STR(VS)//&
  'an error occurred while reading an unformatted string.')
  VS = ""
 END IF
END IF

 