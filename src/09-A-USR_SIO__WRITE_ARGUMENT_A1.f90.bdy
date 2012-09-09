IF( Formatted(SIO) )THEN
 
 !first set the format specifier
 IF( TRIM(FMT_)=='*' )THEN
  !set the default format
  FMT_ = FMT(X(1),Pre='(',Post=')')
 ELSE
  FMT_ = FMT(SIO,X(1),Pre='(',Post=')')
 ENDIF
 
 !now write the data
 DO i=1,SIO%nsubarg(SIO%currarg)-1
  CALL WRITE_CHARS(SIO,TRIM(STR(X(i),TRIM(FMT_))),FdBk,end=SIO%subargdelim,proper_string=.FALSE.)
  IF( SIO%IOSTAT/=0 )GOTO 666
 END DO
 CALL WRITE_CHARS(SIO,TRIM(STR(X(i),TRIM(FMT_))),FdBk,proper_string=.FALSE.)
 
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
 !write the unformatted output
 WRITE(SIO%UNIT,IOSTAT=SIO%IOSTAT) X(1:SIO%nsubarg(SIO%currarg))
 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE(fdbk_error,FdBk,s=STR(VS)//&
  'an error occurred while writing unformatted output.')
  VS = ""
 END IF
END IF
