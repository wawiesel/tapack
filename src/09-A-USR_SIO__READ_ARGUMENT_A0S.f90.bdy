IF( Formatted(SIO) )THEN
 
 IF( SIO%PRESENT(SIO%currarg) )THEN
 
  X=STR(SIO%arg(SIO%currarg))
 
 ELSE 
 
  IF( PRESENT(Default) )THEN
   X=Default
  ELSE
   VS = MODPROC(mod_,proc_)
   CALL UPDATE( fdbk_warning , FdBk , s=STR(VS)//&
     "an optional argument with no default value was specified." )
   VS = ""
   CALL CLEAR(X)
  END IF
 
 END IF

ELSE
 CALL GET( SIO%Unit , VS , SET=SIO%stop , IOSTAT=SIO%IOSTAT)
 X(1:MIN(LEN(X),LEN(VS))) = EXTRACT(VS,1,MIN(LEN(X),LEN(VS)))
END IF

