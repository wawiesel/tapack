IF( Formatted(SIO) )THEN
 
 IF( SIO%PRESENT(SIO%currarg) )THEN
 
  X=SIO%arg(SIO%currarg)
 
 ELSE
 
  IF( PRESENT(Default) )THEN
   X=Default
  ELSE
   VS = MODPROC(mod_,proc_)
   CALL UPDATE( fdbk_warning , FdBk , s=STR(VS)//&
     "an optional argument with no default value was specified." )
   VS = ""
  END IF
 
 END IF

ELSE
 
 CALL GET( SIO%Unit , X , SET=SIO%stop , IOSTAT=SIO%IOSTAT)

END IF
