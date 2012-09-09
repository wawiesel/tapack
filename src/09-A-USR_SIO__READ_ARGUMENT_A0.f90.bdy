IF( Formatted(SIO) )THEN

 !set varying string
 VS = SIO%arg(SIO%currarg)
 
 !optional argument
 IF( SIO%optional(SIO%currarg) )THEN
  
  !not present
  IF( .NOT.SIO%present(SIO%currarg) )THEN
   IF( PRESENT(Default) )THEN
    X = Default
   ELSE
    VS = MODPROC(mod_,proc_)
    CALL UPDATE( fdbk_warning , FdBk , s=STR(VS)//&
      "the optional argument and no default was specified." )
    VS = ""
   END IF
  
  !present
  ELSE
   X = STR(VS)
  END IF
 
 !required argument
 ELSE
  
  !not present
  IF( .NOT.SIO%present(SIO%currarg) )THEN
   VS = MODPROC(mod_,proc_)
   CALL UPDATE(fdbk_error , FdBk , s=STR(VS)//"a required argument &
     &was not present." )
   VS = ""

  !default indicated
  ELSE IF( VS==SIO%argdefault )THEN
  
   IF( PRESENT(Default) )THEN
    X = Default
   ELSE
    VS = MODPROC(mod_,proc_)
    CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//"the default was specified &
     &in the input but not in the source." )
    VS = ""
   END IF

  !normal
  ELSE
   X = STR(VS)
  END IF
 
 END IF
  
ELSE
 READ(SIO%Unit,IOSTAT=SIO%IOSTAT) X
END IF
