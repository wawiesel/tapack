IF( Formatted(SIO) )THEN
 
 IF( PRESENT(Indent) )THEN
  CALL WRITE_CHARS(SIO,REPEAT(" ",Indent),FdBk,proper_string=.FALSE.)
 END IF
 
 !if the format is '*' then use the default formatting
 IF( TRIM(FMT_)=='*' )THEN
  WRITE(SIO%UNIT,FMT=*,IOSTAT=SIO%IOSTAT) X
  IF( SIO%IOSTAT/=0 )THEN
   VS = MODPROC(mod_,proc_)
   CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//&
     ' error writing default-formatted data-block from file/line = '//&
     TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//'.' )
   VS = ""
   RETURN
  END IF  

 !otherwise, use the formats we have stored right now in SIO%FMT_T
 !where T is the type-kind we are dealing with here
 ELSE
  VS = FMT(SIO,X,Pre='(',Post=')')
  WRITE(SIO%UNIT,FMT=STR(VS),IOSTAT=SIO%IOSTAT) X
  IF( SIO%IOSTAT/=0 )THEN
   VS = MODPROC(mod_,proc_)
   CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//&
     ' error writing user-formatted data-block from file/line = '//&
     TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//'.' )
   VS = ""
   RETURN
  END IF 
 END IF
 
ELSE
 WRITE(SIO%UNIT,IOSTAT=SIO%IOSTAT) X
 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//&
    ' error writing unformatted data-block from file/line = '//&
    TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//'.' )
  VS = ""
  RETURN
 END IF
END IF
