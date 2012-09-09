IF( Formatted(SIO) )THEN
 
 !if the format is '*' then use the default formatting
 IF( TRIM(FMT_)=='*' )THEN
  READ(SIO%UNIT,FMT=*,IOSTAT=SIO%IOSTAT)X
  IF( SIO%IOSTAT/=0 )THEN
   VS = MODPROC(mod_,proc_)
   CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//&
     ' error reading default-formatted data-block from file/line = '//&
     TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//'.' )
   VS = ""
   RETURN
  END IF  

 !otherwise, use the formats we have stored right now in SIO%FMT_T
 !where T is the type-kind we are dealing with here
 ELSE
  VS = FMT(SIO,X,Pre='(',Post=')')
  READ(SIO%UNIT,FMT=STR(VS),IOSTAT=SIO%IOSTAT) X
  IF( SIO%IOSTAT/=0 )THEN
   VS = MODPROC(mod_,proc_)
   CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//&
     ' error reading USEr-formatted data-block from file/line = '//&
     TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//'.' )
   VS = ""
   RETURN
  END IF 
 END IF
 
ELSE
 READ(SIO%UNIT,IOSTAT=SIO%IOSTAT) X
 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//&
    ' error reading unformatted data-block from file/line = '//&
    TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//'.' )
  VS = ""
  RETURN
 END IF
END IF
