IF( TRIM(SIO%ACTION).sEQ.'Read' )THEN
 
 ! Increment line number.
 SIO%LINE_NUM = SIO%LINE_NUM + 1
 
 ! Read data.
 CALL READ_DATABLOCK (SIO,X,Fdbk)

ELSE 

 ! Increment line number.
 SIO%LINE_NUM = SIO%LINE_NUM + 1
 
 ! Write data.
 CALL WRITE_DATABLOCK(SIO,X,Fdbk,Indent=Indent)

ENDIF

