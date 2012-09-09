IF( TRIM(SIO%ACTION).sEQ.'Read' )THEN

 !increment line number
 SIO%LINE_NUM = SIO%LINE_NUM + 1
 
 !read data
 CALL READ_DATABLOCK (SIO,X,Fdbk)

ELSE 

 !increment line number
 SIO%LINE_NUM = SIO%LINE_NUM + 1
 
 !write data
 CALL WRITE_DATABLOCK(SIO,X,Fdbk)

ENDIF
