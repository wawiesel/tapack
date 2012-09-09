!initialize
CALL CLEAR(X)

!formatted input
IF( Formatted(SIO) )THEN
 
 DO k=1,SIZE(X,2)
  DO i=1,SIZE(X,1)
   
   CALL READ_CHARS(SIO,VS,FdBk,proper_string=.TRUE.)
   
   IF( sio%end_of_line .AND. i/=SIZE(X,1) )THEN
    GOTO 666
   END IF

   X(i,k) = VS
  END DO

  CALL getNextLine(sio,fdbk)

 END DO

 666 CONTINUE
 
 IF( sio%iostat/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//&
    ' error reading formatted string data-block from file/line = '//&
    TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//'.' )
  VS = ""
  RETURN
 END IF 
  
!unformatted input
ELSE
 
 !start read loop
 DO k=1,SIZE(X,2)
  DO i=1,SIZE(X,1) 
   j = 0 
   DO 
    j = j + 1
    
    !check for exhaustion of string characters
    IF( j>LEN(X(i,k)) )THEN
     VS = MODPROC(mod_,proc_)
     CALL UPDATE( fdbk_warning , FdBk , s=STR(VS)//&
       ' unformatted string data-block read terminated from file/line = '//&
       TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//', becaUSE all the characters&
       & in the provided string for entry '//TRIM(STR(i))//' have been USEd up.' )
     VS = "" 
     EXIT
    END IF
   
    !read a character
    READ(SIO%UNIT,IOSTAT=SIO%IOSTAT)SIO%char

    !error condition
    IF( SIO%IOSTAT/=0 )GOTO 667
   
    !deal with read character
    IF( SIO%char==SIO%stop )THEN
     EXIT
    ELSE
     X(i,k)(j:j) = SIO%char
    ENDIF

   END DO
  END DO
 END DO

 667 CONTINUE
 
 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//&
    ' error reading unformatted string data-block from file/line = '//&
    TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//'.' )
  VS = ""
  RETURN
 END IF

END IF
