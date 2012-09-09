!initialize
CALL CLEAR(X)
 
!formatted input
IF( Formatted(SIO) )THEN
 
 DO i=1,SIZE(X,1)
  CALL READ_CHARS(SIO,VS,FdBk,proper_string=.TRUE.)
  X(i) = VS
  IF( SIO%IOSTAT/=0 )GOTO 666
 END DO
 CALL getNextLine(sio)

 666 IF( SIO%IOSTAT/=0 )THEN
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
 DO i=1,SIZE(X) 
  j = 0 
  DO 
   j = j + 1
   
   !check for exhaustion of string characters
   IF( j>LEN(X(i)) )THEN
    VS = MODPROC(mod_,proc_)
    CALL UPDATE( fdbk_warning , FdBk , s=STR(VS)//&
      ' unformatted string data-block read terminated from file/line = '//&
      TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//', because all the characters&
      & in the provided string for entry '//TRIM(STR(i))//' have been used up.' )
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
    X(i)(j:j) = SIO%char
   END IF

  END DO
 END DO
 
 667 IF( SIO%IOSTAT/=0 )THEN
  VS = MODPROC(mod_,proc_)
  CALL UPDATE( fdbk_error , FdBk , s=STR(VS)//&
    ' error reading unformatted string data-block from file/line = '//&
    TRIM(STR(SIO%FILE))//'/'//TRIM(STR(SIO%LINE_NUM))//'.' )
  VS = ""
  RETURN
 END IF

END IF
