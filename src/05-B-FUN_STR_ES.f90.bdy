!handle options
AdjustLeft_  = DEFAULT( .FALSE. , AdjustLeft )
AdjustRight_ = DEFAULT( .FALSE. , AdjustRight )


!pass X through S with optional format
IF( PRESENT(FMT) )THEN
 WRITE(S,FMT,IOSTAT=jerr)X
ELSE
 S = X
 jerr=0
END IF

!check for an error
IF( jerr/=0 )THEN
 CALL CLEAR(S)
 RETURN
END IF

!adjust
IF( AdjustLeft_ )THEN
 S = ADJUSTL(S) 
ELSE IF( AdjustRight_ )THEN
 S = ADJUSTR(S)
END IF
