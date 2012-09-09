!initialize
jerr = 0

!handle options
AdjustLeft_  = DEFAULT( .FALSE. , AdjustLeft )
AdjustRight_ = DEFAULT( .FALSE. , AdjustRight )

!first get string version
S = X

!pass through format
IF( PRESENT(FMT) )THEN
 WRITE(S,FMT,IOSTAT=jerr)S
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
