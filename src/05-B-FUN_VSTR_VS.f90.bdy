!handle options
AdjustLeft_  = DEFAULT( .TRUE.  , AdjustLeft )
AdjustRight_ = DEFAULT( .FALSE. , AdjustRight )

!first get string version
S = X

!pass S through format
IF( PRESENT(FMT) )THEN
 WRITE(S,FMT,IOSTAT=jerr)S
END IF

!set varying string
vS = S

!check for an error
IF( jerr/=0 )THEN
 CALL CLEAR(VS)
 RETURN
END IF

!adjust
IF( AdjustLeft_ )THEN
 VS = ADJUSTL(VS) 
ELSE IF( AdjustRight_ )THEN
 VS = ADJUSTR(VS)
END IF
