!initialize
jerr = 0

!handle options
AdjustLeft_  = DEFAULT( .FALSE. , AdjustLeft )
AdjustRight_ = DEFAULT( .FALSE. , AdjustRight )

DO i=1,SIZE(X)
 !first get string version
 S(i) = X(i)

 !pass through format
 IF( PRESENT(FMT) )THEN
  WRITE(S(i),FMT,IOSTAT=jerr)S(i)
 END IF

 !check for an error
 IF( jerr/=0 )THEN
  CALL CLEAR(S(i))
  RETURN
 END IF

 !adjust
 IF( AdjustLeft_ )THEN
  S(i) = ADJUSTL(S(i)) 
 ELSE IF( AdjustRight_ )THEN
  S(i) = ADJUSTR(S(i))
 END IF

END DO