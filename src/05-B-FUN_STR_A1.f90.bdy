!handle options
AdjustLeft_  = DEFAULT( .TRUE.  , AdjustLeft )
AdjustRight_ = DEFAULT( .FALSE. , AdjustRight )

DO i=1,SIZE(X)
 
 !write X into S with optional format
 IF( PRESENT(FMT) )THEN
  WRITE(S(i),FMT,IOSTAT=jerr)X(i)
 ELSE
  WRITE(S(i),*,IOSTAT=jerr)X(i)
 ENDIF
 
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