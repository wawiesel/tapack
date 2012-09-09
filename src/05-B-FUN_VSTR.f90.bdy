!handle options
AdjustLeft_  = DEFAULT( .TRUE.  , AdjustLeft )
AdjustRight_ = DEFAULT( .FALSE. , AdjustRight )

!write X into S with optional format
IF( PRESENT(FMT) )THEN
 WRITE(S,FMT,IOSTAT=jerr)X
ELSE
 WRITE(S,*,IOSTAT=jerr)X
ENDIF

!set the varying string
VS = S

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
