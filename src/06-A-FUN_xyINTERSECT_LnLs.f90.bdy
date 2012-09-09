IF( PRESENT(tol) )THEN
 tol_ = tol
ELSE
 tol_ = 0._KIND_R
END IF

IF( PRESENT(IncludeEnds) )THEN
 IncludeEnds_ = IncludeEnds
ELSE
 IncludeEnds_ = DEFAULT_IncludeEnds
END IF

IF( PRESENT(key) )THEN
 key = 0
END IF

IF( PRESENT(SDIST_Ln) )THEN
 SDIST_Ln = HUGE(SDIST_Ln)
ENDIF

IF( PRESENT(FRAC_Ls) )THEN
 FRAC_Ls = HUGE(FRAC_Ls)
ENDIF

Ln_a = xyLINE_PP( Ls(:,1) , Ls(:,2) )

!call the line-line intersection kernel
INTERSECT = xyINTERSECT_LnLn( Ln_a , Ln , SDIST_a=FRAC , SDIST_b=SDIST )

!simple return on no intersection of two lines
IF( .NOT.INTERSECT )RETURN

!normalize <FRAC> to 1
FRAC = FRAC/xyDIST_PP( Ls(:,1) , Ls(:,2) )


!if we are at the left side
IF( IsApprox(abs(FRAC),tol_) )THEN
 INTERSECT = MERGE( .TRUE. , .FALSE. , IncludeEnds_(1) )
 IF( PRESENT(key) )THEN
  key = IBSET( key , 1 )
  IF( INTERSECT )THEN
   key = IBSET( key , 2 )
  END IF
 END IF

!if we are at the right side
ELSE IF( IsApprox(abs(FRAC-1._KIND_R),tol_) )THEN
 INTERSECT = MERGE( .TRUE. , .FALSE. , IncludeEnds_(2) )
 IF( PRESENT(key) )THEN
  key = IBSET( key , 3 )
  IF( INTERSECT )THEN
   key = IBSET( key , 4 )
  END IF
 END IF

!if we are outside of the interval
ELSE IF( FRAC<0._KIND_R .OR. FRAC>1._KIND_R )THEN
 INTERSECT = .FALSE.
 RETURN

!if we are in the middle
ELSE
 INTERSECT = .TRUE. 
END IF

IF( PRESENT(SDIST_Ln) )THEN
 SDIST_Ln = SDIST
ENDIF

IF( PRESENT(FRAC_Ls) )THEN
 FRAC_Ls = FRAC
ENDIF

IF( PRESENT(P_intersect) )THEN
 P_intersect = Ln(:,1) + SDIST*Ln(:,2)
ENDIF