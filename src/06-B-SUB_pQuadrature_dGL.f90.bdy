!! Error checking.
errint_ = 0

IF( MOD(pOrder,2)/=0 )THEN
 errint_ = -1
 GOTO 666
END IF

!! Get the half-space number of ordinates.
H1 = pOrder/2
H2 = pOrder

!! Get abcissas and weights for polar cosine $cos(\theta)$, where 
!! $\theta \in [0,\Pi]$.
CALL Quadrature_GL( xp(1:H1) , wp(1:H1) , domain=(/ 0._KIND_R , 1._KIND_R /))

!reverse original
xp(1:H1) = Reverse( xp(1:H1) )
wp(1:H1) = Reverse( wp(1:H1) )

!get the other half-plane values
xp(H1+1:H2) = -Reverse( xp(1:H1) )
wp(H1+1:H2) =  Reverse( wp(1:H1) )

!! Jump to here on error.
666 CONTINUE

!! Set error integer.
IF( PRESENT(errint) )THEN
 errint = errint_
END IF

!! Output error message.
IF( PRESENT(errmsg) )THEN
  
 SELECT CASE( errint_ )
  CASE(  0 ) ; CALL CLEAR( errmsg )
  CASE( -1 ) ; errmsg = "ERROR: The order of the polar quadrature <pOrder> &
                         &must be divisible by 2 for symmetry!"
 END SELECT

END IF