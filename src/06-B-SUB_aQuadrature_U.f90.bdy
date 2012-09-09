!! Error checking.
errint_ = 0

IF( MOD(aOrder,4)/=0 )THEN
 errint_ = -1
 GOTO 666
END IF


!! Calculate the weights and abcissas for quadrant I.
Q1 = aOrder/4
Q2 = aOrder/2
Q4 = aOrder

!! Get abcissas and weights for polar angle $\gamma$, where, $\gamma \in [0,\Pi/2]$.
DO q=1,Q1
 gammas(q)  = (2.d0*q-1.d0)*C_PI_by_2/(2.d0*Q1)
 weights(q) = C_PI_by_2/Q1
END DO

!! Get the weights in quadrant I
wa(  1:Q1) = weights

!! Get the abcissa directions in quadrant I
xa(1,1:Q1) = cos(gammas)
xa(2,1:Q1) = sin(gammas)

!! Reflect around unit circle.
CALL aQuadratureReflect( aOrder , xa , wa , errint=errint , errmsg=errmsg )


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
  CASE( -1 ) ; errmsg = "ERROR: The order of the azimuthal quadrature <aOrder> &
                        &must be divisible by 4 for symmetry!"
 END SELECT

END IF