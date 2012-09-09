
!! calculate each component <Y> based on formulas
IF( PRESENT(ro) )THEN
 RGBA(1) = ro   (1)**(ro   (2) + ro   (3)*ABS( X - ro   (4) )**ro   (5))
ELSE
 RGBA(1) = X
END IF

IF( PRESENT(gamma) )THEN
 RGBA(2) = gamma(1)**(gamma(2) + gamma(3)*ABS( X - gamma(4) )**gamma(5))
ELSE
 RGBA(2) = X
END IF

IF( PRESENT(beta) )THEN
 RGBA(3) = beta (1)**(beta (2) + beta (3)*ABS( X - beta (4) )**beta (5))
ELSE
 RGBA(3) = X
END IF

IF( PRESENT(alpha) )THEN
 RGBA(4) = alpha(1)**(alpha(2) + alpha(3)*ABS( X - alpha(4) )**alpha(5))
ELSE
 RGBA(4) = X
END IF

!! fix ups for stuff outside of range
WHERE(RGBA<0._KIND_R)RGBA=0._KIND_R
WHERE(RGBA>1._KIND_R)RGBA=1._KIND_R

