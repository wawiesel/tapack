DO i = 1 , N-1
 integral = integral + fbar_(i) * delx_(i)
END DO

IF( PRESENT(delxout) )delxout = delx_
IF( PRESENT(fbarout) )fbarout = fbar_

