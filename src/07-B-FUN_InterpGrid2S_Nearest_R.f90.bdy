FORALL(ix=1:SIZE(xout))
 FORALL(iy=1:SIZE(yout))
  Fout(ix,iy) = F( NEARLOC( r , (/xout(ix),yout(iy)/) ) )
 END FORALL
END FORALL

!! Impossible to have error with this algorithm so just
!! always return clear and 0.
IF( PRESENT(errmsg) )THEN
 CALL CLEAR(errmsg)
END IF
IF( PRESENT(errint) )THEN
 errint = 0
END IF