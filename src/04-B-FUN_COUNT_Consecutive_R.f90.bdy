COUNT = 1
DO I=2,SIZE(array)
 IF( IsApprox(array(1),array(I),tol,reltol) )THEN
  COUNT = COUNT + 1
 ELSE
  EXIT
 END IF
END DO