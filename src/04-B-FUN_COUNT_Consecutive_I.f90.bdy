COUNT = 1
DO I=2,SIZE(array)
 IF( array(1)==array(I) )THEN
  COUNT = COUNT + 1
 ELSE
  EXIT
 END IF
END DO