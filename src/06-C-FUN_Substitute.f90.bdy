
!initialize
B = A

!replace indices
DO s=1,SIZE(var)
 IF( .NOT.IsError(int(s)) )THEN
  IF( PRESENT(fmt) )THEN
   B = Replace(B,var(s),TRIM(STR(int(s),FMT(s))))
  ELSE
   B = Replace(B,var(s),TRIM(STR(int(s))))
  END IF
 END IF
END DO