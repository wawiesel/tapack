!proceed to flag neighbors that are the same
k1 = 1 !master
k2 = 2 !compare to master
flag = .FALSE.
DO 
 IF( k2>SIZE(flag) )EXIT
 flag(k2) = IsApprox(sorted_SCAL(k2),sorted_SCAL(k1),tol,reltol)
 IF( .NOT.flag(k2) )THEN
  k1 = k2
 END IF
 k2 = k2 + 1
END DO

!set duplicates to Error() 
FORALL(k2=1:SIZE(flag),flag(k2))
 sorted_SCAL(k2) = Error(1._KIND_R)
END FORALL

IF( PRESENT(unique) )THEN
 unique = .NOT.flag
END IF
