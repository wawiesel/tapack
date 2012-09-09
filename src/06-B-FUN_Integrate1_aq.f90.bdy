!initialize
result   = c_0
i        = 1
S1       = c_0
S2       = c_0
TOLER(i) = c_1*tol
A(i)     = p(1)
H(i)     =(p(2)-p(1))*c_1_by_2
FA(i)    = f( p(1) )
FC(i)    = f( p(1) + H(i) )
FB(i)    = f( p(2) )
S(i)     =(FA(i) + c_4*FC(i) + FB(i))*H(i)*c_1_by_3
L(i)     = c_1
DO
 IF( i<=0 )EXIT
 FD   = f( A(i) + H(i)*c_1_by_2 )
 FE   = f( A(i) + H(i)*c_3_by_2 )
 S1   =(FA(i) + c_4*FD+FC(i) )*H(i)*c_1_by_6
 S2   =(FC(i) + c_4*FE+FB(i) )*H(i)*c_1_by_6
 v(1) = A(i)
 v(2) = FA(i)
 v(3) = FC(i)
 v(4) = FB(i)
 v(5) = H(i)
 v(6) = TOLER(i)
 v(7) = S(i)
 v(8) = L(i)
 i=i-1
 IF( abs(S1+S2-v(7))<v(6) )THEN
  result = result + (S1+S2)
 ELSE
  IF( v(8)>=N )THEN
   result = ERROR(result)
   EXIT
  ELSE
   i=i+1
   A(i)     = v(1)+v(5)
   FA(i)    = v(3)
   FC(i)    = FE 
   FB(i)    = v(4)
   H(i)     = v(5)*c_1_by_2
   TOLER(i) = v(6)*c_1_by_2
   S(i)     = S2
   L(i)     = v(8)+1
   i=i+1
   A(i)     = v(1)
   FA(i)    = v(2)
   FC(i)    = FD
   FB(i)    = v(3)
   H(i)     = H(i-1)
   TOLER(i) = TOLER(i-1)
   S(i)     = S1
   L(i)     = L(i-1)
  END IF
 END IF
END DO