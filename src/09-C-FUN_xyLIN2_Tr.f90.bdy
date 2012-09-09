
A(:,1) = 1._KIND_R
DO n=1,3
 A(n,2:3) = Tr(1:2,n)
 b(n,1)   = fv(n)
END DO

CALL LA_GESV( A, b, INFO=info )

LIN2 = b(:,1)