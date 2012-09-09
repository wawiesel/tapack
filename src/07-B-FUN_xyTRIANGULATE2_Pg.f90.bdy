
DO i=2,N-1
 order_Tr(1:2,i) = (/i,i+1/)
 order_Tr(3  ,i) = 0
END DO

order_Tr(1:2,i) = (/N,1/)
order_Tr(3  ,i) = 0