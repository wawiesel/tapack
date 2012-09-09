

AREA = 0._KIND_R

DO i=1,N-1
 
 !get the parallelogram
 Qp(:,1) = (/ Sli(1,i  ) , Sli(2,i)   /)
 Qp(:,2) = (/ Sli(1,i+1) , Sli(2,i+1) /)
 Qp(:,3) = (/ Sli(1,i+1) , Sli(3,i+1) /)
 Qp(:,4) = (/ Sli(1,i  ) , Sli(3,i)   /)
 
 !should use another algorithm here instead of for a polygon [waw]
 AREA = AREA + ABS(  xySAREA_Pg( 4 , Qp )  )

END DO