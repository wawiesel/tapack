!calculate first triangle from 1st,2nd,3rd vertices
sAREA = xySAREA_Tr(           Qg(:,1:3)        ) + &
!calculate second triangle from 3rd,4th,1st vertices
        xySAREA_Tr( RESHAPE((/ Qg(:,3:4) , Qg(:,1) /),(/2,3/)) )

