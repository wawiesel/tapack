!calculate signed area
sAREA = ( Tr(1,2)-Tr(1,1) )*( Tr(2,3)-Tr(2,1) ) - &
        ( Tr(1,3)-Tr(1,1) )*( Tr(2,2)-Tr(2,1) )

!apply 1/2 normalization
sAREA = 0.5_KIND_R*sAREA