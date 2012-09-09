!USE simplified matrix multiply
FORALL( j = 1:3 )
 xyzP(j) = M32_transform(j,1)*P(1) + &
           M32_transform(j,2)*P(2)
END FORALL
