!collapse to R^2 using inverse of M32_transform (just transpose)
FORALL( j = 1:2 )
 xyP(j) = M32_transform(1,j)*P(1) + &
          M32_transform(2,j)*P(2) + &
          M32_transform(3,j)*P(3)
END FORALL