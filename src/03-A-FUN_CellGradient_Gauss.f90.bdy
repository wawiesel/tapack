CellGradient = 0._KIND_R
DO j=1,Na
 DO d=1,Nd

  CellGradient(d) = CellGradient(d) + A(d,j)*Fa(j)

 END DO
END DO

CellGradient = CellGradient/Vc
