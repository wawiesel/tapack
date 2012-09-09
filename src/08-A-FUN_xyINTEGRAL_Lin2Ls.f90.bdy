
!get the integrals we need
ix  = xyINTEGRALX_Ls ( Ls )
iy  = xyINTEGRALY_Ls ( Ls )
i1  = xyINTEGRAL1_Ls ( Ls )

!combine with coefficients
INTEGRAL = Lin2(1)*ix  + Lin2(2)*iy  + Lin2(3)*i1
