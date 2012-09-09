MODULE PAR_DiscreteOrdinates
!!#### PURPOSE
!! Contains parameters for the discrete ordinates method, mostly
!! default values.

INTEGER,PARAMETER :: DOR_LEVEL=1,DOR_PRODUCT=2,DOR_GENERAL=3

INTEGER,PARAMETER :: DEFAULT_QuadratureType = DOR_PRODUCT
CHARACTER(10),PARAMETER :: DEFAULT_pQuadrature = "dGL       "
CHARACTER(10),PARAMETER :: DEFAULT_aQuadrature = "GL        "

CHARACTER(10),PARAMETER :: DEFAULT_Quadrature  = "LQ        "

INTEGER,PARAMETER :: DEFAULT_aOrder = 16
INTEGER,PARAMETER :: DEFAULT_pOrder = 16
LOGICAL,PARAMETER :: DEFAULT_UniformZGeometry = .FALSE.
INTEGER,PARAMETER :: DEFAULT_Order  = 128

ENDMODULE
