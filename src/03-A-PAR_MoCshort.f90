MODULE PAR_MoCshort
!!#### PURPOSE
!! Contains parameters for the MoCShort package.

INTEGER     ,PARAMETER,PRIVATE                  :: SIZE_KEY_BC = 7
CHARACTER(*),PARAMETER,DIMENSION(1:SIZE_KEY_BC) :: KEY_BC = (/"vacuum    ",&
                                                              "fixed     ",&
                                                              "reflective",&
                                                              "infinite  ",&
                                                              "unittest  ",&
                                                              "planewave ",&
                                                              "function  "/)
INTEGER,PARAMETER :: vacuum_    =1
INTEGER,PARAMETER :: fixed_     =2
INTEGER,PARAMETER :: reflective_=3
INTEGER,PARAMETER :: infinite_  =4
INTEGER,PARAMETER :: unittest_  =5
INTEGER,PARAMETER :: planewave_ =6
INTEGER,PARAMETER :: function_  =7


!
!! * scalar flux function in a cell generation options
INTEGER,PARAMETER :: MCS_FLAT         = 1
INTEGER,PARAMETER :: MCS_LINEAR_LLS   = 2
INTEGER,PARAMETER :: MCS_LINEAR_GAUSS = 3
CHARACTER(5),PARAMETER :: MCS_KEY_CellFunction(1:3)=&
  (/"Flat ","LLS  ","Gauss"/)

!!#### OPTIONS
!! @ values for the InterpPlaneU
INTEGER,PARAMETER :: MCS_perp = 1
INTEGER,PARAMETER :: MCS_face = 2
INTEGER,PARAMETER :: MCS_diag = 3
CHARACTER(*),PARAMETER,DIMENSION(1:3) :: KEY_InterpPlane = (/"perp","face","diag"/)

!! * values for the solver
INTEGER,PARAMETER :: MCS_SHORT=1
INTEGER,PARAMETER :: MCS_SB=2
INTEGER,PARAMETER :: MCS_LONG=3
INTEGER,PARAMETER :: MCS_CACHED_LONG=4
INTEGER,PARAMETER :: MCS_SB_PIECEWISE=5

!CHARACTER(*),PARAMETER,DIMENSION(1:4) :: KEY_CharacteristicsSolver = &
!  (/"Short     ",&
!    "SBalance  ",&
!    "Long      ",&
!    "CachedLong"/)

!! * scalar flux function in a cell fixup options
INTEGER,PARAMETER :: MCS_NONE = 1
INTEGER,PARAMETER :: MCS_TAU  = 2
INTEGER,PARAMETER :: MCS_OI3  = 3
INTEGER,PARAMETER :: MCS_Min0 = 4
INTEGER,PARAMETER :: MCS_Switch = 5
INTEGER,PARAMETER :: MCS_TAUN  = 6
INTEGER,PARAMETER :: MCS_TAUDN  = 7
CHARACTER(6),PARAMETER :: MCS_KEY_NonlinearFixup(1:7)=(/"None  ",&
                                                        "Tau   ",&
                                                        "OI3   ",&
                                                        "Min0  ",&
                                                        "Switch",&
                                                        "TauN  ",&
                                                        "TauDN "/)


END MODULE
