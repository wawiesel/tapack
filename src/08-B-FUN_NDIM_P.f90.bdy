SELECT CASE(Coords)
 CASE(CGE_REC) ; NDIM = SIZE(P)
 CASE(CGE_SPH) ; NDIM = Warning(NDIM)
 CASE(CGE_CYL) ; NDIM = WARNING(NDIM)
 CASE DEFAULT  ; NDIM = Error(NDIM)
END SELECT