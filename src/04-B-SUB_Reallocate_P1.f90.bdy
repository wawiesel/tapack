
NULLIFY( p )
IF( ASSOCIATED(data) )THEN
 p => data
END IF
NULLIFY( data )

!get original lower bound
IF( ASSOCIATED(p) )THEN
 lbo = LBOUND(p,1) 
ELSE
 lbo = 1
END IF
!get original upper bound
IF( ASSOCIATED(p) )THEN
 ubo = UBOUND(p,1) 
ELSE
 ubo = 0
END IF

!get new bounds
lb = getNewLowBound( lbo , dnl )
ub = getNewUppBound( ubo , dn  )
IF( PRESENT(Noisy) )THEN
 IF( Noisy )THEN
  WRITE(*,*)"new lb,ub=",lb,ub
  WRITE(*,*)"old lb,ub=",lbo,ubo
  IF( PRESENT(dnl) )THEN
   WRITE(*,*)"change dnl,dn=",dnl,dn
  ELSE
   WRITE(*,*)"change dn=",dn
  END IF
 END IF
END IF

!if the simple condition is true then we can allocate and copy in data
IF( lb<=ub )THEN
 ALLOCATE( data(lb:ub) )
 
 !copy in the old values if the range still exists
 lc = getCoreLowBound(lbo,lb)
 uc = getCoreUppBound(ubo,ub)
 IF( PRESENT(Noisy) )THEN
  IF( Noisy )THEN
   WRITE(*,*)"lc,uc=",lc,uc
  END IF
 END IF
 IF( lc<=uc )THEN
  DO i=lc,uc
   data(i) = p(i)
  END DO
 END IF

 !fill up the outliers
 IF( PRESENT(fill) )THEN
  lf = getFillLowBound(lbo,lb)
  uf = getFillUppBound(ubo,ub)
  IF( uf<=UBOUND(data,1) )THEN
   DO i=uf,UBOUND(data,1)
    data(i) = fill
   END DO
  END IF
  IF( lf>=LBOUND(data,1) )THEN
   DO i=LBOUND(data,1),lf
    data(i) = fill
   END DO
  END IF
 END IF
 
END IF

!clean up pointer
IF( ASSOCIATED(p) )THEN
 DEALLOCATE( p )
 NULLIFY( p )
END IF