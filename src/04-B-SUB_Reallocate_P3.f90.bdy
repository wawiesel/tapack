
NULLIFY( p )
IF( ASSOCIATED(data) )THEN
 p => data
END IF
NULLIFY( data )

!get original lower bound
IF( ASSOCIATED(p) )THEN
 lbo1 = LBOUND(p,1) 
 lbo2 = LBOUND(p,2) 
 lbo3 = LBOUND(p,3) 
ELSE
 lbo1 = 1
 lbo2 = 1
 lbo3 = 1
END IF
!get original upper bound
IF( ASSOCIATED(p) )THEN
 ubo1 = UBOUND(p,1) 
 ubo2 = UBOUND(p,2) 
 ubo3 = UBOUND(p,3) 
ELSE
 ubo1 = 0
 ubo2 = 0
 ubo3 = 0
END IF

!get the local values
dn_  = dn
IF( PRESENT(dnl) )THEN
 dnl_ = dnl
ELSE
 dnl_ = 0
END IF

!get new bounds
lb1 = getNewLowBound( lbo1 , dnl_(1) )
ub1 = getNewUppBound( ubo1 , dn_(1)  )
lb2 = getNewLowBound( lbo2 , dnl_(2) )
ub2 = getNewUppBound( ubo2 , dn_(2)  )
lb3 = getNewLowBound( lbo3 , dnl_(3) )
ub3 = getNewUppBound( ubo3 , dn_(3)  )

!if the simple condition is true then we can allocate and copy in data
IF( lb1<=ub1 .AND. lb2<=ub2 .AND. lb3<=ub3 )THEN
 ALLOCATE( data(lb1:ub1,lb2:ub2,lb3:ub3) )
 
 !copy in the old values if the range still exists
 lc1 = getCoreLowBound(lbo1,lb1)
 uc1 = getCoreUppBound(ubo1,ub1)
 lc2 = getCoreLowBound(lbo2,lb2)
 uc2 = getCoreUppBound(ubo2,ub2)
 lc3 = getCoreLowBound(lbo3,lb3)
 uc3 = getCoreUppBound(ubo3,ub3)
 IF( lc1<=uc1 .AND. lc2<=uc2 .AND. lc3<=uc3 )&
   data(lc1:uc1,lc2:uc2,lc3:uc3) = p(lc1:uc1,lc2:uc2,lc3:uc3)
 
 !fill up the outliers
 IF( PRESENT(fill) )THEN
  lf1 = getFillLowBound(lbo1,lb1)
  uf1 = getFillUppBound(ubo1,ub1)
  lf2 = getFillLowBound(lbo2,lb2)
  uf2 = getFillUppBound(ubo2,ub2)
  lf3 = getFillLowBound(lbo3,lb3)
  uf3 = getFillUppBound(ubo3,ub3)
  IF( uf1<=UBOUND(data,1) ) data(uf1:,:,:) = fill
  IF( lf1>=LBOUND(data,1) ) data(:lf1,:,:) = fill
  IF( uf2<=UBOUND(data,2) ) data(:,uf2:,:) = fill
  IF( lf2>=LBOUND(data,2) ) data(:,:lf2,:) = fill
  IF( uf3<=UBOUND(data,3) ) data(:,:,uf2:) = fill
  IF( lf3>=LBOUND(data,3) ) data(:,:,:lf2) = fill
 END IF
 
END IF

!clean up pointer
IF( ASSOCIATED(p) )THEN
 DEALLOCATE( p )
 NULLIFY( p )
END IF