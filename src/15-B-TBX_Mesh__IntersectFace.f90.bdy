Nj_ = SIZE(Mesh%Cells(i)%FaceList)
ALLOCATE( jseq(Nj_) , PnDist(Nj_) )

IF( PreferBoundaryFace_ )THEN

 !divide faces into boundary and non-boundary
 j1 = 1
 j2 = Nj_
 DO j_=1,Nj_
  
  jcheck = Mesh%Cells(i)%FaceList(j_)
  
  IF( IsBoundaryFace(Mesh,ABS(jcheck)) )THEN
   jseq(j1) = jcheck
   j1 = j1 + 1
  ELSE
   jseq(j2) = jcheck
   j2 = j2 - 1
  END IF

 END DO

ELSE

 jseq(:) = Mesh%Cells(i)%FaceList(:)

END IF

!initialize
PnDist = HUGE(1._KIND_MSH)
DO j_=1,Nj_

 jcheck = jseq(j_)
 
 !exclude some faces from the check
 IF( PRESENT(ExcludeFaces) )THEN
  IF( ANY(ExcludeFaces==ABS(jcheck)) )THEN
   CYCLE
  END IF 
 END IF
  
 Xf = ExplicitFace( Mesh , jcheck )
 
 !yields inward facing plane
 Pn = PLANE_Xf( Xf )
 IF( Noisy_ )THEN
  WRITE(*,"(3Es12.3)")Pn(1),Pn(2),Pn(3)
 END IF

 Ry = xyRAY_PU( r0 , Omega0(1:Mesh%NDim) )

 INTERSECT = xyINTERSECT_PnRy( Pn , Ry , DIST=SDIST_ )
 
 IF( INTERSECT )THEN
  !can only register as intersection if facing same direction
  !as plane because we are INSIDE a cell
  U = xyDIRECTION_Pn( Pn )
  dotprod = DOT_PRODUCT(U,Omega0(1:Mesh%NDim) )
  IF( dotprod<0._KIND_MSH )THEN
   PnDist(j_) = ABS(SDIST_)
  END IF
 END IF

 CALL CLEAN(Xf)

END DO

!old way
j_ = MINLOC(PnDist,1)
j = ABS(jseq(j_))
SDIST_ = PnDist(j_)  

!!new way because if two faces have the same plane equation then
!!there was not a way to decide the actual one intersected

!if there is another distance very close to the minimum
DO n=1,SIZE(PnDist)
 IF( (ABS(PnDist(n)-SDIST_)/SDIST_) < &
     (SQRT(EPSILON(1._KIND_MSH))  ) )THEN
   
   P(1:Mesh%Ndim) = Ry(:,1) + PnDist(n)*Ry(:,2)

   IF( IsPointOnFace(Mesh,jseq(n),P) )THEN
    j = ABS(jseq(n)) 
    SDIST_=PnDist(j_)
    EXIT
   END IF

 END IF
END DO


DEALLOCATE( jseq , PnDist ) 