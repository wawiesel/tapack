IF( .NOT.PRESENT(NormSpec) )THEN
 FORALL( i=1:SIZE(VECS,2) ) &
   proximity(i) = NormEll2Sqr( VECS(:,i) - VEC )

ELSE

 SELECT CASE(NormSpec)
 
  CASE("L1")
    FORALL( i=1:SIZE(VECS,2) ) &
      proximity(i) = NormEll1( VECS(:,i) - VEC )
  
  CASE("Inf")
    FORALL( i=1:SIZE(VECS,2) ) &
      proximity(i) = NormInfty( VECS(:,i) - VEC )
  
  CASE DEFAULT
    FORALL( i=1:SIZE(VECS,2) ) &
      proximity(i) = NormEll2Sqr( VECS(:,i) - VEC )
 
 END SELECT

END IF


loc = MINLOC( proximity , 1 )