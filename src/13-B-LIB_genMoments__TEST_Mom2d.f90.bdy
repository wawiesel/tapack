DO d=1,SIZE(x,1)
 t1(d) = Moment2_Dim(f,x,w,Dim1=d,Dim2=d)
END DO
d = SIZE(x,1)
DO d1=1,SIZE(x,1)
 DO d2=1,SIZE(x,1)
  IF( d1<=d2 )CYCLE
  d = d + 1
  t1(d) = Moment2_Dim(f,x,w,Dim1=d1,Dim2=d2)
 END DO
END DO
t2  = Moment2(f,x,w)
resid_  = NormEll1( t1-t2 )
