DO d=1,SIZE(x,1)
 n1(d)  = Moment1_Dim(f,x,w,Dim=d)
END DO
n2  = Moment1(f,x,w)
resid_  = resid_ + NormEll1( n1-n2 )