
N_ = N

!initialize
IF( PRESENT(Nm) )THEN
 Nm_ = Nm
ELSE
 Nm_ = (N_+2)*N_
END IF


!find the nearest value
val = NEARLOC( KEY_Nm , Nm_ )


!select quadrature set based on val
SELECT CASE( val ) 

!! S4
CASE(1)
N_=4

ALLOCATE( abcs(2),wts(1),wtloc(3) )
abcs = (/0.8688903_KIND_R,0.3500212_KIND_R/)
wts  = (/0.3333333_KIND_R/)
wtloc= (/1,&
        1,1/)

!! S6
CASE(2)
N_=6

ALLOCATE( abcs(3),wts(2),wtloc(6) )
abcs = (/0.9261808_KIND_R,0.6815076_KIND_R,&
         0.2666355_KIND_R/)
wts  = (/0.1761263_KIND_R,0.15720271_KIND_R/)
wtloc= (/1,&
        2,2,&
       1,2,1/)


!! S8
CASE(3)
N_=8

ALLOCATE( abcs(4),wts(3),wtloc(10) )
abcs = (/0.9511897_KIND_R,0.7867958_KIND_R,&
         0.5773503_KIND_R,0.2182179_KIND_R/)
wts  = (/0.1209877_KIND_R,0.0907407_KIND_R,&
         0.0925926_KIND_R/)
wtloc= (/1,&
        2,2,&
       2,3,2,&
      1,2,2,1/)


!! S12
CASE(4) 
N_=12

ALLOCATE( abcs(6),wts(5),wtloc(21) )
abcs = (/0.9716377_KIND_R,0.8722706_KIND_R,&
         0.7600210_KIND_R,0.6280191_KIND_R,&
                 0.4595476_KIND_R,0.1672126_KIND_R/)
wts  = (/0.0707626_KIND_R,0.0558811_KIND_R,&
         0.0373377_KIND_R,0.0502819_KIND_R,&
                 0.0258513_KIND_R/)
wtloc= (/1,&
        2,2,&
       3,4,3,&
      3,5,5,3,&
     2,4,5,4,2,&
    1,2,3,3,2,1/)


!! S16
CASE(5)
N_=16
ALLOCATE( abcs(8),wts(8),wtloc(36) )
abcs = (/0.9805009_KIND_R,0.9092855_KIND_R,&
         0.8319966_KIND_R,0.7467506_KIND_R,&
                 0.6504264_KIND_R,0.5370966_KIND_R,&
                 0.3922893_KIND_R,0.1389568_KIND_R/)
wts  = (/0.0489872_KIND_R,0.0413296_KIND_R,&
         0.0212326_KIND_R,0.0256207_KIND_R,&
                 0.0360486_KIND_R,0.0144589_KIND_R,&
                 0.0344958_KIND_R,0.0085179_KIND_R/)
wtloc= (/1,&
        2,2,&
       3,5,3,&
      4,6,6,4,&
     4,7,8,7,4,&
    3,6,8,8,6,3,&
   2,5,6,7,6,5,2,&
  1,2,3,4,4,3,2,1/)

CASE DEFAULT
 xa = ERROR(1._KIND_R) 
 xp = ERROR(1._KIND_R)
 wa = ERROR(1._KIND_R)
 wp = ERROR(1._KIND_R)
 RETURN

END SELECT


!calculate numbers of polar and azimuthal angles
Nmp = N_
Nma = 2*N_

!allocate temp for abcissas
ALLOCATE( abcs_(3,Nmp/2,Nmp/2,Nmp/2) )
abcs_ = ERROR(1._KIND_R)

!determine abcissas
DO i=1,Nmp/2
 mu = abcs(i)
 DO j=Nmp/2,Nmp/2+1-i,-1
  eta = abcs(j)
  xhi = sqrt( 1._KIND_R - eta**2 - mu**2 )
  k = NEARLOC( abcs , xhi )
  abcs_(:,i,j,k) = (/mu,eta,xhi/)
 END DO
END DO

!set abcissas and weights
ALLOCATE( xa(2,Nma,Nmp) , wa(Nma,Nmp) )
ALLOCATE( xp(Nmp) , wp(Nmp) )
xa = ERROR(1._KIND_R)
xp = ERROR(1._KIND_R)
wp = 1._KIND_R
wa = 0._KIND_R
mw = 0
DO k=1,Nmp/2
 mp = k
 xp(mp) = abcs(k)
 ma = 0
 DO j=Nmp/2,1,-1
  DO i=1,Nmp/2
   u = abcs_(:,i,j,k)
   IF( .NOT.ANY(IsError(u)) )THEN
    ma = ma + 1
        xa(:,ma,mp) = u
        mw = mw + 1
    wa(ma,mp) = wts( wtloc(mw) )
   END IF
  END DO
 END DO
END DO

!deallocate
DEALLOCATE( abcs_,wts,wtloc,abcs)

!reflect to other quadrants
CALL levQuadratureReflect( xa,xp,wa,wp )

CALL levQuadratureNormalize( xa,xp,wa,wp )