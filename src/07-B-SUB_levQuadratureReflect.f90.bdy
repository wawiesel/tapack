Nmp = SIZE(xp)
N = Nmp/2

!reflect polar angles
xp(N+1:Nmp) = -Reverse( xp(1:N) )
wp(N+1:Nmp) =  Reverse( wp(1:N) )

DO mp=1,N
 !reflect appropriately in upper +z half-plane
 Nma = 4*SIZEa( xa(1,:,mp) )
 CALL aQuadratureReflect( Nma , xa(:,1:Nma,mp) , wa(1:Nma,mp) )
 
 !reflect to the lower -z half-plane
 xa(1,1:Nma,Nmp+1-mp) = +Reverse( xa(1,1:Nma,mp) )
 xa(2,1:Nma,Nmp+1-mp) = +Reverse( xa(2,1:Nma,mp) )
 wa(  1:Nma,Nmp+1-mp) = +Reverse( wa(  1:Nma,mp) )
END DO
