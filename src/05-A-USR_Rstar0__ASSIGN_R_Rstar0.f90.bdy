SELECT CASE( Rstar0%KIND ) 
 CASE(KIND_Rsp) ; R = Rstar0%Rsp
 CASE(KIND_Rdp) ; R = Rstar0%Rdp
 CASE DEFAULT   ; R = ERROR(R)
END SELECT