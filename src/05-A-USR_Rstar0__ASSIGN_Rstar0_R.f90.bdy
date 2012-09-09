SELECT CASE( KIND_R ) 
 CASE(KIND_Rsp) ; Rstar0 % KIND = KIND_Rsp ; Rstar0%Rsp = R 
 CASE(KIND_Rdp) ; Rstar0 % KIND = KIND_Rdp ; Rstar0%Rdp = R 
 CASE DEFAULT   ; Rstar0 % KIND = -1
END SELECT