SELECT CASE( KIND_C ) 
 CASE(KIND_Csp) ; Cstar0% KIND = KIND_Csp ; Cstar0%Csp = C 
 CASE(KIND_Cdp) ; Cstar0% KIND = KIND_Cdp ; Cstar0%Cdp = C 
 CASE DEFAULT  ; Cstar0% KIND = 0
END SELECT