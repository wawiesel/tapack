
Pg_Rdp = Pg
CALL dtris2 ( N, Pg_Rdp, NTr, order_Tr, links_Tr )

IF( NTr/=N-2 )THEN
 WRITE(*,*)"FUN_xyTRIANGULATE2_Pg: the number of triangles is not as expected"
 STOP
END IF

