
!WRITE(*,*)"====inside xyINTEGRALF_Tr==="
Trx(:,:,1) = Tr

!WRITE(*,*)"  Tr=",Tr
!WRITE(*,*)"  f(0,0)=",f(0._KIND_R,0._KIND_R)

INTEGRALF = xyINTEGRALF_Trx( f , 1 , Trx , &
  reltol , abstol , &
  nmin,nmax , err,num_eval,ierr )

!WRITE(*,*)" INTEGRALF = ",INTEGRALF

