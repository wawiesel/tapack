
IF( PRESENT(irng) )THEN
 irng_ = irng
ELSE
 irng_ = (/0,20/)
END IF

IF( PRESENT(jrng) )THEN
 jrng_ = jrng
ELSE
 jrng_ = (/0,20/)
END IF

ALLOCATE( Entry(1:jrng_(2)) )
ALLOCATE( Entry1_(1:jrng_(2)) )
ALLOCATE( Entry2_(1:jrng_(2)) )

IF( PRESENT(preall) )THEN
 preall_ = preall
ELSE
 preall_ = (/10,10/)
END IF

!initialize
CALL INIT_DENSEARRAY( DenseArray , preall_ )
CALL INIT_SPARSEARRAY( SparseArray , IndexArray , preall_ )

!Get the number of arrays
IF( PRESENT(Rseed) )THEN
 CALL RANDOM_SEED( size=seed_size )
 ALLOCATE( local_Rseed(seed_size) )
 local_Rseed(1:2)=Rseed(1:2)
 CALL RANDOM_SEED( put=local_Rseed )
ELSE
 CALL RANDOM_SEED()
END IF
IF( PRESENT(Rseed_out) )THEN
 CALL RANDOM_SEED( size=seed_size )
 ALLOCATE( local_Rseed(seed_size) )
 CALL RANDOM_SEED( get=local_Rseed )
 Rseed_out(1:2)=local_Rseed(1:2)
END IF

!WRITE(*,*)"RandomSeed=",Rseed
Ni = Random( irng_ )

!build arrays
DO i=1,Ni
 Nj = Random( jrng_ )
 CALL Randomize( Entry(1:Nj) )
 
 !WRITE(*,*)"i="//TRIM(STR(i))
 !WRITE(*,*)"  Entry=(/"//TRIM(ADJUSTL(Sentence(STR(Entry(1:Nj)),delim=",")))//"/)"
 !WRITE(*,*)"entering assign"
 CALL ASSIGN_DENSEARRAY( DenseArray , i , Entry(1:Nj) )
 CALL ASSIGN_SPARSEARRAY( SparseArray , IndexArray , i , Entry(1:Nj) )
END DO
!WRITE(*,*)"entering finalize_d"
!WRITE(*,*)"Ni=",Ni
!IF( Ni==0 )THEN
! 33 CONTINUE
!END IF
CALL FINALIZE_DENSEARRAY( DenseArray )
!WRITE(*,*)"entering finalize_s"
CALL FINALIZE_SPARSEARRAY( SparseArray , IndexArray )


!WRITE(*,*)"entering num_entry"
Ni1 = NUM_Entry_DENSEARRAY( DenseArray )
Ni2 = NUM_Entry_SPARSEARRAY( SparseArray , IndexArray )
IF( Ni1/=Ni2 )THEN
 WRITE(*,*)" failed Ni1==Ni2 check"
 Pass = .FALSE.
 GOTO 333
END IF

!check arrays
DO i=1,Ni
 !WRITE(*,*)"entering size_entry"
 Nj1 = SIZE_Entry_DENSEARRAY( DenseArray , i )
 Nj2 = SIZE_Entry_SPARSEARRAY( SparseArray , IndexArray , i )
 IF( Nj1/=Nj2 )THEN
  WRITE(*,*)" failed Nj1==Nj2 check for i=",i
  Pass = .FALSE.
  GOTO 333
 END IF
 
 Nj = Nj1

 DO j=1,Nj
  !WRITE(*,*)"entering access"
  CALL ACCESS_DENSEARRAY( DenseArray , j , i , Entry1 )
  CALL ACCESS_SPARSEARRAY( SparseArray , IndexArray , j , i , Entry2 )
  IF( Entry1/=Entry2 )THEN
   WRITE(*,*)" failed Entry1==Entry2 for i,j=",i,j
   Pass = .FALSE.
   GOTO 333
  END IF
 END DO

END DO

!check arrays
DO i=1,Ni

 !WRITE(*,*)"entering size_entry2"
 Nj1 = SIZE_Entry_DENSEARRAY( DenseArray , i )
 Nj2 = SIZE_Entry_SPARSEARRAY( SparseArray , IndexArray , i )
 IF( Nj1/=Nj2 )THEN
  WRITE(*,*)" failed Nj1==Nj2 check for i=",i
  Pass = .FALSE.
  GOTO 333
 END IF
  
 !WRITE(*,*)"entering access2"
 CALL ACCESS_DENSEARRAY( DenseArray , i , Entry1_ )
 CALL ACCESS_SPARSEARRAY( SparseArray , IndexArray , i , Entry2_ )
 
 IF( .NOT.ALL(Entry1_(1:Nj1)==Entry2_(1:Nj2)) )THEN
  WRITE(*,*)" failed Entry1==Entry2 for i=",i
  Pass = .FALSE.
  GOTO 333
 END IF
 
END DO

!access random entries
DO j=1,100
 i = Random( irng_ )
 !WRITE(*,*)"entering access3"
 CALL ACCESS_DENSEARRAY( DenseArray , j , i , Entry1 )
 CALL ACCESS_SPARSEARRAY( SparseArray , IndexArray , j , i , Entry2 )
 IF( .NOT.(Entry1==Entry2) )THEN
  WRITE(*,*)" failed random Entry1==Entry2 for i,j=",i,j
  Pass = .FALSE.
  GOTO 333
 END IF
END DO

!WRITE(*,*)" passed all checks"
Pass = .TRUE.

333 CONTINUE
!WRITE(*,*)"entering free"
CALL FREE_DENSEARRAY( DenseArray )
CALL FREE_SPARSEARRAY( SparseArray , IndexArray )
