
IF( Unformatted_ )THEN
 
 READ(Unit)k
 READ(Unit)l
 READ(Unit)m
 READ(Unit)n
 IF( k>0 .AND. l>0 .AND. m>0 .AND. n>0 )THEN

  ALLOCATE( P(k,l,m,n) )
  DO n=LBOUND(P,4),UBOUND(P,4)
   DO m=LBOUND(P,3),UBOUND(P,3) 
    DO l=LBOUND(P,2),UBOUND(P,2)
     DO k=LBOUND(P,1),UBOUND(P,1)
      READ(Unit)P(k,l,m,n)
     END DO
    END DO
   END DO
  END DO

 END IF

ELSE
 
 READ(Unit,*)k
 READ(Unit,*)l
 READ(Unit,*)m
 READ(Unit,*)n
 IF( k>0 .AND. l>0 .AND. m>0 .AND. n>0 )THEN
  ALLOCATE( P(k,l,m,n) )
  DO n=LBOUND(P,4),UBOUND(P,4)
   DO m=LBOUND(P,3),UBOUND(P,3) 
    DO l=LBOUND(P,2),UBOUND(P,2)
     DO k=LBOUND(P,1),UBOUND(P,1)
      READ(Unit,100,IOSTAT=ios)P(k,l,m,n)
      IF( ios/=0 )THEN
       P(k,l,m,n) = Error( P(k,l,m,n) )
      END IF
     END DO
    END DO
   END DO
  END DO
 END IF

END IF

