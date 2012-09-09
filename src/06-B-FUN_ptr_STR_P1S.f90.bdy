
!initialize
S=>NULL()

!quick return if X is not associated
IF( .NOT.ASSOCIATED(Sin) )RETURN

!allocate
ALLOCATE( S(1:SIZE(Sin)) )

!USE <STR>
DO i=1,SIZE(Sin)
 S(i) = STR( Sin(i) , FMT , AdjustLeft , AdjustRight )
END DO