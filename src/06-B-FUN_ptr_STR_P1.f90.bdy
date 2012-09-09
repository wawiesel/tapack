
!initialize
S=>NULL()

!quick return if X is not associated
IF( .NOT.ASSOCIATED(X) )RETURN

!allocate
ALLOCATE( S(1:SIZE(X)) )

!USE <STR>
S = STR( X , FMT , AdjustLeft , AdjustRight )
