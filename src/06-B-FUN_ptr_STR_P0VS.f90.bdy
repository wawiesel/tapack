
!initialize
S=>NULL()

!quick return if X is not associated
IF( .NOT.ASSOCIATED(VSin) )RETURN

!allocate
ALLOCATE( S )

!USE <STR>
S = STR( VSin , FMT , AdjustLeft , AdjustRight )
