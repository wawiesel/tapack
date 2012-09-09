!!# USER MODULE: <<USR_GraphTraversal>>
MODULE USR_GraphTraversal

!!## PURPOSE
!! Provides a few routines for traversing graphs (of node
!! dependencies), which are assuming to be Directed Acyclic
!! Graphs (DAGs).


!!## EXTERNAL MODULES
USE FUN_STR   !!((05-B-FUN_STR.f90))
USE PAR_Units !!((02-A-PAR_Units.f90))


!!## DEFAULT IMPLICIT
IMPLICIT NONE


!!## DEFAULT ACCESS
PRIVATE


!!## PUBLIC ACCESS
PUBLIC :: TEST_GraphOrdering
PUBLIC :: TEST_Graph0
PUBLIC :: GraphOrdering_stack
PUBLIC :: GraphOrdering_rec


!!## PROCEDURES
CONTAINS


!!### SUBROUTINE <<TEST_Graph0>>
SUBROUTINE TEST_Graph0()

!!#### PURPOSE
!! Test a Graph traversal algorithms.

!!#### LOCAL VARIABLES
INTEGER :: order(12)
INTEGER :: Graph(4,12)
INTEGER :: k

!!--begin--

Graph = 0
Graph(1:3,1 ) = (/2,3,4/)
Graph(1:1,2 ) = (/4/)
Graph(1:3,3 ) = (/4,6,5/)
Graph(1:1,4 ) = (/0/)
Graph(1:1,5 ) = (/0/)
Graph(1:1,6 ) = (/4/)

Graph(1:3,7 ) = (/8,9,10/)
Graph(1:1,8 ) = (/10/)
Graph(1:3,9 ) = (/10,12,11/)
Graph(1:1,10) = (/0/)
Graph(1:1,11) = (/0/)
Graph(1:1,12) = (/10/)


order = GraphOrdering_rec(Graph)
WRITE(*,*)"qgr Ordering:"
DO k=1,SIZE(order)
 WRITE(*,*)order(k)
END DO
WRITE(*,*)
WRITE(*,*)MERGE("PASS","FAIL",TEST_GraphOrdering(Graph,order))
WRITE(*,*)

order = GraphOrdering_stack(Graph)
WRITE(*,*)"Stack Ordering:"
DO k=1,SIZE(order)
 WRITE(*,*)order(k)
END DO
WRITE(*,*)
WRITE(*,*)MERGE("PASS","FAIL",TEST_GraphOrdering(Graph,order))
WRITE(*,*)

!!--end--
END SUBROUTINE



!!### SUBROUTINE <<GraphOrdering_stack>>
FUNCTION GraphOrdering_stack(Graph) RESULT(order)

!!#### PURPOSE
!! Order a graph of dependencies by placing each
!! available node on a stack.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: Graph(:,:)

!!#### REQUIRED OUTPUT
INTEGER :: order(SIZE(Graph,2))

!!#### LOCAL VARIABLES
LOGICAL :: Used(SIZE(Graph,2))
INTEGER :: i,n

!!--begin--
order = 0
Used = .FALSE.
i=1
n=1
DO
 IF( ALL(Used) )EXIT

 IF( .NOT.Used(i) )THEN
  IF( HasDepsKnown(Graph(:,i),Used) )THEN
   order(n) = i
   n = n + 1
   Used(i) = .TRUE.
  END IF
 END IF
 i = MERGE( 1 , i+1 , i==SIZE(Graph,2) )

END DO
!!--end--
CONTAINS

FUNCTION HasDepsKnown(Graph,Used)
LOGICAL :: HasDepsKnown
INTEGER :: Graph(:)
LOGICAL :: Used(:)
INTEGER :: k,ii
!!--begin--
HasDepsKnown = .TRUE.
DO k=1,SIZE(Graph,1)
 ii = Graph(k)
 IF( ii==0 )EXIT
 IF( .NOT.Used(ii) )THEN
  HasDepsKnown=.FALSE.
  EXIT
 END IF
END DO
!!--end--
END FUNCTION

END FUNCTION


!!### FUNCTION <<GraphOrdering_rec>>
FUNCTION GraphOrdering_rec(Graph) RESULT(order)

!!#### PURPOSE
!! Order a graph of nodes by traversing the ''tree''
!! from the bottom to the top level using a recursive
!! algorithm.


!!#### DETAILS
!! I think this is faster than the ''stack'' based
!! ordering, but I am not sure.


!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: Graph(:,:)

!!#### REQUIRED OUTPUT
INTEGER :: order(SIZE(Graph,2))

!!#### LOCAL VARIABLES
LOGICAL :: Used(SIZE(Graph,2))
INTEGER :: Nc,No
INTEGER :: i,o,level

!!#### LOCAL PARAMETERS
LOGICAL,PARAMETER :: Noisy_=.FALSE.
INTEGER,PARAMETER :: unit_noisy = window_unit

!!--begin--

Nc = SIZE(Graph,1)
No = SIZE(Graph,2)

!initialize level and used array
level = 0
Used  = .FALSE.

!initialize order array and order index
order = 0
o     = 0

!enter
DO i=1,No
 IF( Noisy_ )WRITE(unit_noisy,*)REPEAT("  ",level)//"i="//TRIM(STR(i))
 CALL Recurse(i,o,level=level)
END DO

!!--end--
CONTAINS

RECURSIVE SUBROUTINE Recurse(i,o,level)
INTEGER,INTENT(IN)    :: i
INTEGER,INTENT(INOUT) :: o
INTEGER,INTENT(IN)    :: level
INTEGER :: ic,c
!!--begin--

IF( Noisy_ )WRITE(unit_noisy,*)REPEAT("  ",level)//"i="//TRIM(STR(i))
IF( i==0 )RETURN

!check if all children are dead and if not
!move to each non-dead child
DO c=1,Nc
 ic = Graph(c,i)
 IF( ic==0 )CYCLE
 IF( Used(ic) )CYCLE
 CALL Recurse(ic,o,level+1)
END DO

!all children are dead
IF( .NOT.Used(i) )THEN
 o = o + 1
 IF( Noisy_ )WRITE(unit_noisy,*)REPEAT("  ",level)//"<o>="//TRIM(STR(o))
 IF( Noisy_ )WRITE(unit_noisy,*)REPEAT("  ",level)//"<i>="//TRIM(STR(i))
 Used(i) = .TRUE.
 order(o) = i
END IF

!!--end--
END SUBROUTINE

END FUNCTION GraphOrdering_rec



!!### FUNCTION <<TEST_GraphOrdering>>
FUNCTION TEST_GraphOrdering(Graph,order) RESULT(Pass)

!!#### PURPOSE
!! Test a graph ordering for validity.

!!#### DETAILS
!! It is much easier to check an order for validity
!! than come up with a valid ordering.

!!#### REQUIRED INPUT
INTEGER,INTENT(IN) :: Graph(:,:)
INTEGER,INTENT(IN) :: order(SIZE(Graph,2))

!!#### REQUIRED OUTPUT
LOGICAL :: Pass

!!#### LOCAL VARIABLES
LOGICAL :: Used(SIZE(Graph,2))
INTEGER :: i,k,j,n

!!--begin--

IF( ANY(order==0) )THEN
 Pass = .FALSE.
ELSE
 Pass = .TRUE.
 Used = .FALSE.
 DO i=1,SIZE(Graph,2)
  k = order(i)
  DO n=1,SIZE(Graph,1)
   j = Graph(n,k)
   IF( j==0 )EXIT
   IF( .NOT.Used(j) )THEN
    Pass = .FALSE.
	RETURN
   END IF
  END DO
  Used(k) = .TRUE.
 END DO
END IF

!!--end--
END FUNCTION




END MODULE
