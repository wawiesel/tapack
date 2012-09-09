!!# MODULE <<FUN_xyTRIANGULATE>>
MODULE FUN_xyTRIANGULATE
!!## PURPOSE
!! Triangulate the following shapes.
!!
!! * xyTRIANGULATE_Pg - returns a triangulation of a polygon.

!!## DISCUSSION
!!
!! * xyTRIANGULATE_Pg
!!
!!    Given a polygon with N (distinct) nodes, a triangulation consists of
!!    N-2 (distinct) triangles, with the property that each triangle is
!!    constructed from vertices of the polygon, and that every point in the
!!    polygon is contained in exactly one triangle, unless it lies on the
!!    boundary of two or more triangles, and that no point exterior to the
!!    polygon is contained in any triangle.  In other words, a triangulation
!!    is a dissection of the area of the polygon into triangles.


!!## COMMENTS
!!    This algorithm should be correct, but not efficient.


!!## MODIFIED
!!  09 November 2000

!!## AUTHOR
!!  John Burkardt

!!## REFERENCE
!!
!!    de Berg, van Krevald, Overmars, Schwarzkopf,
!!    Computational Geometry,
!!    Second Edition,
!!    Springer, 2000, pages 46-49.
!!

!!## USAGE
!
!     order_Tr = xyTRIANGULATE_Pg(N,Pg)
!
!! where <N> is the input number of nodes of the polygon and
!! Pg(1,1:N), Pg(2,1:N) are the counter-clockwise ordered
!! x and y-coordinates of the polygon, starting with the
!! left-lowest point.
!!
!! The output, <order_Tr(1:3,N-1)> are the N-2 triangles
!! that make up the polygon.

!!## EXTERNAL KINDS
USE KND_IntrinsicTypes      ,ONLY: KIND_Rsp , KIND_Rdp      !!((01-A-KND_IntrinsicTypes.f90))

!!## EXTERNAL PARAMETERS
USE PAR_Constants_Rsp,ONLY: c_1_Rsp => c_1 , c_0_Rsp => c_0 !!((02-A-PAR_Constants_Rsp.f90))
USE PAR_Constants_Rdp,ONLY: c_1_Rdp => c_1 , c_0_Rdp => c_0 !!((02-A-PAR_Constants_Rdp.f90))

!!## EXTERNAL PROCEDURES
USE FUN_xyDIRECTION       ,ONLY: xyDIRECTION_PP             !!((04-B-FUN_xyDIRECTION.f90))
USE FUN_xyANGLE           ,ONLY: xyANGLE_UU                 !!((05-B-FUN_xyANGLE.f90))
USE FUN_xyROTATE          ,ONLY: xyROTATE_Px                !!((05-B-FUN_xyROTATE.f90))
USE FUN_xyTRANSLATE       ,ONLY: xyTRANSLATE_Px             !!((05-B-FUN_xyTRANSLATE.f90))
USE FUN_xyCOUNTERCLOCKWISE,ONLY: xyCOUNTERCLOCKWISE_Cn      !!((06-B-FUN_xyCOUNTERCLOCKWISE.f90))

USE LIB_MeshQuality,ONLY: dtris2                            !!((04-B-LIB_MeshQuality.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## PROCEDURE OVERLOADING
INTERFACE xyTRIANGULATE_Pg
 MODULE PROCEDURE xyTRIANGULATE_Pg_Rsp
 MODULE PROCEDURE xyTRIANGULATE_Pg_Rdp
 MODULE PROCEDURE xyTRIANGULATE2_Pg_Rsp
 MODULE PROCEDURE xyTRIANGULATE2_Pg_Rdp
END INTERFACE
INTERFACE D_SWAP
 MODULE PROCEDURE D_SWAP_Rsp
 MODULE PROCEDURE D_SWAP_Rdp
END INTERFACE
INTERFACE ENORM0_2D
 MODULE PROCEDURE ENORM0_2D_Rsp
 MODULE PROCEDURE ENORM0_2D_Rdp
END INTERFACE
INTERFACE ENORMSQ0_2D
 MODULE PROCEDURE ENORMSQ0_2D_Rsp
 MODULE PROCEDURE ENORMSQ0_2D_Rdp
END INTERFACE
INTERFACE DMAT_SOLVE
 MODULE PROCEDURE DMAT_SOLVE_Rsp
 MODULE PROCEDURE DMAT_SOLVE_Rdp
END INTERFACE
INTERFACE LINE_EXP_POINT_DIST_2D
 MODULE PROCEDURE LINE_EXP_POINT_DIST_2D_Rsp
 MODULE PROCEDURE LINE_EXP_POINT_DIST_2D_Rdp
END INTERFACE
INTERFACE TRIANGLE_CONTAINS_POINT_2D
 MODULE PROCEDURE TRIANGLE_CONTAINS_POINT_2D_Rsp
 MODULE PROCEDURE TRIANGLE_CONTAINS_POINT_2D_Rdp
END INTERFACE
INTERFACE POLY_REORDER_NODES
 MODULE PROCEDURE POLY_REORDER_NODES_Rsp
 MODULE PROCEDURE POLY_REORDER_NODES_Rdp
END INTERFACE

!!## PUBLIC ACCESS LIST
PUBLIC :: xyTRIANGULATE_Pg
PUBLIC :: xyTRIANGULATE_Pg_Rsp
PUBLIC :: xyTRIANGULATE_Pg_Rdp

!!## MODULE PROCEDURES
CONTAINS


!!### FUNCTION: xyTRIANGULATE2_Pg_Rsp
FUNCTION xyTRIANGULATE2_Pg_Rsp( N , Pg , P_centroid ) RESULT(order_Tr)
!!#### LOCAL KINDS
INTEGER    ,PARAMETER :: KIND_R = KIND_Rsp
INCLUDE "07-B-FUN_xyTRIANGULATE2_Pg.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyTRIANGULATE2_Pg.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: xyTRIANGULATE2_Pg_Rdp
FUNCTION xyTRIANGULATE2_Pg_Rdp( N , Pg , P_centroid ) RESULT(order_Tr)
!!#### LOCAL KINDS
INTEGER    ,PARAMETER :: KIND_R = KIND_Rdp
INCLUDE "07-B-FUN_xyTRIANGULATE2_Pg.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyTRIANGULATE2_Pg.f90.bdy"
!!--end--
END FUNCTION



!!### FUNCTION: xyTRIANGULATE_Pg_Rsp
FUNCTION xyTRIANGULATE_Pg_Rsp( N , Pg ) RESULT(order_Tr)
!!#### LOCAL KINDS
INTEGER    ,PARAMETER :: KIND_R = KIND_Rsp
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1 = c_1_Rsp
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rsp
INCLUDE "07-B-FUN_xyTRIANGULATE_Pg.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyTRIANGULATE_Pg.f90.bdy"
!!--end--
END FUNCTION


!!### FUNCTION: xyTRIANGULATE_Pg_Rdp
FUNCTION xyTRIANGULATE_Pg_Rdp( N , Pg ) RESULT(order_Tr)
!!#### LOCAL KINDS
INTEGER    ,PARAMETER :: KIND_R = KIND_Rdp
!!#### LOCAL PARAMETERS
REAL(KIND_R),PARAMETER :: c_1 = c_1_Rdp
REAL(KIND_R),PARAMETER :: c_0 = c_0_Rdp
INCLUDE "07-B-FUN_xyTRIANGULATE_Pg.f90.hdr"
!!--begin--
INCLUDE "07-B-FUN_xyTRIANGULATE_Pg.f90.bdy"
!!--end--
END FUNCTION


subroutine ivec_indicator ( n, a )

!*******************************************************************************
!
!! IVEC_INDICATOR sets an integer vector to the vector A(I)=I.
!
!  Modified:
!
!    09 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of elements of A.
!
!    Output, integer A(N), the array to be initialized.
!
  implicit none

  integer n

  integer a(n)
  integer i

  do i = 1, n
    a(i) = i
  end do

  return
end subroutine

subroutine ivec_push ( n, x, stack1_max, stack1_num, stack1, stack2_max, &
  stack2_num, stack2 )

!*******************************************************************************
!
!! IVEC_PUSH pushes an integer vector onto a stack.
!
!  Discussion:
!
!    STACK1 contains a list of the dimensions of the objects stored.
!    Therefore, STACK1_MAX should be at least as big as the maximum number
!    of objects to be considered.
!
!    STACK2 contains the values of the objects.  Therefore, STACK2_MAX
!    should probably be as big as the maximum total length of the maximum
!    number of objects stored.
!
!    On first call, the USEr should have set STACK1_NUM and STACK2_NUM to zero.
!
!  Modified:
!
!    09 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the dimension of the vector.  N may be zero.
!
!    Input, integer X(N), the value of the vector.
!
!    Input, integer STACK1_MAX, the maximum size of STACK1.
!
!    Input/output, integer STACK1_NUM, the current size of STACK1.
!
!    Input/output, integer STACK1(STACK1_MAX), the vector dimension stack.
!
!    Input, integer STACK2_MAX, the maximum size of STACK2.
!
!    Input/output, integer STACK2_NUM, the current size of STACK2.
!
!    Input/output, integer STACK2(STACK2_MAX), the vector value stack.
!
  implicit none

  integer n
  integer stack1_max
  integer stack2_max

  integer stack1(stack1_max)
  integer stack1_num
  integer stack2(stack2_max)
  integer stack2_num
  integer x(n)

  if ( n < 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'IVEC_PUSH - Fatal error!'
    write ( *, * ) '  Input dimension N is negative.'
    stop
  end if

  if ( stack1_max < stack1_num + 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'IVEC_PUSH - Fatal error!'
    write ( *, * ) '  Exceeding size of stack #1.'
    stop
  end if

  if ( stack2_max < stack2_num + n ) then
    write ( *, * ) ' '
    write ( *, * ) 'IVEC_PUSH - Fatal error!'
    write ( *, * ) '  Exceeding size of stack #2.'
    stop
  end if

  stack1_num = stack1_num + 1
  stack1(stack1_num) = n

  stack2(stack2_num+1:stack2_num+n) = x(1:n)
  stack2_num = stack2_num + n

end subroutine


subroutine ivec_pop ( n, x, stack1_max, stack1_num, stack1, stack2_max, &
  stack2_num, stack2 )

!*******************************************************************************
!
!! IVEC_POP pops an integer vector off of a stack.
!
!  Discussion:
!
!    If there are no more objects in the stack, N is returned as -1.
!
!  Modified:
!
!    09 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer N, the dimension of the vector.
!
!    Output, integer X(*), the value of the vector.
!
!    Input, integer STACK1_MAX, the maximum size of STACK1.
!
!    Input/output, integer STACK1_NUM, the current size of STACK1.
!
!    Input/output, integer STACK1(STACK1_MAX), the vector dimension stack.
!
!    Input, integer STACK2_MAX, the maximum size of STACK2.
!
!    Input/output, integer STACK2_NUM, the current size of STACK2.
!
!    Input/output, integer STACK2(STACK2_MAX), the vector value stack.
!
  implicit none

  integer n
  integer stack1_max
  integer stack2_max

  integer stack1(stack1_max)
  integer stack1_num
  integer stack2(stack2_max)
  integer stack2_num
  integer x(*)

  if ( stack1_num < 1 ) then
    n = -1
    return
  end if

  n = stack1(stack1_num)
  stack1_num = stack1_num - 1

  stack2_num = stack2_num - n
  x(1:n) = stack2(stack2_num+1:stack2_num+n)

end subroutine

subroutine d_swap_Rsp ( x, y )

!*******************************************************************************
!
!! D_SWAP swaps two real values.
!
!  Modified:
!
!    01 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, real ( kind = 8 ) X, Y.  On output, the values of X and
!    Y have been interchanged.
!
  implicit none

  real ( kind = KIND_Rsp ) x
  real ( kind = KIND_Rsp ) y
  real ( kind = KIND_Rsp ) z

  z = x
  x = y
  y = z

end subroutine


function enorm0_2d_Rsp ( x0, y0, x1, y1 ) RESULT(enorm0_2D)

!*******************************************************************************
!
!! ENORM0_2D computes the Euclidean norm of (P1-P0) in 2D.
!
!  Modified:
!
!    16 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = KIND_R ) X0, Y0, X1, Y1, the coordinates of the points P0 and P1.
!
!    Output, real ( kind = KIND_R ) ENORM0_2D, the Euclidean norm of (P1-P0).
!
  implicit none

  real ( kind = KIND_Rsp ) enorm0_2d
  real ( kind = KIND_Rsp ) x0
  real ( kind = KIND_Rsp ) x1
  real ( kind = KIND_Rsp ) y0
  real ( kind = KIND_Rsp ) y1

  enorm0_2d = sqrt ( ( x1 - x0 )**2 + ( y1 - y0 )**2 )

end function

function enormsq0_2d_Rsp ( x0, y0, x1, y1 )  RESULT(enormsq0_2d)

!*******************************************************************************
!
!! ENORMSQ0_2D computes the square of the Euclidean norm of (P1-P0) in 2D.
!
!  Modified:
!
!    06 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = KIND_R ) X0, Y0, X1, Y1, the coordinates of the points
!    P0 and P1.
!
!    Output, real ( kind = KIND_R ) ENORMSQ0_2D, the square of the Euclidean norm of (P1-P0).
!
  implicit none

  real ( kind = KIND_Rsp ) enormsq0_2d
  real ( kind = KIND_Rsp ) x0
  real ( kind = KIND_Rsp ) x1
  real ( kind = KIND_Rsp ) y0
  real ( kind = KIND_Rsp ) y1

  enormsq0_2d = ( x1 - x0 )**2 + ( y1 - y0 )**2

end function



subroutine poly_reorder_nodes_Rsp ( nxy, x, y, npoly, poly )

!*******************************************************************************
!
!! POLY_REORDER_NODES reorders nodes of a polygon so node 1 is leftest lowest.
!
!  Modified:
!
!    12 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer NXY, the number of nodes.
!
!    Input, real ( kind = KIND_R ) X(NXY), Y(NXY), the coordinates of the nodes.
!
!    Input, integer NPOLY, the number of nodes of the polygon.
!
!    Input/output, POLY(NPOLY), the indices of the nodes.
!
  implicit none

  integer npoly
  integer nxy

  integer i
  integer imin
  integer p
  integer pmin
  integer poly(npoly)
  integer poly2(npoly)
  real ( kind = KIND_Rsp ) x(nxy)
  real ( kind = KIND_Rsp ) y(nxy)

  imin = 1
  pmin = poly(imin)

  do i = 2, npoly
    p = poly(i)
    if ( &
      ( x(p) < x(pmin) ) .or. &
      ( x(p) == x(pmin) .and. y(p) < y(pmin) ) ) then
      imin = i
      pmin = p
    end if
  end do

  if ( imin /= 1 ) then

    poly2(1:npoly+1-imin) = poly(imin:npoly)
    poly2(npoly+2-imin:npoly) = poly(1:imin-1)

    poly(1:npoly) = poly2(1:npoly)

  end if

end subroutine

subroutine triangle_contains_point_2d_Rsp ( x1, y1, x2, y2, x3, y3, x, y, inside )

!*******************************************************************************
!
!! TRIANGLE_CONTAINS_POINT_2D finds if a point is inside a triangle in 2D.
!
!  Modified:
!
!    12 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = KIND_Rsp ) X1, Y1, X2, Y2, X3, Y3, the coordinates of
!    the corners of the triangle.
!
!    Input, real ( kind = KIND_Rsp ) X, Y, the point to be checked.
!
!    Output, logical INSIDE, is .TRUE. if (X,Y) is inside
!    the triangle or on its boundary, and .FALSE. otherwise.
!
  implicit none

  integer, parameter :: N = 2
  integer, parameter :: NRHS = 1

  real ( kind = KIND_Rsp ) a(N,N+NRHS)
  real ( kind = KIND_Rsp ) c1
  real ( kind = KIND_Rsp ) c2
  integer info
  logical inside
  real ( kind = KIND_Rsp ) x
  real ( kind = KIND_Rsp ) x1
  real ( kind = KIND_Rsp ) x2
  real ( kind = KIND_Rsp ) x3
  real ( kind = KIND_Rsp ) y
  real ( kind = KIND_Rsp ) y1
  real ( kind = KIND_Rsp ) y2
  real ( kind = KIND_Rsp ) y3
!
!  Set up the linear system
!
!    ( X2-X1  X3-X1 ) C1  = X-X1
!    ( Y2-Y1  Y3-Y1 ) C2    Y-Y1
!
!  which is satisfied by the barycentric coordinates of (X,Y).
!
  a(1,1) = x2 - x1
  a(1,2) = x3 - x1
  a(1,3) = x - x1

  a(2,1) = y2 - y1
  a(2,2) = y3 - y1
  a(2,3) = y - y1
!
!  Solve the linear system.
!
  call dmat_solve ( a, N, NRHS, info )

  if ( info /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'TRIANGLE_CONTAINS_POINT - Fatal error!'
    write ( *, * ) '  The linear system is singular.'
    write ( *, * ) '  The input data does not form a proper triangle.'
    stop
  end if

  c1 = a(1,3)
  c2 = a(2,3)
!
!  If the point is in the triangle, its barycentric coordinates
!  must both be nonnegative, and sum to no more than 1.
!
  if ( c1 < 0._KIND_Rsp .or. c2 < 0._KIND_Rsp ) then
    inside = .false.
  else if ( 1._KIND_Rsp < c1 + c2 ) then
    inside = .false.
  else
    inside = .true.
  end if

end subroutine

subroutine line_exp_point_dist_2d_Rsp ( x1, y1, x2, y2, x, y, dist )

!*******************************************************************************
!
!! LINE_EXP_POINT_DIST_2D: distance ( explicit line, point ) in 2D.
!
!  Formula:
!
!    The explicit form of a line in 2D is:
!
!      (X1,Y1), (X2,Y2).
!
!  Modified:
!
!    27 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = KIND_Rsp ) X1, Y1, X2, Y2.  (X1,Y1) and (X2,Y2) are two
!    points on the line.
!
!    Input, real ( kind = KIND_Rsp ) X, Y, the point whose distance from the line is
!    to be measured.
!
!    Output, real ( kind = KIND_Rsp ) DIST, the distance from the point to the line.
!
  implicit none

  real ( kind = KIND_Rsp ) bot
  real ( kind = KIND_Rsp ) dist
  real ( kind = KIND_Rsp ) dot
  real ( kind = KIND_Rsp ) t
  real ( kind = KIND_Rsp ) x
  real ( kind = KIND_Rsp ) xn
  real ( kind = KIND_Rsp ) x1
  real ( kind = KIND_Rsp ) x2
  real ( kind = KIND_Rsp ) y
  real ( kind = KIND_Rsp ) yn
  real ( kind = KIND_Rsp ) y1
  real ( kind = KIND_Rsp ) y2

  bot = enormsq0_2d ( x1, y1, x2, y2 )

  if ( bot == 0._KIND_Rsp ) then

    xn = x1
    yn = y1
!
!  (P-P1) dot (P2-P1) = Norm(P-P1) * Norm(P2-P1) * Cos(Theta).
!
!  (P-P1) dot (P2-P1) / Norm(P-P1)**2 = normalized coordinate T
!  of the projection of (P-P1) onto (P2-P1).
!
  else

    dot = ( x - x1 ) * ( x2 - x1 ) + ( y - y1 ) * ( y2 - y1 )

    t = dot / bot

    xn = x1 + t * ( x2 - x1 )
    yn = y1 + t * ( y2 - y1 )

  end if

  dist = enorm0_2d ( xn, yn, x, y )

end subroutine


subroutine dmat_solve_Rsp ( a, n, nrhs, info )

!*******************************************************************************
!
!! DMAT_SOLVE USEs Gauss-Jordan elimination to solve an N by N linear system.
!
!  Modified:
!
!    08 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, real ( kind = KIND_Rsp ) A(N,N+NRHS), contains in rows and columns 1
!    to N the coefficient matrix, and in columns N+1 through
!    N+NRHS, the right hand sides.  On output, the coefficient matrix
!    area has been destroyed, while the right hand sides have
!    been overwritten with the corresponding solutions.
!
!    Input, integer NRHS, the number of right hand sides.  NRHS
!    must be at least 0.
!
!    Output, integer INFO, singularity flag.
!    0, the matrix was not singular, the solutions were computed;
!    J, factorization failed on step J, and the solutions could not
!    be computed.
!
  implicit none

  integer n
  integer nrhs

  real ( kind = KIND_Rsp ) a(n,n+nrhs)
  real ( kind = KIND_Rsp ) apivot
  real ( kind = KIND_Rsp ) factor
  integer i
  integer info
  integer ipivot
  integer j

  info = 0

  do j = 1, n
!
!  Choose a pivot row.
!
    ipivot = j
    apivot = a(j,j)

    do i = j+1, n
      if ( abs ( apivot ) < abs ( a(i,j) ) ) then
        apivot = a(i,j)
        ipivot = i
      end if
    end do

    if ( apivot == 0._KIND_Rsp ) then
      info = j
      return
    end if
!
!  Interchange.
!
    do i = 1, n + nrhs
      call d_swap ( a(ipivot,i), a(j,i) )
    end do
!
!  A(J,J) becomes 1.
!
    a(j,j) = 1._KIND_Rsp
    a(j,j+1:n+nrhs) = a(j,j+1:n+nrhs) / apivot
!
!  A(I,J) becomes 0.
!
    do i = 1, n

      if ( i /= j ) then

        factor = a(i,j)
        a(i,j) = 0._KIND_Rsp
        a(i,j+1:n+nrhs) = a(i,j+1:n+nrhs) - factor * a(j,j+1:n+nrhs)

      end if

    end do

  end do

end subroutine



subroutine d_swap_Rdp ( x, y )

!*******************************************************************************
!
!! D_SWAP swaps two real values.
!
!  Modified:
!
!    01 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, real ( kind = 8 ) X, Y.  On output, the values of X and
!    Y have been interchanged.
!
  implicit none

  real ( kind = KIND_Rdp ) x
  real ( kind = KIND_Rdp ) y
  real ( kind = KIND_Rdp ) z

  z = x
  x = y
  y = z

end subroutine


function enorm0_2d_Rdp ( x0, y0, x1, y1 ) RESULT(enorm0_2d)

!*******************************************************************************
!
!! ENORM0_2D computes the Euclidean norm of (P1-P0) in 2D.
!
!  Modified:
!
!    16 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = KIND_R ) X0, Y0, X1, Y1, the coordinates of the points P0 and P1.
!
!    Output, real ( kind = KIND_R ) ENORM0_2D, the Euclidean norm of (P1-P0).
!
  implicit none

  real ( kind = KIND_Rdp ) enorm0_2d
  real ( kind = KIND_Rdp ) x0
  real ( kind = KIND_Rdp ) x1
  real ( kind = KIND_Rdp ) y0
  real ( kind = KIND_Rdp ) y1

  enorm0_2d = sqrt ( ( x1 - x0 )**2 + ( y1 - y0 )**2 )

end function

function enormsq0_2d_Rdp ( x0, y0, x1, y1 ) RESULT(enormsq0_2d)

!*******************************************************************************
!
!! ENORMSQ0_2D computes the square of the Euclidean norm of (P1-P0) in 2D.
!
!  Modified:
!
!    06 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = KIND_R ) X0, Y0, X1, Y1, the coordinates of the points
!    P0 and P1.
!
!    Output, real ( kind = KIND_R ) ENORMSQ0_2D, the square of the Euclidean norm of (P1-P0).
!
  implicit none

  real ( kind = KIND_Rdp ) enormsq0_2d
  real ( kind = KIND_Rdp ) x0
  real ( kind = KIND_Rdp ) x1
  real ( kind = KIND_Rdp ) y0
  real ( kind = KIND_Rdp ) y1

  enormsq0_2d = ( x1 - x0 )**2 + ( y1 - y0 )**2

end function



subroutine poly_reorder_nodes_Rdp ( nxy, x, y, npoly, poly )

!*******************************************************************************
!
!! POLY_REORDER_NODES reorders nodes of a polygon so node 1 is leftest lowest.
!
!  Modified:
!
!    12 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer NXY, the number of nodes.
!
!    Input, real ( kind = KIND_R ) X(NXY), Y(NXY), the coordinates of the nodes.
!
!    Input, integer NPOLY, the number of nodes of the polygon.
!
!    Input/output, POLY(NPOLY), the indices of the nodes.
!
  implicit none

  integer npoly
  integer nxy

  integer i
  integer imin
  integer p
  integer pmin
  integer poly(npoly)
  integer poly2(npoly)
  real ( kind = KIND_Rdp ) x(nxy)
  real ( kind = KIND_Rdp ) y(nxy)

  imin = 1
  pmin = poly(imin)

  do i = 2, npoly
    p = poly(i)
    if ( &
      ( x(p) < x(pmin) ) .or. &
      ( x(p) == x(pmin) .and. y(p) < y(pmin) ) ) then
      imin = i
      pmin = p
    end if
  end do

  if ( imin /= 1 ) then

    poly2(1:npoly+1-imin) = poly(imin:npoly)
    poly2(npoly+2-imin:npoly) = poly(1:imin-1)

    poly(1:npoly) = poly2(1:npoly)

  end if

end subroutine

subroutine triangle_contains_point_2d_Rdp ( x1, y1, x2, y2, x3, y3, x, y, inside )

!*******************************************************************************
!
!! TRIANGLE_CONTAINS_POINT_2D finds if a point is inside a triangle in 2D.
!
!  Modified:
!
!    12 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = KIND_Rdp ) X1, Y1, X2, Y2, X3, Y3, the coordinates of
!    the corners of the triangle.
!
!    Input, real ( kind = KIND_Rdp ) X, Y, the point to be checked.
!
!    Output, logical INSIDE, is .TRUE. if (X,Y) is inside
!    the triangle or on its boundary, and .FALSE. otherwise.
!
  implicit none

  integer, parameter :: N = 2
  integer, parameter :: NRHS = 1

  real ( kind = KIND_Rdp ) a(N,N+NRHS)
  real ( kind = KIND_Rdp ) c1
  real ( kind = KIND_Rdp ) c2
  integer info
  logical inside
  real ( kind = KIND_Rdp ) x
  real ( kind = KIND_Rdp ) x1
  real ( kind = KIND_Rdp ) x2
  real ( kind = KIND_Rdp ) x3
  real ( kind = KIND_Rdp ) y
  real ( kind = KIND_Rdp ) y1
  real ( kind = KIND_Rdp ) y2
  real ( kind = KIND_Rdp ) y3
!
!  Set up the linear system
!
!    ( X2-X1  X3-X1 ) C1  = X-X1
!    ( Y2-Y1  Y3-Y1 ) C2    Y-Y1
!
!  which is satisfied by the barycentric coordinates of (X,Y).
!
  a(1,1) = x2 - x1
  a(1,2) = x3 - x1
  a(1,3) = x - x1

  a(2,1) = y2 - y1
  a(2,2) = y3 - y1
  a(2,3) = y - y1
!
!  Solve the linear system.
!
  call dmat_solve ( a, N, NRHS, info )

  if ( info /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'TRIANGLE_CONTAINS_POINT - Fatal error!'
    write ( *, * ) '  The linear system is singular.'
    write ( *, * ) '  The input data does not form a proper triangle.'
    stop
  end if

  c1 = a(1,3)
  c2 = a(2,3)
!
!  If the point is in the triangle, its barycentric coordinates
!  must both be nonnegative, and sum to no more than 1.
!
  if ( c1 < 0._KIND_Rdp .or. c2 < 0._KIND_Rdp ) then
    inside = .false.
  else if ( 1._KIND_Rdp < c1 + c2 ) then
    inside = .false.
  else
    inside = .true.
  end if

end subroutine

subroutine line_exp_point_dist_2d_Rdp ( x1, y1, x2, y2, x, y, dist )

!*******************************************************************************
!
!! LINE_EXP_POINT_DIST_2D: distance ( explicit line, point ) in 2D.
!
!  Formula:
!
!    The explicit form of a line in 2D is:
!
!      (X1,Y1), (X2,Y2).
!
!  Modified:
!
!    27 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = KIND_Rdp ) X1, Y1, X2, Y2.  (X1,Y1) and (X2,Y2) are two
!    points on the line.
!
!    Input, real ( kind = KIND_Rdp ) X, Y, the point whose distance from the line is
!    to be measured.
!
!    Output, real ( kind = KIND_Rdp ) DIST, the distance from the point to the line.
!
  implicit none

  real ( kind = KIND_Rdp ) bot
  real ( kind = KIND_Rdp ) dist
  real ( kind = KIND_Rdp ) dot
  real ( kind = KIND_Rdp ) t
  real ( kind = KIND_Rdp ) x
  real ( kind = KIND_Rdp ) xn
  real ( kind = KIND_Rdp ) x1
  real ( kind = KIND_Rdp ) x2
  real ( kind = KIND_Rdp ) y
  real ( kind = KIND_Rdp ) yn
  real ( kind = KIND_Rdp ) y1
  real ( kind = KIND_Rdp ) y2

  bot = enormsq0_2d ( x1, y1, x2, y2 )

  if ( bot == 0._KIND_Rdp ) then

    xn = x1
    yn = y1
!
!  (P-P1) dot (P2-P1) = Norm(P-P1) * Norm(P2-P1) * Cos(Theta).
!
!  (P-P1) dot (P2-P1) / Norm(P-P1)**2 = normalized coordinate T
!  of the projection of (P-P1) onto (P2-P1).
!
  else

    dot = ( x - x1 ) * ( x2 - x1 ) + ( y - y1 ) * ( y2 - y1 )

    t = dot / bot

    xn = x1 + t * ( x2 - x1 )
    yn = y1 + t * ( y2 - y1 )

  end if

  dist = enorm0_2d ( xn, yn, x, y )

end subroutine


subroutine dmat_solve_Rdp ( a, n, nrhs, info )

!*******************************************************************************
!
!! DMAT_SOLVE USEs Gauss-Jordan elimination to solve an N by N linear system.
!
!  Modified:
!
!    08 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, real ( kind = KIND_Rdp ) A(N,N+NRHS), contains in rows and columns 1
!    to N the coefficient matrix, and in columns N+1 through
!    N+NRHS, the right hand sides.  On output, the coefficient matrix
!    area has been destroyed, while the right hand sides have
!    been overwritten with the corresponding solutions.
!
!    Input, integer NRHS, the number of right hand sides.  NRHS
!    must be at least 0.
!
!    Output, integer INFO, singularity flag.
!    0, the matrix was not singular, the solutions were computed;
!    J, factorization failed on step J, and the solutions could not
!    be computed.
!
  implicit none

  integer n
  integer nrhs

  real ( kind = KIND_Rdp ) a(n,n+nrhs)
  real ( kind = KIND_Rdp ) apivot
  real ( kind = KIND_Rdp ) factor
  integer i
  integer info
  integer ipivot
  integer j

  info = 0

  do j = 1, n
!
!  Choose a pivot row.
!
    ipivot = j
    apivot = a(j,j)

    do i = j+1, n
      if ( abs ( apivot ) < abs ( a(i,j) ) ) then
        apivot = a(i,j)
        ipivot = i
      end if
    end do

    if ( apivot == 0._KIND_Rdp ) then
      info = j
      return
    end if
!
!  Interchange.
!
    do i = 1, n + nrhs
      call d_swap ( a(ipivot,i), a(j,i) )
    end do
!
!  A(J,J) becomes 1.
!
    a(j,j) = 1._KIND_Rdp
    a(j,j+1:n+nrhs) = a(j,j+1:n+nrhs) / apivot
!
!  A(I,J) becomes 0.
!
    do i = 1, n

      if ( i /= j ) then

        factor = a(i,j)
        a(i,j) = 0._KIND_Rdp
        a(i,j+1:n+nrhs) = a(i,j+1:n+nrhs) - factor * a(j,j+1:n+nrhs)

      end if

    end do

  end do

end subroutine


END MODULE
