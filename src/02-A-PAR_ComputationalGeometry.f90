!!# PARAMETERS MODULE <<PAR_ComputationalGeometry>>
MODULE PAR_ComputationalGeometry

!!## PURPOSE
!! This module provides the specification for computational
!! geometry functions and defines some constants.



!!##  DETAILS
!! The computational geometry functions provide both single and
!! double precision algorithms to manipulate geometric entities
!! contained in standard array containers.  There are two types
!! of ways to store the values which define a geometric entity:
!! in a multi-rank array (rank greater than 1) <StdVals> or
!! in a rank 1 array <PackVals>.
!!
!! In some cases, the rank 1 array specification results in
!! smaller storage requirements.  In some cases the <StdVals>
!! representation and <PackVals> representation are the same.



!!##  HISTORY
! 1.[waw] Author   = William A. Wieselquist
!         Modified = 105.2006
!         Contact  = william.wieselquist AT gmail.com



!!##  FUNCTION NAMES/ARGUMENTS
!! The functions available in this library take on a number of formats,
!! basically the more you specify in the function name, the
!! more you get speed-wise, but the more you need to type.  The things
!! every <CGE> function needs to know is:
!!
!!   * the coordinate system (e.g. rectangular,spherical,cylindrical)
!!   * the number of dimensions (e.g. 1,2,3)
!!   * the function name (e.g. INTERSECT,DIST,etc.)
!!   * the shape of the operands (e.g. Triange,Square,etc.)
!!
!! These specifications can be given as part of the function name
!! or as arguments to the function itself.
!!
!!
!!###  FUNCTION NAME SPECS
!! The basic format of a function name is
!! <coond|NAME|_shape(...)>.
!! Those specs are summarized below:
!!
!! * <coond>    the combined coordinate system
!!                and number of dimensions specifier
!! * <shape>    the shape specifier


!!##  FUNCTION ARGUMENT SPECS
!! Instead of specifying some aspects of the function you wish
!! to call in the name of the function, you can pass them as arguments:
!! <FUNCTION(COO,ND,SHAPE,...)>.  Those specs are summarized below.
!!
!!  <> COO   :: the coordinate system number
!!  <> ND    :: the dimension number
!!  <> SHAPE :: the shape number



!!##  FUNCTION ARGUMENTS
!! The main function arguments (<...>) are the values that describe
!! the geometric shapes.  They can be in one of two formats:
!!
!! * <PackVals> the packed format of numbers describing
!!              the shape (rank 1 array)
!! * <StdVals > the standard format of numbers describing
!!              the shape (may be same as the packed format
!!              for some shapes)
!!
!! Note that lower case means a specifier, and uppercase is a number.  Specifiers
!! become part of the functional names themselves.



!!##  6 different ways to do the same thing
!! The different combinations for a single function are given below.  In each
!! grouping, the functions are ordered fastest to slowest.
!!
!!###  Explicit <shape> and standard values <StdVals>
!! First, the FUNCTIONS that use the standard format (possibly multi-rank
!! arrays),
!!
!!  1.a.  < coondFUNCTION_shape(            StdVals ) >
!!
!!  1.b.  <      FUNCTION_shape( COO , ND , StdVals ) >
!!
!!
!!###  Explicit <shape> and packed values <PackVals>
!! Now the FUNCTIONS that use the packed format (always rank-1 arrays), notice
!! the only difference is a trailing underscore (to discourage using these versions!)
!!
!!  2.a.  < coondFUNCTION_shape_(            PackVals ) >
!!
!!  2.b.  <      FUNCTION_shape_( COO , ND , PackVals ) >
!!
!!
!!###  Passed <SHAPE> argument
!! Now finally, the functions which accept <SHAPE> numbers
!! instead of specifiying the shape in the FUNCTION name---note
!! only <PackVals> representation is allowed.
!!
!!  3.a.  < coondFUNCTION(            SHAPE , PackVals ) >
!!
!!  3.b.  <      FUNCTION( COO , ND , SHAPE , PackVals ) >



!!##  NAME SPECS
!! The allowed function name specifiers will now be discussed in detail.
!!
!!###  Specifier: <coond>
!!
!! * <x>   : rectangular coordinates in 1D (plane in 3D)
!! * <xy>  : rectangular coordinates in 2D (line in 3D)
!! * <xyz> : rectangular coordinates in 3D (point in 3D)
!!
!! * <r>   : spherical coordinates in 1D (spherical shell in 3D)
!! * <ra>  : spherical coordinates in 2D (half-circle arc in 3D)
!! * <rap> : spherical coordinates in 3D (point in 3D)
!!
!! * <d>   : cylindrical coordinates in 1D (infinite tube in 3D)
!! * <dz>  : cylindrical coordinates in 2D (circle in 3D)
!! * <dza> : cylindrical coordinates in 3D (point in 3D)
!!
!!
!!###  Specifier: <coo>
!!
!! * <rec> : rectangular coordinates
!! * <sph> : spherical coordinates
!! * <cyl> : cylindrical coordinates
!!
!!
!!###  Specifier: <nd>
!!
!! * <nd1> : one dimension
!! * <nd2> : two dimensions
!! * <nd3> : three dimensions
!!
!!
!!###  Specifier: <shape>
!! * <P >  : point
!! * <V >  : vector
!! * <U >  : unit vector
!! * <Pn>  : plane
!! * <Ln>  : line
!! * <Ry>  : ray
!! * <Ls>  : line segment
!! * <Tr>  : triangle
!! * <Qs>  : square
!! * <Qr>  : rectangle
!! * <Qg>  : general quadrilateral
!! * <Pg>  : polygon
!! * <El>  : ellipse
!! * <Td>  : tetrahedron
!! * <Hd>  : hexahedron
!! * <Hb>  : cube
!! * <Hx>  : box (rectangular prism)
!! * <Pd>  : polyhedron
!!


!!##  ARGUMENT SPECS
!! The allowed function argument specifiers will now be discussed in detail.

!!###  Argument <COO>
!! Prepend a <CGE_> to the coordinate system specifier <coo> to
!! retrieve the <COO> number from this module
INTEGER,PARAMETER :: CGE_rec = 1
INTEGER,PARAMETER :: CGE_sph = 2
INTEGER,PARAMETER :: CGE_cyl = 3
!!
!!###  Argument <ND>
!! These are just 1,2, or 3, but you can use
!! <CGE_nd1,CGE_nd2,CGE_nd3> if you want...
INTEGER,PARAMETER :: CGE_nd1 = 1
INTEGER,PARAMETER :: CGE_nd2 = 2
INTEGER,PARAMETER :: CGE_nd3 = 3
!!
!!###  Argument <SHAPE>
!! prepend a <CGE_> to the shape specifier <shape> to retrieve
!! the <SHAPE> number from this module
!!   * primaries
INTEGER,PARAMETER :: CGE_S  = 0    !scalar
INTEGER,PARAMETER :: CGE_V  = 1    !vector
!!   * 0D shapes
INTEGER,PARAMETER :: CGE_P  = 2    !point
INTEGER,PARAMETER :: CGE_U  = 3    !direction
!!   * 1D shapes
INTEGER,PARAMETER :: CGE_Ln = 4    !line
INTEGER,PARAMETER :: CGE_Ry = 5    !ray
INTEGER,PARAMETER :: CGE_Ls = 6    !line segment
!!   * 2D shapes
INTEGER,PARAMETER :: CGE_Pn = 7    !plane
INTEGER,PARAMETER :: CGE_Tg = 8    !general triangle
INTEGER,PARAMETER :: CGE_Qg = 9    !general quadrilateral
INTEGER,PARAMETER :: CGE_Qs = 10   !square
INTEGER,PARAMETER :: CGE_Qr = 11   !rectangle
INTEGER,PARAMETER :: CGE_Pg = 12   !polygon
INTEGER,PARAMETER :: CGE_El = 13   !ellipse
INTEGER,PARAMETER :: CGE_Ci = 14   !circle
!!   * 3D shapes
INTEGER,PARAMETER :: CGE_Td = 15   !tetrahedron
INTEGER,PARAMETER :: CGE_Hd = 16   !hexahedron
INTEGER,PARAMETER :: CGE_Hs = 17   !cube
INTEGER,PARAMETER :: CGE_Hr = 17   !rectangular prism
INTEGER,PARAMETER :: CGE_Pd = 18   !polyhedron


!!## ALGORITHM SPECIFICATION II: operands
!! To differentiate an algorithm for the intersection of two lines and the
!! intersection of a line and a plane, for example, the main operands are
!! appended to the operator name for any call to the toolkit functions.
!
!   ### UNIT VECTOR (U) operands
!     * cartesian coordinates
!       (/U1/)   : $R^1$ unit vector, U1  = V1  / ||V1||
!       (/U2/)   : $R^2$ unit vector, U2  = V2  / ||V2||
!       (/U3/)   : $R^3$ unit vector, U3  = V3  / ||V3||
!     * spherical coordinates
!       (/Us2/)  : $R^2$ unit vector, Us2 = Vs2 / ||Vs2||
!       (/Us3/)  : $R^3$ unit vector, Us3 = Vs3 / ||Vs3||
!     * cylidnrical coordinates
!       (/Uc2/)  : $R^2$ unit vector, Uc2 = Vc2 / ||Uc2||
!       (/Uc3/)  : $R^3$ unit vector, Uc3 = Vc3 / ||Uc3||
!
!   ### POINT (P) operands
!     * cartesian coordinates
!       (/P1/)   : $R^1$ point, P1  = (/x/)
!       (/P2/)   : $R^2$ point, P2  = (/x,y/)
!       (/P3/)   : $R^3$ point, P3  = (/x,y,z/)
!     * spherical coordinates
!       (/Ps2/)  : $R^2$ point, Ps2 = (/r,p/)
!       (/Ps3/)  : $R^3$ point, Ps3 = (/r,p,a/)
!     * cylidnrical coordinates
!       (/Pc2/)  : $R^2$ point, Pc2 = (/r,h/)
!       (/Pc3/)  : $R^3$ point, Pc3 = (/r,h,p/)
!
!   ### CHAIN (Cn) operands
!     (/N,Cn/) : nearest neighbor connection of N points
!     * cartesian coordinates
!       (/Cn1/)   : $R^1$ points, Cn1  = (/P1_a,P1_b,...,P1_N/)
!       (/Cn2/)   : $R^2$ points, Cn2  = (/P2_a,P2_b,...,P2_N/)
!       (/Cn3/)   : $R^3$ points, Cn3  = (/P3_a,P3_b,...,P3_N/)
!
!   ### MATRIX operands
!     (/Mrc/)  : where r and c are numbers of rows and columns, respectively
!
!   ### LINE (Ln) operands
!     Lines are described with a base point and a direction vector.
!     (/Ln2/)  : $R^2$ line, Ln2 = (/ P2_base , U2_direction /)
!     (/Ln3/)  : $R^3$ line, Ln3 = (/ P3_base , U3_direction /)
!
!   ### LINE SEGMENT (Ls) operands
!     * rectangular coordinates <RECTANGULAR>
!       * $R^1$ line segment < Ls=(/P_a,P_b/), P_a=(/x_a/), P_b=(/x_b/) >
!       * $R^2$ line segment < Ls=(/P_a,P_b/), P_a=(/x_a,y_a/), P_b=(/x_b,y_b/) >
!       * $R^3$ line segment < Ls=(/P_a,P_b/), P_a=(/x_a,y_a,z_a/), P_b=(/x_b,y_b,z_b/) >
!
!   ### RAY (Ry) operands
!     Rays are described with a base point and a direction vector.
!     (/Ry2/) : $R^2$ ray, Ry2 = (/ P2_base , U2_direction /)
!     (/Ry3/) : $R^3$ ray, Ry3 = (/ P3_base , U3_direction /)
!
!   ### PLANE (Pn) operands
!     Planes are described with a unit vector normal to the plane and a constant.
!     The form is the Hessian normal form for the equation of a plane,
!     A(x-x0)+B(y-y0)+C(z-z0)+D=0.
!     (/Pn2/) : Pn2 = (/ U2_normal , s /)
!     (/Pn3/) : Pn3 = (/ U3_normal , s /)
!
!   ### POLYGON (Pg) operands
!     A polygon is described by the number of vertices (N) and a counterclockwise
!     list of its vertices.
!     (/N,Pg2/) : $R^2$ polygon, Pgn2 = (/ P2_1 , P2_2 , ... , P2_N /)
!     (/N,Pg3/) : $R^3$ polygon, Pgn3 = (/ P3_1 , P3_2 , ... , P3_N /)
!
!   ### TRIANGLE (Tr) operands
!     A triangle is described by 3 points.  In some triangle functions, the number of
!     vertices for a triangle is defined, N_=3, in order to call a
!     function for a general polygon.
!     (/Tr2/) : $R^2$ triangle, Tr2 = (/ P2_1 , P2_2 , P2_3 /)
!     (/Tr3/) : $R^3$ triangle, Tr3 = (/ P3_1 , P3_2 , P3_3 /)
!
!   #QUADRILATERAL (Qd) operands
!     A quadrilateral is described by 4 points.  In some quadrilateral functions, the number of
!     vertices for a quadrilateral is defined, N_=4, in order to call a function for a general polygon.
!     (/Qd2/) : $R^2$ quadrilateral, Qud2 = (/ P2_1 , P2_2 , P2_3 , P2_4 /)
!     (/Qd3/) : $R^3$ quadrilateral, Qud3 = (/ P3_1 , P3_2 , P3_3 , P2_4 /)
!
!   #SPHERE (Sphr) operands
!     A sphere is described by the coordinates of the center and the radius squared.  The
!     radius squared is used because it is more useful for intersection checking than just
!     the radius.
!     (/Sphr3/) : sphere, Sphr3 = (/ P3_center , rsqr /)
!
!   #SHOE BOX (Shbx) operands
!     A shoe box is described by the coordinates of the center, the polar (z-axis) and
!     azimuthal angles (xy-axes), and the span in each dimension.
!     (/Shbx3/) : $R^3$ shoe box, Shbx3 = (/ P3_center , p , a , s_x , s_y , s_z /)
!
!   #CUBE (Cube) operands
!     A cube is described by the coordinates of the center, the polar (z-axis) and
!     azimuthal angles (xy-axes), and the span in all dimensions.
!     (/Cube3/) : $R^3$ cube, Cube3 = (/ P3_center , p , a , s /)



!!###  SHAPE FUNCTION
!!   The shape "FUNCTION" is provided to allocate arrays of the
!!   appropriate dimension to hold shapes.  Obviously, it is
!!   not actually a function, but merely a parameter array.
!!
!!   * VECTOR <V> operands
!!     * rectangular coordinates <REC>
!!       * $R^1$ vector < V=(/x/) >
INTEGER,PARAMETER,DIMENSION(1) ::   xSHAPE_V = (/1/)
!!       * $R^2$ vector < V=(/x,y/) >
INTEGER,PARAMETER,DIMENSION(1) ::  xySHAPE_V = (/2/)
!!       * $R^3$ vector < V=(/x,y,z/) >
INTEGER,PARAMETER,DIMENSION(1) :: xyzSHAPE_V = (/3/)
!!
!!     * spherical coordinates <SPH>
!!       * $R^1$ vector < V=(/r/) >
INTEGER,PARAMETER,DIMENSION(1) ::   rSHAPE_V = (/1/)
!!       * $R^2$ vector < V=(/r,a/) >
INTEGER,PARAMETER,DIMENSION(1) ::  rpSHAPE_V = (/2/)
!!       * $R^3$ vector < V=(/r,a,p/) >
INTEGER,PARAMETER,DIMENSION(1) :: rpaSHAPE_V = (/3/)
!!
!!     * cylindrical coordinates <CYL>
!!       * $R^1$ vector < V = (/d/) >
INTEGER,PARAMETER,DIMENSION(1) ::   dSHAPE_V = (/1/)
!!       * $R^2$ vector < V = (/d,z/) >
INTEGER,PARAMETER,DIMENSION(1) ::  dzSHAPE_V = (/2/)
!!       * $R^3$ vector < V = (/d,z,a/) >
INTEGER,PARAMETER,DIMENSION(1) :: dzaSHAPE_V = (/3/)
!!
!!   * LINESEGMENT <Ls> operands
!!     * rectangular coordinates <REC>
INTEGER,PARAMETER,DIMENSION(2) ::   xSHAPE_Ls = (/1,2/)
INTEGER,PARAMETER,DIMENSION(2) ::  xySHAPE_Ls = (/2,2/)
INTEGER,PARAMETER,DIMENSION(2) :: xyzSHAPE_Ls = (/3,2/)
!!
!!   * POLYGON <Pg> operands (up to 200 points)
!!     * rectangular coordinates <REC>
INTEGER,PARAMETER,DIMENSION(2,200) :: xySHAPE_Pg =RESHAPE((/ &
  2,  1,  2,  2,  2,  3,  2,  4,  2,  5,  2,  6,  2,  7,  2,  8,  2,  9,  2, 10,  2, 11,&
  2, 12,  2, 13,  2, 14,  2, 15,  2, 16,  2, 17,  2, 18,  2, 19,  2, 20,  2, 21,  2, 22,&
  2, 23,  2, 24,  2, 25,  2, 26,  2, 27,  2, 28,  2, 29,  2, 30,  2, 31,  2, 32,  2, 33,&
  2, 34,  2, 35,  2, 36,  2, 37,  2, 38,  2, 39,  2, 40,  2, 41,  2, 42,  2, 43,  2, 44,&
  2, 45,  2, 46,  2, 47,  2, 48,  2, 49,  2, 50,  2, 51,  2, 52,  2, 53,  2, 54,  2, 55,&
  2, 56,  2, 57,  2, 58,  2, 59,  2, 60,  2, 61,  2, 62,  2, 63,  2, 64,  2, 65,  2, 66,&
  2, 67,  2, 68,  2, 69,  2, 70,  2, 71,  2, 72,  2, 73,  2, 74,  2, 75,  2, 76,  2, 77,&
  2, 78,  2, 79,  2, 80,  2, 81,  2, 82,  2, 83,  2, 84,  2, 85,  2, 86,  2, 87,  2, 88,&
  2, 89,  2, 90,  2, 91,  2, 92,  2, 93,  2, 94,  2, 95,  2, 96,  2, 97,  2, 98,  2, 99,&
  2,100,  2,101,  2,102,  2,103,  2,104,  2,105,  2,106,  2,107,  2,108,  2,109,  2,110,&
  2,111,  2,112,  2,113,  2,114,  2,115,  2,116,  2,117,  2,118,  2,119,  2,120,  2,121,&
  2,122,  2,123,  2,124,  2,125,  2,126,  2,127,  2,128,  2,129,  2,130,  2,131,  2,132,&
  2,133,  2,134,  2,135,  2,136,  2,137,  2,138,  2,139,  2,140,  2,141,  2,142,  2,143,&
  2,144,  2,145,  2,146,  2,147,  2,148,  2,149,  2,150,  2,151,  2,152,  2,153,  2,154,&
  2,155,  2,156,  2,157,  2,158,  2,159,  2,160,  2,161,  2,162,  2,163,  2,164,  2,165,&
  2,166,  2,167,  2,168,  2,169,  2,170,  2,171,  2,172,  2,173,  2,174,  2,175,  2,176,&
  2,177,  2,178,  2,179,  2,180,  2,181,  2,182,  2,183,  2,184,  2,185,  2,186,  2,187,&
  2,188,  2,189,  2,190,  2,191,  2,192,  2,193,  2,194,  2,195,  2,196,  2,197,  2,198,&
  2,199,  2,200 /),(/2,200/))

!!   * PLANE <Pn> operands
INTEGER,PARAMETER,DIMENSION(1)  :: xySHAPE_Pn   = (/ 3 /)
INTEGER,PARAMETER,DIMENSION(1)  :: xyzSHAPE_Pn  = (/ 4 /)
!!   * POINT <P> operands
INTEGER,PARAMETER,DIMENSION(1)  :: xySHAPE_P    = (/ 2 /)
!!   * GENERAL QUADRILATERAL <Pg> operands
INTEGER,PARAMETER,DIMENSION(2)  :: xySHAPE_Qg   = (/ 2 , 4 /)
!!   * RECTANGLE <Qr> operands
INTEGER,PARAMETER,DIMENSION(2)  :: xySHAPE_Qr   = (/ 2 , 4 /)
!!   * SQUARE <Qs> operands
INTEGER,PARAMETER,DIMENSION(2)  :: xySHAPE_Qs   = (/ 2 , 4 /)
!!   * TRIANGLE <Tr> operands
INTEGER,PARAMETER,DIMENSION(2)  :: xySHAPE_Tr   = (/ 2 , 3 /)


CHARACTER(*),PARAMETER :: KEY_FaceShape(1) = (/"PlaneSegment"/)
INTEGER     ,PARAMETER :: Ps_ = 1

CHARACTER(*),PARAMETER :: KEY_EdgeShape(1) = (/"Straight"/)
INTEGER     ,PARAMETER :: Straight_ = 1

!!## EXTERNAL PARAMETERS
!! * 2D explicit cell shapes
!!   * triangle <Tr_>
INTEGER,PARAMETER :: Tr_=01
!!   * square <Qs_>
!!   * rectangle <Qr_>
!!   * general quadrilateral <Qg_>
INTEGER,PARAMETER :: Qs_=02
INTEGER,PARAMETER :: Qr_=03
INTEGER,PARAMETER :: Qg_=04
!!   * polygon <Pg_>
INTEGER,PARAMETER :: Pg_=05
!! * 3D explicit cell shapes
!!   * tetrahedron <Te_>
INTEGER,PARAMETER :: Te_=11
!!   * cube <Qc_>
!!   * rectangular prism/parallel-piped <Qp_>
!!   * general box formed from 6 planes <Qh_>
INTEGER,PARAMETER :: Qc_=12
INTEGER,PARAMETER :: Qp_=13
INTEGER,PARAMETER :: Qh_=14
!! * cell name keys <KEY_CellShape>
CHARACTER(*),PARAMETER :: KEY_CellShape(1:14) = (/"Triangle     ",&
                                                  "Square       ",&
                                                  "Rectangle    ",&
                                                  "Quadrilateral",&
                                                  "Polygon      ",&
                                                  "             ",&
                                                  "             ",&
                                                  "             ",&
                                                  "             ",&
                                                  "             ",&
                                                  "Tetrahedron  ",&
                                                  "Cube         ",&
                                                  "RecPrism     ",&
                                                  "Box          "/)

END MODULE
