!!# LIBRARY MODULE: <<LIB_BiVar>>
MODULE LIB_BiVar

!!## PURPOSE
!! Contains a library of routines to
!! interpolate irregular X,Y data via bivariate polynomial
!! construction.



!!## EXTERNAL KINDS
USE KND_IntrinsicTypes,ONLY: KIND_Rdp !!((01-A-KND_IntrinsicTypes.f90))



!!## HISTORY
! 1.[waw] Reorganizer = William A. Wieselquist
!         Modified    = 105.2006
!         Contact     = william.wieselquist AT gmail.com



!!## DEFAULT IMPLICIT
IMPLICIT NONE



!!## DFEAULT ACCESS
PRIVATE



!!## PUBLIC ACCESS LIST
PUBLIC :: idbvip,idsfft



!!## LOCAL VARIABLES
INTEGER                 :: itpv,itipv,ntsc(9)
REAL( kind = KIND_Rdp ) :: xs1,xs2,ys1,ys2,&
  x0,y0,ap,bp,cp,dp,&
  p00,p10,p20,p30,p40,p50,p01,p11,p21,p31,p41, &
  p02,p12,p22,p32,p03,p13,p23,p04,p14,p05



!!## MODULE PROCEDURES
CONTAINS



subroutine idbvip ( md, ndp, xd, yd, zd, nip, xi, yi, zi , &
  errint , errmsg )

!*******************************************************************************
!
!! IDBVIP performs bivariate interpolation of irregular X, Y data.
!
!  Discussion:
!
!    The data points must be distinct and their projections in the
!    X-Y plane must not be collinear, otherwise an error return
!    occurs.
!
!  Purpose:
!
!    To provide bivariate interpolation and smooth surface fitting for
!    values given at irregularly distributed points.
!
!    The resulting interpolating function and its first-order partial
!    derivatives are continuous.
!
!    The method employed is local, i.e. a change in the data in one area
!    of the plane does not affect the interpolating function except in
!    that local area.  This is advantageous over global interpolation
!    methods.
!
!    Also, the method gives exact results when all points lie in a plane.
!    This is advantageous over other methods such as two-dimensional
!    Fourier series interpolation.
!
!  Usage:
!
!    This package contains two USEr entries, IDBVIP and IDSFFT, both
!    requiring input data to be given at points
!      ( X(I), Y(I) ), I = 1,...,N.
!
!    If the USEr desires the interpolated data to be output at grid
!    points, i.e. at points
!      ( XI(I), YI(J) ), I = 1,...,NXI, J=1,...,NYI,
!    then IDSFFT should be used.  This is USEful for generating an
!    interpolating surface.
!
!    The other USEr entry point, IDBVIP, will produce interpolated
!    values at scattered points
!      ( XI(I), YI(I) ), i = 1,...,NIP.
!    This is USEful for filling in missing data points on a grid.
!
!  History:
!
!    The original version of BIVAR was written by Hiroshi Akima in
!    August 1975 and rewritten by him in late 1976.  It was incorporated
!    into NCAR's public software libraries in January 1977.  In August
!    1984 a new version of BIVAR, incorporating changes described in the
!    Rocky Mountain Journal of Mathematics article cited below, was
!    obtained from Dr Akima by Michael Pernice of NCAR's Scientific
!    Computing Division, who evaluated it and made it available in February,
!    1985.
!
!  Accuracy:
!
!    Accurate to machine precision on the input data points.  Accuracy at
!    other points greatly depends on the input data.
!
!  Modified:
!
!    23 January 2003
!
!  References:
!
!    Hiroshi Akima,
!    Algorithm 526,
!    A Method of Bivariate Interpolation and Smooth Surface Fitting
!      for Values Given at Irregularly Distributed Points,
!    ACM Transactions on Mathematical Software,
!    Volume 4, Number 2, June 1978.
!
!    Hiroshi Akima,
!    On Estimating Partial Derivatives for Bivariate Interpolation
!      of Scattered Data,
!    Rocky Mountain Journal of Mathematics,
!    Volume 14, Number 1, Winter 1984.
!
!  Method:
!
!    The XY plane is divided into triangular cells, each cell having
!    projections of three data points in the plane as its vertices, and
!    a bivariate quintic polynomial in X and Y is fitted to each
!    triangular cell.
!
!    The coefficients in the fitted quintic polynomials are determined
!    by continuity requirements and by estimates of partial derivatives
!    at the vertices and along the edges of the triangles.  The method
!    described in the rocky mountain journal reference guarantees that
!    the generated surface depends continuously on the triangulation.
!
!    The resulting interpolating function is invariant under the following
!    types of linear coordinate transformations:
!      1) a rotation of the XY coordinate system
!      2) linear scale transformation of the Z axis
!      3) tilting of the XY plane, i.e. new coordinates (u,v,w) given by
!           u = x
!           v = y
!           w = z + a*x + b*y
!         where a, b are arbitrary constants.
!
!    complete details of the method are given in the reference publications.
!
!  Parameters:
!
!    Input, integer MD, mode of computation.   MD must be 1,
!    2, or 3, else an error return occurs.
!
!    1: if this is the first call to this subroutine, or if the
!    value of NDP has been changed from the previous call, or
!    if the contents of the XD or YD arrays have been changed
!    from the previous call.
!
!    2: if the values of NDP and the XD and YD arrays are unchanged
!    from the previous call, but new values for XI, YI are being
!    used.  If MD = 2 and NDP has been changed since the previous
!    call to IDBVIP, an error return occurs.
!
!    3: if the values of NDP, NIP, XD, YD, XI, YI are unchanged from
!    the previous call, i.e. if the only change on input to IDBVIP is
!    in the ZD array.  If MD = 3 and NDP or NIP has been changed since
!    the previous call to IDBVIP, an error return occurs.
!
!    Between the call with MD = 2 or MD = 3 and the preceding call, the
!    IWK and WK work arrays should not be disturbed.
!
!    Input, integer NDP, the number of data points (must be 4 or
!    greater, else an error return occurs).
!
!    Input, real ( kind = KIND_Rdp ) XD(NDP), Y(NDP), the X and Y coordinates
!    of the data points.
!
!    Input, real ( kind = KIND_Rdp ) ZD(NDP), the data values at the data points.
!
!    Input, integer NIP, the number of output points at which
!    interpolation is to be performed (must be 1 or greater, else an
!    error return occurs).
!
!    Input, real ( kind = KIND_Rdp ) XI(NIP), YI(NIP), the coordinates of the
!    points at which interpolation is to be performed.
!
!    Output, real ( kind = KIND_Rdp ) ZI(NIP), the interpolated data values.
!
!  Local parameters:
!
!    Workspace, integer IWK(31*NDP+NIP).
!
!    Workspace, real ( kind = KIND_Rdp ) WK(8*NDP).
!
  implicit none

  integer ndp
  integer nip
  integer errint
  character ( len = * ) errmsg
  optional errint,errmsg
  integer iip
  integer iwk(31*ndp + nip)
  integer jwipl
  integer jwipt
  integer jwit
  integer jwit0
  integer jwiwk
  integer jwiwl
  integer jwiwp
  integer jwwpd
  integer md
  integer nl
  integer nt
  real ( kind = KIND_Rdp ) wk(8*ndp)
  real ( kind = KIND_Rdp ) xd(ndp)
  real ( kind = KIND_Rdp ) xi(nip)
  real ( kind = KIND_Rdp ) yd(ndp)
  real ( kind = KIND_Rdp ) yi(nip)
  real ( kind = KIND_Rdp ) zd(ndp)
  real ( kind = KIND_Rdp ) zi(nip)

!
!  Error check.
!
  IF( PRESENT(errmsg) )errmsg = ""
  IF( PRESENT(errint) )errint = 0
  if ( md < 1 .or. 3 < md ) then
    if( present(errint) )errint = -11
    if( present(errmsg) )then
      write (errmsg, '(a)' )'Input parameter MD out of range. [LIB_BiVar:IDBVIP]'
    end if
    return
  end if

  if ( ndp < 4 ) then
    if( present(errint) )errint = -12
    if( present(errmsg) )then
     write (errmsg, '(a)' ) 'Input parameter NDP out of range. [LIB_BiVar:IDBVIP]'
    end if
    return
  end if

  if ( nip < 1 ) then
    if( present(errint) )errint = -13
    if( present(errmsg) )then
     write (errmsg, '(a)' ) 'Input parameter NIP out of range. [LIB_BiVar:IDBVIP]'
    end if
    return
  end if

  if ( md == 1 ) then
    iwk(1) = ndp
  else
    if ( ndp /= iwk(1) ) then
      if( present(errint) )errint = -14
      if( present(errmsg) )then
       write (errmsg, '(a)' ) 'MD = 2 or 3 but NDP was changed since last call. [LIB_BiVar:IDBVIP]'
      end if
      return
    end if
  end if

  if ( md <= 2 ) then
    iwk(3) = nip
  else
    if ( nip < iwk(3) ) then
      if( present(errint) )errint = -15
      if( present(errmsg) )then
       write (errmsg, '(a)' ) 'MD = 3 but NIP was changed since last call. [LIB_BiVar:IDBVIP]'
      end if
      return
    end if
  end if
!
!  Allocation of storage areas in the IWK array.
!
  jwipt = 16
  jwiwl = 6*ndp+1
  jwiwk = jwiwl
  jwipl = 24*ndp+1
  jwiwp = 30*ndp+1
  jwit0 = 31*ndp
  jwwpd = 5*ndp+1
!
!  Triangulate the XY plane.
!
  if ( md == 1 ) then

    call idtang ( ndp, xd, yd, nt, iwk(jwipt), nl, iwk(jwipl), &
      iwk(jwiwl), iwk(jwiwp), wk )

    iwk(5) = nt
    iwk(6) = nl

    if ( nt == 0 ) then
      return
    end if

  else

    nt = iwk(5)
    nl = iwk(6)

  end if
!
!  Locate all points at which interpolation is to be performed.
!
  if ( md <= 2 ) then

    itipv = 0
    jwit = jwit0

    do iip = 1, nip

      jwit = jwit+1

      call idlctn ( ndp, xd, yd, nt, iwk(jwipt), nl, iwk(jwipl), &
        xi(iip), yi(iip), iwk(jwit) )

    end do

  end if
!
!  Estimate the partial derivatives at all data points.
!
  call idpdrv ( ndp, xd, yd, zd, nt, iwk(jwipt), wk, wk(jwwpd) )
!
!  Interpolate the ZI values.
!
  itpv = 0
  jwit = jwit0

  do iip = 1, nip

    jwit = jwit + 1

    call idptip ( ndp, xd, yd, zd, nt, iwk(jwipt), nl, iwk(jwipl), wk, &
      iwk(jwit), xi(iip), yi(iip), zi(iip) )

  end do

  return
end subroutine

subroutine idgrid ( xd, yd, nt, ipt, nl, ipl, nxi, nyi, xi, yi, ngp, igp )

!*******************************************************************************
!
!! IDGRID organizes grid points for surface fitting.
!
!  Discussion:
!
!    IDGRID sorts the points in ascending order of triangle numbers and
!    of the border line segment number.
!
!  Reference:
!
!    Hiroshi Akima,
!    Algorithm 526,
!    A Method of Bivariate Interpolation and Smooth Surface Fitting
!    for Values Given at Irregularly Distributed Points,
!    ACM Transactions on Mathematical Software,
!    Volume 4, Number 2, June 1978.
!
!  Parameters:
!
!    Input, real ( kind = KIND_Rdp ) XD(NDP), YD(NDP), the X and Y coordinates of
!    the data points.
!
!    Input, integer NT, the number of triangles.
!
!    Input, integer IPT(3*NT), the indices of the triangle vertexes.
!
!    Input, integer NL, the number of border line segments.
!
!    Input, integer IPL(3*NL), containing the point numbers of the end points
!    of the border line segments and their respective triangle numbers,
!
!    Input, integer NXI, NYI, the number of grid points in the X and Y
!    coordinates.
!
!    Input, real ( kind = KIND_Rdp ) XI(NXI), YI(NYI), the coordinates of the
!    grid points.
!
!    Output, integer NGP(2*(NT+2*NL)) where the
!    number of grid points that belong to each of the
!    triangles or of the border line segments are to be stored.
!
!    Output, integer IGP(NXI*NYI), where the grid point numbers are to be
!    stored in ascending order of the triangle number and the border line
!    segment number.
!
  implicit none

  integer nl
  integer nt
  integer nxi
  integer nyi

  integer igp(nxi*nyi)
  integer il0
  integer il0t3
  integer ilp1
  integer ilp1t3
  integer insd
  integer ip1
  integer ip2
  integer ip3
  integer ipl(3*nl)
  integer ipt(3*nt)
  integer it0
  integer it0t3
  integer ixi
  integer iximn
  integer iximx
  integer iyi
  integer izi
  integer jigp0
  integer jigp1
  integer jigp1i
  integer jngp0
  integer jngp1
  integer l
  integer ngp(2*(nt+2*nl))
  integer ngp0
  integer ngp1
  integer nl0
  integer nt0
  integer nxinyi
  real ( kind = KIND_Rdp ) u1
  real ( kind = KIND_Rdp ) u2
  real ( kind = KIND_Rdp ) u3
  real ( kind = KIND_Rdp ) v1
  real ( kind = KIND_Rdp ) v2
  real ( kind = KIND_Rdp ) v3
  real ( kind = KIND_Rdp ) x1
  real ( kind = KIND_Rdp ) x2
  real ( kind = KIND_Rdp ) x3
  real ( kind = KIND_Rdp ) xd(*)
  real ( kind = KIND_Rdp ) xi(nxi)
  real ( kind = KIND_Rdp ) xii
  real ( kind = KIND_Rdp ) ximn
  real ( kind = KIND_Rdp ) ximx
  real ( kind = KIND_Rdp ) xmn
  real ( kind = KIND_Rdp ) xmx
  real ( kind = KIND_Rdp ) y1
  real ( kind = KIND_Rdp ) y2
  real ( kind = KIND_Rdp ) y3
  real ( kind = KIND_Rdp ) yd(*)
  real ( kind = KIND_Rdp ) yi(nyi)
  real ( kind = KIND_Rdp ) yii
  real ( kind = KIND_Rdp ) yimn
  real ( kind = KIND_Rdp ) yimx
  real ( kind = KIND_Rdp ) ymn
  real ( kind = KIND_Rdp ) ymx
!
!  Preliminary processing
!
  nt0 = nt
  nl0 = nl
  nxinyi = nxi * nyi
  ximn = min ( xi(1), xi(nxi) )
  ximx = max ( xi(1), xi(nxi) )
  yimn = min ( yi(1), yi(nyi) )
  yimx = max ( yi(1), yi(nyi) )
!
!  Determine grid points inside the data area.
!
  jngp0 = 0
  jngp1 = 2 * ( nt0 + 2 * nl0 ) + 1
  jigp0 = 0
  jigp1 = nxinyi + 1

  do it0 = 1, nt0

    ngp0 = 0
    ngp1 = 0
    it0t3 = it0 * 3
    ip1 = ipt(it0t3-2)
    ip2 = ipt(it0t3-1)
    ip3 = ipt(it0t3)
    x1 = xd(ip1)
    y1 = yd(ip1)
    x2 = xd(ip2)
    y2 = yd(ip2)
    x3 = xd(ip3)
    y3 = yd(ip3)
    xmn = min ( x1, x2, x3 )
    xmx = max ( x1, x2, x3 )
    ymn = min ( y1, y2, y3 )
    ymx = max ( y1, y2, y3 )
    insd = 0

    do ixi = 1, nxi

      if ( xi(ixi) < xmn .or. xmx < xi(ixi) ) then
        if ( insd == 0 ) then
          cycle
        end if
        iximx = ixi - 1
        go to 23
      end if

      if ( insd /= 1 ) then
        insd = 1
        iximn = ixi
      end if

    end do

    if ( insd == 0 ) then
      go to 38
    end if

    iximx = nxi

23  continue

    do iyi = 1, nyi

      yii = yi(iyi)

      if ( yii < ymn .or. yii > ymx ) then
        go to 37
      end if

      do ixi = iximn, iximx

        xii = xi(ixi)
        l = 0
        if ( vpdt(x1,y1,x2,y2,xii,yii)<0._KIND_Rdp ) then
         goto 36
        else if( vpdt(x1,y1,x2,y2,xii,yii)==0._KIND_Rdp ) then
         goto 25
        else
         goto 26
        end if

25      continue

        l = 1
26      continue
        if ( vpdt ( x2,y2,x3,y3,xii,yii )<0._KIND_Rdp ) then
         goto 36
        else if( vpdt ( x2,y2,x3,y3,xii,yii )==0._KIND_Rdp ) then
         goto 27
        else
         goto 28
        end if

27      continue
        l = 1
28      continue
        if ( vpdt ( x3,y3,x1,y1,xii,yii)<0._KIND_Rdp ) then
         goto 36
        else if( vpdt ( x3,y3,x1,y1,xii,yii)==0._KIND_Rdp ) then
         goto 29
        else
         goto 30
        end if

29      continue
        l = 1
30      continue
        izi = nxi * ( iyi - 1 ) + ixi

        if ( l == 1 ) go to 31

        ngp0 = ngp0 + 1
        jigp0 = jigp0 + 1
        igp(jigp0) = izi
        go to 36

31      continue

        do jigp1i = jigp1, nxinyi
          if ( izi == igp(jigp1i) ) then
            go to 36
          end if
        end do

        ngp1 = ngp1 + 1
        jigp1 = jigp1 - 1
        igp(jigp1) = izi

36      continue

      end do

37    continue

    end do

38  continue

    jngp0 = jngp0 + 1
    ngp(jngp0) = ngp0
    jngp1 = jngp1 - 1
    ngp(jngp1) = ngp1

  end do
!
!  Determine grid points outside the data area.
!  in semi-infinite rectangular area.
!
  do il0 = 1, nl0

    ngp0 = 0
    ngp1 = 0
    il0t3 = il0*3
    ip1 = ipl(il0t3-2)
    ip2 = ipl(il0t3-1)
    x1 = xd(ip1)
    y1 = yd(ip1)
    x2 = xd(ip2)
    y2 = yd(ip2)

    xmn = ximn
    xmx = ximx
    ymn = yimn
    ymx = yimx

    if ( y2 >= y1 ) then
      xmn = min ( x1, x2 )
    end if

    if ( y2 <= y1 ) then
      xmx = max ( x1, x2 )
    end if

    if ( x2 <= x1 ) then
      ymn = min ( y1, y2 )
    end if

    if ( x2 >= x1 ) then
      ymx = max ( y1, y2 )
    end if

    insd = 0

    do ixi = 1, nxi

      if ( xi(ixi) < xmn .or. xi(ixi) > xmx ) then
        if ( insd == 0 ) then
          go to 42
        end if
        iximx = ixi-1
        go to 43
      end if

      if ( insd /= 1 ) then
        insd = 1
        iximn = ixi
      end if

42    continue

    end do

    if ( insd == 0 ) then
      go to 58
    end if

    iximx = nxi

43  continue

    do iyi = 1, nyi

      yii = yi(iyi)

      if ( yii < ymn .or. yii > ymx ) then
        go to 57
      end if

      do ixi = iximn,iximx

        xii = xi(ixi)
        l = 0
        if( vpdt(x1,y1,x2,y2,xii,yii)<0._KIND_Rdp ) then
         go to 46
        else if( vpdt(x1,y1,x2,y2,xii,yii)==0._KIND_Rdp ) then
         go to 45
        else
         go to 56
        end if

   45   l = 1
   46   continue
        if( spdt(x2,y2,x1,y1,xii,yii)<0._KIND_Rdp ) then
         go to 56
        else if( spdt(x2,y2,x1,y1,xii,yii)==0._KIND_Rdp ) then
         go to 47
        else
         go to 48
        end if
   47   l = 1

   48   continue
        if( spdt(x1,y1,x2,y2,xii,yii)<0._KIND_Rdp ) then
         go to 56
        else if( spdt(x1,y1,x2,y2,xii,yii)==0._KIND_Rdp ) then
         go to 49
        else
         go to 50
        end if

   49   l = 1
   50   izi = nxi*(iyi-1)+ixi

        if ( l /= 1 ) then
          ngp0 = ngp0+1
          jigp0 = jigp0+1
          igp(jigp0) = izi
          go to 56
        end if

        do jigp1i = jigp1, nxinyi
          if ( izi == igp(jigp1i) )     go to 56
        end do

53      continue

        ngp1 = ngp1+1
        jigp1 = jigp1-1
        igp(jigp1) = izi

56      continue

      end do

57    continue

    end do

58  continue

    jngp0 = jngp0+1
    ngp(jngp0) = ngp0
    jngp1 = jngp1-1
    ngp(jngp1) = ngp1
!
!  In semi-infinite triangular area.
!
60  continue

    ngp0 = 0
    ngp1 = 0
    ilp1 = mod(il0,nl0)+1
    ilp1t3 = ilp1*3
    ip3 = ipl(ilp1t3-1)
    x3 = xd(ip3)
    y3 = yd(ip3)
    xmn = ximn
    xmx = ximx
    ymn = yimn
    ymx = yimx

    if ( y2 <= y3 .and. y1 <= y2 ) then
      xmn = x2
    end if

    if ( y3 <= y2 .and. y2 <= y1 ) then
      xmx = x2
    end if

    if ( x3 <= x2 .and. x2 <= x1 ) then
      ymn = y2
    end if

    if ( x2 <= x3 .and. x1 <= x2 ) then
      ymx = y2
    end if

    insd = 0

    do ixi = 1, nxi

      if ( xi(ixi) < xmn .or. xmx < xi(ixi) ) then
        if ( insd == 0 ) then
          go to 62
        end if
        iximx = ixi - 1
        go to 63
      end if

      if ( insd /= 1 ) then
        insd = 1
        iximn = ixi
      end if

62    continue

    end do

    if ( insd == 0 ) then
      go to 78
    end if

    iximx = nxi

63  continue

    do iyi = 1, nyi

      yii = yi(iyi)
      if ( yii < ymn .or. yii > ymx ) go to 77

      do ixi = iximn, iximx

        xii = xi(ixi)
        l = 0
        if ( spdt(x1,y1,x2,y2,xii,yii)<0._KIND_Rdp ) then
         go to 66
        else if ( spdt(x1,y1,x2,y2,xii,yii)==0._KIND_Rdp ) then
         go to 65
        else
         go to 76
        end if

   65       l = 1
   66       continue
            if ( spdt(x3,y3,x2,y2,xii,yii)<0._KIND_Rdp ) then
             go to 70
            else if( spdt(x3,y3,x2,y2,xii,yii)==0._KIND_Rdp ) then
             go to 67
            else
             go to 76
            end if

   67       l = 1
   70       izi = nxi*(iyi-1)+ixi

        if ( l /= 1 ) then
          ngp0 = ngp0+1
          jigp0 = jigp0+1
          igp(jigp0) = izi
          go to 76
        end if

        do jigp1i = jigp1, nxinyi
          if ( izi == igp(jigp1i) ) then
            go to 76
          end if
        end do

        ngp1 = ngp1+1
        jigp1 = jigp1-1
        igp(jigp1) = izi

76      continue

      end do

77    continue

    end do

78  continue

    jngp0 = jngp0+1
    ngp(jngp0) = ngp0
    jngp1 = jngp1-1
    ngp(jngp1) = ngp1

  end do

  return
end subroutine

subroutine idlctn ( ndp, xd, yd, nt, ipt, nl, ipl, xii, yii, iti )

!*******************************************************************************
!
!! IDLCTN finds the triangle that contains a point.
!
!  Discusstion:
!
!    IDLCTN determines what triangle a given point (XII, YII) belongs to.
!    When the given point does not lie inside the data area, IDLCTN
!    determines the border line segment when the point lies in an outside
!    rectangular area, and two border line segments when the point
!    lies in an outside triangular area.
!
!  Modified:
!
!    23 January 2003
!
!  Reference:
!
!    Hiroshi Akima,
!    Algorithm 526,
!    A Method of Bivariate Interpolation and Smooth Surface Fitting
!    for Values Given at Irregularly Distributed Points,
!    ACM Transactions on Mathematical Software,
!    Volume 4, Number 2, June 1978.
!
!  Parameters:
!
!    Input, integer NDP, the number of data points.
!
!    Input, real ( kind = KIND_Rdp ) XD(NDP), YD(NDP), the X and Y coordinates
!    of the data.
!
!    Input, integer NT, the number of triangles.
!
!    Input, integer IPT(3*NT), the point numbers of the vertexes of
!    the triangles,
!
!    Input, integer NL, the number of border line segments.
!
!    Input, integer IPL(3*NL), the point numbers of the end points of
!    the border line segments and their respective triangle numbers.
!
!    Input, real ( kind = KIND_Rdp ) XII, YII, the coordinates of the point
!    to be located.
!
!    Output, integer ITI, the triangle number, when the point is inside the
!    data area, or two border line segment numbers, il1 and il2,
!    coded to il1*(nt+nl)+il2, when the point is outside the data area.
!
!  Local parameters:
!
!    Workspace, integer IWK(18*NDP).
!
!    Workspace, real ( kind = KIND_Rdp ) WK(8*NDP).
!
  implicit none

  integer ndp
  integer nl
  integer nt

  integer i1
  integer i2
  integer i3
  integer idp
  integer idsc(9)
  integer il1
  integer il1t3
  integer il2
  integer ip1
  integer ip2
  integer ip3
  integer ipl(3*nl)
  integer ipt(3*nt)
  integer isc
  integer it0
  integer it0t3
  integer iti
  integer itsc
  integer iwk(18*ndp)
  integer jiwk
  integer jwk
  integer nl0
  integer nt0
  integer ntl
  integer ntsci
  real ( kind = KIND_Rdp ) u1
  real ( kind = KIND_Rdp ) u2
  real ( kind = KIND_Rdp ) u3
  real ( kind = KIND_Rdp ) v1
  real ( kind = KIND_Rdp ) v2
  real ( kind = KIND_Rdp ) v3
  real ( kind = KIND_Rdp ) wk(8*ndp)
  real ( kind = KIND_Rdp ) x0
  real ( kind = KIND_Rdp ) x1
  real ( kind = KIND_Rdp ) x2
  real ( kind = KIND_Rdp ) x3
  real ( kind = KIND_Rdp ) xd(ndp)
  real ( kind = KIND_Rdp ) xii
  real ( kind = KIND_Rdp ) xmn
  real ( kind = KIND_Rdp ) xmx
  real ( kind = KIND_Rdp ) y0
  real ( kind = KIND_Rdp ) y1
  real ( kind = KIND_Rdp ) y2
  real ( kind = KIND_Rdp ) y3
  real ( kind = KIND_Rdp ) yd(ndp)
  real ( kind = KIND_Rdp ) yii
  real ( kind = KIND_Rdp ) ymn
  real ( kind = KIND_Rdp ) ymx

!
!  Preliminary processing
!
  nt0 = nt
  nl0 = nl
  ntl = nt0+nl0
  x0 = xii
  y0 = yii
!
!  Processing for a new set of data points
!
  if ( itipv /= 0 ) then
    go to 30
  end if
!
!  Divide the x-y plane into nine rectangular sections.
!
  xmn = xd(1)
  xmx = xd(1)
  ymn = yd(1)
  ymx = yd(1)
  do idp = 2, ndp
    xmn = min ( xd(idp), xmn )
    xmx = max ( xd(idp), xmx )
    ymn = min ( yd(idp), ymn )
    ymx = max ( yd(idp), ymx )
  end do

  xs1 = ( xmn + xmn + xmx ) / 3.0D+00
  xs2 = ( xmn + xmx + xmx ) / 3.0D+00
  ys1 = ( ymn + ymn + ymx ) / 3.0D+00
  ys2 = ( ymn + ymx + ymx ) / 3.0D+00
!
!  Determine and store in the iwk array, triangle numbers of
!  the triangles associated with each of the nine sections.
!
  ntsc(1:9) = 0
  idsc(1:9) = 0

  it0t3 = 0
  jwk = 0

  do it0 = 1, nt0

    it0t3 = it0t3+3
    i1 = ipt(it0t3-2)
    i2 = ipt(it0t3-1)
    i3 = ipt(it0t3)
    xmn = min ( xd(i1), xd(i2), xd(i3) )
    xmx = max ( xd(i1), xd(i2), xd(i3) )
    ymn = min ( yd(i1), yd(i2), yd(i3) )
    ymx = max ( yd(i1), yd(i2), yd(i3) )

    if ( ymn <= ys1 ) then

      if ( xmn <= xs1 ) then
        idsc(1) = 1
      end if

      if ( xmx>=xs1.and.xmn<=xs2 ) then
        idsc(2) = 1
      end if

      if ( xmx>=xs2 ) then
        idsc(3) = 1
      end if

    end if

    if ( ymx >= ys1 .and. ymn <= ys2 ) then
      if(xmn<=xs1)                   idsc(4) = 1
      if(xmx>=xs1.and.xmn<=xs2)    idsc(5) = 1
      if(xmx>=xs2)                   idsc(6) = 1
    end if

    if ( ymx < ys2) go to 25
    if(xmn<=xs1)                   idsc(7) = 1
    if(xmx>=xs1.and.xmn<=xs2)    idsc(8) = 1
    if(xmx>=xs2)                   idsc(9) = 1

25  continue

    do isc = 1, 9
      if ( idsc(isc) /= 0 ) then
        jiwk = 9*ntsc(isc)+isc
        iwk(jiwk) = it0
        ntsc(isc) = ntsc(isc)+1
        idsc(isc) = 0
      end if
    end do
!
!  Store in the wk array the minimum and maximum of the X and
!  Y coordinate values for each of the triangle.
!
    jwk = jwk+4
    wk(jwk-3) = xmn
    wk(jwk-2) = xmx
    wk(jwk-1) = ymn
    wk(jwk)   = ymx

  end do

  go to 60
!
!  Check if in the same triangle as previous.
!
30 continue

  it0 = itipv

  if(it0>nt0)      go to 40

  it0t3 = it0*3
  ip1 = ipt(it0t3-2)
  x1 = xd(ip1)
  y1 = yd(ip1)
  ip2 = ipt(it0t3-1)
  x2 = xd(ip2)
  y2 = yd(ip2)
  if(vpdt(x1,y1,x2,y2,x0,y0) < 0.0D+00 )      go to 60
  ip3 = ipt(it0t3)
  x3 = xd(ip3)
  y3 = yd(ip3)
  if(vpdt(x2,y2,x3,y3,x0,y0) < 0.0D+00 )      go to 60
  if(vpdt(x3,y3,x1,y1,x0,y0) < 0.0D+00 )      go to 60

  iti = it0
  itipv = it0

  return
!
!  Check if on the same border line segment.
!
40 continue

  il1 = it0 / ntl
  il2 = it0-il1*ntl
  il1t3 = il1*3
  ip1 = ipl(il1t3-2)
  x1 = xd(ip1)
  y1 = yd(ip1)
  ip2 = ipl(il1t3-1)
  x2 = xd(ip2)
  y2 = yd(ip2)
  if(il2/=il1)      go to 50
  if(spdt(x1,y1,x2,y2,x0,y0) < 0.0D+00 )      go to 60
  if(spdt(x2,y2,x1,y1,x0,y0) < 0.0D+00 )      go to 60
  if(vpdt(x1,y1,x2,y2,x0,y0) > 0.0D+00 )      go to 60

  iti = it0
  itipv = it0

  return
!
!  Check if between the same two border line segments.
!
50 continue

  if(spdt(x1,y1,x2,y2,x0,y0) > 0.0D+00 )      go to 60

  ip3 = ipl(3*il2-1)
  x3 = xd(ip3)
  y3 = yd(ip3)

  if ( spdt(x3,y3,x2,y2,x0,y0) <= 0.0D+00 )  then
    iti = it0
    itipv = it0
    return
  end if
!
!  Locate inside the data area.
!  Determine the section in which the point in question lies.
!
60 continue

  isc = 1

  if ( x0 >= xs1 ) then
    isc = isc+1
  end if

  if ( x0 >= xs2 ) then
    isc = isc+1
  end if

  if ( y0 >= ys1 ) then
    isc = isc+3
  end if

  if ( y0 >= ys2 ) then
    isc = isc+3
  end if
!
!  Search through the triangles associated with the section.
!
  ntsci = ntsc(isc)
  if(ntsci<=0)      go to 70
  jiwk = -9+isc

  do itsc = 1, ntsci

    jiwk = jiwk+9
    it0 = iwk(jiwk)
    jwk = it0*4
    if(x0<wk(jwk-3))    go to 61
    if(x0>wk(jwk-2))    go to 61
    if(y0<wk(jwk-1))    go to 61
    if(y0>wk(jwk))      go to 61
    it0t3 = it0*3
    ip1 = ipt(it0t3-2)
    x1 = xd(ip1)
    y1 = yd(ip1)
    ip2 = ipt(it0t3-1)
    x2 = xd(ip2)
    y2 = yd(ip2)
    if(vpdt(x1,y1,x2,y2,x0,y0)<0.0D+00 )    go to 61
    ip3 = ipt(it0t3)
    x3 = xd(ip3)
    y3 = yd(ip3)

    if ( vpdt(x2,y2,x3,y3,x0,y0) >= 0.0D+00 ) then

      if ( vpdt(x3,y3,x1,y1,x0,y0) >= 0.0D+00 ) then
        iti = it0
        itipv = it0
        return
      end if

    end if

61  continue

  end do
!
!  Locate outside the data area.
!
70 continue

  do il1 = 1, nl0

    il1t3 = il1*3
    ip1 = ipl(il1t3-2)
    x1 = xd(ip1)
    y1 = yd(ip1)
    ip2 = ipl(il1t3-1)
    x2 = xd(ip2)
    y2 = yd(ip2)
    if(spdt(x2,y2,x1,y1,x0,y0)<0.0D+00 )    go to 72
    if(spdt(x1,y1,x2,y2,x0,y0)<0.0D+00 )    go to 71
    if(vpdt(x1,y1,x2,y2,x0,y0)>0.0D+00 )    go to 72
    il2 = il1
    go to 75

   71   continue

    il2 = mod(il1,nl0)+1
    ip3 = ipl(3*il2-1)
    x3 = xd(ip3)
    y3 = yd(ip3)
    if(spdt(x3,y3,x2,y2,x0,y0)<=0.0D+00 )    go to 75

   72   continue

  end do

  it0 = 1
  iti = it0
  itipv = it0

  return

   75 continue

  it0 = il1*ntl+il2
  iti = it0
  itipv = it0

  return
end subroutine

subroutine idpdrv ( ndp, xd, yd, zd, nt, ipt, pd, wk )

!*******************************************************************************
!
!! IDPDRV estimates first and second partial derivatives at data points.
!
!  Modified:
!
!    04 June 2003
!
!  Reference:
!
!    Hiroshi Akima,
!    Algorithm 526,
!    A Method of Bivariate Interpolation and Smooth Surface Fitting
!    for Values Given at Irregularly Distributed Points,
!    ACM Transactions on Mathematical Software,
!    Volume 4, Number 2, June 1978.
!
!  Parameters:
!
!    Input, integer NDP, the number of data points.
!
!    Input, real ( kind = KIND_Rdp ) XD(NDP), YD(NDP), the X and Y coordinates
!    of the data.
!
!    Input, real ( kind = KIND_Rdp ) ZD(NDP), the data values.
!
!    Input, integer NT, the number of triangles.
!
!    Input, integer IPT(3*NT), the point numbers of the vertexes of the
!    triangles.
!
!    Output, real ( kind = KIND_Rdp ) PD(5*NDP), the estimated zx, zy, zxx, zxy,
!    and zyy values at the ith data point are to be stored as the
!    (5*i-4)th, (5*i-3)rd, (5*i-2)nd, (5*i-1)st and (5*i)th elements,
!    respectively, where i = 1, 2, ..., ndp.
!
!    Workspace, real ( kind = KIND_Rdp ) WK(NDP).
!
  implicit none

  integer ndp
  integer nt

  real ( kind = KIND_Rdp ) d12
  real ( kind = KIND_Rdp ) d23
  real ( kind = KIND_Rdp ) d31
  real ( kind = KIND_Rdp ) dx1
  real ( kind = KIND_Rdp ) dx2
  real ( kind = KIND_Rdp ) dy1
  real ( kind = KIND_Rdp ) dy2
  real ( kind = KIND_Rdp ) dz1
  real ( kind = KIND_Rdp ) dz2
  real ( kind = KIND_Rdp ) dzx1
  real ( kind = KIND_Rdp ) dzx2
  real ( kind = KIND_Rdp ) dzy1
  real ( kind = KIND_Rdp ) dzy2
  integer idp
  integer ipt(3*nt)
  integer ipti(3)
  integer it
  integer iv
  integer jpd0
  integer jpdmx
  integer jpt
  integer jpt0
  integer nt0
  real ( kind = KIND_Rdp ) pd(5*ndp)
  real ( kind = KIND_Rdp ) vpx
  real ( kind = KIND_Rdp ) vpxx
  real ( kind = KIND_Rdp ) vpxy
  real ( kind = KIND_Rdp ) vpy
  real ( kind = KIND_Rdp ) vpyx
  real ( kind = KIND_Rdp ) vpyy
  real ( kind = KIND_Rdp ) vpz
  real ( kind = KIND_Rdp ) vpzmn
  real ( kind = KIND_Rdp ) w1(3)
  real ( kind = KIND_Rdp ) w2(3)
  real ( kind = KIND_Rdp ) wi
  real ( kind = KIND_Rdp ) wk(ndp)
  real ( kind = KIND_Rdp ) xd(ndp)
  real ( kind = KIND_Rdp ) xv(3)
  real ( kind = KIND_Rdp ) yd(ndp)
  real ( kind = KIND_Rdp ) yv(3)
  real ( kind = KIND_Rdp ) zd(ndp)
  real ( kind = KIND_Rdp ) zv(3)
  real ( kind = KIND_Rdp ) zxv(3)
  real ( kind = KIND_Rdp ) zyv(3)
!
!  Preliminary processing.
!
  nt0 = nt
!
!  Clear the PD array.
!
  jpdmx = 5*ndp

  pd(1:jpdmx) = 0.0D+00

  wk(1:ndp) = 0.0D+00
!
!  Estimate ZX and ZY.
!
  do it = 1, nt0

    jpt0 = 3*(it-1)

    do iv = 1, 3
      jpt = jpt0+iv
      idp = ipt(jpt)
      ipti(iv) = idp
      xv(iv) = xd(idp)
      yv(iv) = yd(idp)
      zv(iv) = zd(idp)
    end do

    dx1 = xv(2)-xv(1)
    dy1 = yv(2)-yv(1)
    dz1 = zv(2)-zv(1)
    dx2 = xv(3)-xv(1)
    dy2 = yv(3)-yv(1)
    dz2 = zv(3)-zv(1)
    vpx = dy1*dz2-dz1*dy2
    vpy = dz1*dx2-dx1*dz2
    vpz = dx1*dy2-dy1*dx2
    vpzmn = abs ( dx1*dx2+dy1*dy2 )* epsilon ( vpzmn )

    if ( vpzmn < abs ( vpz ) ) then

      d12 = sqrt((xv(2)-xv(1))**2+(yv(2)-yv(1))**2)
      d23 = sqrt((xv(3)-xv(2))**2+(yv(3)-yv(2))**2)
      d31 = sqrt((xv(1)-xv(3))**2+(yv(1)-yv(3))**2)
      w1(1) = 1.0D+00 / (d31*d12)
      w1(2) = 1.0D+00 / (d12*d23)
      w1(3) = 1.0D+00 / (d23*d31)
      w2(1) = vpz*w1(1)
      w2(2) = vpz*w1(2)
      w2(3) = vpz*w1(3)

      do iv = 1, 3
        idp = ipti(iv)
        jpd0 = 5*(idp-1)
        wi = (w1(iv)**2)*w2(iv)
        pd(jpd0+1) = pd(jpd0+1)+vpx*wi
        pd(jpd0+2) = pd(jpd0+2)+vpy*wi
        wk(idp) = wk(idp)+vpz*wi
      end do

    end if

  end do

  do idp = 1, ndp
    jpd0 = 5*(idp-1)
    pd(jpd0+1) = -pd(jpd0+1)/wk(idp)
    pd(jpd0+2) = -pd(jpd0+2)/wk(idp)
  end do
!
!  Estimate ZXX, ZXY, and ZYY.
!
  do it = 1, nt0

    jpt0 = 3*(it-1)

    do iv = 1, 3
      jpt = jpt0+iv
      idp = ipt(jpt)
      ipti(iv) = idp
      xv(iv) = xd(idp)
      yv(iv) = yd(idp)
      jpd0 = 5*(idp-1)
      zxv(iv) = pd(jpd0+1)
      zyv(iv) = pd(jpd0+2)
    end do

    dx1 = xv(2)-xv(1)
    dy1 = yv(2)-yv(1)
    dzx1 = zxv(2)-zxv(1)
    dzy1 = zyv(2)-zyv(1)
    dx2 = xv(3)-xv(1)
    dy2 = yv(3)-yv(1)
    dzx2 = zxv(3)-zxv(1)
    dzy2 = zyv(3)-zyv(1)
    vpxx = dy1*dzx2-dzx1*dy2
    vpxy = dzx1*dx2-dx1*dzx2
    vpyx = dy1*dzy2-dzy1*dy2
    vpyy = dzy1*dx2-dx1*dzy2
    vpz = dx1*dy2-dy1*dx2
    vpzmn = abs ( dx1 * dx2 + dy1 * dy2 ) * epsilon ( vpzmn )

    if ( abs(vpz) > vpzmn ) then

      d12 = sqrt((xv(2)-xv(1))**2+(yv(2)-yv(1))**2)
      d23 = sqrt((xv(3)-xv(2))**2+(yv(3)-yv(2))**2)
      d31 = sqrt((xv(1)-xv(3))**2+(yv(1)-yv(3))**2)
      w1(1) = 1.0D+00 /(d31*d12)
      w1(2) = 1.0D+00 /(d12*d23)
      w1(3) = 1.0D+00 /(d23*d31)
      w2(1) = vpz*w1(1)
      w2(2) = vpz*w1(2)
      w2(3) = vpz*w1(3)

      do iv = 1, 3
        idp = ipti(iv)
        jpd0 = 5*(idp-1)
        wi = (w1(iv)**2)*w2(iv)
        pd(jpd0+3) = pd(jpd0+3)+vpxx*wi
        pd(jpd0+4) = pd(jpd0+4)+(vpxy+vpyx)*wi
        pd(jpd0+5) = pd(jpd0+5)+vpyy*wi
      end do

    end if

  end do

  do idp = 1, ndp
    jpd0 = 5*(idp-1)
    pd(jpd0+3) = -pd(jpd0+3) / wk(idp)
    pd(jpd0+4) = -pd(jpd0+4) / (2.0*wk(idp))
    pd(jpd0+5) = -pd(jpd0+5) / wk(idp)
  end do

  return
end subroutine

subroutine idptip ( ndp,xd, yd, zd, nt, ipt, nl, ipl, pdd, iti, xii, yii, zii )

!*******************************************************************************
!
!! IDPTIP performs interpolation, determining a value of Z given X and Y.
!
!  Modified:
!
!    19 February 2001
!
!  Reference:
!
!    Hiroshi Akima,
!    Algorithm 526,
!    A Method of Bivariate Interpolation and Smooth Surface Fitting
!    for Values Given at Irregularly Distributed Points,
!    ACM Transactions on Mathematical Software,
!    Volume 4, Number 2, June 1978.
!
!  Parameters:
!
!    Input, integer NDP, the number of data values.
!
!    Input, real ( kind = KIND_Rdp ) XD(NDP), YD(NDP), the X and Y coordinates
!    of the data.
!
!    Input, real ( kind = KIND_Rdp ) ZD(NDP), the data values.
!
!    Input, integer NT, the number of triangles.
!
!    Input, integer IPT(3*NT), the point numbers of the vertexes of
!    the triangles.
!
!    Input, integer NL, the number of border line segments.
!
!    Input, integer IPL(3*NL), the point numbers of the end points of the
!    border line segments and their respective triangle numbers,
!
!    Input, real ( kind = KIND_Rdp ) PDD(5*NDP). the partial derivatives at
!    the data points,
!
!    Input, integer ITI, triangle number of the triangle in which lies
!    the point for which interpolation is to be performed,
!
!    Input, real ( kind = KIND_Rdp ) XII, YII, the X and Y coordinates of the
!    point for which interpolation is to be performed.
!
!    Output, real ( kind = KIND_Rdp ) ZII, the interpolated Z value.
!
  implicit none

  integer ndp
  integer nl
  integer nt

  real ( kind = KIND_Rdp ) a
  real ( kind = KIND_Rdp ) aa
  real ( kind = KIND_Rdp ) ab
  real ( kind = KIND_Rdp ) ac
  real ( kind = KIND_Rdp ) act2
  real ( kind = KIND_Rdp ) ad
  real ( kind = KIND_Rdp ) adbc
  real ( kind = KIND_Rdp ) b
  real ( kind = KIND_Rdp ) bb
  real ( kind = KIND_Rdp ) bc
  real ( kind = KIND_Rdp ) bdt2
  real ( kind = KIND_Rdp ) c
  real ( kind = KIND_Rdp ) cc
  real ( kind = KIND_Rdp ) cd
  real ( kind = KIND_Rdp ) csuv
  real ( kind = KIND_Rdp ) d
  real ( kind = KIND_Rdp ) dd
  real ( kind = KIND_Rdp ) dlt
  real ( kind = KIND_Rdp ) dx
  real ( kind = KIND_Rdp ) dy
  real ( kind = KIND_Rdp ) g1
  real ( kind = KIND_Rdp ) g2
  real ( kind = KIND_Rdp ) h1
  real ( kind = KIND_Rdp ) h2
  real ( kind = KIND_Rdp ) h3
  integer i
  integer idp
  integer il1
  integer il2
  integer ipl(3*nl)
  integer ipt(3*nt)
  integer it0
  integer iti
  integer jipl
  integer jipt
  integer jpd
  integer jpdd
  integer kpd
  integer ntl
  real ( kind = KIND_Rdp ) lu
  real ( kind = KIND_Rdp ) lv
  real ( kind = KIND_Rdp ) p0
  real ( kind = KIND_Rdp ) p1
  real ( kind = KIND_Rdp ) p2
  real ( kind = KIND_Rdp ) p3
  real ( kind = KIND_Rdp ) p4
  real ( kind = KIND_Rdp ) p5
  real ( kind = KIND_Rdp ) pd(15)
  real ( kind = KIND_Rdp ) pdd(5*ndp)
  real ( kind = KIND_Rdp ) thsv
  real ( kind = KIND_Rdp ) thus
  real ( kind = KIND_Rdp ) thuv
  real ( kind = KIND_Rdp ) thxu
  real ( kind = KIND_Rdp ) u
  real ( kind = KIND_Rdp ) v
  real ( kind = KIND_Rdp ) x(3)
  real ( kind = KIND_Rdp ) xd(*)
  real ( kind = KIND_Rdp ) xii
  real ( kind = KIND_Rdp ) y(3)
  real ( kind = KIND_Rdp ) yd(*)
  real ( kind = KIND_Rdp ) yii
  real ( kind = KIND_Rdp ) z(3)
  real ( kind = KIND_Rdp ) z0
  real ( kind = KIND_Rdp ) zd(*)
  real ( kind = KIND_Rdp ) zii
  real ( kind = KIND_Rdp ) zu(3)
  real ( kind = KIND_Rdp ) zuu(3)
  real ( kind = KIND_Rdp ) zuv(3)
  real ( kind = KIND_Rdp ) zv(3)
  real ( kind = KIND_Rdp ) zvv(3)

!
!  Preliminary processing
!
  it0 = iti
  ntl = nt+nl

  if ( ntl < it0 ) then
    il1 = it0/ntl
    il2 = it0-il1*ntl
    if(il1==il2) go to 40
    go to 60
  end if
!
!  Calculation of ZII by interpolation.
!  Check if the necessary coefficients have been calculated.
!
  if ( it0 == itpv ) then
    go to 30
  end if
!
!  Load coordinate and partial derivative values at the vertexes.
!
  jipt = 3*(it0-1)
  jpd = 0

  do i = 1, 3

    jipt = jipt+1
    idp = ipt(jipt)
    x(i) = xd(idp)
    y(i) = yd(idp)
    z(i) = zd(idp)
    jpdd = 5*(idp-1)

    do kpd = 1, 5
      jpd = jpd+1
      jpdd = jpdd+1
      pd(jpd) = pdd(jpdd)
    end do

  end do
!
!  Determine the coefficients for the coordinate system
!  transformation from the XY system to the UV system and vice versa.
!
  x0 = x(1)
  y0 = y(1)
  a = x(2)-x0
  b = x(3)-x0
  c = y(2)-y0
  d = y(3)-y0
  ad = a*d
  bc = b*c
  dlt = ad-bc
  ap =  d/dlt
  bp = -b/dlt
  cp = -c/dlt
  dp =  a/dlt
!
!  Convert the partial derivatives at the vertexes of the
!  triangle for the UV coordinate system.
!
  aa = a*a
  act2 = 2.0D+00 *a*c
  cc = c*c
  ab = a*b
  adbc = ad+bc
  cd = c*d
  bb = b*b
  bdt2 = 2.0D+00 *b*d
  dd = d*d

  do i = 1, 3
    jpd = 5*i
    zu(i) = a*pd(jpd-4)+c*pd(jpd-3)
    zv(i) = b*pd(jpd-4)+d*pd(jpd-3)
    zuu(i) = aa*pd(jpd-2)+act2*pd(jpd-1)+cc*pd(jpd)
    zuv(i) = ab*pd(jpd-2)+adbc*pd(jpd-1)+cd*pd(jpd)
    zvv(i) = bb*pd(jpd-2)+bdt2*pd(jpd-1)+dd*pd(jpd)
  end do
!
!  Calculate the coefficients of the polynomial.
!
  p00 = z(1)
  p10 = zu(1)
  p01 = zv(1)
  p20 = 0.5D+00 * zuu(1)
  p11 = zuv(1)
  p02 = 0.5D+00 * zvv(1)
  h1 = z(2)-p00-p10-p20
  h2 = zu(2)-p10-zuu(1)
  h3 = zuu(2)-zuu(1)
  p30 =  10.0D+00 * h1 - 4.0D+00 * h2 + 0.5D+00 * h3
  p40 = -15.0D+00 * h1 + 7.0D+00 * h2           - h3
  p50 =   6.0D+00 * h1 - 3.0D+00 * h2 + 0.5D+00 * h3
  h1 = z(3)-p00-p01-p02
  h2 = zv(3)-p01-zvv(1)
  h3 = zvv(3)-zvv(1)
  p03 =  10.0D+00 * h1 - 4.0D+00 * h2 + 0.5D+00 * h3
  p04 = -15.0D+00 * h1 + 7.0D+00 * h2    -h3
  p05 =   6.0D+00 * h1 - 3.0D+00 * h2 + 0.5D+00 * h3
  lu = sqrt(aa+cc)
  lv = sqrt(bb+dd)
  thxu = atan2(c,a)
  thuv = atan2(d,b)-thxu
  csuv = cos(thuv)
  p41 = 5.0D+00*lv*csuv/lu*p50
  p14 = 5.0D+00*lu*csuv/lv*p05
  h1 = zv(2)-p01-p11-p41
  h2 = zuv(2)-p11-4.0D+00 * p41
  p21 =  3.0D+00 * h1-h2
  p31 = -2.0D+00 * h1+h2
  h1 = zu(3)-p10-p11-p14
  h2 = zuv(3)-p11- 4.0D+00 * p14
  p12 =  3.0D+00 * h1-h2
  p13 = -2.0D+00 * h1+h2
  thus = atan2(d-c,b-a)-thxu
  thsv = thuv-thus
  aa =  sin(thsv)/lu
  bb = -cos(thsv)/lu
  cc =  sin(thus)/lv
  dd =  cos(thus)/lv
  ac = aa*cc
  ad = aa*dd
  bc = bb*cc
  g1 = aa * ac*(3.0D+00*bc+2.0D+00*ad)
  g2 = cc * ac*(3.0D+00*ad+2.0D+00*bc)
  h1 = -aa*aa*aa*(5.0D+00*aa*bb*p50+(4.0D+00*bc+ad)*p41) &
       -cc*cc*cc*(5.0D+00*cc*dd*p05+(4.0D+00*ad+bc)*p14)
  h2 = 0.5D+00 * zvv(2)-p02-p12
  h3 = 0.5D+00 * zuu(3)-p20-p21
  p22 = (g1*h2+g2*h3-h1)/(g1+g2)
  p32 = h2-p22
  p23 = h3-p22
  itpv = it0
!
!  Convert XII and YII to UV system.
!
30 continue

  dx = xii-x0
  dy = yii-y0
  u = ap*dx+bp*dy
  v = cp*dx+dp*dy
!
!  Evaluate the polynomial.
!
  p0 = p00+v*(p01+v*(p02+v*(p03+v*(p04+v*p05))))
  p1 = p10+v*(p11+v*(p12+v*(p13+v*p14)))
  p2 = p20+v*(p21+v*(p22+v*p23))
  p3 = p30+v*(p31+v*p32)
  p4 = p40+v*p41
  p5 = p50
  zii = p0+u*(p1+u*(p2+u*(p3+u*(p4+u*p5))))
  return
!
!  Calculation of ZII by extrapolation in the rectangle.
!  Check if the necessary coefficients have been calculated.
!
40 continue

  if ( it0 == itpv ) then
    go to 50
  end if
!
!  Load coordinate and partial derivative values at the end
!  points of the border line segment.
!
  jipl = 3*(il1-1)
  jpd = 0

  do i = 1, 2

    jipl = jipl+1
    idp = ipl(jipl)
    x(i) = xd(idp)
    y(i) = yd(idp)
    z(i) = zd(idp)
    jpdd = 5*(idp-1)

    do kpd = 1, 5
      jpd = jpd+1
      jpdd = jpdd+1
      pd(jpd) = pdd(jpdd)
    end do

  end do
!
!  Determine the coefficients for the coordinate system
!  transformation from the XY system to the UV system
!  and vice versa.
!
  x0 = x(1)
  y0 = y(1)
  a = y(2)-y(1)
  b = x(2)-x(1)
  c = -b
  d = a
  ad = a * d
  bc = b * c
  dlt = ad - bc
  ap =  d / dlt
  bp = -b / dlt
  cp = -bp
  dp =  ap
!
!  Convert the partial derivatives at the end points of the
!  border line segment for the UV coordinate system.
!
  aa = a*a
  act2 = 2.0D+00 * a * c
  cc = c*c
  ab = a*b
  adbc = ad+bc
  cd = c*d
  bb = b*b
  bdt2 = 2.0D+00 * b * d
  dd = d*d

  do i = 1, 2
    jpd = 5*i
    zu(i) = a*pd(jpd-4)+c*pd(jpd-3)
    zv(i) = b*pd(jpd-4)+d*pd(jpd-3)
    zuu(i) = aa*pd(jpd-2)+act2*pd(jpd-1)+cc*pd(jpd)
    zuv(i) = ab*pd(jpd-2)+adbc*pd(jpd-1)+cd*pd(jpd)
    zvv(i) = bb*pd(jpd-2)+bdt2*pd(jpd-1)+dd*pd(jpd)
  end do
!
!  Calculate the coefficients of the polynomial.
!
  p00 = z(1)
  p10 = zu(1)
  p01 = zv(1)
  p20 = 0.5D+00 * zuu(1)
  p11 = zuv(1)
  p02 = 0.5D+00 * zvv(1)

  h1 = z(2)-p00-p01-p02
  h2 = zv(2)-p01-zvv(1)
  h3 = zvv(2)-zvv(1)

  p03 =  10.0D+00 * h1 - 4.0D+00*h2+0.5D+00*h3
  p04 = -15.0D+00 * h1 + 7.0D+00*h2    -h3
  p05 =   6.0D+00 * h1 - 3.0D+00*h2+0.5D+00*h3

  h1 = zu(2)-p10-p11
  h2 = zuv(2)-p11

  p12 =  3.0D+00*h1-h2
  p13 = -2.0D+00*h1+h2
  p21 = 0.0D+00
  p23 = -zuu(2)+zuu(1)
  p22 = -1.5D+00*p23

  itpv = it0
!
!  Convert XII and YII to UV system.
!
50 continue

  dx = xii-x0
  dy = yii-y0
  u = ap*dx+bp*dy
  v = cp*dx+dp*dy
!
!  Evaluate the polynomial.
!
  p0 = p00+v*(p01+v*(p02+v*(p03+v*(p04+v*p05))))
  p1 = p10+v*(p11+v*(p12+v*p13))
  p2 = p20+v*(p21+v*(p22+v*p23))
  zii = p0+u*(p1+u*p2)

  return
!
!  Calculation of ZII by extrapolation in the triangle.
!  Check if the necessary coefficients have been calculated.
!
60 continue

  if ( it0 /= itpv ) then
!
!  Load coordinate and partial derivative values at the vertex of the triangle.
!
    jipl = 3*il2-2
    idp = ipl(jipl)
    x0 = xd(idp)
    y0 = yd(idp)
    z0 = zd(idp)
    jpdd = 5*(idp-1)

    do kpd = 1, 5
      jpdd = jpdd+1
      pd(kpd) = pdd(jpdd)
    end do
!
!  Calculate the coefficients of the polynomial.
!
    p00 = z0
    p10 = pd(1)
    p01 = pd(2)
    p20 = 0.5D+00*pd(3)
    p11 = pd(4)
    p02 = 0.5D+00*pd(5)
    itpv = it0

  end if
!
!  Convert XII and YII to UV system.
!
  u = xii-x0
  v = yii-y0
!
!  Evaluate the polynomial.
!
  p0 = p00+v*(p01+v*p02)
  p1 = p10+v*p11
  zii = p0+u*(p1+u*p20)

  return
end subroutine

subroutine idsfft ( md, ndp, xd, yd, zd, nxi, nyi, nzi, xi, yi, zi , &
  errint , errmsg )

!*******************************************************************************
!
!! IDSFFT fits a smooth surface Z(X,Y) given irregular (X,Y,Z) data.
!
!  Discussion:
!
!    IDSFFT performs smooth surface fitting when the projections of the
!    data points in the (X,Y) plane are irregularly distributed.
!
!  Special conditions:
!
!    The data points must be distinct and their projections in the XY
!    plane must not be collinear, otherwise an error return occurs.
!
!  Reference:
!
!    Hiroshi Akima,
!    Algorithm 526,
!    A Method of Bivariate Interpolation and Smooth Surface Fitting
!    for Values Given at Irregularly Distributed Points,
!    ACM Transactions on Mathematical Software,
!    Volume 4, Number 2, June 1978.
!
!  Parameters:
!
!    Input, integer MD, mode of computation (must be 1, 2, or 3,
!    else an error return will occur).
!
!    1, if this is the first call to this routine, or if the value of
!    NDP has been changed from the previous call, or if the contents of
!    the XD or YD arrays have been changed from the previous call.
!
!    2, if the values of NDP and the XD, YD arrays are unchanged from
!    the previous call, but new values for XI, YI are being used.  If
!    MD = 2 and NDP has been changed since the previous call to IDSFFT,
!    an error return occurs.
!
!    3, if the values of NDP, NXI, NYI, XD, YD, XI, YI are unchanged
!    from the previous call, i.e. if the only change on input to idsfft
!    is in the ZD array.  If MD = 3 and NDP, nxi or nyi has been changed
!    since the previous call to idsfft, an error return occurs.
!
!    Between the call with MD = 2 or MD = 3 and the preceding call, the
!    iwk and wk work arrays should not be disturbed.
!
!    Input, integer NDP, the number of data points.  NDP must be at least 4.
!
!    Input, real ( kind = KIND_Rdp ) XD(NDP), YD(NDP), the X and Y coordinates
!    of the data.
!
!    Input, real ( kind = KIND_Rdp ) ZD(NDP), the data values.
!
!    Input, integer NXI, NYI, the number of output grid points in the
!    X and Y directions.  NXI and NYI must each be at least 1.
!
!    Input, integer NZI, the first dimension of ZI.  NZI must be at
!    least NXI.
!
!    Input, real ( kind = KIND_Rdp ) XI(NXI), YI(NYI), the X and Y coordinates
!    of the grid points.
!
!    Output, real ( kind = KIND_Rdp ) ZI(NZI,NYI), contains the interpolated Z
!    values at the grid points.
!
!  Local parameters:
!
!    Workspace, integer IWK(31*NDP+NXI*NYI).
!
!    Workspace, real ( kind = KIND_Rdp ) WK(6*NDP).
!
  implicit none

  integer ndp
  integer nxi
  integer nyi
  integer nzi

  integer il1
  integer il2
  integer iti
  integer iwk(31*ndp + nxi*nyi)
  integer ixi
  integer iyi
  integer izi
  integer jig0mn
  integer jig0mx
  integer jig1mn
  integer jig1mx
  integer jigp
  integer jngp
  integer jwigp
  integer jwigp0
  integer jwipl
  integer jwipt
  integer jwiwl
  integer jwiwp
  integer jwngp
  integer jwngp0
  integer jwwpd
  integer md
  integer ngp0
  integer ngp1
  integer nl
  integer nngp
  integer nt
  integer errint
  character ( len = * ) errmsg
  optional errint,errmsg
  real ( kind = KIND_Rdp ) wk(6*ndp)
  real ( kind = KIND_Rdp ) xd(ndp)
  real ( kind = KIND_Rdp ) xi(nxi)
  real ( kind = KIND_Rdp ) yd(ndp)
  real ( kind = KIND_Rdp ) yi(nyi)
  real ( kind = KIND_Rdp ) zd(ndp)
  real ( kind = KIND_Rdp ) zi(nzi,nyi)

!
!  Error check.
!
  IF( PRESENT(errmsg) )errmsg = ""
  IF( PRESENT(errint) )errint = 0
  if ( md < 1 .or. 3 < md ) then
    if( present(errint) )errint = -1
    if( present(errmsg) )then
     write (errmsg, '(a,i1,a)' )'Input parameter MD=',MD,&
       ' out of range. Should be 1-3. [LIB_BiVar:IDSFFT]'
    end if
    return
  end if

  if ( ndp < 4 ) then
    if( present(errint) )errint = -2
    if( present(errmsg) )then
     write (errmsg, '(a,i7,a)' )'Input parameter NDP=',NDP,&
       ' less than minimum number of data points of 4. [LIB_BiVar:IDSFFT]'
    end if
    return
  end if

  if ( nxi < 1 .or. nyi < 1 ) then
    if( present(errint) )errint = -3
    if( present(errmsg) )then
     write (errmsg, '(a,i7,1x,i7,a)' )'Input parameter NXI,NYI=',NXI,NYI,&
       ' are out of range. [LIB_BiVar:IDSFFT]'
    end if
    return
  end if

  if ( nxi > nzi ) then
    if( present(errint) )errint = -4
    if( present(errmsg) )then
     write (errmsg, '(a,i7,1x,i7,a)' )'Input parameter NZI,NXI=',NZI,NXI,&
       ' are out of range: NZI<NXI. [LIB_BiVar:IDSFFT]'
    end if
    return
  end if

  if ( md <= 1 ) then

    iwk(1) = ndp

  else

    if ( ndp /= iwk(1) ) then
       if( present(errint) )errint = -5
       if( present(errmsg) )then
        write (errmsg, '(a,i1,a,i7,a2,i7,a)' )'Input parameter MD=',MD,&
          ' but NDP was changed since last call: ',iwk(1),"=>",ndp," [LIB_BiVar:IDSFFT]"
       end if
       return
    end if

  end if

  if ( md <= 2 ) then

    iwk(3) = nxi
    iwk(4) = nyi

  else

    if ( nxi /= iwk(3) ) then
      if( present(errint) )errint = -6
      if( present(errmsg) )then
       write (errmsg, '(a,i1,a,i7,a2,i7,a)' )'IDSFFT - Fatal error!  Input parameter MD=',MD,&
         ' but NXI was changed since last call: ',iwk(3),"=>",nxi,"[LIB_BiVar:IDSFFT]"
      end if
      return
    end if

    if ( nyi /= iwk(4) ) then
      if( present(errint) )errint = -7
      if( present(errmsg) )then
       write (errmsg, '(a,i1,a,i7,a2,i7,a)' )'IDSFFT - Fatal error!  Input parameter MD=',MD,&
         ' but NYI was changed since last call: ',iwk(4),"=>",nyi,"[LIB_BiVar:IDSFFT]"
      end if
      return
    end if

  end if
!
!  Allocation of storage areas in the IWK array.
!
  jwipt = 16
  jwiwl = 6*ndp+1
  jwngp0 = jwiwl-1
  jwipl = 24*ndp+1
  jwiwp = 30*ndp+1
  jwigp0 = 31*ndp
  jwwpd = 5*ndp+1
!
!  Triangulate the XY plane.
!
  if ( md == 1 ) then

    call idtang ( ndp, xd, yd, nt, iwk(jwipt), nl, iwk(jwipl), &
      iwk(jwiwl), iwk(jwiwp), wk )

    iwk(5) = nt
    iwk(6) = nl

    if ( nt == 0 ) then
      return
    end if

  else

    nt = iwk(5)
    nl = iwk(6)

  end if
!
!  Sort output grid points in ascending order of the triangle
!  number and the border line segment number.
!
  if ( md <= 2 ) then

    call idgrid ( xd, yd, nt, iwk(jwipt), nl, iwk(jwipl), nxi, &
      nyi, xi, yi, iwk(jwngp0+1), iwk(jwigp0+1) )

  end if
!
!  Estimate partial derivatives at all data points.
!
  call idpdrv ( ndp, xd, yd, zd, nt, iwk(jwipt), wk, wk(jwwpd) )
!
!  Interpolate the ZI values.
!
  itpv = 0
  jig0mx = 0
  jig1mn = nxi * nyi + 1
  nngp = nt + 2 * nl

  do jngp = 1, nngp

    iti = jngp

    if ( jngp > nt ) then
      il1 = (jngp-nt+1)/2
      il2 = (jngp-nt+2)/2
      if ( nl < il2 ) then
        il2 = 1
      end if
      iti = il1*(nt+nl)+il2
    end if

    jwngp = jwngp0+jngp
    ngp0 = iwk(jwngp)

    if ( ngp0 /= 0 ) then

      jig0mn = jig0mx+1
      jig0mx = jig0mx+ngp0

      do jigp = jig0mn, jig0mx

        jwigp = jwigp0+jigp
        izi = iwk(jwigp)
        iyi = (izi-1)/nxi+1
        ixi = izi-nxi*(iyi-1)

        call idptip ( ndp, xd, yd, zd, nt, iwk(jwipt), nl, iwk(jwipl), &
          wk, iti, xi(ixi), yi(iyi), zi(ixi,iyi) )

      end do

    end if

    jwngp = jwngp0+2*nngp+1-jngp
    ngp1 = iwk(jwngp)

    if ( ngp1 /= 0 ) then

      jig1mx = jig1mn-1
      jig1mn = jig1mn-ngp1

      do jigp = jig1mn, jig1mx

        jwigp = jwigp0+jigp
        izi = iwk(jwigp)
        iyi = (izi-1)/nxi+1
        ixi = izi-nxi*(iyi-1)

        call idptip ( ndp, xd, yd, zd, nt, iwk(jwipt), nl, iwk(jwipl), &
          wk, iti, xi(ixi), yi(iyi), zi(ixi,iyi) )

      end do

    end if

  end do

  return
end subroutine

subroutine idtang ( ndp, xd, yd, nt, ipt, nl, ipl, iwl, iwp, wk , &
  errint , errmsg )

!*******************************************************************************
!
!! IDTANG performs triangulation.
!
!  Discussion:
!
!    The routine divides the XY plane into a number of triangles according to
!    given data points in the plane, determines line segments that form
!    the border of data area, and determines the triangle numbers
!    corresponding to the border line segments.
!
!    At completion, point numbers of the vertexes of each triangle
!    are listed counter-clockwise.  Point numbers of the end points
!    of each border line segment are listed counter-clockwise,
!    listing order of the line segments being counter-clockwise.
!
!  Modified:
!
!    04 June 2003
!
!  Reference:
!
!    Hiroshi Akima,
!    Algorithm 526,
!    A Method of Bivariate Interpolation and Smooth Surface Fitting
!    for Values Given at Irregularly Distributed Points,
!    ACM Transactions on Mathematical Software,
!    Volume 4, Number 2, June 1978.
!
!  Parameters:
!
!    Input, integer NDP, the number of data points.
!
!    Input, real ( kind = KIND_Rdp ) XD(NDP), YD(NDP), the X and Y coordinates
!    of the data.
!
!    Output, integer NT, the number of triangles,
!
!    Output, integer IPT(6*NDP-15), where the point numbers of the
!    vertexes of the IT-th triangle are to be stored as entries
!    3*IT-2, 3*IT-1, and 3*IT, for IT = 1 to NT.
!
!    Output, integer NL, the number of border line segments.
!
!    Output, integer IPL(6*NDP), where the point numbers of the end
!    points of the (il)th border line segment and its respective triangle
!    number are to be stored as the (3*il-2)nd, (3*il-1)st, and (3*il)th
!    elements, il = 1,2,..., nl.
!
!    Workspace, integer IWL(18*NDP),
!
!    Workspace, integer IWP(NDP),
!
!    Workspace, real ( kind = KIND_Rdp ) WK(NDP).
!
  implicit none

  integer ndp

  ![waw] real ( kind = KIND_Rdp ) dsqf
  real ( kind = KIND_Rdp ) dsqi
  real ( kind = KIND_Rdp ) dsqmn
  integer errint
  character( len = * ) errmsg
  optional errint,errmsg
  ![waw] integer idxchg
  integer il
  integer ilf
  integer iliv
  integer ilt3
  integer ilvs
  integer ip
  integer ip1
  integer ip1p1
  integer ip2
  integer ip3
  integer ipl(6*ndp)
  integer ipl1
  integer ipl2
  integer iplj1
  integer iplj2
  integer ipmn1
  integer ipmn2
  integer ipt(6*ndp-15)
  integer ipt1
  integer ipt2
  integer ipt3
  integer ipti
  integer ipti1
  integer ipti2
  integer irep
  integer it
  integer it1t3
  integer it2t3
  integer itf(2)
  integer its
  integer itt3
  integer itt3r
  integer iwl(18*ndp)
  integer iwp(ndp)
  integer ixvs
  integer ixvspv
  integer jl1
  integer jl2
  integer jlt3
  integer jp
  integer jp1
  integer jp2
  integer jpc
  integer jpmn
  integer jpmx
  integer jwl
  integer jwl1
  integer jwl1mn
  integer nl
  integer nl0
  integer nlf
  integer nlfc
  integer nlft2
  integer nln
  integer nlnt3
  integer nlsh
  integer nlsht3
  integer nlt3
  integer, parameter :: nrep = 100
  integer nt
  integer nt0
  integer ntf
  integer ntt3
  integer ntt3p3
  real ( kind = KIND_Rdp ) sp
  real ( kind = KIND_Rdp ) u1
  real ( kind = KIND_Rdp ) u2
  real ( kind = KIND_Rdp ) u3
  real ( kind = KIND_Rdp ) v1
  real ( kind = KIND_Rdp ) v2
  real ( kind = KIND_Rdp ) v3
  real ( kind = KIND_Rdp ) vp
  real ( kind = KIND_Rdp ) wk(ndp)
  real ( kind = KIND_Rdp ) x1
  real ( kind = KIND_Rdp ) x2
  real ( kind = KIND_Rdp ) x3
  real ( kind = KIND_Rdp ) xd(ndp)
  real ( kind = KIND_Rdp ) xdmp
  real ( kind = KIND_Rdp ) y1
  real ( kind = KIND_Rdp ) y2
  real ( kind = KIND_Rdp ) y3
  real ( kind = KIND_Rdp ) yd(ndp)
  real ( kind = KIND_Rdp ) ydmp
!
!  Preliminary processing
!
  IF( PRESENT(errmsg) )errmsg = ""
  IF( PRESENT(errint) )errint = 0
  if ( ndp < 4 ) then
    if( present(errint) )errint = -9
    if( present(errmsg) )then
     write(errmsg,"(a)")'Input parameter &
       &NDP=',NDP,' out of range.  NDP >= 4. [LIB_BiVar:IDTANG]'
    end if
    return
  end if
!
!  Determine IPMN1 and IPMN2, the closest pair of data points.
!
  dsqmn = dsqf(xd(1),yd(1),xd(2),yd(2))
  ipmn1 = 1
  ipmn2 = 2

  do ip1 = 1, ndp-1

    x1 = xd(ip1)
    y1 = yd(ip1)
    ip1p1 = ip1+1

    do ip2 = ip1p1, ndp

      dsqi = dsqf(x1,y1,xd(ip2),yd(ip2))

      if ( dsqi == 0.0 ) then
        if( present(errint) )errint = -10
        if( present(errmsg) )then
         write(errmsg,"(a)")'Two of the input data points are&
         & identical. [LIB_Bivar:IDTANG]'
        end if
        return
      end if

      if ( dsqi < dsqmn ) then
        dsqmn = dsqi
        ipmn1 = ip1
        ipmn2 = ip2
      end if

    end do

  end do
!
!  Compute the midpoint of the closest two data points.
!
  xdmp = (xd(ipmn1)+xd(ipmn2)) / 2.0D+00
  ydmp = (yd(ipmn1)+yd(ipmn2)) / 2.0D+00
!
!  Sort the other (NDP-2) data points in ascending order of
!  distance from the midpoint and store the sorted data point
!  numbers in the IWP array.
!
  jp1 = 2

  do ip1 = 1, ndp
    if ( ip1 /= ipmn1 .and. ip1 /= ipmn2 ) then
      jp1 = jp1+1
      iwp(jp1) = ip1
      wk(jp1) = dsqf(xdmp,ydmp,xd(ip1),yd(ip1))
    end if
  end do

  do jp1 = 3, ndp-1

    dsqmn = wk(jp1)
    jpmn = jp1

    do jp2 = jp1, ndp
      if ( wk(jp2) < dsqmn ) then
        dsqmn = wk(jp2)
        jpmn = jp2
      end if
    end do

    its = iwp(jp1)
    iwp(jp1) = iwp(jpmn)
    iwp(jpmn) = its
    wk(jpmn) = wk(jp1)

  end do
!
!  If necessary, modify the ordering in such a way that the
!  first three data points are not collinear.
!
  x1 = xd(ipmn1)
  y1 = yd(ipmn1)
  x2 = xd(ipmn2)
  y2 = yd(ipmn2)

  do jp = 3, ndp

    ip = iwp(jp)
    sp = spdt(xd(ip),yd(ip),x1,y1,x2,y2)
    vp = vpdt(xd(ip),yd(ip),x1,y1,x2,y2)

    if ( ( abs ( sp ) * epsilon ( sp ) ) < abs ( vp ) ) then
      go to 37
    end if

  end do

  if( present(errint) )errint = -9
  if( present(errmsg) )then
    write(errmsg,"(a)")'All colinear data points. [LIB_Bivar:IDTANG]'
  end if
  return

   37 continue

  if ( jp /= 3 ) then

    jpmx = jp

    do jpc = 4, jpmx
      jp = jpmx+4-jpc
      iwp(jp) = iwp(jp-1)
    end do

    iwp(3) = ip

  end if
!
!  Form the first triangle.
!
!  Store point numbers of the vertexes of the triangle in the IPT array,
!  store point numbers of the border line segments and the triangle number in
!  the IPL array.
!
  ip1 = ipmn1
  ip2 = ipmn2
  ip3 = iwp(3)

  if ( vpdt(xd(ip1),yd(ip1),xd(ip2),yd(ip2),xd(ip3),yd(ip3)) < 0.0D+00 ) then
    ip1 = ipmn2
    ip2 = ipmn1
  end if

  nt0 = 1
  ntt3 = 3
  ipt(1) = ip1
  ipt(2) = ip2
  ipt(3) = ip3
  nl0 = 3
  nlt3 = 9
  ipl(1) = ip1
  ipl(2) = ip2
  ipl(3) = 1
  ipl(4) = ip2
  ipl(5) = ip3
  ipl(6) = 1
  ipl(7) = ip3
  ipl(8) = ip1
  ipl(9) = 1
!
!  Add the remaining data points, one by one.
!
  do jp1 = 4, ndp

    ip1 = iwp(jp1)
    x1 = xd(ip1)
    y1 = yd(ip1)
!
!  Determine the first invisible and visible border line segments, iliv and
!  ilvs.
!
    do il = 1, nl0

      ip2 = ipl(3*il-2)
      ip3 = ipl(3*il-1)
      x2 = xd(ip2)
      y2 = yd(ip2)
      x3 = xd(ip3)
      y3 = yd(ip3)
      sp = spdt(x1,y1,x2,y2,x3,y3)
      vp = vpdt(x1,y1,x2,y2,x3,y3)

      if ( il == 1 ) then
        ixvs = 0
        if ( vp <= - abs ( sp ) * epsilon ( sp ) ) then
          ixvs = 1
        end if
        iliv = 1
        ilvs = 1
        go to 53
      end if

      ixvspv = ixvs

      if ( vp <= - abs ( sp ) * epsilon ( sp ) ) then
        ixvs = 1
        if(ixvspv==1)      go to 53
        ilvs = il
        if(iliv/=1)        go to 54
        go to 53
      end if

      ixvs = 0

      if ( ixvspv /= 0 ) then
        iliv = il
        if ( ilvs /= 1 ) then
          go to 54
        end if
      end if

53     continue

    end do

    if ( iliv == 1 .and. ilvs == 1 ) then
      ilvs = nl0
    end if

54  continue

    if ( ilvs < iliv ) then
      ilvs = ilvs+nl0
    end if
!
!  Shift (rotate) the IPL array to have the invisible border
!  line segments contained in the first part of the array.
!
55   continue

    if ( iliv /= 1 ) then

      nlsh = iliv-1
      nlsht3 = nlsh*3

      do jl1 = 1,nlsht3
        jl2 = jl1+nlt3
        ipl(jl2) = ipl(jl1)
      end do

      do jl1 = 1,nlt3
        jl2 = jl1+nlsht3
        ipl(jl1) = ipl(jl2)
      end do

      ilvs = ilvs-nlsh

    end if
!
!  Add triangles to the IPT array,
!  update border line segments in the IPL array,
!  set flags for the border line segments to be reexamined in the IWL array.
!
    jwl = 0

    do il = ilvs, nl0

      ilt3 = il*3
      ipl1 = ipl(ilt3-2)
      ipl2 = ipl(ilt3-1)
      it   = ipl(ilt3)
!
!  Add a triangle to the IPT array.
!
      nt0 = nt0+1
      ntt3 = ntt3+3
      ipt(ntt3-2) = ipl2
      ipt(ntt3-1) = ipl1
      ipt(ntt3)   = ip1
!
!  Update border line segments in the IPL array.
!
      if ( il == ilvs ) then
        ipl(ilt3-1) = ip1
        ipl(ilt3)   = nt0
      end if

      if ( il == nl0 ) then
        nln = ilvs+1
        nlnt3 = nln*3
        ipl(nlnt3-2) = ip1
        ipl(nlnt3-1) = ipl(1)
        ipl(nlnt3)   = nt0
      end if
!
!  Determine the vertex that does not lie on the border
!  line segments.
!
      itt3 = it*3
      ipti = ipt(itt3-2)

      if ( ipti == ipl1 .or. ipti == ipl2 ) then
        ipti = ipt(itt3-1)
        if ( ipti == ipl1 .or. ipti == ipl2 ) then
          ipti = ipt(itt3)
        end if
      end if
!
!  Check if the exchange is necessary.
!
      if ( idxchg(xd,yd,ip1,ipti,ipl1,ipl2) /= 0 ) then
!
!  Modify the IPT array.
!
        ipt(itt3-2) = ipti
        ipt(itt3-1) = ipl1
        ipt(itt3)   = ip1
        ipt(ntt3-1) = ipti
        if(il==ilvs)  ipl(ilt3) = it
        if(il==nl0.and.ipl(3)==it)      ipl(3) = nt0
!
!  Set flags in the IWL array.
!
        jwl = jwl+4
        iwl(jwl-3) = ipl1
        iwl(jwl-2) = ipti
        iwl(jwl-1) = ipti
        iwl(jwl)   = ipl2

      end if

    end do

    nl0 = nln
    nlt3 = nlnt3
    nlf = jwl/2

    if ( nlf == 0 ) then
      cycle
    end if
!
!  Improve triangulation.
!
    ntt3p3 = ntt3+3

    do irep = 1, nrep

      do ilf = 1,nlf

        ipl1 = iwl(2*ilf-1)
        ipl2 = iwl(2*ilf)
!
!  Locate in the ipt array two triangles on both sides of
!  the flagged line segment.
!
        ntf = 0

        do itt3r = 3,ntt3,3
          itt3 = ntt3p3-itt3r
          ipt1 = ipt(itt3-2)
          ipt2 = ipt(itt3-1)
          ipt3 = ipt(itt3)
          if(ipl1/=ipt1.and.ipl1/=ipt2.and. ipl1/=ipt3)      go to 71
          if(ipl2/=ipt1.and.ipl2/=ipt2.and. ipl2/=ipt3)      go to 71
          ntf = ntf+1
          itf(ntf) = itt3/3
          if(ntf==2)     go to 72
71         continue
        end do

        if ( ntf < 2 )       go to 76
!
!  Determine the vertexes of the triangles that do not lie
!  on the line segment.
!
72       continue

        it1t3 = itf(1)*3
        ipti1 = ipt(it1t3-2)

        if ( ipti1 == ipl1 .or. ipti1 == ipl2 ) then

          ipti1 = ipt(it1t3-1)

          if ( ipti1 == ipl1 .or. ipti1 == ipl2 ) then
            ipti1 = ipt(it1t3)
          end if

        end if

        it2t3 = itf(2)*3
        ipti2 = ipt(it2t3-2)
        if(ipti2/=ipl1.and.ipti2/=ipl2)    go to 74
        ipti2 = ipt(it2t3-1)
        if(ipti2/=ipl1.and.ipti2/=ipl2)    go to 74
        ipti2 = ipt(it2t3)
!
!  Check if the exchange is necessary.
!
74       continue

        if(idxchg(xd,yd,ipti1,ipti2,ipl1,ipl2)==0) then
          go to 76
        end if
!
!  Modify the IPT array.
!
        ipt(it1t3-2) = ipti1
        ipt(it1t3-1) = ipti2
        ipt(it1t3)   = ipl1
        ipt(it2t3-2) = ipti2
        ipt(it2t3-1) = ipti1
        ipt(it2t3)   = ipl2
!
!  Set new flags.
!
        jwl = jwl+8
        iwl(jwl-7) = ipl1
        iwl(jwl-6) = ipti1
        iwl(jwl-5) = ipti1
        iwl(jwl-4) = ipl2
        iwl(jwl-3) = ipl2
        iwl(jwl-2) = ipti2
        iwl(jwl-1) = ipti2
        iwl(jwl)   = ipl1
        do jlt3 = 3,nlt3,3
          iplj1 = ipl(jlt3-2)
          iplj2 = ipl(jlt3-1)

          if((iplj1==ipl1.and.iplj2==ipti2).or. &
             (iplj2==ipl1.and.iplj1==ipti2)) then
                               ipl(jlt3) = itf(1)
          end if

          if((iplj1==ipl2.and.iplj2==ipti1).or. &
             (iplj2==ipl2.and.iplj1==ipti1)) then
                              ipl(jlt3) = itf(2)
          end if

        end do

76       continue

      end do

      nlfc = nlf
      nlf = jwl/2
!
!  Reset the IWL array for the next round.
!
      if ( nlf == nlfc ) then
        exit
      end if

      jwl1mn = 2*nlfc+1
      nlft2 = nlf*2

      do jwl1 = jwl1mn,nlft2
        jwl = jwl1+1-jwl1mn
        iwl(jwl) = iwl(jwl1)
      end do

      nlf = jwl / 2

    end do

  end do
!
!  Rearrange the IPT array so that the vertexes of each triangle
!  are listed counter-clockwise.
!
  do itt3 = 3, ntt3, 3

    ip1 = ipt(itt3-2)
    ip2 = ipt(itt3-1)
    ip3 = ipt(itt3)

    if(vpdt(xd(ip1),yd(ip1),xd(ip2),yd(ip2),xd(ip3),yd(ip3)) < 0.0D+00 ) then
      ipt(itt3-2) = ip2
      ipt(itt3-1) = ip1
    end if

  end do

  nt = nt0
  nl = nl0

  return
end subroutine

function idxchg ( x, y, i1, i2, i3, i4 )

!*******************************************************************************
!
!! IDXCHG determines whether two triangles should be exchanged.
!
!  Discussion:
!
!    The max-min-angle criterion of C L Lawson is used.
!
!  Modified:
!
!    04 June 2003
!
!  Reference:
!
!    Hiroshi Akima,
!    Algorithm 526,
!    A Method of Bivariate Interpolation and Smooth Surface Fitting
!    for Values Given at Irregularly Distributed Points,
!    ACM Transactions on Mathematical Software,
!    Volume 4, Number 2, June 1978.
!
!  Parameters:
!
!    Input, real ( kind = KIND_Rdp ) X(*), Y(*), the coordinates of the data points.
!
!    Input, integer I1, I2, I3, I4, are the point numbers of
!    four points P1, P2, P3, and P4 that form a quadrilateral,
!    with P3 and P4 connected diagonally.
!
!    Output, integer IDXCHG, reports whether the triangles should be
!    exchanged:
!    0, no exchange is necessary.
!    1, an exchange is necessary.
!
  implicit none

  real ( kind = KIND_Rdp ) a1sq
  real ( kind = KIND_Rdp ) a2sq
  real ( kind = KIND_Rdp ) a3sq
  real ( kind = KIND_Rdp ) a4sq
  real ( kind = KIND_Rdp ) c1sq
  real ( kind = KIND_Rdp ) c3sq
  integer i1
  integer i2
  integer i3
  integer i4
  integer idx
  integer idxchg
  real ( kind = KIND_Rdp ) c2sq
  real ( kind = KIND_Rdp ) c4sq
  real ( kind = KIND_Rdp ) b1sq
  real ( kind = KIND_Rdp ) b2sq
  real ( kind = KIND_Rdp ) b3sq
  real ( kind = KIND_Rdp ) b4sq
  real ( kind = KIND_Rdp ) s1sq
  real ( kind = KIND_Rdp ) s2sq
  real ( kind = KIND_Rdp ) s3sq
  real ( kind = KIND_Rdp ) s4sq
  real ( kind = KIND_Rdp ) u1
  real ( kind = KIND_Rdp ) u2
  real ( kind = KIND_Rdp ) u3
  real ( kind = KIND_Rdp ) u4
  real ( kind = KIND_Rdp ) x(*)
  real ( kind = KIND_Rdp ) x1
  real ( kind = KIND_Rdp ) x2
  real ( kind = KIND_Rdp ) x3
  real ( kind = KIND_Rdp ) x4
  real ( kind = KIND_Rdp ) y(*)
  real ( kind = KIND_Rdp ) y1
  real ( kind = KIND_Rdp ) y2
  real ( kind = KIND_Rdp ) y3
  real ( kind = KIND_Rdp ) y4
EQUIVALENCE (C2SQ,C1SQ),(A3SQ,B2SQ),(B3SQ,A1SQ),&
            (A4SQ,B1SQ),(B4SQ,A2SQ),(C4SQ,C3SQ)
!
!  Preliminary processing
!
x1=x(i1)
y1=y(i1)
x2=x(i2)
y2=y(i2)
x3=x(i3)
y3=y(i3)
x4=x(i4)
y4=y(i4)

idx=0

u3=(y2-y3)*(x1-x3)-(x2-x3)*(y1-y3)
u4=(y1-y4)*(x2-x4)-(x1-x4)*(y2-y4)

if(u3*u4.le.0.0)    go to 30

u1=(y3-y1)*(x4-x1)-(x3-x1)*(y4-y1)
u2=(y4-y2)*(x3-x2)-(x4-x2)*(y3-y2)
a1sq=(x1-x3)**2+(y1-y3)**2
b1sq=(x4-x1)**2+(y4-y1)**2
c1sq=(x3-x4)**2+(y3-y4)**2
a2sq=(x2-x4)**2+(y2-y4)**2
b2sq=(x3-x2)**2+(y3-y2)**2
c3sq=(x2-x1)**2+(y2-y1)**2
s1sq=u1*u1/(c1sq*max(a1sq,b1sq))
s2sq=u2*u2/(c2sq*max(a2sq,b2sq))
s3sq=u3*u3/(c3sq*max(a3sq,b3sq))
s4sq=u4*u4/(c4sq*max(a4sq,b4sq))

if(min(s1sq,s2sq).lt.min(s3sq,s4sq)) idx=1
30 CONTINUE
idxchg=idx

return
end function

function spdt(u1,v1,u2,v2,u3,v3)
  real ( kind = KIND_Rdp ) u1
  real ( kind = KIND_Rdp ) u2
  real ( kind = KIND_Rdp ) u3
  real ( kind = KIND_Rdp ) v1
  real ( kind = KIND_Rdp ) v2
  real ( kind = KIND_Rdp ) v3
  real ( kind = KIND_Rdp ) spdt

  spdt = (u1-u2)*(u3-u2)+(v1-v2)*(v3-v2)

end function

function vpdt(u1,v1,u2,v2,u3,v3)
  real ( kind = KIND_Rdp ) u1
  real ( kind = KIND_Rdp ) u2
  real ( kind = KIND_Rdp ) u3
  real ( kind = KIND_Rdp ) v1
  real ( kind = KIND_Rdp ) v2
  real ( kind = KIND_Rdp ) v3
  real ( kind = KIND_Rdp ) vpdt

  vpdt = (u1-u3)*(v2-v3)-(v1-v3)*(u2-u3)

end function

function dsqf(u1,v1,u2,v2)
  real ( kind = KIND_Rdp ) u1
  real ( kind = KIND_Rdp ) u2
  real ( kind = KIND_Rdp ) v1
  real ( kind = KIND_Rdp ) v2
  real ( kind = KIND_Rdp ) dsqf

 dsqf = (u2-u1)**2+(v2-v1)**2

end function
END MODULE
