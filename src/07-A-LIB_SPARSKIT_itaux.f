      MODULE LIB_SPARSKIT_itaux
      use LIB_SPARSKIT_ilut !!((06-A-LIB_SPARSKIT_ilut.f))
      use FUN_Default       !!((04-A-FUN_Default.f90))
      use FUN_Error         !!((04-A-FUN_Error.f90))
      use VAR_Units         !!((03-A-VAR_Units.f90))
      use FUN_STR           !!((05-B-FUN_STR.f90))

        public :: runrc,dump,ilut

        CONTAINS

        subroutine runrc(n,rhs,sol,ipar,fpar,wk,guess,a,ja,ia,
     +     au,jau,ju,solver,okprint,printunit,residit,residval)
      implicit none
        integer n,ipar(16),ia(n+1),ja(*),ju(*),jau(*)
      real*8 fpar(16),rhs(n),sol(n),guess(n),wk(*),a(*),au(*)
        logical,intent(in),optional :: okprint
      integer,intent(in),optional :: printunit
        integer,intent(out),optional :: residit(:)
      real*8 ,intent(out),optional :: residval(:)
      external solver
c-----------------------------------------------------------------------
c     the actual tester. It starts the iterative linear system solvers
c     with a initial guess suppied by the user.
c
c     The structure {au, jau, ju} is assumed to have the output from
c     the ILU* routines in ilut.f.
c
c-----------------------------------------------------------------------
c     local variables
c
      integer i, iou, its, ii
      real*8 res
        logical okprint_
        integer printunit_
        character(100) :: ms
c[waw]-not external anymore
c       real*8 dnrm2
c     real dtime, dt(2), time
c     external dtime
c[waw]-not external anymore
c[waw]     external dnrm2
      save its,res
c
c     ipar(2) can be 0, 1, 2, please don't use 3
c
      if (ipar(2).gt.2) then
         print *, 'I can not do both left and right preconditioning.'
         return
      endif
c
c     normal execution
c
      ii = 0

        ![waw] get defaults and initialize
        ms=REPEAT(' ',LEN(ms))
        okprint_ = DEFAULT(.TRUE.,okprint)
        printunit_ = DEFAULT(DEFAULT_OUTPUT_UNIT,printunit)
        if( present(residval) )residval = ERROR(residval)
        if( present(residit) )residit = ERROR(residit)

        its = 0
      res = 0.0D0
c
      do i = 1, n
         sol(i) = guess(i)
      enddo
c
      iou = printunit_ ![waw]
      ipar(1) = 0

c     time = dtime(dt)
 10   call solver(n,rhs,sol,ipar,fpar,wk)
c
c     output the residuals
c

        if ( its/=ipar(7) .and. res/=fpar(5) ) then
         its = ipar(7)
           res = fpar(5)
           if( okprint_ )write (iou, *) its, real(res)
           if( present(residval) .AND. present(residit) )then
          ii = ii + 1
                residit(ii) = ii
                residval(ii) = res
           end if
      endif
c
      if (ipar(1).eq.1) then
         call amux(n, wk(ipar(8)), wk(ipar(9)), a, ja, ia)
           goto 10
      else if (ipar(1).eq.2) then
         call atmux(n, wk(ipar(8)), wk(ipar(9)), a, ja, ia)
           goto 10
      else if (ipar(1).eq.3 .or. ipar(1).eq.5) then
         call lusol(n,wk(ipar(8)),wk(ipar(9)),au,jau,ju)
           goto 10
      else if (ipar(1).eq.4 .or. ipar(1).eq.6) then
         call lutsol(n,wk(ipar(8)),wk(ipar(9)),au,jau,ju)
           goto 10
      else if (ipar(1).le.0) then
         select case(ipar(1))
          case( 0);ms='Iterative solver satisfied convergence test.'
          case(-1);ms='Iterative solver iterated too many times.'
          case(-2);ms='Iterative solver not given enough work space.'//
     &                ' The work space should have '//
     &                TRIM(STR(ipar(4)))//' elements.'
          case(-3);ms= 'Iterative sovler is facing a break-down.'
                     select case(ipar(12))
                      case(-1);ms=TRIM(ms)//
     &                      ' Zero input vector.'
                      case(-2);ms=TRIM(ms)//
     &                      ' Abnormal numbers in input vector.'
                      case(-3);ms=TRIM(ms)//
     &                      ' Input vector is linear combo.'
                      case(-4);ms=TRIM(ms)//
     &                      ' Triangular system has null rank.'
                   end select
            case(-4);ms='Tolerances are negative.'
            case(-9);ms='Abnormal number found while detecting '//
     &                  'breakdown.'
            case(-10);ms='Return due to non-numerical reason.'
            case default;ms='Iterative solver terminated. code ='//
     &                      TRIM(STR(ipar(1)))
         end select
         if(okprint_ )write (iou, *)"#  "//TRIM(ms)
      endif
c     time = dtime(dt)
      if( okprint_ )write (iou, *) ipar(7), real(fpar(6))
      if( okprint_ )write (iou, *) '# return code =',ipar(1),
     +     '    convergence rate =', fpar(7)
c     write (iou, *) '# total execution time (sec)', time
c
c     check the error
c
      call amux(n,sol,wk,a,ja,ia)
      do i = 1, n
         wk(n+i) = sol(i) -1.0D0
         wk(i) = wk(i) - rhs(i)
      enddo
      if( okprint_ )write (iou, *)
     &     '# the actual residual norm is', dnrm2(n,wk,1)
      if( okprint_ )write (iou, *)
     &     '# the error norm is', dnrm2(n,wk(1+n),1)
c
      return
      end subroutine
c-----end-of-runrc
c-----------------------------------------------------------------------
      function distdot(n,x,ix,y,iy)
        use LIB_SPARSKIT_blas1,ONLY: ddot ![waw]
        integer n, ix, iy
      real*8 distdot, x(*), y(*)
      ![waw]-added with module
        ![waw]real*8 ddot
        ![waw]external ddot
      distdot = ddot(n,x,ix,y,iy)
      return
      end function
c-----end-of-distdot
c-----------------------------------------------------------------------
c
      function afun (x,y,z)
      real*8 afun, x,y, z
      afun = -1.0D0
      return
      end function

      function bfun (x,y,z)
      real*8 bfun, x,y, z
      bfun = -1.0D0
      return
      end function

      function cfun (x,y,z)
      real*8 cfun, x,y, z
      cfun = -1.0D0
      return
      end function

      function dfun (x,y,z)
      real*8 dfun, x,y, z, gammax, gammay, alpha
      common /func/ gammax, gammay, alpha
      dfun = gammax*exp(x*y)
      return
      end function

      function efun (x,y,z)
      real*8 efun, x,y, z, gammax, gammay, alpha
      common /func/ gammax, gammay, alpha
      efun = gammay*exp(-x*y)
      return
      end function

      function ffun (x,y,z)
      real*8 ffun, x,y, z
      ffun = 0.0D0
      return
      end function

      function gfun (x,y,z)
      real*8 gfun, x,y, z, gammax, gammay, alpha
      common /func/ gammax, gammay, alpha
      gfun = alpha
      return
      end function

      function hfun (x,y,z)
      real*8 hfun, x,y, z, gammax, gammay, alpha
      common /func/ gammax, gammay, alpha
      hfun = alpha * sin(gammax*x+gammay*y-z)
      return
      end function


      function betfun(side, x, y, z)
      real*8 betfun, x, y, z
      character*2 side
      betfun = 1.0
      return
      end function

      function gamfun(side, x, y, z)
      real*8 gamfun, x, y, z
      character*2 side
      if (side.eq.'x2') then
         gamfun = 5.0
      else if (side.eq.'y1') then
         gamfun = 2.0
      else if (side.eq.'y2') then
         gamfun = 7.0
      else
         gamfun = 0.0
      endif
      return
      end function
c-----------------------------------------------------------------------
c     functions for the block PDE's
c-----------------------------------------------------------------------
      subroutine afunbl (nfree,x,y,z,coeff)
      return
      end subroutine
c
      subroutine bfunbl (nfree,x,y,z,coeff)
      return
      end subroutine

      subroutine cfunbl (nfree,x,y,z,coeff)
c
      return
      end subroutine

      subroutine dfunbl (nfree,x,y,z,coeff)

      return
      end subroutine
c
      subroutine efunbl (nfree,x,y,z,coeff)
      return
      end subroutine
c
      subroutine ffunbl (nfree,x,y,z,coeff)
      return
      end subroutine
c
      subroutine gfunbl (nfree,x,y,z,coeff)
      return
      end subroutine

      END MODULE
