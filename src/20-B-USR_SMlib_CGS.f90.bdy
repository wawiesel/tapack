  !  SMLIB 1.0b
  !  Copyright (C) 1996 Ernst A. Meese
  !    Refer to the file copyright.doc for details and important disclaimer.
  !
      REAL(prec), DIMENSION(:),             INTENT (INOUT) :: x
      REAL(prec), DIMENSION(A%N),           INTENT (IN)    :: b
      LOGICAL,                    OPTIONAL, INTENT (IN)    :: init
      REAL(prec),                 OPTIONAL, INTENT (IN)    :: tol, reduction_factor
      INTEGER,                    OPTIONAL, INTENT (IN)    :: min_it, max_it, INFO
      INTEGER,                    OPTIONAL, INTENT (OUT)   :: no_of_its
      REAL(prec),                 OPTIONAL, INTENT (OUT)   :: residual_norm
      TYPE(ConvHist),             OPTIONAL, INTENT (OUT)   :: History
!---
      INTEGER                              :: ic, min_iter, max_iter, infolevel
      REAL(prec)                           :: toler, norm
      REAL(prec)                           :: alpha, beta, rho, temp
      REAL(prec), DIMENSION ( A % N)       :: r, r_bar, u, v, p, q, z
!---

      CALL Check_Options ()

      IF (infolevel/=SMLIB_NONE) PRINT F1, 'CGS', toler, min_iter, max_iter
      ic = 0; r = b - A*x; r_bar = r; rho = 1; p=0; q=0
      DO
         norm = SQRT(DOT_PRODUCT(r,r))
         IF (PRESENT(history)) CALL SetIterationHistory(history,norm)
         IF (infolevel==SMLIB_ALL) PRINT F2, ic, norm
         IF ((norm<=toler .or. ic>=max_iter) .and. ic>=min_iter) EXIT
         ic = ic + 1
         temp = DOT_PRODUCT (r_bar, r)
         beta = temp/rho; rho = temp
         u = r + beta * q
         p = u + beta * (q + beta*p)
         CALL solve (PC, p, v); v = A*v
         alpha = rho / DOT_PRODUCT (r_bar, v)
         q = u - alpha * v
         CALL solve ( PC, u + q, z); z = alpha*z
         x = x + z
         r = r - A*z
      END DO

      IF (infolevel==SMLIB_SOME) PRINT F3, ic, norm, SQRT(SUM((b-A*x)**2))

      INCLUDE '20-B-USR_SMlib_check-options.f90.bdy'
