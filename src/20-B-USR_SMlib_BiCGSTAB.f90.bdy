  !  SMLIB 1.0b
  !  Copyright (C) 1996 Ernst A. Meese
  !    Refer to the file copyright.doc for details and important disclaimer.
  !
      REAL(prec), DIMENSION(A%N),           INTENT (IN)    :: b
      LOGICAL,                    OPTIONAL, INTENT (IN)    :: init
      REAL(prec),                 OPTIONAL, INTENT (IN)    :: tol, reduction_factor
      INTEGER,                    OPTIONAL, INTENT (IN)    :: min_it, max_it, INFO
      INTEGER,                    OPTIONAL, INTENT (OUT)   :: no_of_its
      REAL(prec),                 OPTIONAL, INTENT (OUT)   :: residual_norm
      type(ConvHist),             OPTIONAL, INTENT (OUT)   :: History
!---
      INTEGER                        :: ic, min_iter, max_iter, infolevel
      REAL(prec)                     :: toler, norm
      REAL(prec)                     :: alpha, beta, rho, omega, temp, c, norm_s, norm_t
      REAL(prec), DIMENSION ( A % N) :: p, r, r_bar, s, t, v, y, z
!--- 
      Call Check_Options ()

      IF (infolevel/=SMLIB_NONE) Print F1, 'BiCGSTAB-P', toler, min_iter, max_iter
      ic = 0; r = b - A*x; r_bar = r; rho = 1; alpha = 1; omega = 1; p = 0; v = 0
      DO
         norm = sqrt(dot_product(r,r))
         if (present(history)) call SetIterationHistory (history, norm)
         IF (infolevel==SMLIB_ALL) Print F2, ic, norm
         IF ((norm<=toler .or. ic>=max_iter) .and. ic>=min_iter) EXIT
         ic = ic + 1 
         temp = dot_product (r_bar, r)
         beta = alpha*(temp/(rho+SIGN(EPSILON(1.0_prec),rho) ))/omega ; rho = temp
         p = r + beta*(p-omega*v)
         call solve (PC, p, y)
         v = A * y
         alpha = rho / (temp+SIGN(EPSILON(1.0_prec),temp))
         s = r - alpha*v
         call solve(PC, s, z)
         t = A*z

!  Stabilizing BiCGSTAB by alternative evaluation of omega

         norm_s = sqrt(dot_product(s,s))
         norm_t = sqrt(dot_product(t,t))
         c = dot_product (s,t)/(norm_s*norm_t)
         omega = sign(max(abs(c),0.7_prec),c)*norm_s/norm_t

         x = x + alpha*y + omega*z
         r = s - omega*t

      END DO

      IF (infolevel==SMLIB_SOME) Print F3, ic, norm, sqrt(sum((b-A*x)**2))

      INCLUDE '20-B-USR_SMlib_check-options.f90.bdy'

