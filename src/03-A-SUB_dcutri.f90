!!# SUBROUTINE MODULE: <SUB_dcutri>
Module SUB_dcutri

!!## PURPOSE
!! Computes the integral of a set of 2D functions <f1,f2,...,fN>
!! over a triangulated region.

!!## NOTES
!! This version is modified from the original version in TOMS to
!! have a function subroutine passed through the argument list
!! instead of declared external.


!!## ACCESS
Private
Public :: dcutri


!!## MODULE PROCEDURES
Contains

      Function d1mach (i)
!
!*********************************************************************72
!
!c D1MACH returns double precision machine-dependent constants.
!
!  Discussion:
!
!    D1MACH can be used to obtain machine-dependent parameters
!    for the local machine environment.  It is a function
!    with one input argument, and can be called as follows:
!
!      D = D1MACH ( I )
!
!    where I=1,...,5.  The output value of D above is
!    determined by the input value of I:.
!
!    D1MACH ( 1) = B**(EMIN-1), the smallest positive magnitude.
!    D1MACH ( 2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
!    D1MACH ( 3) = B**(-T), the smallest relative spacing.
!    D1MACH ( 4) = B**(1-T), the largest relative spacing.
!    D1MACH ( 5) = LOG10(B)
!
!  Modified:
!
!    06 December 2006
!
!  Author:
!
!    Phyllis Fox, Andrew Hall, Norman Schryer
!
!  Reference:
!
!    Phyllis Fox, Andrew Hall, Norman Schryer,
!    Algorithm 528:
!    Framework for a Portable Library,
!    ACM Transactions on Mathematical Software,
!    Volume 4, Number 2, June 1978, page 176-188.
!
!  Parameters:
!
!    Input, integer I, the index of the desired constant.
!
!    Output, double precision D1MACH, the value of the constant.
!
         Implicit None
!
         Double Precision d1mach
         Integer diver (4)
         Double Precision dmach (5)
         Integer i
         Integer large (4)
         Integer Log10 (4)
         Integer right (4)
         Integer small (4)
!
         Equivalence (dmach(1), small(1))
         Equivalence (dmach(2), large(1))
         Equivalence (dmach(3), right(1))
         Equivalence (dmach(4), diver(1))
         Equivalence (dmach(5), Log10(1))
!
!     MACHINE CONSTANTS FOR IEEE ARITHMETIC MACHINES, SUCH AS THE AT&T
!     3B SERIES AND MOTOROLA 68000 BASED MACHINES (E.G. SUN 3 AND AT&T
!     PC 7300), IN WHICH THE MOST SIGNIFICANT BYTE IS STORED FIRST.
!
! === MACHINE = IEEE.MOST-SIG-BYTE-FIRST
! === MACHINE = SUN
! === MACHINE = 68000
! === MACHINE = ATT.3B
! === MACHINE = ATT.7300
!     DATA SMALL(1),SMALL(2) /    1048576,          0 /
!     DATA LARGE(1),LARGE(2) / 2146435071,         -1 /
!     DATA RIGHT(1),RIGHT(2) / 1017118720,          0 /
!     DATA DIVER(1),DIVER(2) / 1018167296,          0 /
!     DATA LOG10(1),LOG10(2) / 1070810131, 1352628735 /
!
!     MACHINE CONSTANTS FOR IEEE ARITHMETIC MACHINES AND 8087-BASED
!     MICROS, SUCH AS THE IBM PC AND AT&T 6300, IN WHICH THE LEAST
!     SIGNIFICANT BYTE IS STORED FIRST.
!
! === MACHINE = IEEE.LEAST-SIG-BYTE-FIRST
! === MACHINE = 8087
! === MACHINE = IBM.PC
! === MACHINE = ATT.6300
!
         Data small (1), small (2) / 0, 1048576 /
         Data large (1), large (2) / - 1, 2146435071 /
         Data right (1), right (2) / 0, 1017118720 /
         Data diver (1), diver (2) / 0, 1018167296 /
         Data Log10 (1), Log10 (2) / 1352628735, 1070810131 /
!
!     MACHINE CONSTANTS FOR AMDAHL MACHINES.
!
! === MACHINE = AMDAHL
!      DATA SMALL(1),SMALL(2) /    1048576,          0 /
!      DATA LARGE(1),LARGE(2) / 2147483647,         -1 /
!      DATA RIGHT(1),RIGHT(2) /  856686592,          0 /
!      DATA DIVER(1),DIVER(2) /  873463808,          0 /
!      DATA LOG10(1),LOG10(2) / 1091781651, 1352628735 /
!
!     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
!
! === MACHINE = BURROUGHS.1700
!      DATA SMALL(1) / ZC00800000 /
!      DATA SMALL(2) / Z000000000 /
!      DATA LARGE(1) / ZDFFFFFFFF /
!      DATA LARGE(2) / ZFFFFFFFFF /
!      DATA RIGHT(1) / ZCC5800000 /
!      DATA RIGHT(2) / Z000000000 /
!      DATA DIVER(1) / ZCC6800000 /
!      DATA DIVER(2) / Z000000000 /
!      DATA LOG10(1) / ZD00E730E7 /
!      DATA LOG10(2) / ZC77800DC0 /
!
!     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM.
!
! === MACHINE = BURROUGHS.5700
!      DATA SMALL(1) / O1771000000000000 /
!      DATA SMALL(2) / O0000000000000000 /
!      DATA LARGE(1) / O0777777777777777 /
!      DATA LARGE(2) / O0007777777777777 /
!      DATA RIGHT(1) / O1461000000000000 /
!      DATA RIGHT(2) / O0000000000000000 /
!      DATA DIVER(1) / O1451000000000000 /
!      DATA DIVER(2) / O0000000000000000 /
!      DATA LOG10(1) / O1157163034761674 /
!      DATA LOG10(2) / O0006677466732724 /
!
!     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS.
!
! === MACHINE = BURROUGHS.6700
! === MACHINE = BURROUGHS.7700
!      DATA SMALL(1) / O1771000000000000 /
!      DATA SMALL(2) / O7770000000000000 /
!      DATA LARGE(1) / O0777777777777777 /
!      DATA LARGE(2) / O7777777777777777 /
!      DATA RIGHT(1) / O1461000000000000 /
!      DATA RIGHT(2) / O0000000000000000 /
!      DATA DIVER(1) / O1451000000000000 /
!      DATA DIVER(2) / O0000000000000000 /
!      DATA LOG10(1) / O1157163034761674 /
!      DATA LOG10(2) / O0006677466732724 /
!
!     MACHINE CONSTANTS FOR THE CONVEX C-120 (NATIVE MODE)
!     WITH OR WITHOUT -R8 OPTION
!
! === MACHINE = CONVEX.C1
! === MACHINE = CONVEX.C1.R8
!      DATA DMACH(1) / 5.562684646268007D-309 /
!      DATA DMACH(2) / 8.988465674311577D+307 /
!      DATA DMACH(3) / 1.110223024625157D-016 /
!      DATA DMACH(4) / 2.220446049250313D-016 /
!      DATA DMACH(5) / 3.010299956639812D-001 /
!
!     MACHINE CONSTANTS FOR THE CONVEX C-120 (IEEE MODE)
!     WITH OR WITHOUT -R8 OPTION
!
! === MACHINE = CONVEX.C1.IEEE
! === MACHINE = CONVEX.C1.IEEE.R8
!      DATA DMACH(1) / 2.225073858507202D-308 /
!      DATA DMACH(2) / 1.797693134862315D+308 /
!      DATA DMACH(3) / 1.110223024625157D-016 /
!      DATA DMACH(4) / 2.220446049250313D-016 /
!      DATA DMACH(5) / 3.010299956639812D-001 /
!
!     MACHINE CONSTANTS FOR THE CYBER 170/180 SERIES USING NOS (FTN5).
!
! === MACHINE = CYBER.170.NOS
! === MACHINE = CYBER.180.NOS
!      DATA SMALL(1) / O"00604000000000000000" /
!      DATA SMALL(2) / O"00000000000000000000" /
!      DATA LARGE(1) / O"37767777777777777777" /
!      DATA LARGE(2) / O"37167777777777777777" /
!      DATA RIGHT(1) / O"15604000000000000000" /
!      DATA RIGHT(2) / O"15000000000000000000" /
!      DATA DIVER(1) / O"15614000000000000000" /
!      DATA DIVER(2) / O"15010000000000000000" /
!      DATA LOG10(1) / O"17164642023241175717" /
!      DATA LOG10(2) / O"16367571421742254654" /
!
!     MACHINE CONSTANTS FOR THE CDC 180 SERIES USING NOS/VE
!
! === MACHINE = CYBER.180.NOS/VE
!      DATA SMALL(1) / Z"3001800000000000" /
!      DATA SMALL(2) / Z"3001000000000000" /
!      DATA LARGE(1) / Z"4FFEFFFFFFFFFFFE" /
!      DATA LARGE(2) / Z"4FFE000000000000" /
!      DATA RIGHT(1) / Z"3FD2800000000000" /
!      DATA RIGHT(2) / Z"3FD2000000000000" /
!      DATA DIVER(1) / Z"3FD3800000000000" /
!      DATA DIVER(2) / Z"3FD3000000000000" /
!      DATA LOG10(1) / Z"3FFF9A209A84FBCF" /
!      DATA LOG10(2) / Z"3FFFF7988F8959AC" /
!
!     MACHINE CONSTANTS FOR THE CYBER 205
!
! === MACHINE = CYBER.205
!      DATA SMALL(1) / X'9000400000000000' /
!      DATA SMALL(2) / X'8FD1000000000000' /
!      DATA LARGE(1) / X'6FFF7FFFFFFFFFFF' /
!      DATA LARGE(2) / X'6FD07FFFFFFFFFFF' /
!      DATA RIGHT(1) / X'FF74400000000000' /
!      DATA RIGHT(2) / X'FF45000000000000' /
!      DATA DIVER(1) / X'FF75400000000000' /
!      DATA DIVER(2) / X'FF46000000000000' /
!      DATA LOG10(1) / X'FFD04D104D427DE7' /
!      DATA LOG10(2) / X'FFA17DE623E2566A' /
!
!     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.
!
! === MACHINE = CDC.6000
! === MACHINE = CDC.7000
!      DATA SMALL(1) / 00604000000000000000B /
!      DATA SMALL(2) / 00000000000000000000B /
!      DATA LARGE(1) / 37767777777777777777B /
!      DATA LARGE(2) / 37167777777777777777B /
!      DATA RIGHT(1) / 15604000000000000000B /
!      DATA RIGHT(2) / 15000000000000000000B /
!      DATA DIVER(1) / 15614000000000000000B /
!      DATA DIVER(2) / 15010000000000000000B /
!      DATA LOG10(1) / 17164642023241175717B /
!      DATA LOG10(2) / 16367571421742254654B /
!
!     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3.
!
! === MACHINE = CRAY
!      DATA SMALL(1) / 201354000000000000000B /
!      DATA SMALL(2) / 000000000000000000000B /
!      DATA LARGE(1) / 577767777777777777777B /
!      DATA LARGE(2) / 000007777777777777776B /
!      DATA RIGHT(1) / 376434000000000000000B /
!      DATA RIGHT(2) / 000000000000000000000B /
!      DATA DIVER(1) / 376444000000000000000B /
!      DATA DIVER(2) / 000000000000000000000B /
!      DATA LOG10(1) / 377774642023241175717B /
!      DATA LOG10(2) / 000007571421742254654B /
!
!     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
!
!     NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING LINE -
!     STATIC DMACH(5)
!
! === MACHINE = DATA_GENERAL.ECLIPSE.S/200
!      DATA SMALL/20K,3*0/,LARGE/77777K,3*177777K/
!      DATA RIGHT/31420K,3*0/,DIVER/32020K,3*0/
!      DATA LOG10/40423K,42023K,50237K,74776K/
!
!     ELXSI 6400
!
! === MACHINE = ELSXI.6400
!      DATA SMALL(1), SMALL(2) / '00100000'X,'00000000'X /
!      DATA LARGE(1), LARGE(2) / '7FEFFFFF'X,'FFFFFFFF'X /
!      DATA RIGHT(1), RIGHT(2) / '3CB00000'X,'00000000'X /
!      DATA DIVER(1), DIVER(2) / '3CC00000'X,'00000000'X /
!      DATA LOG10(1), DIVER(2) / '3FD34413'X,'509F79FF'X /
!
!     MACHINE CONSTANTS FOR THE HARRIS 220
!     MACHINE CONSTANTS FOR THE HARRIS SLASH 6 AND SLASH 7
!
! === MACHINE = HARRIS.220
! === MACHINE = HARRIS.SLASH6
! === MACHINE = HARRIS.SLASH7
!      DATA SMALL(1),SMALL(2) / '20000000, '00000201 /
!      DATA LARGE(1),LARGE(2) / '37777777, '37777577 /
!      DATA RIGHT(1),RIGHT(2) / '20000000, '00000333 /
!      DATA DIVER(1),DIVER(2) / '20000000, '00000334 /
!      DATA LOG10(1),LOG10(2) / '23210115, '10237777 /
!
!     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES.
!     MACHINE CONSTANTS FOR THE HONEYWELL DPS 8/70 SERIES.
!
! === MACHINE = HONEYWELL.600/6000
! === MACHINE = HONEYWELL.DPS.8/70
!      DATA SMALL(1),SMALL(2) / O402400000000, O000000000000 /
!      DATA LARGE(1),LARGE(2) / O376777777777, O777777777777 /
!      DATA RIGHT(1),RIGHT(2) / O604400000000, O000000000000 /
!      DATA DIVER(1),DIVER(2) / O606400000000, O000000000000 /
!      DATA LOG10(1),LOG10(2) / O776464202324, O117571775714 /
!
!      MACHINE CONSTANTS FOR THE HP 2100
!      3 WORD DOUBLE PRECISION OPTION WITH FTN4
!
! === MACHINE = HP.2100.3_WORD_DP
!      DATA SMALL(1), SMALL(2), SMALL(3) / 40000B,       0,       1 /
!      DATA LARGE(1), LARGE(2), LARGE(3) / 77777B, 177777B, 177776B /
!      DATA RIGHT(1), RIGHT(2), RIGHT(3) / 40000B,       0,    265B /
!      DATA DIVER(1), DIVER(2), DIVER(3) / 40000B,       0,    276B /
!      DATA LOG10(1), LOG10(2), LOG10(3) / 46420B,  46502B,  77777B /
!
!      MACHINE CONSTANTS FOR THE HP 2100
!      4 WORD DOUBLE PRECISION OPTION WITH FTN4
!
! === MACHINE = HP.2100.4_WORD_DP
!      DATA SMALL(1), SMALL(2) /  40000B,       0 /
!      DATA SMALL(3), SMALL(4) /       0,       1 /
!      DATA LARGE(1), LARGE(2) /  77777B, 177777B /
!      DATA LARGE(3), LARGE(4) / 177777B, 177776B /
!      DATA RIGHT(1), RIGHT(2) /  40000B,       0 /
!      DATA RIGHT(3), RIGHT(4) /       0,    225B /
!      DATA DIVER(1), DIVER(2) /  40000B,       0 /
!      DATA DIVER(3), DIVER(4) /       0,    227B /
!      DATA LOG10(1), LOG10(2) /  46420B,  46502B /
!      DATA LOG10(3), LOG10(4) /  76747B, 176377B /
!
!     HP 9000
!
!      D1MACH(1) = 2.8480954D-306
!      D1MACH(2) = 1.40444776D+306
!      D1MACH(3) = 2.22044605D-16
!      D1MACH(4) = 4.44089210D-16
!      D1MACH(5) = 3.01029996D-1
!
! === MACHINE = HP.9000
!      DATA SMALL(1), SMALL(2) / 00040000000B, 00000000000B /
!      DATA LARGE(1), LARGE(2) / 17737777777B, 37777777777B /
!      DATA RIGHT(1), RIGHT(2) / 07454000000B, 00000000000B /
!      DATA DIVER(1), DIVER(2) / 07460000000B, 00000000000B /
!      DATA LOG10(1), LOG10(2) / 07764642023B, 12047674777B /
!
!     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
!     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86, AND
!     THE INTERDATA 3230 AND INTERDATA 7/32.
!
! === MACHINE = IBM.360
! === MACHINE = IBM.370
! === MACHINE = XEROX.SIGMA.5
! === MACHINE = XEROX.SIGMA.7
! === MACHINE = XEROX.SIGMA.9
! === MACHINE = SEL.85
! === MACHINE = SEL.86
! === MACHINE = INTERDATA.3230
! === MACHINE = INTERDATA.7/32
!      DATA SMALL(1),SMALL(2) / Z00100000, Z00000000 /
!      DATA LARGE(1),LARGE(2) / Z7FFFFFFF, ZFFFFFFFF /
!      DATA RIGHT(1),RIGHT(2) / Z33100000, Z00000000 /
!      DATA DIVER(1),DIVER(2) / Z34100000, Z00000000 /
!      DATA LOG10(1),LOG10(2) / Z41134413, Z509F79FF /
!
!     MACHINE CONSTANTS FOR THE INTERDATA 8/32
!     WITH THE UNIX SYSTEM FORTRAN 77 COMPILER.
!
!     FOR THE INTERDATA FORTRAN VII COMPILER REPLACE
!     THE Z'S SPECIFYING HEX CONSTANTS WITH Y'S.
!
! === MACHINE = INTERDATA.8/32.UNIX
!      DATA SMALL(1),SMALL(2) / Z'00100000', Z'00000000' /
!      DATA LARGE(1),LARGE(2) / Z'7EFFFFFF', Z'FFFFFFFF' /
!      DATA RIGHT(1),RIGHT(2) / Z'33100000', Z'00000000' /
!      DATA DIVER(1),DIVER(2) / Z'34100000', Z'00000000' /
!      DATA LOG10(1),LOG10(2) / Z'41134413', Z'509F79FF' /
!
!     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).
!
! === MACHINE = PDP-10.KA
!      DATA SMALL(1),SMALL(2) / "033400000000, "000000000000 /
!      DATA LARGE(1),LARGE(2) / "377777777777, "344777777777 /
!      DATA RIGHT(1),RIGHT(2) / "113400000000, "000000000000 /
!      DATA DIVER(1),DIVER(2) / "114400000000, "000000000000 /
!      DATA LOG10(1),LOG10(2) / "177464202324, "144117571776 /
!
!     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).
!
! === MACHINE = PDP-10.KI
!      DATA SMALL(1),SMALL(2) / "000400000000, "000000000000 /
!      DATA LARGE(1),LARGE(2) / "377777777777, "377777777777 /
!      DATA RIGHT(1),RIGHT(2) / "103400000000, "000000000000 /
!      DATA DIVER(1),DIVER(2) / "104400000000, "000000000000 /
!      DATA LOG10(1),LOG10(2) / "177464202324, "047674776746 /
!
!     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
!     32-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
!
! === MACHINE = PDP-11.32-BIT
!      DATA SMALL(1),SMALL(2) /    8388608,           0 /
!      DATA LARGE(1),LARGE(2) / 2147483647,          -1 /
!      DATA RIGHT(1),RIGHT(2) /  612368384,           0 /
!      DATA DIVER(1),DIVER(2) /  620756992,           0 /
!      DATA LOG10(1),LOG10(2) / 1067065498, -2063872008 /
!
!      DATA SMALL(1),SMALL(2) / O00040000000, O00000000000 /
!      DATA LARGE(1),LARGE(2) / O17777777777, O37777777777 /
!      DATA RIGHT(1),RIGHT(2) / O04440000000, O00000000000 /
!      DATA DIVER(1),DIVER(2) / O04500000000, O00000000000 /
!      DATA LOG10(1),LOG10(2) / O07746420232, O20476747770 /
!
!     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
!     16-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
!
! === MACHINE = PDP-11.16-BIT
!      DATA SMALL(1),SMALL(2) /    128,      0 /
!      DATA SMALL(3),SMALL(4) /      0,      0 /
!      DATA LARGE(1),LARGE(2) /  32767,     -1 /
!      DATA LARGE(3),LARGE(4) /     -1,     -1 /
!      DATA RIGHT(1),RIGHT(2) /   9344,      0 /
!      DATA RIGHT(3),RIGHT(4) /      0,      0 /
!      DATA DIVER(1),DIVER(2) /   9472,      0 /
!      DATA DIVER(3),DIVER(4) /      0,      0 /
!      DATA LOG10(1),LOG10(2) /  16282,   8346 /
!      DATA LOG10(3),LOG10(4) / -31493, -12296 /
!
!      DATA SMALL(1),SMALL(2) / O000200, O000000 /
!      DATA SMALL(3),SMALL(4) / O000000, O000000 /
!      DATA LARGE(1),LARGE(2) / O077777, O177777 /
!      DATA LARGE(3),LARGE(4) / O177777, O177777 /
!      DATA RIGHT(1),RIGHT(2) / O022200, O000000 /
!      DATA RIGHT(3),RIGHT(4) / O000000, O000000 /
!      DATA DIVER(1),DIVER(2) / O022400, O000000 /
!      DATA DIVER(3),DIVER(4) / O000000, O000000 /
!      DATA LOG10(1),LOG10(2) / O037632, O020232 /
!      DATA LOG10(3),LOG10(4) / O102373, O147770 /
!
!     MACHINE CONSTANTS FOR THE SEQUENT BALANCE 8000
!
! === MACHINE = SEQUENT.BALANCE.8000
!      DATA SMALL(1),SMALL(2) / $00000000,  $00100000 /
!      DATA LARGE(1),LARGE(2) / $FFFFFFFF,  $7FEFFFFF /
!      DATA RIGHT(1),RIGHT(2) / $00000000,  $3CA00000 /
!      DATA DIVER(1),DIVER(2) / $00000000,  $3CB00000 /
!      DATA LOG10(1),LOG10(2) / $509F79FF,  $3FD34413 /
!
!     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES. FTN COMPILER
!
! === MACHINE = UNIVAC.1100
!      DATA SMALL(1),SMALL(2) / O000040000000, O000000000000 /
!      DATA LARGE(1),LARGE(2) / O377777777777, O777777777777 /
!      DATA RIGHT(1),RIGHT(2) / O170540000000, O000000000000 /
!      DATA DIVER(1),DIVER(2) / O170640000000, O000000000000 /
!      DATA LOG10(1),LOG10(2) / O177746420232, O411757177572 /
!
!     MACHINE CONSTANTS FOR VAX 11/780
!     (EXPRESSED IN INTEGER AND HEXADECIMAL)
!    *** THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS***
!
! === MACHINE = VAX.11/780
!      DATA SMALL(1), SMALL(2) /        128,           0 /
!      DATA LARGE(1), LARGE(2) /     -32769,          -1 /
!      DATA RIGHT(1), RIGHT(2) /       9344,           0 /
!      DATA DIVER(1), DIVER(2) /       9472,           0 /
!      DATA LOG10(1), LOG10(2) /  546979738,  -805796613 /
!
!    ***THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSYEMS***
!      DATA SMALL(1), SMALL(2) / Z00000080, Z00000000 /
!      DATA LARGE(1), LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /
!      DATA RIGHT(1), RIGHT(2) / Z00002480, Z00000000 /
!      DATA DIVER(1), DIVER(2) / Z00002500, Z00000000 /
!      DATA LOG10(1), LOG10(2) / Z209A3F9A, ZCFF884FB /
!
!   MACHINE CONSTANTS FOR VAX 11/780 (G-FLOATING)
!     (EXPRESSED IN INTEGER AND HEXADECIMAL)
!    *** THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS***
!
!      DATA SMALL(1), SMALL(2) /         16,           0 /
!      DATA LARGE(1), LARGE(2) /     -32769,          -1 /
!      DATA RIGHT(1), RIGHT(2) /      15552,           0 /
!      DATA DIVER(1), DIVER(2) /      15568,           0 /
!      DATA LOG10(1), LOG10(2) /  1142112243, 2046775455 /
!
!    ***THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSYEMS***
!      DATA SMALL(1), SMALL(2) / Z00000010, Z00000000 /
!      DATA LARGE(1), LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /
!      DATA RIGHT(1), RIGHT(2) / Z00003CC0, Z00000000 /
!      DATA DIVER(1), DIVER(2) / Z00003CD0, Z00000000 /
!      DATA LOG10(1), LOG10(2) / Z44133FF3, Z79FF509F /
!
         If (i .Lt. 1 .Or. 5 .Lt. i) Then
            Write (*, '(a)') ' '
            Write (*, '(a)') 'D1MACH - Fatal error!'
            Write (*, '(a)') '  I out of bounds.'
            Stop
         End If
!
         d1mach = dmach (i)
!
         Return
      End Function


          Subroutine dchtri (numfun, mdiv, ver, numtri, minpts, maxpts, &
     & epsabs, epsrel, lenver, nw, restar, maxsub, minsub, ifail)
!
!*********************************************************************72
!
!c DCHTRI checks the validity of the input parameters to DCUTRI.
!
!  Discussion:
!
!    DCHTRI computes MAXSUB, MINSUB and IFAIL as
!    functions of the input parameters to DCUTRI,
!    and checks the validity of the input parameters to DCUTRI.
!
!  Modified:
!
!    09 December 2006
!
!  Author:
!
!    Jarle Berntsen, Terje Espelid
!
!  Reference:
!
!    Jarle Berntsen, Terje Espelid,
!    Algorithm 706:
!    DCUTRI, an Algorithm for Adaptive Cubature over a Collection
!    of Triangles,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1992, pages 329-342.
!
!  Parameters:
!
!    Input, NUMFUN Integer.
!            Number of components of the integral.
!
!    Input, MDIV   Integer.
!            MDIV is the number of triangles that are divided in
!            each subdivision step in DADTRI.
!            MDIV is chosen default to 1.
!            For efficient execution on parallel computers
!            with NPROC processors MDIV should be set equal to
!            the smallest integer such that MOD(4*MDIV,NPROC) = 0.
!
!    Input, VER    Real array of dimension (2,3,LENVER).
!            VER(1,K,L) and VER(2,K,L) are the x and y coordinates
!            respectively of vertex K in triangle L.
!            On entry VER(*,*,L) must contain the vertices of the
!            NUMTRI user specified triangles for L = 1,2,...,NUMTRI.
!
!    Input, NUMTRI Integer.
!            The number of triangles.
!
!    Input, MINPTS Integer.
!            Minimum number of function evaluations.
!
!    Input, MAXPTS Integer.
!            Maximum number of function evaluations.
!            The number of function evaluations over each subregion
!            is 37.
!            MAXPTS >= 37*NUMTRI and MAXPTS >= MINPTS
!
!    Input, EPSABS Real.
!            Requested absolute error.
!
!    Input, EPSREL Real.
!            Requested relative error.
!
!    Input, LENVER Integer.
!            Defines the length of the array VER.
!
!            We let
!            MAXSUB denote the maximum allowed number of subregions
!            for the given values of MAXPTS.
!            MAXSUB = 3*((MAXPTS-37*NUMTRI)/(4*37)) + NUMTRI
!            LENVER should then be greater or equal to MAXSUB.
!
!    Input, NW     Integer.
!            Defines the length of the working array WORK.
!
!            We let
!            MAXSUB denote the maximum allowed number of subregions
!            for the given values of MAXPTS.
!            MAXSUB = 3*((MAXPTS-37*NUMTRI)/(4*37)) + NUMTRI
!            NW should then be greater or equal to
!            MAXSUB*(2*NUMFUN+1) + MAX(32*MDIV,8*NUMTRI)*NUMFUN + 1
!
!    Input, RESTAR Integer.
!            If RESTAR = 0, this is the first attempt to compute
!            the integral.
!            If RESTAR = 1,
!            then we restart a previous attempt.
!            In this case the only parameters for DCUTRI that may
!            be changed (with respect to the previous call of DCUTRI)
!            are MINPTS, MAXPTS, EPSABS, EPSREL and RESTAR.
!
!    Output, MAXSUB Integer.
!            The maximum allowed number of subregions for the
!            given values of MAXPTS.
!
!    Output, MINSUB Integer.
!            The minimum allowed number of subregions for the given
!            values of MINPTS.
!
!    Output, IFAIL  Integer.
!            IFAIL = 0 for normal exit.
!            IFAIL = 2 if NUMFUN is less than 1.
!            IFAIL = 3 if area of one of the initially given
!                      triangles is zero.
!            IFAIL = 4 if MAXPTS is less than 37*NUMTRI.
!            IFAIL = 5 if MAXPTS is less than MINPTS.
!            IFAIL = 6 if EPSABS <= 0 and EPSREL <= 0.
!            IFAIL = 7 if LENVER is less than MAXSUB.
!            IFAIL = 8 if NW is less than MAXSUB*(2*NUMFUN+1) +
!                      NUMFUN*MAX(32*MDIV,8*NUMTRI) + 1.
!            IFAIL = 9 if illegal RESTAR.
!
         Implicit None
!
!   Global variables.
!
         Integer numfun, mdiv, numtri, minpts, maxpts, nw, maxsub, &
        & minsub, restar, lenver, ifail
         Double Precision ver (2, 3, numtri), epsabs, epsrel
!
!   Local variables.
!
         Integer limit, j
         Double Precision area
!
         ifail = 0
!
!   We compute MAXSUB and MINSUB.
!
         maxsub = 3 * ((maxpts-37*numtri)/(4*37)) + numtri
         minsub = 3 * ((minpts-37*numtri)/(4*37)) + numtri
         If (Mod((minpts-37*numtri), 4*37) .Gt. 0) Then
            minsub = minsub + 3
         End If
         minsub = Max (numtri, minsub)
!
!   Check on positive NUMFUN.
!
         If (numfun .Lt. 1) Then
            ifail = 2
            Return
         End If
!
!   Check on legal area of the region of integration.
!
         Do j = 1, numtri
!
            area = Abs (ver(1, 1, j)*ver(2, 2, j)-ver(1, 2, j)*ver(2, &
           & 1, j)-ver(1, 1, j)*ver(2, 3, j)+ver(1, 3, j)*ver(2, 1, &
           & j)+ver(1, 2, j)*ver(2, 3, j)-ver(1, 3, j)*ver(2, 2, j)) * &
           & 0.5
!
            If (area .Eq. 0) Then
               ifail = 3
               Return
            End If
!
         End Do
!
!   Check on MAXPTS >= 37*NUMTRI.
!
         If (maxpts .Lt. 37*numtri) Then
            ifail = 4
            Return
         End If
!
!   Check on MAXPTS >= MINPTS.
!
         If (maxpts .Lt. minpts) Then
            ifail = 5
            Return
         End If
!
!   Check on legal accuracy requests.
!
         If (epsabs .Le. 0 .And. epsrel .Le. 0) Then
            ifail = 6
            Return
         End If
!
!   Check on big enough LENVER.
!
         If (lenver .Lt. maxsub) Then
            ifail = 7
            Return
         End If
!
!   Check on big enough NW.
!
         limit = maxsub * (2*numfun+1) + Max (32*mdiv, 8*numtri) * &
        & numfun + 1
         If (nw .Lt. limit) Then
            ifail = 8
            Return
         End If
!
!    Check on legal RESTAR.
!
         If (restar .Ne. 0 .And. restar .Ne. 1) Then
            ifail = 9
            Return
         End If
!
         Return
      End Subroutine

      Subroutine dcutri (funsub, numfun, ver, numtri, minpts, maxpts, &
     & epsabs, epsrel, lenver, nw, restar, Result, abserr, neval, &
     & ifail, work, iwork)
!
!*********************************************************************72
!
!c DCUTRI integrates a function over a triangulated region.
!
!  Discussion:
!
!            The routine calculates an approximation to a given
!            vector of definite integrals
!
!            I  I (F ,F ,...,F )     DX(2)DX(1),
!                   1  2      NUMFUN
!
!            where the region of integration is a selection of
!            NUMTRI triangles and
!            where F = F (X(1),X(2)), J = 1,2,...,NUMFUN.
!                   J   J
!
!
!            A globally adaptive strategy is applied in order to
!            compute approximations RESULT(K)
!            hopefully satisfying for each component of I the following
!            claim for accuracy:
!            ABS(I(K)-RESULT(K)).LE.MAX(EPSABS,EPSREL*ABS(I(K)))
!
!            DCUTRI is a driver for the integration routine
!            DADTRI.
!
!            DADTRI repeatedly
!            subdivides the triangles with greatest estimated  errors
!            and estimates the integrals and the
!            errors over the new subtriangles
!            until either the error request
!            is met or MAXPTS function evaluations have been used.
!
!            A 37 point integration rule
!            with all evaluation points inside the triangle
!            is applied. The rule has polynomial degree 13.
!
!            If the values of the input parameters EPSABS
!            or EPSREL are selected great enough,
!            an integration rule is applied over each triangle and
!            the results are added up to give the approximations
!            RESULT(K). No further subdivision
!            of the triangles will then be applied.
!
!            When DCUTRI computes estimates to a vector of
!            integrals, all components of the vector are given
!            the same treatment. That is, I(F ) and I(F ) for
!                                            J         K
!            J not equal to K, are estimated with the same
!            subdivision of the region of integration.
!            For integrals with enough similarity, we may save
!            time by applying DCUTRI to all integrands in one call.
!            For integrals that varies continuously as functions of
!            some parameter, the estimates produced by DCUTRI will
!            also vary continuously when the same subdivision is
!            applied to all components. This will generally not be
!            the case when the different components are given
!            separate treatment.
!
!            On the other hand this feature should be used with
!            caution when the different components of the integrals
!            require clearly different subdivisions.
!
!  Modified:
!
!    09 December 2006
!
!  Author:
!
!    Jarle Berntsen, Terje Espelid
!
!  Reference:
!
!    Jarle Berntsen, Terje Espelid,
!    Algorithm 706:
!    DCUTRI, an Algorithm for Adaptive Cubature over a Collection
!    of Triangles,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1992, pages 329-342.
!
!  Parameters:
!
!   ON ENTRY
!
!     FUNSUB Externally declared subroutine for computing
!            all components of the integrand at the given
!            evaluation point.
!            It must have parameters (X,NUMFUN,FUNVLS)
!            Input parameters:
!              X(1)   The x-coordinate of the evaluation point.
!              X(2)   The y-coordinate of the evaluation point.
!              NUMFUN Integer that defines the number of
!                     components of I.
!            Output parameter:
!              FUNVLS Real array of dimension NUMFUN
!                     that defines NUMFUN components of the integrand.
!
!     NUMFUN Integer.
!            Number of components of the integral.
!     VER    Real array of dimension (2,3,LENVER).
!            VER(1,K,L) and VER(2,K,L) are the x and y coordinates
!            respectively of vertex K in triangle L.
!            On entry VER(*,*,L) must contain the vertices of the
!            NUMTRI user specified triangles for L = 1,2,...,NUMTRI.
!            VER may be changed on exit.
!
!     NUMTRI Integer.
!            The number of triangles.
!     MINPTS Integer.
!            Minimum number of function evaluations.
!     MAXPTS Integer.
!            Maximum number of function evaluations.
!            The number of function evaluations over each subregion
!            is 37.
!
!            MAXPTS >= 37*NUMTRI and MAXPTS >= MINPTS
!
!     EPSABS Real.
!            Requested absolute error.
!     EPSREL Real.
!            Requested relative error.
!     LENVER Integer.
!            Defines the length of the array VER.
!
!            We let
!            MAXSUB denote the maximum allowed number of subregions
!            for the given value of MAXPTS.
!            MAXSUB = 3*((MAXPTS-37*NUMTRI)/(4*37)) + NUMTRI
!            LENVER should be greater or equal to MAXSUB.
!
!     NW     Integer.
!            Defines the length of the working array WORK.
!
!            If LENVER is chosen correctly as a function of MAXPTS
!            and NUMTRI and NW is selected to be greater or equal to
!            LENVER*(2*NUMFUN+1) + MAX(32*MDIV,8*NUMTRI)*NUMFUN + 1 ,
!            then the size of the working storage will be great enough.
!            MDIV is the number of triangles that are divided in
!            each subdivision step.
!            MDIV is default set internally in DCUTRI equal to 1.
!            For efficient execution on parallel computers
!            with NPROC processors MDIV should be set equal to
!            the smallest integer such that MOD(4*MDIV,NPROC) = 0.
!
!     RESTAR Integer.
!            If RESTAR = 0, this is the first attempt to compute
!            the integral.
!            If RESTAR = 1,
!            then we restart a previous attempt.
!            In this case the only parameters for DCUTRI that may
!            be changed (with respect to the previous call of DCUTRI)
!            are MINPTS, MAXPTS, EPSABS, EPSREL and RESTAR.
!            Note: If MAXPTS was too small in the previous call,
!            then we get a second chance to continue the computations
!            with MAXPTS increased.
!            LENVER can not be changed, therefore the new value of MAXPTS
!            should not be chosen greater than the value of LENVER
!            allows us to do.
!     WORK   Real array of dimension NW.
!            Used as working storage.
!     IWORK  Integer array of dimension LENVER + MDIV.
!            Used as working storage.
!
!   ON RETURN
!
!     RESULT Real array of dimension NUMFUN.
!            Approximations to all components of the integral.
!     ABSERR Real array of dimension NUMFUN.
!            Estimates of absolute errors.
!     NEVAL  Integer.
!            Number of function evaluations used by DCUTRI.
!     IFAIL  Integer.
!            IFAIL = 0 for normal exit.
!
!              ABSERR(K) <=  EPSABS or
!              ABSERR(K) <=  ABS(RESULT(K))*EPSREL with MAXPTS or less
!              function evaluations for all values of K,
!              1 <= K <= NUMFUN .
!
!            IFAIL = 1 if MAXPTS was too small for DCUTRI
!              to obtain the required accuracy. In this case DCUTRI
!              returns values of RESULT with estimated absolute
!              errors ABSERR.
!            IFAIL = 2 if NUMFUN is less than 1.
!            IFAIL = 3 if area of one of the initially given
!                      triangles is zero.
!            IFAIL = 4 if MAXPTS is less than 37*NUMTRI.
!            IFAIL = 5 if MAXPTS is less than MINPTS.
!            IFAIL = 6 if EPSABS <= 0 and EPSREL <= 0.
!            IFAIL = 7 if LENVER is less than MAXSUB.
!            IFAIL = 8 if NW is less than MAXSUB*(2*NUMFUN+1) +
!                      NUMFUN*MAX(32*MDIV,8*NUMTRI) + 1.
!            IFAIL = 9 if unlegal RESTAR.
!     VER    Real array of dimension (2,3,LENVER).
!            On exit VER
!            contains the vertices of the triangles used to produce
!            the approximations to the integrals.
!     WORK   Real array of dimension NW.
!            Used as working storage.
!            WORK(NW) = NSUB, the number of subregions in the data
!            structure.
!            Let WRKSUB=(NW-1-NUMFUN*MAX(32*MDIV,8*NUMTRI))/
!                        (2*NUMFUN+1).
!            WORK(1),...,WORK(NUMFUN*WRKSUB) contain
!              the estimated components of the integrals over the
!              subregions.
!            WORK(NUMFUN*WRKSUB+1),...,WORK(2*NUMFUN*WRKSUB) contain
!              the estimated errors over the subregions.
!            WORK(2*NUMFUN*WRKSUB+1),...,WORK(2*NUMFUN*
!              WRKSUB+WRKSUB) contain the greatest errors
!              in each subregion.
!            WORK((2*NUMFUN+1)*WRKSUB),...,WORK(NW - 1) is used as
!              temporary storage in DADTRI.
!     IWORK  Integer array of dimension LENVER + MDIV.
!            Used as working storage.
!
!           SAMPLE PROGRAM
!   The following program will approximate the integral of
!                     exp(x*x+y*y)
!   over the triangle (0.,0.),(2.,0.),(0.,2.) and print out the
!   values of the estimated integral, the estimated error, the number
!   of function evaluations, and IFAIL.
!     PROGRAM DTEST1
!     INTEGER NUMFUN,NW,MDIV,MINPTS,LENVER,NUMTRI
!     PARAMETER (NUMFUN=1,MDIV=1,NUMTRI=1,MINPTS=37)
!     PARAMETER ( LENVER = 100 )
!
!   We need to choose LENVER  such that:
!   LENVER >= 3*((MAXPTS-37*NUMTRI)/(4*37)) + NUMTRI
!   This simply means that we have enough space to achieve MAXPTS
!   function evaluations. By choosing LENVER bigger than
!   this lower bound we may later change MAXPTS and RESTAR and restart
!   the computations from the point where the last run stopped.
!   The reason for stopping may have been that MAXPTS was too small
!   to achieve the requested error.
!          With our choice LENVER = 100 any value of MAXPTS
!   between 37 and 5068 will be accepted by the code. Choosing
!   MAXPTS = 2000 allows us to restart with a greater value
!   if necessary.
!
!     PARAMETER (NW = LENVER*(2*NUMFUN+1) +
!    +                MAX(32*MDIV,8*NUMTRI)*NUMFUN + 1)
!     DOUBLE PRECISION VER(2,3,LENVER),RESULT(NUMFUN),
!    +                 ABSERR(NUMFUN),WORK(NW),EPSABS,EPSREL
!     INTEGER RESTAR,NEVAL,IFAIL,MDIV,IWORK(LENVER+MDIV),MAXPTS
!     EXTERNAL F
!     VER(1,1,1) = 0.D0
!     VER(1,2,1) = 2.D0
!     VER(1,3,1) = 0.D0
!     VER(2,1,1) = 0.D0
!     VER(2,2,1) = 0.D0
!     VER(2,3,1) = 2.D0
!     EPSABS = 0.D0
!     EPSREL = 1.D-5
!     RESTAR = 0
!     MAXPTS = 2000
!     CALL DCUTRI(F,NUMFUN,VER,NUMTRI,MINPTS,MAXPTS,EPSABS,
!    +            EPSREL,LENVER,NW,RESTAR,RESULT,ABSERR,NEVAL,
!    +            IFAIL,WORK,IWORK)
!     WRITE(*,*)'RESULT=',RESULT(1)
!     WRITE(*,*)'ERROR =',ABSERR(1)
!     WRITE(*,*)'NEVAL =',NEVAL
!     WRITE(*,*)'IFAIL =',IFAIL
!     END
!     SUBROUTINE F(X,NUMFUN,FUNVLS)
!     INTEGER NUMFUN
!     DOUBLE PRECISION X(2),FUNVLS(NUMFUN)
!     FUNVLS(1) = EXP(X(1)*X(1)+X(2)*X(2))
!     RETURN
!     END
!
!   Output produced on a SUN SPARC station 1.
!
!       RESULT=   11.181284417019
!       ERROR =    4.7009048669038D-06
!       NEVAL =  185
!       IFAIL =  0
!
!
!***LONG DESCRIPTION
!
!   The information for each triangle is contained in the
!   data structures VER, WORK and IWORK.
!   VER contains the coordinates of the triangles.
!   When passed on to DADTRI, WORK is split into four
!   arrays VALUES, ERRORS, GREATE and WORK2.
!   VALUES contains the estimated values of the integrals.
!   ERRORS contains the estimated errors.
!   GREATE contains the greatest estimated error for each triangle.
!   The data structures for the triangles are in DADTRI organized
!   as a heap, and the size of GREATE(I) defines the position of
!   triangle I in the heap. The heap is maintained by the program
!   DTRTRI and we use a partially ordered list of pointers, saved
!   in IWORK. When passed on to DADTRI, IWORK is split into two
!   arrays LIST and VACANT. LIST is a partially ordered list
!   such that GREATE(LIST(1)) is the maximum error estimate for
!   all sub-triangles in our datastructure. VACANT is a work space
!   needed in the updating process of the list.
!
!   The subroutine DADTRI is written for efficient execution on shared
!   memory parallel computer. On a computer with NPROC processors we will
!   in each subdivision step divide MDIV triangles, where MDIV is
!   chosen such that MOD(4*MDIV,NPROC) = 0, in totally 4*MDIV new triangles.
!   Each processor will then compute estimates of the integrals and errors
!   over 4*MDIV/NPROC triangles in each subdivision step.
!   The subroutine for estimating the integral and the error over
!   each subregion, DRLTRI, uses WORK2 as a work array.
!   We must make sure that each processor writes its results to
!   separate parts of the memory, and therefore the sizes of WORK and
!   WORK2 are functions of MDIV.
!   In order to achieve parallel processing of triangles, compiler
!   directives should be placed in front of the DO 20 and the DO 200
!   loops in DADTRI on machines like Alliant and CRAY.
!
!  Local Parameters:
!
!   MDIV   Integer.
!          MDIV is the number of triangles that are divided in
!          each subdivision step in DADTRI.
!          MDIV is chosen default to 1.
!          For efficient execution on parallel computers
!          with NPROC processors MDIV should be set equal to
!          the smallest integer such that MOD(4*MDIV,NPROC) = 0.
!
!   MAXSUB Integer.
!          The maximum allowed number of subregions
!          for the given values of MAXPTS.
!
!   MINSUB Integer.
!          The minimum allowed number of subregions for the given
!          values of MINPTS.
!
!   WRKSUB Integer.
!          Determines the length of the main work arrays.
!
         Implicit None
!
         Integer mdiv
         Parameter (mdiv=1)
!
         Interface
           Subroutine funsub( x , numfun , funvls )
           Integer numfun
           Double Precision x(2),funvls(numfun)
           End Subroutine
         End Interface

         Integer numfun, numtri, minpts, maxpts, lenver, nw, restar, &
        & neval, ifail, iwork (lenver+mdiv)
         Double Precision ver (2, 3, lenver), epsabs, epsrel, result &
        & (numfun), abserr (numfun), work (nw)
!
         Integer maxsub, minsub, nsub, lenw
         Integer wrksub, i1, i2, i3, i4
!
!   Compute MAXSUB and MINSUB,
!   and check the input parameters.
!
         Call dchtri (numfun, mdiv, ver, numtri, minpts, maxpts, &
        & epsabs, epsrel, lenver, nw, restar, maxsub, minsub, ifail)
         wrksub = (nw-1-numfun*Max(32*mdiv, 8*numtri)) / (2*numfun+1)
         If (ifail .Ne. 0) Then
            Return
         End If
!
!   Split up the work space.
!
         i1 = 1
         i2 = i1 + wrksub * numfun
         i3 = i2 + wrksub * numfun
         i4 = i3 + wrksub
!
!   On restart runs the number of subregions from the
!   previous call is assigned to NSUB.
!
         If (restar .Eq. 1) Then
            nsub = work (nw)
         End If
!
!   Compute the size of the temporary work space needed in DADTRI.
!
         lenw = Max (32*mdiv, 8*numtri) * numfun
         Call dadtri (numfun, mdiv, ver, numtri, minsub, maxsub, &
        & funsub, epsabs, epsrel, lenver, restar, lenw, result, abserr, &
        & neval, nsub, ifail, work(i1), work(i2), work(i3), work(i4), &
        & iwork(1), iwork(1+lenver))
         work (nw) = nsub
!
         Return
      End Subroutine



      Subroutine drltri (ver, numfun, funsub, null, basval, rgnerr, &
     & greate)
!
!*********************************************************************72
!
!c DRLTRI computes basic integration rules and error estimates.
!
!  Discussion:
!
!    DRLTRI computes basic integration rule values
!    for a vector of integrands over a triangular region.
!    DRLTRI also computes estimates for the errors by
!    using several null rule approximations.
!
!  Modified:
!
!    09 December 2006
!
!  Author:
!
!    Jarle Berntsen, Terje Espelid
!
!  Reference:
!
!    Jarle Berntsen, Terje Espelid,
!    Algorithm 706:
!    DCUTRI, an Algorithm for Adaptive Cubature over a Collection
!    of Triangles,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1992, pages 329-342.
!
!    Jarle Berntsen, Terje Espelid,
!    Degree 13 Symmetric Quadrature Rules for the Triangle,
!    Reports in Informatics 44,
!    Department of Informatics, University of Bergen, 1990.
!
!  Parameters:
!
!   ON ENTRY
!
!   VER    Real array of dimension (2,3).
!          The coordinates of the vertices of the triangle.
!   NUMFUN Integer.
!          Number of components of the vector integrand.
!   FUNSUB Externally declared subroutine for computing
!            all components of the integrand at the given
!            evaluation point.
!            It must have parameters (X,NUMFUN,FUNVLS)
!            Input parameters:
!              X(1)      The x-coordinate of the evaluation point.
!              X(2)      The y-coordinate of the evaluation point.
!              NUMFUN Integer that defines the number of
!                     components of I.
!            Output parameter:
!              FUNVLS Real array of dimension NUMFUN
!                     that defines NUMFUN components of the integrand.
!
!   NULL   Real array of dimension (NUMFUN,8).
!          A work array.
!
!   ON RETURN
!
!   BASVAL Real array of dimension NUMFUN.
!          The values for the basic rule for each component
!          of the integrand.
!   RGNERR Real array of dimension NUMFUN.
!          The error estimates for each component of the integrand.
!   GREATE Real.
!          The greatest error component of the integrand.
!
!  Local parameters:
!
!   WTLENG The number of weights of the integration rule.
!
!   G      Real array of dimension (2,WTLENG).
!          The homogeneous coordinates for the generators of
!          the evaluation points.
!          The integration rule is using symmetric evaluation
!          points and has the structure (1,6,3). That is,
!          1 point of multiplicity 1,
!          6 sets of points of multiplicity 3 and
!          3 sets of points of multiplicity 6.
!          This gives totally 37 evaluation points.
!          In order to reduce the number of loops in DRLTRI,
!          the 3 loops for the sets of multiplicity 6 are split
!          into 6 loops and added to the loops for the sets of
!          multiplicity 3.
!          The number of weights we have to give with
!          this splitting is WTLENG = 13.
!
!   W      Real array of dimension (9,WTLENG).
!          The weights of the basic rule and the null rules.
!          W(1,1),...,W(1,WTLENG) are weights for the basic rule.
!          W(I,1),...,W(I,WTLENG) for I>1 are null rule weights.
!
         Implicit None
!
         Interface
                  Subroutine funsub( x , numfun , funvls )
                   Integer numfun
                   Double Precision x(2),funvls(numfun)
                  End Subroutine
                 End Interface

         Integer numfun
         Double Precision ver (2, 3), basval (numfun), rgnerr (numfun), &
        & null (numfun, 8), greate, noise, tres
!
         Integer wtleng
         Parameter (wtleng=13)
         Double Precision area, x (2, 3), z1, z2, z3, r1, r2, r3, r, &
        & deg7, deg5, deg3, deg1, g (2, 13), w (9, 13)
         Integer i, j, k, l
!
!  The abscissas are given in homogeneous coordinates.
!
         Data (g(1, i), i=1, 13) / 0.333333333333333333333333333333d0, &
        & 0.950275662924105565450352089520d0, &
        & 0.171614914923835347556304795551d0, &
        & 0.539412243677190440263092985511d0, &
        & 0.772160036676532561750285570113d0, &
        & 0.009085399949835353883572964740d0, &
        & 0.062277290305886993497083640527d0, &
        & 0.022076289653624405142446876931d0, &
        & 0.018620522802520968955913511549d0, &
        & 0.096506481292159228736516560903d0, &
        & 0.851306504174348550389457672223d0, &
        & 0.689441970728591295496647976487d0, &
        & 0.635867859433872768286976979827d0 /
         Data (g(2, i), i=1, 13) / 0.333333333333333333333333333333d0, &
        & 0.024862168537947217274823955239d0, &
        & 0.414192542538082326221847602214d0, &
        & 0.230293878161404779868453507244d0, &
        & 0.113919981661733719124857214943d0, &
        & 0.495457300025082323058213517632d0, &
        & 0.468861354847056503251458179727d0, &
        & 0.851306504174348550389457672223d0, &
        & 0.689441970728591295496647976487d0, &
        & 0.635867859433872768286976979827d0, &
        & 0.022076289653624405142446876931d0, &
        & 0.018620522802520968955913511549d0, &
        & 0.096506481292159228736516560903d0 /
!
!   Weights of the degree 13 quadrature rule.
!
         Data (w(1, i), i=1, 13) / 0.051739766065744133555179145422d0, &
        & 0.008007799555564801597804123460d0, &
        & 0.046868898981821644823226732071d0, &
        & 0.046590940183976487960361770070d0, &
        & 0.031016943313796381407646220131d0, &
        & 0.010791612736631273623178240136d0, &
        & 0.032195534242431618819414482205d0, &
        & 0.015445834210701583817692900053d0, &
        & 0.017822989923178661888748319485d0, &
        & 0.037038683681384627918546472190d0, &
        & 0.015445834210701583817692900053d0, &
        & 0.017822989923178661888748319485d0, &
        & 0.037038683681384627918546472190d0 /
!
!   Weights of the first null rule of degree 7.
!
         Data (w(2, i), i=1, 13) / - &
        & 0.077738051051462052051304462750d0, &
        & 0.001640389740236881582083124927d0, &
        & 0.078124083459915167386776552733d0, - &
        & 0.030706528522391137165581298102d0, &
        & 0.010246307817678312345028512621d0, &
        & 0.012586300774453821540476193059d0, - &
        & 0.043630506151410607808929481439d0, - &
        & 0.004567055157220063810223671248d0, &
        & 0.003393373439889186878847613140d0, &
        & 0.000000000000000000000000000000d0, - &
        & 0.004567055157220063810223671248d0, &
        & 0.003393373439889186878847613140d0, &
        & 0.000000000000000000000000000000d0 /
!
!   Weights of the second null rule of degree 7.
!
         Data (w(3, i), i=1, 13) / - &
        & 0.064293709240668260928898888457d0, &
        & 0.003134665264639380635175608661d0, &
        & 0.007822550509742830478456728602d0, &
        & 0.048653051907689492781049400973d0, &
        & 0.032883327334384971735434067029d0, - &
        & 0.017019508374229390108580829589d0, &
        & 0.025973557893399824586684707198d0, - &
        & 0.010716753326806275930657622320d0, &
        & 0.018315629578968063765722278290d0, - &
        & 0.047607080313197299401024682666d0, - &
        & 0.010716753326806275930657622320d0, &
        & 0.018315629578968063765722278290d0, - &
        & 0.047607080313197299401024682666d0 /
!
!   Weights of the first degree 5 null rule.
!
!
         Data (w(4, i), i=1, 13) / 0.021363205584741860993131879186d0, &
        & 0.022716410154120323440432428315d0, - &
        & 0.026366191271182090678117381002d0, &
        & 0.029627021479068212693155637482d0, &
        & 0.004782834546596399307634111034d0, &
        & 0.004178667433984132052378990240d0, - &
        & 0.065398996748953861618846710897d0, - &
        & 0.033589813176131630980793760168d0, &
        & 0.033018320112481615757912576257d0, &
        & 0.012241086002709814125707333127d0, - &
        & 0.033589813176131630980793760168d0, &
        & 0.033018320112481615757912576257d0, &
        & 0.012241086002709814125707333127d0 /
!
!   Weights of the second degree 5 null rule.
!
         Data (w(5, i), i=1, 13) / - &
        & 0.046058756832790538620830792345d0, &
        & 0.005284159186732627192774759959d0, &
        & 0.009325799301158899112648198129d0, - &
        & 0.006101110360950124560783393745d0, - &
        & 0.056223328794664871336486737231d0, - &
        & 0.062516479198185693171971930698d0, &
        & 0.022428226812039547178810743269d0, - &
        & 0.000026014926110604563130107142d0, &
        & 0.032882099937471182365626663487d0, &
        & 0.018721740987705986426812755881d0, - &
        & 0.000026014926110604563130107142d0, &
        & 0.032882099937471182365626663487d0, &
        & 0.018721740987705986426812755881d0 /
!
!   Weights of first degree 3 null rule.
!
         Data (w(6, i), i=1, 13) / 0.080867117677405246540283712799d0, &
        & - 0.033915806661511608094988607349d0, &
        & 0.014813362053697845461526433401d0, &
        & 0.001442315416337389214102507204d0, - &
        & 0.024309696484708683486455879210d0, - &
        & 0.005135085639122398522835391664d0, - &
        & 0.034649417896235909885490654650d0, &
        & 0.035748423431577326597742956780d0, &
        & 0.024548155266816447583155562333d0, - &
        & 0.032897267038856299280541675107d0, &
        & 0.035748423431577326597742956780d0, &
        & 0.024548155266816447583155562333d0, - &
        & 0.032897267038856299280541675107d0 /
!
!   Weights of second degree 3 null rule.
!
         Data (w(7, i), i=1, 13) / - &
        & 0.038457863913548248582247346193d0, - &
        & 0.055143631258696406147982448269d0, - &
        & 0.021536994314510083845999131455d0, &
        & 0.001547467894857008228010564582d0, &
        & 0.057409361764652373776043522086d0, - &
        & 0.040636938884669694118908764512d0, - &
        & 0.020801144746964801777584428369d0, &
        & 0.019490770404993674256256421103d0, &
        & 0.002606109985826399625043764771d0, &
        & 0.023893703367437102825618048130d0, &
        & 0.019490770404993674256256421103d0, &
        & 0.002606109985826399625043764771d0, &
        & 0.023893703367437102825618048130d0 /
!
!   Weights of first degree 1 null rule.
!
         Data (w(8, i), i=1, 13) / 0.074839568911184074117081012527d0, &
        & - 0.004270103034833742737299816615d0, &
        & 0.049352639555084484177095781183d0, &
        & 0.048832124609719176627453278550d0, &
        & 0.001042698696559292759051590242d0, - &
        & 0.044445273029113458906055765365d0, - &
        & 0.004670751812662861209726508477d0, - &
        & 0.015613390485814379318605247424d0, - &
        & 0.030581651696100000521074498679d0, &
        & 0.010801113204340588798240297593d0, - &
        & 0.015613390485814379318605247424d0, - &
        & 0.030581651696100000521074498679d0, &
        & 0.010801113204340588798240297593d0 /
!
!   Weights of second degree 1 null rule.
!
         Data (w(9, i), i=1, 13) / 0.009373028261842556370231264134d0, &
        & - 0.074249368848508554545399978725d0, &
        & 0.014709707700258308001897299938d0, &
        & 0.009538502545163567494354463302d0, - &
        & 0.014268362488069444905870465047d0, &
        & 0.040126396495352694403045023109d0, &
        & 0.028737181842214741174950928350d0, - &
        & 0.031618075834734607275229608099d0, &
        & 0.016879961075872039084307382161d0, &
        & 0.010878914758683152984395046434d0, - &
        & 0.031618075834734607275229608099d0, &
        & 0.016879961075872039084307382161d0, &
        & 0.010878914758683152984395046434d0 /
!
         tres = 50 * d1mach (4)
!
!  Compute area of triangle.
!
         area = Abs (ver(1, 1)*ver(2, 2)-ver(1, 2)*ver(2, 1)-ver(1, &
        & 1)*ver(2, 3)+ver(1, 3)*ver(2, 1)+ver(1, 2)*ver(2, 3)-ver(1, &
        & 3)*ver(2, 2)) / 2
!
!   Compute contributions from the center of the triangle.
!
         x (1, 1) = (ver(1, 1)+ver(1, 2)+ver(1, 3)) / 3
         x (2, 1) = (ver(2, 1)+ver(2, 2)+ver(2, 3)) / 3
!
         Call funsub (x, numfun, rgnerr)
!
         Do j = 1, numfun
            basval (j) = w (1, 1) * rgnerr (j)
            Do k = 1, 8
               null (j, k) = w (k+1, 1) * rgnerr (j)
            End Do
         End Do
!
!   Compute the contributions from the points with
!   multiplicity 3.
!
         Do i = 2, wtleng
            z1 = g (1, i)
            z2 = g (2, i)
            z3 = 1 - z1 - z2
            x (1, 1) = z1 * ver (1, 1) + z2 * ver (1, 2) + z3 * ver (1, &
           & 3)
            x (2, 1) = z1 * ver (2, 1) + z2 * ver (2, 2) + z3 * ver (2, &
           & 3)
            x (1, 2) = z2 * ver (1, 1) + z3 * ver (1, 2) + z1 * ver (1, &
           & 3)
            x (2, 2) = z2 * ver (2, 1) + z3 * ver (2, 2) + z1 * ver (2, &
           & 3)
            x (1, 3) = z3 * ver (1, 1) + z1 * ver (1, 2) + z2 * ver (1, &
           & 3)
            x (2, 3) = z3 * ver (2, 1) + z1 * ver (2, 2) + z2 * ver (2, &
           & 3)
            Do l = 1, 3
               Call funsub (x(1, l), numfun, rgnerr)
               Do j = 1, numfun
                  basval (j) = basval (j) + w (1, i) * rgnerr (j)
                  Do k = 1, 8
                     null (j, k) = null (j, k) + w (k+1, i) * rgnerr &
                    & (j)
                  End Do
               End Do
            End Do
         End Do
!
!    Compute the errors.
!
         greate = 0
!
         Do j = 1, numfun
!
            deg7 = area * Sqrt (null(j, 1)**2+null(j, 2)**2)
            deg5 = area * Sqrt (null(j, 3)**2+null(j, 4)**2)
            deg3 = area * Sqrt (null(j, 5)**2+null(j, 6)**2)
            deg1 = area * Sqrt (null(j, 7)**2+null(j, 8)**2)
!
            If (deg5 .Ne. 0) Then
               r1 = deg7 / deg5
            Else
               r1 = 1
            End If
!
            If (deg3 .Ne. 0) Then
               r2 = deg5 / deg3
            Else
               r2 = 1
            End If
!
            If (deg1 .Ne. 0) Then
               r3 = deg3 / deg1
            Else
               r3 = 1
            End If
!
            r = Max (r1, r2, r3)
!
            If (r .Ge. 1) Then
               rgnerr (j) = 10 * Max (deg1, deg3, deg5, deg7)
            Else If (r .Ge. 0.5d0) Then
               rgnerr (j) = 10 * r * deg7
            Else
               rgnerr (j) = 40 * (r**3) * deg7
            End If
!
            basval (j) = area * basval (j)
            noise = Abs (basval(j)) * tres
!
!  The following 6 statements added as the result of authors remark
!
!    The following statement is included to set the error to the noise
!    level if the two error estimates, assumed to be the best, are both
!    below or on the noise level
!
            If ((deg7 .Le. noise) .And. (deg5 .Le. noise)) rgnerr (j) = &
           & noise
            rgnerr (j) = Max (noise, rgnerr(j))
!
            If (rgnerr(j) .Gt. greate) Then
               greate = rgnerr (j)
            End If
!
         End Do
!
         Return
      End Subroutine
      Subroutine dtrtri (dvflag, sbrgns, greate, list, new)
!
!*********************************************************************72
!
!c DTRTRI maintains a heap of subregions.
!
!  Discussion:
!
!    DTRTRI maintains a heap of subregions.  The subregions are stored
!    in a partially sorted binary tree, ordered according to the size
!    of the greatest error estimates of each subregion(GREATE).
!    The subregion with greatest error estimate is in the
!    first position of the heap.
!
!  Modified:
!
!    09 December 2006
!
!  Author:
!
!    Jarle Berntsen, Terje Espelid
!
!  Reference:
!
!    Jarle Berntsen, Terje Espelid,
!    Algorithm 706:
!    DCUTRI, an Algorithm for Adaptive Cubature over a Collection
!    of Triangles,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1992, pages 329-342.
!
!  Parameters:
!
!     DVFLAG Integer.
!            If DVFLAG = 1, we remove the subregion with
!            greatest error from the heap.
!            If DVFLAG = 2, we insert a new subregion in the heap.
!
!     SBRGNS Integer.
!            Number of subregions in the heap.
!
!     GREATE Real array of dimension SBRGNS.
!            Used to store the greatest estimated errors in
!            all subregions.
!
!     LIST   Integer array of dimension SBRGNS.
!            Used as a partially ordered list of pointers to the
!            different subregions. This list is a heap where the
!            element on top of the list is the subregion with the
!            greatest error estimate.
!
!     NEW    Integer.
!            Index to the new region to be inserted in the heap.
!
!  Local parameters:
!
!   GREAT  is used as intermediate storage for the greatest error of a
!          subregion.
!
!   SUBRGN Position of child/parent subregion in the heap.
!
!   SUBTMP Position of parent/child subregion in the heap.
!
         Implicit None
!
         Integer dvflag, new, sbrgns, list (*)
         Double Precision greate (*)
!
         Integer subrgn, subtmp
         Double Precision great
!
!    If DVFLAG = 1, we will reduce the partial ordered list by the
!    element with greatest estimated error. Thus the element in
!    in the heap with index LIST(1) is vacant and can be used later.
!    Reducing the heap by one element implies that the last element
!    should be re-positioned.
!
         If (dvflag .Eq. 1) Then
            great = greate (list(sbrgns))
            sbrgns = sbrgns - 1
            subrgn = 1
20          subtmp = 2 * subrgn
            If (subtmp .Le. sbrgns) Then
               If (subtmp .Ne. sbrgns) Then
!
!   Find max. of left and right child.
!
                  If (greate(list(subtmp)) .Lt. greate(list(subtmp+1))) &
                 & Then
                     subtmp = subtmp + 1
                  End If
               End If
!
!   Compare max.child with parent.
!   If parent is max., then done.
!
               If (great .Lt. greate(list(subtmp))) Then
!
!   Move the pointer at position SUBTMP up the heap.
!
                  list (subrgn) = list (subtmp)
                  subrgn = subtmp
                  Go To 20
               End If
            End If
!
!   Update the pointer.
!
            If (sbrgns .Gt. 0) Then
               list (subrgn) = list (sbrgns+1)
            End If
         Else If (dvflag .Eq. 2) Then
!
!   If DVFLAG = 2, find the position for the NEW region in the heap.
!
            great = greate (new)
            subrgn = sbrgns
40          subtmp = subrgn / 2
            If (subtmp .Ge. 1) Then
!
!   Compare max.child with parent.
!   If parent is max, then done.
!
               If (great .Gt. greate(list(subtmp))) Then
!
!   Move the pointer at position SUBTMP down the heap.
!
                  list (subrgn) = list (subtmp)
                  subrgn = subtmp
                  Go To 40
               End If
            End If
!
!    Set the pointer to the new region in the heap.
!
            list (subrgn) = new
         End If
         Return
      End Subroutine


          Subroutine dadtri (numfun, mdiv, ver, numtri, minsub, maxsub, &
     & funsub, epsabs, epsrel, lenver, restar, lenw, Result, abserr, &
     & neval, nsub, ifail, values, errors, greate, work2, list, vacant)
!
!*********************************************************************72
!
!c DADTRI computes integrals over triangular regions.
!
!  Discussion:
!
!    DADTRI repeatedly subdivides the triangles with greatest estimated
!    errors and estimates the integrals and the errors over the new
!    subtriangles until either the error request is met or MAXPTS
!    function evaluations have been used.
!
!    A 37 point integration rule with all evaluation points inside
!    the triangle is applied.  The rule has polynomial degree 13.
!
!  Modified:
!
!    09 December 2006
!
!  Author:
!
!    Jarle Berntsen, Terje Espelid
!
!  Reference:
!
!    Jarle Berntsen, Terje Espelid,
!    Algorithm 706:
!    DCUTRI, an Algorithm for Adaptive Cubature over a Collection
!    of Triangles,
!    ACM Transactions on Mathematical Software,
!    Volume 18, Number 3, September 1992, pages 329-342.
!
!  Parameters:
!
!   ON ENTRY
!
!     NUMFUN Integer.
!            Number of components of the integral.
!
!     MDIV   Integer.
!            MDIV is the number of triangles that are divided in
!            each subdivision step in DADTRI.
!            MDIV is chosen by default to be 1.
!            For efficient execution on parallel computers
!            with NPROC processors MDIV should be set equal to
!            the smallest integer such that MOD(4*MDIV,NPROC) = 0.
!
!     VER    Real array of dimension (2,3,LENVER).
!            VER(1,K,L) and VER(2,K,L) are the x and y coordinates
!            respectively of vertex K in triangle L.
!            On entry VER(*,*,L) must contain the vertices of the
!            NUMTRI user specified triangles for L = 1,2,...,NUMTRI.
!
!     NUMTRI Integer.
!            The number of triangles.
!
!     MINSUB Integer.
!            The minimum allowed number of subregions.
!
!     MAXSUB Integer.
!            The maximum allowed number of subregions.
!
!     FUNSUB Externally declared subroutine for computing
!            all components of the integrand at the given
!            evaluation point.
!            It must have parameters (X,NUMFUN,FUNVLS)
!            Input parameters:
!              X(1)      The x-coordinate of the evaluation point.
!              X(2)      The y-coordinate of the evaluation point.
!              NUMFUN Integer that defines the number of
!                     components of I.
!            Output parameter:
!              FUNVLS Real array of dimension NUMFUN
!                     that defines NUMFUN components of the integrand.
!
!     EPSABS Real.
!            Requested absolute error.
!
!     EPSREL Real.
!            Requested relative error.
!
!     LENVER Integer.
!            Defines the length of the array VER.
!
!            We let
!            MAXSUB denote the maximum allowed number of subregions
!            for the given values of MAXPTS.
!            MAXSUB = 3*((MAXPTS-37*NUMTRI)/(4*37)) + NUMTRI
!            LENVER should then be greater or equal to MAXSUB.
!
!     RESTAR Integer.
!            If RESTAR = 0, this is the first attempt to compute
!            the integral.
!            If RESTAR = 1,
!            then we restart a previous attempt.
!            In this case the only parameters for DCUTRI that may
!            be changed (with respect to the previous call of DCUTRI)
!            are MINPTS, MAXPTS, EPSABS, EPSREL and RESTAR.
!
!     LENW   Integer.
!            Length of the workspace WORK2.
!
!   ON RETURN
!
!     RESULT Real array of dimension NUMFUN.
!            Approximations to all components of the integral.
!
!     ABSERR Real array of dimension NUMFUN.
!            Estimates of absolute errors.
!
!     NEVAL  Integer.
!            Number of function evaluations used by DCUTRI.
!
!     NSUB   Integer.
!            The number of triangles in the data structure.
!
!     IFAIL  Integer.
!            IFAIL = 0 for normal exit.
!
!              ABSERR(K) <=  EPSABS or
!              ABSERR(K) <=  ABS(RESULT(K))*EPSREL with MAXPTS or less
!              function evaluations for all values of K,
!              1 <= K <= NUMFUN .
!
!            IFAIL = 1 if MAXSUB was too small for DADTRI
!              to obtain the required accuracy. In this case DADTRI
!              returns values of RESULT with estimated absolute
!              errors ABSERR.
!
!     VALUES Real array of dimension (NUMFUN,MAXSUB).
!            The estimated components of the integrals over the
!            subregions.
!
!     ERRORS Real array of dimension (NUMFUN,MAXSUB).
!            The estimated errors over the subregions.
!
!     GREATE Real array of dimension MAXSUB.
!            The greatest errors in each subregion.
!
!     WORK2   Real array of dimension LENW.
!            Work array used in DTRTRI and DRLTRI.
!
!     LIST   Integer array used in DTRTRI of dimension LENVER.
!            Is a partially sorted list, where LIST(1) is the top
!            element in a heap of subregions.
!
!     VACANT Integer array of dimension MDIV.
!            Used as intermediate storage of pointers.
!
!  Local parameters:
!
!   SBRGNS is the number of stored subregions.
!
!   NDIV   The number of subregions to be divided in each main step.
!
!   POINTR Pointer to the position in the data structure where
!          the new subregions are to be stored.
!
!   TOP    is a pointer to the top element in the heap of subregions.
!
         Implicit None
!
         Interface
                  Subroutine funsub( x , numfun , funvls )
                   Integer numfun
                   Double Precision x(2),funvls(numfun)
                  End Subroutine
                 End Interface

         Integer numfun, mdiv, numtri, minsub, maxsub, lenw, restar, &
        & lenver, neval, nsub, ifail, list (lenver), vacant (mdiv)
         Double Precision ver (2, 3, lenver), epsabs, epsrel, result &
        & (numfun), abserr (numfun), values (numfun, maxsub), errors &
        & (numfun, maxsub), greate (maxsub), work2 (lenw)
!
         Integer i, j, k, l
         Integer sbrgns, i1, i2, i3, i4, size
         Integer l1
         Integer ndiv, pointr, index, top
         Double Precision verold (2, 3)
!
         If (restar .Eq. 1) Then
            sbrgns = nsub
            Go To 110
         End If
!
!   Initiate SBRGNS, RESULT, ABSERR and NEVAL.
!
         sbrgns = 0
!
         Do j = 1, numfun
            result (j) = 0
            abserr (j) = 0
         End Do
!
         neval = 0
!
!   Apply DRLTRI over the NUMTRI triangles.
!   This loop may be run in parallel.
!
         Do i = 1, numtri
            l1 = 1 + (i-1) * 8 * numfun
            Call drltri (ver(1, 1, i), numfun, funsub, work2(l1), &
           & values(1, i), errors(1, i), greate(i))
            neval = neval + 37
            sbrgns = sbrgns + 1
         End Do
!
!   Add the computed values to RESULT and ABSERR.
!
         Do i = 1, numtri
            Do j = 1, numfun
               result (j) = result (j) + values (j, i)
               abserr (j) = abserr (j) + errors (j, i)
            End Do
         End Do
!
!   Store results in heap.
!
         Do i = 1, numtri
            index = i
            Call dtrtri (2, index, greate, list, i)
         End Do
!
!   We check for termination.
!
         If (sbrgns .Lt. minsub) Then
            Go To 110
         End If
!
         Do j = 1, numfun
            If (abserr(j) .Gt. epsrel*Abs(result(j)) .And. abserr(j) &
           & .Gt. epsabs) Then
               Go To 110
            End If
         End Do
!
         ifail = 0
         Go To 499
!
!  End initiation.
!
!  Begin loop while the error is too great,
!  and SBRGNS+3 is less than MAXSUB.
!
110      Continue
!
         If (sbrgns+3 .Le. maxsub) Then
!
!   If we are allowed to divide further,
!   prepare to apply basic rule over  the triangles produced
!   by dividing the
!   NDIV subtriangles with greatest errors.
!   If MAXSUB and SBRGNS are great enough, NDIV = MDIV.
!
            ndiv = maxsub - sbrgns
            ndiv = Min (ndiv, mdiv, sbrgns)
!
!   Divide the NDIV subtriangles in four new subtriangles, and compute
!   integral and error over each.
!   When we pick a triangle to divide in four, then one of the new
!   sub-triangles is stored in the original triangle's position in the
!   datastructure. Thus we get 3*NDIV new elements in the datastructure
!   after the subdivision. The size of the datastructure before the
!   subdivision is stored in the variable SIZE, while SBRGNS is the
!   size of the heap at any time.
!   Hence.
!
            size = sbrgns
!
            Do i = 1, ndiv
!
               pointr = size + 3 * (ndiv+1-i)
!
!   Adjust RESULT and ABSERR. TOP is a pointer to the top of the heap.
!
               top = list (1)
               vacant (i) = top
               Do j = 1, numfun
                  result (j) = result (j) - values (j, top)
                  abserr (j) = abserr (j) - errors (j, top)
               End Do
!
!   Save the vertices.
!
               Do l = 1, 2
                  Do k = 1, 3
                     verold (l, k) = ver (l, k, top)
                  End Do
               End Do
!
!   Adjust the heap.
!
               Call dtrtri (1, sbrgns, greate, list, k)
!
!   Compute the four new triangles.
!
               i1 = top
               i2 = pointr - 2
               i3 = pointr - 1
               i4 = pointr
               ver (1, 1, i1) = verold (1, 1)
               ver (2, 1, i1) = verold (2, 1)
               ver (1, 2, i1) = (verold(1, 1)+verold(1, 2)) / 2
               ver (2, 2, i1) = (verold(2, 1)+verold(2, 2)) / 2
               ver (1, 3, i1) = (verold(1, 1)+verold(1, 3)) / 2
               ver (2, 3, i1) = (verold(2, 1)+verold(2, 3)) / 2
               ver (1, 1, i2) = ver (1, 2, i1)
               ver (2, 1, i2) = ver (2, 2, i1)
               ver (1, 2, i2) = verold (1, 2)
               ver (2, 2, i2) = verold (2, 2)
               ver (1, 3, i2) = (verold(1, 2)+verold(1, 3)) / 2
               ver (2, 3, i2) = (verold(2, 2)+verold(2, 3)) / 2
               ver (1, 1, i3) = ver (1, 3, i1)
               ver (2, 1, i3) = ver (2, 3, i1)
               ver (1, 2, i3) = ver (1, 3, i2)
               ver (2, 2, i3) = ver (2, 3, i2)
               ver (1, 3, i3) = verold (1, 3)
               ver (2, 3, i3) = verold (2, 3)
               ver (1, 1, i4) = ver (1, 3, i2)
               ver (2, 1, i4) = ver (2, 3, i2)
               ver (1, 2, i4) = ver (1, 3, i1)
               ver (2, 2, i4) = ver (2, 3, i1)
               ver (1, 3, i4) = ver (1, 2, i1)
               ver (2, 3, i4) = ver (2, 2, i1)
!
            End Do
!
!   Apply basic rule over 4*NDIV triangles.
!   This loop may be run in parallel.
!
            Do i = 1, 4 * ndiv
               If (i .Le. ndiv) Then
                  index = vacant (i)
               Else
                  index = sbrgns + i
               End If
               l1 = 1 + (i-1) * 8 * numfun
               Call drltri (ver(1, 1, index), numfun, funsub, &
              & work2(l1), values(1, index), errors(1, index), &
              & greate(index))
            End Do
!
            neval = neval + 4 * ndiv * 37
!
!   Add new contributions to RESULT and ABSERR.
!
            Do i = 1, 4 * ndiv
               If (i .Le. ndiv) Then
                  index = vacant (i)
               Else
                  index = sbrgns + i
               End If
!
               Do j = 1, numfun
                  result (j) = result (j) + values (j, index)
                  abserr (j) = abserr (j) + errors (j, index)
               End Do
!
            End Do
!
!   Store results in heap.
!
            Do i = 1, 4 * ndiv
               If (i .Le. ndiv) Then
                  index = vacant (i)
               Else
                  index = sbrgns + i
               End If
               j = sbrgns + i
               Call dtrtri (2, j, greate, list, index)
            End Do
!
            sbrgns = sbrgns + 4 * ndiv
!
!   Check for termination.
!
            If (sbrgns .Lt. minsub) Then
               Go To 110
            End If
!
            Do j = 1, numfun
               If (abserr(j) .Gt. epsrel*Abs(result(j)) .And. abserr(j) &
              & .Gt. epsabs) Then
                  Go To 110
               End If
            End Do
!
            ifail = 0
!
!   Else we did not succeed with the
!   given value of MAXSUB.
!
         Else
            ifail = 1
         End If
!
!   Compute more accurate values of RESULT and ABSERR.
!
499      Continue
!
         Do j = 1, numfun
            result (j) = 0
            abserr (j) = 0
         End Do
!
         Do i = 1, sbrgns
            Do j = 1, numfun
               result (j) = result (j) + values (j, i)
               abserr (j) = abserr (j) + errors (j, i)
            End Do
         End Do
!
         nsub = sbrgns
         Return
      End Subroutine
!

End Module
