!!# TOOLBOX MODULE: <<TBX_DiscreteOrdinates>>
MODULE TBX_DiscreteOrdinates

!!## PURPOSE
!! The toolbox for the Discrete Ordinates Method angular
!! discretization procedures.

!!## EXTERNAL KINDS
USE KND_DiscreteOrdinates       !!((02-A-KND_DiscreteOrdinates.f90))

!!## EXTERNAL PARAMETERS
USE PAR_DiscreteOrdinates       !!((03-A-PAR_DiscreteOrdinates.f90))
USE PAR_Units                   !!((02-A-PAR_Units.f90))

!!## EXTERNAL PROCEDURES
USE FUN_Default                 !!((04-A-FUN_Default.f90))
USE FUN_STR                     !!((05-B-FUN_STR.f90))
USE FUN_SIZEa                   !!((06-B-FUN_SIZEa.f90))
USE PRN_Text                    !!((07-B-PRN_Text.f90))
USE FUN_xyANGLE                 !!((05-B-FUN_xyANGLE.f90))
USE FUN_STRTIME                 !!((06-C-FUN_STRTIME.f90))
USE SUB_genQuadrature           !!((09-B-SUB_genQuadrature.f90))
USE SUB_levQuadrature           !!((09-B-SUB_levQuadrature.f90))
USE SUB_prodQuadrature          !!((07-B-SUB_prodQuadrature.f90))
USE LIB_genMoments              !!((13-B-LIB_genMoments.f90))
USE SUB_genQuadrature_FROM_prod !!((07-B-SUB_genQuadrature_FROM_prod.f90))
USE SUB_genQuadrature_FROM_lev  !!((07-B-SUB_genQuadrature_FROM_lev.f90))

!!## GLOBAL LIBRARIES
USE LIB_GenericPhrases          !!((07-B-LIB_GenericPhrases.f90))
USE LIB_Prompts                 !!((06-B-LIB_Prompts.f90))

!!## GLOBAL VARIABLES
USE VAR_Units                   !!((03-A-VAR_Units.f90))

!!## USER MODULES
USE USR_fdbk                    !!((08-C-USR_fdbk.f90))

!!## DEFAULT IMPLICIT
IMPLICIT NONE

!!## DEFAULT ACCESS
PRIVATE

!!## ACCESS
PUBLIC :: SETUP_DiscreteOrdinates
PUBLIC :: AziAngle,PolAngle

!!## MODULE PROCEDURES
CONTAINS

!!### FUNCTION <<PolAngle>>
FUNCTION PolAngle( U )

!!#### PURPOSE
!! Return the polar angle of a direction in $R^3$.

REAL(KIND_DOR),INTENT(IN) :: U(3)
REAL(KIND_DOR) :: PolAngle

!!--begin--

!calculate polar angle
PolAngle = ACOS(U(3))

!!--end--
END FUNCTION


!!### FUNCTION <<AziAngle>>
FUNCTION AziAngle( U )

!!#### PURPOSE
!! Return the azimuthal angle of a direction in $R^3$.

REAL(KIND_DOR),INTENT(IN) :: U(3)
REAL(KIND_DOR) :: AziAngle

!get origin to find azimuthal angle gamma with
REAL(KIND_DOR),PARAMETER :: origin(2) = (/1._KIND_DOR,0._KIND_DOR/)

!!--begin--

!calculate azimuthal angle
AziAngle = xyANGLE_VV( origin , U(1:2) )

!!--end--
END FUNCTION



!!### SUBROUTINE <<SETUP_DiscreteOrdinates>>
SUBROUTINE SETUP_DiscreteOrdinates( NDim , UniformZGeometry , &
 Order,aOrder,pOrder,&
 aQuadrature,pQuadrature,Quadrature,&
 QuadratureType,Ordinates , Weights , fdbk )

!!#### PURPOSE
!! Set up all the discrete ordinates variables according
!! to the option settings currently in <VAR_DiscreteOrdinates>.


!!#### REQUIRED INPUT
!! + number of dimensions <NDim>
INTEGER      ,INTENT(IN) :: NDim
LOGICAL      ,INTENT(IN) :: UniformZGeometry
INTEGER      ,INTENT(IN) :: Order,aOrder,pOrder
CHARACTER(10),INTENT(IN) :: Quadrature,aQuadrature,pQuadrature


!!#### REQUIRED OUTPUT
INTEGER       ,INTENT(INOUT) :: QuadratureType
REAL(KIND_DOR),POINTER       :: Ordinates(:,:),Weights(:)

!!#### OPTIONAL INPUT/OUTPUT
!! + feedback variable <fdbk>
TYPE(TYPE_fdbk),OPTIONAL,INTENT(INOUT) :: fdbk


!!#### LOCAL VARIABLES
INTEGER                :: jerr
REAL                   :: tin,tout,dt
INTEGER                :: errint
CHARACTER(100)         :: errmsg
REAL(KIND_DOR),POINTER :: xa_prod( : , : )
REAL(KIND_DOR),POINTER :: wa_prod    ( : )
REAL(KIND_DOR),POINTER :: xp_prod( : )
REAL(KIND_DOR),POINTER :: wp_prod( : )
REAL(KIND_DOR),POINTER :: xp(:)    ,wp(:)
REAL(KIND_DOR),POINTER :: x(:,:),w(:)
REAL(KIND_DOR),POINTER :: xa(:,:,:),wa(:,:)
INTEGER       ,POINTER :: m_map(:,:)

!!#### IDENTIFICATION
CHARACTER(*),PARAMETER :: proc_="SETUP_DiscreteOrdinates"

!!--begin--

CALL CPU_TIME(tin)

!get quadrature set
SELECT CASE( QuadratureType )

 CASE( DOR_LEVEL )
  CALL levQuadrature( Quadrature , aOrder , pOrder , &
                      xa , xp , wa , wp , &
                      errint , errmsg , &
                      UniformZGeometry )

 CASE( DOR_PRODUCT  )
  CALL prodQuadrature( aQuadrature , aOrder , &
                       pQuadrature , pOrder , &
                       xa_prod , xp_prod , wa_prod , wp_prod , &
                       errint , errmsg , &
                       UniformZGeometry )

  CASE( DOR_GENERAL )
  CALL genQuadrature( Quadrature , Order , &
                      x , w , &
                      errint , errmsg , &
                      UniformZGeometry )
END SELECT

!update
IF( errint<0 )THEN
 CALL UpdateAndDump(fdbk_error,fdbk,s="[[DOR]] Error code [errint="//&
   TRIM(STR(errint))//"] was returned with [errmsg="//errmsg//"]")
ELSE IF( errint>0 )THEN
 CALL UpdateAndDump(fdbk_warning,fdbk,s="[[DOR]] Warning code [errint="//&
   TRIM(STR(errint))//"] was returned with [errmsg="//errmsg//"]")
END IF

IF( errint==0 )THEN
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[DOR]] Setup completed successfully." )
ELSE
 CALL UpdateAndDump(fdbk_comment,fdbk,s="[[DOR]] Setup may have problems..." )
END IF


CALL UpdateAndDump(fdbk_comment,fdbk,s="[[DOR]] Mapping &
  &all quadrature sets to a general quadrature set." )

SELECT CASE(QuadratureType)
 CASE(DOR_PRODUCT)
   CALL genQuadrature_FROM_prod( xa_prod , xp_prod , wa_prod , wp_prod , &
      m_map , Ordinates , Weights , errint , errmsg )
   DEALLOCATE( m_map , xa_prod , xp_prod , wa_prod , wp_prod )

 CASE(DOR_LEVEL)
   CALL genQuadrature_FROM_lev( xa , xp , wa , wp , &
    m_map , Ordinates , Weights , errint , errmsg )
    DEALLOCATE( m_map , xa , xp , wa , wp )

 CASE(DOR_GENERAL)
   Ordinates => x
   Weights   => w
   NULLIFY( x , w )

END SELECT

IF( errint/=0 )CALL UPDATE(fdbk_Choose,fdbk,I=errint,S=TRIM(errmsg))

QuadratureType=DOR_GENERAL

CALL UpdateAndDump(fdbk_comment,fdbk,s="[[DOR]] Renormalizing &
  &quadrature set to sum to 4*PI." )
CALL RENORMALIZE(Ordinates,Weights)


!end time
CALL CPU_TIME(tout)
dt = (tout-tin)

!timing
CALL UpdateAndDump(fdbk_profile,fdbk,s="[[DOR]] Setup ran in &
  &[time="//STRTIME(dt)//"].")


!!--end--
END SUBROUTINE



END MODULE
