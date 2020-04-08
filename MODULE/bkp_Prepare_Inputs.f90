MODULE Prepare_Inputs
USE Plumed_Read
USE GetSteps
!========================================Preparing Inputs================================================================
!                       THIS SUBROUTINE IS WRITTEN TO PREPARE THE USER INPUT
!========================================================================================================================
IMPLICIT NONE
CHARACTER(len=20)::KEYWORD, cvfile, hillfile,dummy, dummy2
CHARACTER(len=1000)::text
LOGICAL::metad=.TRUE.,periodic
CHARACTER(len=20) ::periodic_CV
REAL*8::cv_temp, sys_temp,biasfactor,KbT
INTEGER::i,n, k, io_stat, ncol,ncolumn, mdsteps,t_min,t_max,mtd_steps
INTEGER::ncv,uscv, mtdcv,hill_freq,cv_freq,nbin,nmetad,mtd_on_whichCV
INTEGER :: w_cv,w_hill,prob_dimension,mtd_dimension
REAL*8,DIMENSION(30,3) :: grid
REAL*8,ALLOCATABLE :: hill(:),width(:),height(:),rbias(:),cv(:,:)
REAL*8,ALLOCATABLE :: prob(:),ct(:),vbias(:)

REAL*8, PARAMETER :: kb=1.9872041E-3 !kcal K-1 mol-1
REAL*8, PARAMETER :: au_to_kcal = 627.51
REAL*8, PARAMETER :: kj_to_kcal = 0.239306
!--------------------------------------------------------------------------------------------------------------------------!
CONTAINS
!--------------------------------------------------------------------------------------------------------------------------!
! subroutine to initialize all the variables
SUBROUTINE Set_defaults(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,periodic,nbin,t_min,&
            &t_max,nmetad,prob_dimension,mtd_dimension)
IMPLICIT NONE
REAL*8, INTENT(INOUT):: cv_temp, sys_temp
INTEGER, INTENT(INOUT)::ncv, uscv, mtdcv,hill_freq,cv_freq,nbin,t_min,t_max,nmetad,prob_dimension,mtd_dimension
CHARACTER (len=20), INTENT(INOUT)::cvfile,hillfile
LOGICAL, INTENT(INOUT)::metad,periodic

cv_temp=300.d0; sys_temp=300.d0
ncv=3; uscv=1; mtdcv=1;hill_freq=500;cv_freq=10
cvfile="COLVAR";hillfile="HILLS"
metad=.TRUE.;periodic=.FALSE.;nbin=101;t_min=1;nmetad=1
prob_dimension=1;mtd_dimension=1

open(12, file="COLVAR", status="old")
t_max=NSteps(12)
close(12)
END SUBROUTINE
!--------------------------------------------------------------------------------------------------------------------------!
!Fortran program to read input from a file
SUBROUTINE Get_Inputs(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,biasfactor,periodic, & 
                & nbin,t_min,t_max,grid,mtd_on_whichCV,prob_dimension,mtd_dimension)

IMPLICIT NONE
!CHARACTER(len=20) ::cvfile, hillfile
CHARACTER(len=20),INTENT(INOUT)::cvfile, hillfile
LOGICAL,INTENT(INOUT)::metad,periodic
REAL*8,INTENT(INOUT)::cv_temp, sys_temp,biasfactor
REAL*8,DIMENSION(30,3) :: grid
CHARACTER*20,DIMENSION(20) :: grid1
INTEGER,INTENT(INOUT)::ncv,nbin,uscv, mtdcv,hill_freq,cv_freq,t_min,t_max,mtd_on_whichCV
INTEGER,INTENT(INOUT)::prob_dimension,mtd_dimension
CHARACTER(len=20)::KEYWORD, dummy, dummy2
CHARACTER(len=1000)::text
INTEGER::i,n, k, io_stat 
!-----------------------------------------------------------------------------------------------------------------------------!
OPEN(unit=10, file='admin_input.inp', status='unknown')
REWIND(10)
DO
READ(10, '(a)', iostat=io_stat) text
if(io_stat .ne. 0) exit
READ(text,*) KEYWORD
SELECT CASE (KEYWORD)

    CASE ('CVFILE')
      READ(text,*) KEYWORD,dummy,cvfile
    CASE ('HILLSFILE')
      READ(text,*) KEYWORD,dummy,hillfile
    CASE ('NCV')
      READ(text,*) KEYWORD,dummy,ncv
    CASE ('NMTDCV')
      READ(text,*) KEYWORD,dummy,mtdcv
    CASE ('USCV')
      READ(text,*) KEYWORD,dummy,uscv
    CASE ('PACE')
      READ(text,*) KEYWORD,dummy,hill_freq
    CASE ('CV_TEMP')
      READ(text,*) KEYWORD,dummy,cv_temp
    CASE ('SYSTEM_TEMP')
      READ(text,*) KEYWORD,dummy,sys_temp
    CASE ('STRIDE')
      READ(text,*) KEYWORD,dummy,cv_freq
    CASE ('BIASFACTOR')
      READ(text,*) KEYWORD,dummy,biasfactor
    CASE ('METAD')
      READ(text,*) KEYWORD,dummy,dummy2
      IF(dummy2 == "OFF") metad=.FALSE.
    CASE ('NBIN')
      READ(text,*) KEYWORD,dummy,nbin
    CASE ('T_MIN')
      READ(text,*) KEYWORD,dummy,t_min
    CASE ('T_MAX')
      READ(text,*) KEYWORD,dummy,t_max
    CASE ('PERIODIC_CV')
      READ(text,*) KEYWORD,dummy,periodic_CV
    CASE ('PERIODICITY')
      READ(text,*) KEYWORD,dummy,dummy2
      IF(dummy2 == "ON") periodic=.TRUE.
    CASE ('MTD_ON_CV')
      READ(text,*) KEYWORD,dummy,mtd_on_whichCV
    CASE ('PROB_DIMENSION')
      READ(text,*) KEYWORD,dummy,prob_dimension
    CASE ('MTD_DIMENSION')
      READ(text,*) KEYWORD,dummy,mtd_dimension
!---------------------------------!
    CASE ('GRID_INFORMATION')

      READ(text,*) KEYWORD
!      GOTO 200
         IF (KEYWORD .eq. 'GRID_INFORMATION') THEN
         DO i = 1,NCV
            READ(10,*) grid1(i),dummy,grid(i,1:2)
            grid(i,3) = (grid(i,2) - grid(i,1))/NBIN
         ENDDO
!   DO i = 1,NCV
!     WRITE(6,100) grid(i),(i),dummy,gridmin(i),gridmax(i),gridwidth(i)
!  ENDDO
        ELSE
          PRINT*,'!!ERROR IN THE INPUT FILE NEAR GRID INFORMATION :-> STOPPING...'
          STOP
       ENDIF
!---------------------------------!
    CASE DEFAULT

END SELECT
ENDDO
!---------------------------------!
! Setting default Periodicity
IF(periodic .eqv..FALSE.) THEN
periodic_CV="NONE"
ENDIF
!---------------------------------!
!100 FORMAT (A4,I1,2X,A,4F16.2)
!200 CONTINUE
!IF (KEYWORD .eq. 'GRID_INFORMATION') THEN
!   DO i = 1,NCV
!     READ(10,*) grid1(i),dummy,grid(i,1:2)
!      grid(i,3) = (grid(i,2) - grid(i,1))/NBIN
!   ENDDO
!!   DO i = 1,NCV
!!     WRITE(6,100) grid(i),(i),dummy,gridmin(i),gridmax(i),gridwidth(i)
!!  ENDDO
 ! ELSE
 !   PRINT*,'!!ERROR IN THE INPUT FILE NEAR GRID INFORMATION :-> STOPPING...'
 !   STOP
!ENDIF
CLOSE(10)
END SUBROUTINE 
!--------------------------------------------------------------------------------------------------------------------------!
END MODULE Prepare_Inputs
