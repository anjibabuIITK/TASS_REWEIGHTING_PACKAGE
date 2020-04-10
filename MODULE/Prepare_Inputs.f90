MODULE Prepare_Inputs
USE GetSteps
!========================================Preparing Inputs================================================================
!                       THIS SUBROUTINE IS WRITTEN TO PREPARE THE USER INPUT
!========================================================================================================================
IMPLICIT NONE
INTEGER :: i,n, k,io_stat, ncol,ncolumn, mdsteps,t_min,t_max,mtd_steps
INTEGER :: ncv,uscv, mtdcv,hill_freq,cv_freq,nbin,nmetad,mtd_on_whichCV
INTEGER :: w_cv,w_hill,prob_dimension,mtd_dimension,i_cv(30)
REAL*8  :: cv_temp, sys_temp,biasfactor,KbT
REAL*8  :: t_start,t_end
REAL*8,ALLOCATABLE     :: hill(:),width(:),height(:),rbias(:),cv(:,:)
REAL*8,ALLOCATABLE     :: prob(:),ct(:),vbias(:)
REAL*8,DIMENSION(30,3) :: grid

CHARACTER(len=20)   :: KEYWORD, cvfile, hillfile,dummy, dummy2
CHARACTER(len=20)   :: periodic_CV
CHARACTER(len=1000) :: text
LOGICAL             :: metad=.TRUE.,periodic

REAL*8, PARAMETER :: kb=1.9872041E-3 !kcal K-1 mol-1
REAL*8, PARAMETER :: au_to_kcal = 627.51
REAL*8, PARAMETER :: kj_to_kcal = 0.239306
!--------------------------------------------------------------------------------------------------------------------------!
CONTAINS
!--------------------------------------------------------------------------------------------------------------------------!
! subroutine to initialize all the variables
SUBROUTINE Set_defaults(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,periodic,nbin,t_min,t_max, &
           & nmetad,prob_dimension,mtd_dimension)

IMPLICIT NONE
REAL*8, INTENT(INOUT)  :: cv_temp, sys_temp
INTEGER  :: prob_dimension,mtd_dimension
INTEGER, INTENT(INOUT) :: ncv, uscv, mtdcv,hill_freq,cv_freq,nbin,t_min,t_max,nmetad
LOGICAL, INTENT(INOUT) :: metad,periodic
CHARACTER (len=20), INTENT(INOUT) :: cvfile,hillfile

cv_temp=300.d0; sys_temp=300.d0
ncv=3; uscv=1; mtdcv=1;hill_freq=500;cv_freq=10
cvfile="COLVAR";hillfile="HILLS"
metad=.TRUE.;periodic=.FALSE.;nbin=101;t_min=1;nmetad=1
prob_dimension = 1 ; mtd_dimension = 1
OPEN(12, FILE="COLVAR", STATUS="OLD")
t_max=NSteps(12)
IF (t_max .lt. 0) THEN
PRINT*,"ERROR: -> PLEASE CHECK COLVAR FILE STOPPING..."
STOP ; ENDIF
CLOSE(12)
END SUBROUTINE
!--------------------------------------------------------------------------------------------------------------------------!
!Fortran program to read input from a file
SUBROUTINE Get_Inputs(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,biasfactor,periodic, & 
                & nbin,t_min,t_max,grid,mtd_on_whichCV,prob_dimension,mtd_dimension,periodic_CV,i_cv,n)

IMPLICIT NONE
INTEGER :: i, n, k, io_stat, it 
INTEGER,INTENT(INOUT)  :: ncv,nbin,uscv, mtdcv,hill_freq,cv_freq,t_min,t_max,mtd_on_whichCV
INTEGER  :: prob_dimension,mtd_dimension,i_cv(30)
REAL*8,INTENT(INOUT)   :: cv_temp, sys_temp,biasfactor
REAL*8,DIMENSION(30,3) :: grid
CHARACTER(len=20)      :: KEYWORD, dummy, dummy2
CHARACTER(len=1000)    :: text
CHARACTER(len=20),INTENT(INOUT) ::cvfile, hillfile
CHARACTER(len=20),DIMENSION(30) :: grid1
CHARACTER(len=20)   :: PERIODICITY,periodic_CV,ch
LOGICAL,INTENT(INOUT) :: metad,periodic
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
    CASE ('MTD_ON_CV')
      READ(text,*) KEYWORD,dummy,mtd_on_whichCV
    CASE ('PROB_DIMENSION')
     READ(text,*) KEYWORD,dummy,prob_dimension
    CASE ('MTD_DIMENSION')
     READ(text,*) KEYWORD,dummy,mtd_dimension
    CASE DEFAULT
    END SELECT
!
    SELECT CASE (KEYWORD) 
    CASE ("PERIODIC")
      READ(text,*) KEYWORD
      !PRINT*,KEYWORD 
      IF (KEYWORD  .eq. "PERIODIC") THEN 
      READ(10,*)KEYWORD,dummy,PERIODICITY
        IF (PERIODICITY .eq. "ON") THEN
          READ(10,*) dummy,dummy2,periodic_CV
          it = ICHAR(periodic_CV(1:1))
        IF (it .ge. 48 .and. it .le. 57)  THEN
          READ(periodic_CV,'(I5)') n
          READ(10,*)dummy,dummy2,i_cv(1:n)
        ENDIF 
      IF(PERIODICITY == "ON") periodic=.TRUE.
      ELSE
      PRINT*,"*************************************************************************************"
      PRINT*, "		  	  ERROR IN PERIODIC SECTION :-> STOPPING...			   "
      PRINT*,"*************************************************************************************"
      STOP ; ENDIF
     READ(10,*)KEYWORD
     IF(KEYWORD .ne. "END_PERIODIC") THEN
      PRINT*,"*************************************************************************************"
      PRINT*, "		    PLEASE END THE SECTION, PERIODIC :-> STOPPING...			   "
      PRINT*,"*************************************************************************************"
      STOP ; ENDIF ; ENDIF

    CASE DEFAULT
    END SELECT
!   
    SELECT CASE (KEYWORD)
    CASE ('GRID_INFORMATION')
      READ(text,*) KEYWORD
    IF (ncv .gt. 30) THEN
    PRINT*,"*************************************************************************************"
    PRINT*,"ERROR: -> MORE THAN 30 CV's ARE NOT IMPLIMENTED PLEASE CONTACT ADMIN :--> STOPPING..." 
    PRINT*,"*************************************************************************************"
    STOP;ENDIF
    IF (mtdcv .gt. 2) THEN
    PRINT*,"*****************************************************************************************"
    PRINT*,"ERROR: -> CAN NOT UNBIAS FOR MORE THAN 2 MetaD CV's PLEASE CONTACT ADMIN :--> STOPPING..."
    PRINT*,"*****************************************************************************************"
    STOP ; ENDIF
    IF (KEYWORD .eq. 'GRID_INFORMATION') THEN
       DO i = 1,NCV
         READ(10,*) grid1(i),dummy,grid(i,1:2)
          grid(i,3) = (grid(i,2) - grid(i,1))/NBIN
            IF (grid(i,3) .lt. 0.0D0 .or. grid(i,3) .gt. 1e+16) THEN
              PRINT*,"*************************************************************************************************************"
              PRINT*,"	  ERROR: -> GRID SIZE CAN NOT BE 0 OR INFINITY, PLEASE CHECK BIN SIZE OR GRID VALUES :--> STOPPING..." 
              PRINT*,"*************************************************************************************************************"
            STOP;ENDIF
       ENDDO
      ELSE
    PRINT*,"**********************************************************************"
    PRINT*,'    !!ERROR IN THE INPUT FILE NEAR GRID INFORMATION :-> STOPPING...'
    PRINT*,"**********************************************************************"
    STOP ; ENDIF
    READ(10,*) KEYWORD
    IF (KEYWORD .ne. "END_GRID_INFORMATION") THEN
    PRINT*,"**********************************************************************"
    PRINT*,'	ERROR: -> PLEASE END SECTION, GRID INFORMATION :-> STOPPING...'
    PRINT*,"**********************************************************************"
    STOP ; ENDIF
    CASE DEFAULT

END SELECT
ENDDO
!---------------------------------!
! Setting default Periodicity
IF(periodic .eqv..FALSE.) THEN
periodic_CV="NONE"
ENDIF
!---------------------------------!
CLOSE(10)
END SUBROUTINE 
!--------------------------------------------------------------------------------------------------------------------------!
END MODULE Prepare_Inputs
