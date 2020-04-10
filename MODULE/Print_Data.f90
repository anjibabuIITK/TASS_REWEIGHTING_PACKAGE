MODULE Print_Data
!USE Prepare_Inputs        
CONTAINS
!-----------------------------------------!
SUBROUTINE data_print (cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,biasfactor,periodic, &
        & nbin,t_min,t_max,grid,mtd_on_whichCV,prob_dimension,mtd_dimension,periodic_CV,i_cv,n)

IMPLICIT NONE
INTEGER :: i,n,i_cv(*),prob_dimension,mtd_dimension
INTEGER,INTENT(INOUT)  :: ncv, uscv, mtdcv,hill_freq,cv_freq,nbin,t_min,t_max,mtd_on_whichCV
REAL*8,INTENT(INOUT)   :: cv_temp, sys_temp,biasfactor
REAL*8,DIMENSION(30,3) :: grid
CHARACTER(len=20),INTENT(INOUT) :: cvfile, hillfile,periodic_CV
CHARACTER(len=20),DIMENSION(20) :: grid1
LOGICAL,INTENT(INOUT) :: metad,periodic

WRITE(*,100)cv_temp
WRITE(*,200)sys_temp
WRITE(*,300)ncv
WRITE(*,400)mtdcv
WRITE(6,99)"METAD_CV_INDEX     =", mtd_on_whichCV
WRITE(*,500)hill_freq
WRITE(*,600)cv_freq
WRITE(*,700)cvfile
WRITE(*,800)hillfile
WRITE(*,900)biasfactor
IF ((periodic) .and. periodic_CV .ne. "ALL") WRITE(*,1100)"PERIODIC CV INDEX  =",i_cv(1:n)
IF ((periodic) .and. periodic_CV .eq. "ALL") WRITE(*,"(A,1X,A)")"PERIODIC CV INDEX  =","ALL CV's ARE PERIODIC"
IF (periodic_CV .eq. "NONE") WRITE(*,"(A,1X,A)")"PERIODIC CV INDEX  =","NO CV IS PERIODIC"
DO i = 1,ncv
WRITE(*,1200)i,'=',grid(i,1:3)
ENDDO
WRITE(*,1400)nbin
WRITE(6,1000)"MTD_DIMENSION      = ",mtd_dimension
WRITE(6,1000)"PROB_DIMENSION     = ",prob_dimension
WRITE(*,1700)t_min
WRITE(*,1800)t_max
99  FORMAT (A,5X,I2)
100 FORMAT("CV TEMP     ",7X,"=",2X,F8.1,'K')
200 FORMAT("SYSTEM TEMP ",7X,"=",2X,F8.1,'K')
300 FORMAT("NO. of CVs  ",7X,"=",2X,I5,2X)
400 FORMAT("METAD CVs   ",7X,"=",2X,I5,2X)
500 FORMAT("METAD PACE  ",7X,"=",2X,I5,2X)
600 FORMAT("CV STRIDE   ",7X,"=",2X,I5,2X)
700 FORMAT("CV FILE     ",7X,"=",4X,A,2X)
800 FORMAT("HILL FILE   ",7X,"=",4X,A,2X)
900 FORMAT("BIASFACTOR  ",7X,"=",F8.1,' JK^-1')
1000 FORMAT(A,4X,I2)
1100 FORMAT(A,4X,2I2)
!1200 FORMAT("GRID1:      ",5X,"=",2X,F8.2,2X,F8.2,2X,F8.2)
1200 FORMAT ("GRID",I1,14X,A,4F8.2)
1400 FORMAT("NBIN        ",7X,"=",2X,I5,2X)
1700 FORMAT("T_MIN       ",7X,"=",2X,I5,2X)
1800 FORMAT("T_MAX       ",7X,"=",2X,I5,2X)

END SUBROUTINE
END MODULE Print_Data
