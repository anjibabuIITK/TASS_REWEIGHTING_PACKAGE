MODULE Print_Data
!USE Prepare_Inputs        
CONTAINS
!-----------------------------------------!
SUBROUTINE data_print (cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,biasfactor,periodic, &
        & nbin,t_min,t_max,grid,mtd_on_whichCV)

IMPLICIT NONE
INTEGER :: i
CHARACTER(len=20),INTENT(INOUT)::cvfile, hillfile
LOGICAL,INTENT(INOUT)::metad,periodic
REAL*8,INTENT(INOUT)::cv_temp, sys_temp,biasfactor
REAL*8,DIMENSION(3,20) :: grid
CHARACTER*20,DIMENSION(20) :: grid1
INTEGER,INTENT(INOUT)::ncv, uscv, mtdcv,hill_freq,cv_freq,nbin,t_min,t_max,mtd_on_whichCV
CHARACTER(len=1000)::text
WRITE(*,100)cv_temp
WRITE(*,200)sys_temp
WRITE(*,300)ncv
WRITE(*,400)mtdcv
WRITE(*,500)hill_freq
WRITE(*,600)cv_freq
WRITE(*,700)cvfile
WRITE(*,800)hillfile
WRITE(*,900)biasfactor
DO i = 1,ncv
WRITE(*,1200)i,'=',grid(1:3,i)
ENDDO
WRITE(*,1400)nbin
WRITE(*,1700)t_min
WRITE(*,1800)t_max
WRITE(6,*)"METAD      ","     ="," ", metad
!WRITE(6,*)"METAD_CV_INDEX","     ="," ", mtd_on_whichCV
WRITE(6,*)"PERIODICITY","     ="," ", periodic
100 FORMAT("CV TEMP     ",5X,"=",2X,F8.2,2X)
200 FORMAT("SYSTEM TEMP ",5X,"=",2X,F8.2,2X)
300 FORMAT("NO. of CVs  ",5X,"=",2X,I5,2X)
!1000 FORMAT("METAD      ",5X,"=",2X,A,2X)
400 FORMAT("METAD CVs   ",5X,"=",2X,I5,2X)
500 FORMAT("METAD PACE  ",5X,"=",2X,I5,2X)
600 FORMAT("CV STRIDE   ",5X,"=",2X,I5,2X)
700 FORMAT("CV FILE     ",5X,"=",2X,a,2X)
800 FORMAT("HILL FILE   ",5X,"=",2X,a,2X)
900 FORMAT("BIASFACTOR  ",5X,"=",2X,F8.2,2X)
!1200 FORMAT("GRID1:      ",5X,"=",2X,F8.2,2X,F8.2,2X,F8.2)
1200 FORMAT ("GRID",I1,12X,A,4F10.2)
1400 FORMAT("NBIN        ",5X,"=",2X,I5,2X)
1700 FORMAT("T_MIN       ",5X,"=",2X,I5,2X)
1800 FORMAT("T_MAX       ",5X,"=",2X,I5,2X)

END SUBROUTINE
END MODULE Print_Data
