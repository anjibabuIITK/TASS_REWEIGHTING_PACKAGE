MODULE Prepare_Inputs
USE GetSteps
USE ReadFiles
IMPLICIT NONE
CHARACTER(len=20)::KEYWORD, cvfile, hillfile,dummy, dummy2,periodic_CV
CHARACTER(len=1000)::text
LOGICAL::metad=.TRUE.,periodic
REAL*8::cv_temp, sys_temp,biasfactor,gridmin1,gridmax1,gridmin2,gridmax2,gridmin3,gridmax3,gridmin4,gridmax4
REAL*8::gridwidth1,gridwidth2,gridwidth3,gridwidth4
INTEGER::i,n, k, io_stat, ncolumn, mdsteps,t_min,t_max,mtdsteps
INTEGER::ncv, uscv, mtdcv,hill_freq,cv_freq,nbin,nmetad,mtd_on_whichCV
REAL*8, PARAMETER :: kb=1.9872041E-3 !kcal K-1 mol-1
REAL*8, PARAMETER :: au_to_kcal = 627.51
REAL*8, PARAMETER :: kj_to_kcal = 0.239306
REAL*8, ALLOCATABLE::cv(:,:),hill(:),height(:),width(:),vbias(:),ct(:),rbias(:)


!--------------------------------------------!
CONTAINS
!--------------------------------------------!
! subroutine to initialize all the variables
SUBROUTINE Set_defaults(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,periodic,nbin,t_min,t_max,nmetad&
&,periodic_CV,mtd_on_whichCV)
IMPLICIT NONE
REAL*8, INTENT(INOUT):: cv_temp, sys_temp
INTEGER, INTENT(INOUT)::ncv, uscv, mtdcv,hill_freq,cv_freq,nbin,t_min,t_max,nmetad,mtd_on_whichCV
CHARACTER (len=20), INTENT(INOUT)::cvfile,hillfile,periodic_CV
LOGICAL, INTENT(INOUT)::metad,periodic

cv_temp=300.d0; sys_temp=300.d0
ncv=3; uscv=1; mtdcv=1;hill_freq=500;cv_freq=10
cvfile="COLVAR";hillfile="HILLS";periodic_CV="NONE";mtd_on_whichCV=2
metad=.TRUE.;periodic=.FALSE.;nbin=101;t_min=1;t_max=0;nmetad=1

END SUBROUTINE
!-----------------------------------------!
! Subroutine to check existance of given file
SUBROUTINE Is_File_Exist(givenfile)
IMPLICIT NONE
CHARACTER(len=10)::givenfile
LOGICAL::fexist

inquire (file=givenfile,exist=fexist)
if(fexist) then
         write(*,*) " The file"," ",givenfile," exists!"
       else
         write(*,*) " The file"," ",givenfile," does not exist!"
         stop
       end if
END SUBROUTINE 
!-----------------------------------------!
SUBROUTINE Read_Plumed_Input()
IMPLICIT NONE
LOGICAL::fexist

! Remove previous;y existed admin_input.inp file
inquire (file="admin_input.inp",exist=fexist)
       if(fexist) then
!         write(*,*) " The file admin_input.inp has exists!"
       CALL SYSTEM("rm admin_input.inp")
       end if

OPEN(unit=100,file='ReadPlumedInput.sh',status='unknown')
WRITE(100,*)'# --------------------------------------------------# '
WRITE(100,*)"# This script is a part of TASS reweighting codes"
WRITE(100,*)'# --------------------------------------------------# '
WRITE(100,*)'#'
WRITE(100,*)'# Authour: Anji Babu Kapakayala'
WRITE(100,*)'#          C/O Prof. Nisanth N. Nair'
WRITE(100,*)'#          Dept. Of Chemistry, IIT Kanpur, India.'
WRITE(100,*)'#'
WRITE(100,*)'# --------------------------------------------------# '
WRITE(100,*)'#'
WRITE(100,*)'#!/bin/bash'
WRITE(100,*)'sed -i "/^#/d" plumed.dat'
WRITE(100,*)"awk -F",'" "', " '{"
WRITE(100,*)'       for(i=1;i<=NF;i++) {'
WRITE(100,*)'{ split($i, a, "=")'
WRITE(100,*)'  if(a[1] == "BIASFACTOR" || a[1] == "PACE" ||a[1] == "STRIDE" ) {'
WRITE(100,*)'#               print a[1] " = "  a[2]'
WRITE(100,*)'                print a[1] " = "  a[2] >> "admin_input.inp"'
WRITE(100,*)'} else if (a[1] == "METAD"){'
WRITE(100,*)'print a[1] " = "  "ON" >> "admin_input.inp"'
WRITE(100,*)'} else if (a[2] == "COLVAR") {'
WRITE(100,*)'print "CVFILE" " = "  a[2] >> "admin_input.inp"'
WRITE(100,*)'} else if (a[2] == "HILLS") {'
WRITE(100,*)'print "HILLSFILE" " = "  a[2] >> "admin_input.inp"'
WRITE(100,*)'} else if (a[1] == "TEMP") {'
WRITE(100,*)'print "CV_TEMP" " = "  a[2] >> "admin_input.inp"'
WRITE(100,*)'}'
WRITE(100,*)
WRITE(100,*)'}'
WRITE(100,*)'  }'
WRITE(100,*)"}' plumed.dat"
WRITE(100,*)'# --------------------------------------------------# '

CLOSE(100)
CALL SYSTEM("sh ReadPlumedInput.sh")
CALL SYSTEM("cat input.inp >> admin_input.inp")

END SUBROUTINE
!-----------------------------------------!
!Fortran program to read input from a file
SUBROUTINE Get_Inputs(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,biasfactor&
&,periodic,gridmin1,gridmin2,gridmax1,gridmax2,gridmin3,gridmax3,gridmin4,gridmax4,gridwidth1,gridwidth2&
&,gridwidth3,gridwidth4,t_min,t_max,nbin,mdsteps,ncolumn,cv,periodic_CV,mtd_on_whichCV)
IMPLICIT NONE
CHARACTER(len=20),INTENT(INOUT)::cvfile, hillfile,periodic_CV
LOGICAL,INTENT(INOUT)::metad,periodic
REAL*8,INTENT(INOUT)::cv_temp, sys_temp,biasfactor,gridmin1,gridmin2,gridmax1,gridmax2,gridmin3,gridmax3,gridmin4,gridmax4
REAL*8,INTENT(INOUT)::gridwidth1,gridwidth2,gridwidth3,gridwidth4
INTEGER,INTENT(INOUT)::ncv, uscv, mtdcv,hill_freq,cv_freq,nbin,t_min,t_max,mdsteps,ncolumn,mtd_on_whichCV
CHARACTER(len=20)::KEYWORD, dummy, dummy2
CHARACTER(len=1000)::text
INTEGER::i,n, k, io_stat 
REAL*8,INTENT(INOUT), ALLOCATABLE::cv(:,:)


!OPEN(unit=10, file='input.inp', status='unknown')
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
    CASE ('PERIODICITY')
      READ(text,*) KEYWORD,dummy,dummy2
      IF(dummy2 == "ON") periodic=.TRUE.
    CASE ('PERIODIC_CV')
      READ(text,*) KEYWORD,dummy,periodic_CV
    CASE ('NBIN')
      READ(text,*) KEYWORD,dummy,nbin
    CASE ('GRID1')
      READ(text,*) KEYWORD,dummy,gridmin1,gridmax1
    CASE ('GRID2')
      READ(text,*) KEYWORD,dummy,gridmin2,gridmax2
    CASE ('GRID3')
      READ(text,*) KEYWORD,dummy,gridmin3,gridmax3
    CASE ('GRID4')
      READ(text,*) KEYWORD,dummy,gridmin4,gridmax4
    CASE ('T_MIN')
      READ(text,*) KEYWORD,dummy,t_min
    CASE ('T_MAX')
      READ(text,*) KEYWORD,dummy,t_max
    CASE ('MTD_ON_CV')
      READ(text,*) KEYWORD,dummy,mtd_on_whichCV
    CASE DEFAULT
!      READ(text,*) KEYWORD,dummy,dummy2

END SELECT
ENDDO

!---------------------------------!
! Setting default Periodicity
IF(periodic .eqv..FALSE.) then
periodic_CV="NONE"
endif
!---------------------------------!
!Calculating gridwidth
gridwidth1=GridWidth(gridmin1,gridmax1,nbin)
gridwidth2=GridWidth(gridmin2,gridmax2,nbin)
gridwidth3=GridWidth(gridmin3,gridmax3,nbin)
gridwidth4=GridWidth(gridmin4,gridmax4,nbin)
!---------------------------------!
! Get CV values, mdsteps, ncolumn
call ReadCVFile(cvfile,cv,mdsteps,ncolumn,periodic_CV)
!---------------------------------!
! Conditions
IF(t_max.gt.mdsteps)STOP '!!ERROR: t_max > total MD steps'
!mdsteps=t_max
!---------------------------------!



CLOSE(10)
END SUBROUTINE 
!-----------------------------------------!
!SUBROUTINE Print_Data(cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq)
SUBROUTINE Print_Data(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,biasfactor,periodic,&
&gridmin1,gridmin2,gridmax1,gridwidth1,gridmax2,gridmin3,gridmax3,gridmin4,gridmax4,gridwidth2,gridwidth3,gridwidth4&
&,t_min,t_max,nbin,mdsteps,ncolumn,periodic_CV,mtd_on_whichCV)
IMPLICIT NONE
CHARACTER(len=20),INTENT(INOUT)::cvfile, hillfile,periodic_CV
LOGICAL,INTENT(INOUT)::metad,periodic
REAL*8,INTENT(INOUT)::cv_temp, sys_temp,biasfactor,gridmin1,gridmin2,gridmax1,gridmax2,gridmin3,gridmax3,gridmin4,gridmax4
REAL*8, INTENT(INOUT)::gridwidth1,gridwidth2,gridwidth3,gridwidth4
INTEGER,INTENT(INOUT)::ncv, uscv, mtdcv,hill_freq,cv_freq,nbin,t_min,t_max,mdsteps,ncolumn,mtd_on_whichCV
CHARACTER(len=1000)::text
INTEGER::i,j

WRITE(*,100)cv_temp
WRITE(*,200)sys_temp
WRITE(*,300)ncv
WRITE(*,400)mtdcv
WRITE(*,500)hill_freq
WRITE(*,600)cv_freq
WRITE(*,700)cvfile
WRITE(*,800)hillfile
WRITE(*,900)biasfactor
WRITE(*,1200)gridmin1,gridmax1,gridwidth1
WRITE(*,1300)gridmin2,gridmax2,gridwidth2
WRITE(*,1500)gridmin3,gridmax3,gridwidth3
WRITE(*,1600)gridmin4,gridmax4,gridwidth4
WRITE(*,1400)nbin
WRITE(*,1700)t_min
WRITE(*,1800)t_max
WRITE(*,1900)mdsteps
WRITE(*,2100)mtd_on_whichCV
write(*,2000)ncolumn
WRITE(*,*)"METAD      ","     ="," ", metad
WRITE(*,*)"PERIODICITY","     ="," ", periodic
WRITE(*,*)"PERIODIC_CV","     ="," ", periodic_CV
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
!1200 FORMAT("GRID:      ",5X,"=",2X,F8.2,2X,F8.2,2X,F8.2)
1200 FORMAT("GRID1:      ",5X,"=",2X,F8.2,2X,F8.2,2X,F8.2)
1300 FORMAT("GRID2:      ",5X,"=",2X,F8.2,2X,F8.2,2X,F8.2)
1500 FORMAT("GRID3:      ",5X,"=",2X,F8.2,2X,F8.2,2X,F8.2)
1600 FORMAT("GRID4:      ",5X,"=",2X,F8.2,2X,F8.2,2X,F8.2)
1400 FORMAT("NBIN        ",5X,"=",2X,I5,2X)
1700 FORMAT("T_MIN        ",5X,"=",2X,I5,2X)
1800 FORMAT("T_MAX        ",5X,"=",2X,I5,2X)
1900 FORMAT("MDSTEPS      ",5X,"=",2X,I5,2X)
2000 FORMAT("No. Columns  ",5X,"=",2X,I5,2X)
2100 FORMAT("MTD ON CV    ",5X,"=",2X,I5,2X)


END SUBROUTINE
!-----------------------------------------!
SUBROUTINE Welcome_Message()
IMPLICIT NONE
WRITE(*,*)
WRITE(*,*)"!------------------------------------"
WRITE(*,*)"      WELCOME TO TASS REWEIGHTING    "  
WRITE(*,*)"!------------------------------------"
WRITE(*,*)
END SUBROUTINE Welcome_Message
!-----------------------------------------!
!SUBROUTINE get_steps(iunit,nsteps)
!IMPLICIT NONE
!INTEGER iunit, nsteps
!INTEGER ios
!nsteps=0
!REWIND(iunit)
!Read_Loop: DO
!   READ(iunit,*,IOSTAT=ios)
!   IF(ios.ne.0)EXIT Read_Loop
!   nsteps=nsteps+1
!END DO Read_Loop
!REWIND(iunit)
!END SUBROUTINE

!-----------------------------------------!
END MODULE Prepare_Inputs
