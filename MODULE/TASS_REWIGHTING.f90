! 
! Fortran program to reweight TASS simulation. 
! This code is MODULE based program.
!
!
!
!
!---------------------------------------------------!
PROGRAM TASS
USE Prepare_Inputs
USE GetSteps
IMPLICIT NONE

CALL Welcome_Message()
CALL Set_defaults(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,periodic,nbin,t_min,t_max,nmetad)

! Check inputfiles 
CALL Is_File_Exist('input.inp ')
CALL Is_File_Exist('plumed.dat')

! Read plumed.dat 
CALL Read_Plumed_Input()

CALL Get_Inputs(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,biasfactor&
&,periodic,gridmin1,gridmin2,gridmax1,gridmax2,gridmin3,gridmax3,gridmin4,gridmax4,gridwidth1,gridwidth2&
&,gridwidth3,gridwidth4,t_min,t_max,nbin)

CALL Print_Data(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,biasfactor,periodic,&
&gridmin1,gridmin2,gridmax1,gridwidth1,gridmax2,gridmin3,gridmax3,gridmin4,gridmax4,gridwidth2,gridwidth3,gridwidth4&
&,t_min,t_max,nbin)


if(metad) then
!CALL Calculate_Ct_Factor()
!CALL Calculate_vbias()
!CALL Calculate_Prob()
PRINT*,"METAD is ON"
endif
!CALL Calculate_Prob()

open(10,file=cvfile,status='old')

CALL GetColumns(10,ncolumn)
CALL get_steps(10,mdsteps)

PRINT*," No. Of Columns: ",ncolumn
PRINT*," No. Of Steps  : ",mdsteps

close(10)

!CALL Get_gridwidth(gridmin1,gridmax1,nbin,gridwidth1)
!WRITE(*,'(A,F10.2)')" GRID WIDTH 1  : ",gridwidth1
!WRITE(*,*)
!gridwidth1=GridWidth(gridmin1,gridmax1,nbin)
!WRITE(*,'(A,F10.2)')" GRID WIDTH 1  : ",gridwidth1
ENDPROGRAM TASS
!---------------------!
! 
