! 
! Fortran program to reweight TASS simulation. 
! This code is MODULE based program.
!
!======================================================================================================================
PROGRAM TASS
USE Prepare_Inputs
USE welcome
USE Check_File
USE Plumed_Read
USE GetSteps
USE ReadFiles
USE Reweight_MTD
USE Print_Data
!======================================================================================================================
IMPLICIT NONE

CALL Welcome_Message()
!CALL Set_defaults(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,periodic,nbin,t_min,t_max,nmetad)

! Check inputfiles 
CALL Is_File_Exist('input.inp ')
CALL Is_File_Exist('plumed.dat')

! Read plumed.dat 
CALL Read_Plumed_Input()

CALL Get_Inputs(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,biasfactor,periodic, &
        & nbin,t_min,t_max,grid,mtd_on_whichCV)

CALL data_print (cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,biasfactor,periodic, &
        & nbin,t_min,t_max,grid,mtd_on_whichCV)

CALL ReadCVFile(cvfile,cv,mdsteps,ncol,periodic_CV)

PRINT*,cvfile
open(10,file=cvfile,status='old')

CALL GetColumns(10,ncolumn)
CALL get_steps(10,mdsteps)

PRINT*," No. Of Columns: ",ncolumn
PRINT*," No. Of Steps  : ",mdsteps
CLOSE(10)
!PRINT*,"HERE"

if(metad) then
!CALL Calculate_Ct_Factor()
!CALL Calculate_vbias()
!CALL Calculate_Prob()
PRINT*,"METAD is ON"

CALL ReadHills(hillfile,hill,width,height,mtd_steps,periodic)
CALL Calculate_RBias(rbias,hill,width,height,cv,hill_freq,cv_freq,mdsteps,ncol,mtd_steps,mtd_on_whichCV, &
     & cv_temp,sys_temp,biasfactor,grid,nbin,periodic) 
endif
!CALL Calculate_Prob()
ENDPROGRAM TASS
!---------------------!
! 
