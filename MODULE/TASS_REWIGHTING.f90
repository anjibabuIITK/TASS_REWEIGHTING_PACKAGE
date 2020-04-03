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
USE Probability
USE Print_Data
!======================================================================================================================
IMPLICIT NONE

CALL Welcome_Message()
! Setting all required input variables to default values
CALL Set_defaults(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,periodic,nbin,t_min,t_max&
        &,nmetad,prob_dimension,mtd_dimension)

! Check inputfiles 
CALL Is_File_Exist('input.inp ')
CALL Is_File_Exist('plumed.dat')

! Read plumed.dat 
CALL Read_Plumed_Input()

! Get user input values
CALL Get_Inputs(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,biasfactor,periodic, &
        & nbin,t_min,t_max,grid,mtd_on_whichCV,prob_dimension,mtd_dimension)

! Print the inputs
CALL data_print (cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,biasfactor,periodic, &
        & nbin,t_min,t_max,grid,mtd_on_whichCV,prob_dimension,mtd_dimension)

! Read CV file
CALL ReadCVFile(cvfile,cv,mdsteps,ncol,periodic_CV)

!if MTD ON, Reweight MTD
if(metad) then
PRINT*,"METAD is ON"
CALL ReadHills(hillfile,hill,width,height,mtd_steps,periodic)

CALL Reweight_METAD_Bias(rbias,vbias,ct,hill,width,height,cv,hill_freq,cv_freq,mdsteps,ncol,mtd_steps,mtd_on_whichCV, &
     & cv_temp,KbT,sys_temp,biasfactor,grid,nbin,periodic) 
endif

! Calculte probability
CALL Calculate_Probability(nbin,ncv,mdsteps,rbias,grid,cv,KbT,t_min,t_max,prob_dimension,metad)

ENDPROGRAM TASS
!---------------------!
! 
