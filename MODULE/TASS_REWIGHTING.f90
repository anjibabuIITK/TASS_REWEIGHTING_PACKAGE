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
USE ReadFiles
USE REWEIGHT
USE HISTOGRAM
IMPLICIT NONE
CALL Welcome_Message()

CALL Set_defaults(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,periodic,nbin,t_min,t_max,nmetad&
&,periodic_CV,mtd_on_whichCV)
! Check inputfiles 
CALL Is_File_Exist('input.inp ')
CALL Is_File_Exist('plumed.dat')

! Read plumed.dat 
CALL Read_Plumed_Input()

CALL Get_Inputs(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,biasfactor&
&,periodic,gridmin1,gridmin2,gridmax1,gridmax2,gridmin3,gridmax3,gridmin4,gridmax4,gridwidth1,gridwidth2&
&,gridwidth3,gridwidth4,t_min,t_max,nbin,mdsteps,ncolumn,cv,periodic_CV,mtd_on_whichCV)

CALL Print_Data(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad,biasfactor,periodic,&
&gridmin1,gridmin2,gridmax1,gridwidth1,gridmax2,gridmin3,gridmax3,gridmin4,gridmax4,gridwidth2,gridwidth3,gridwidth4&
&,t_min,t_max,nbin,mdsteps,ncolumn,periodic_CV,mtd_on_whichCV)


!DO i=1,mdsteps
!WRITE(*,*)cv(i,2:)
!ENDDO
!CALL Calculate_Prob()
Print*,"Periodicity =",periodic,"  ","PERIODIC_CV =","  ",periodic_CV
!open(10,file=cvfile,status='old')

!ALL GetColumns(10,ncolumn)
!ALL get_steps(10,mdsteps)

!RINT*," No. Of Columns: ",ncolumn
!RINT*," No. Of Steps  : ",mdsteps

!close(10)

!CALL Get_gridwidth(gridmin1,gridmax1,nbin,gridwidth1)
!WRITE(*,'(A,F10.2)')" GRID WIDTH 1  : ",gridwidth1
!WRITE(*,*)
!gridwidth1=GridWidth(gridmin1,gridmax1,nbin)
!WRITE(*,'(A,F10.2)')" GRID WIDTH 1  : ",gridwidth1
!a=-3.20d0
!a=Apply_Piriodicity(a)
!write(*,*)"A= ",a

if(metad) then
!CALL Calculate_Ct_Factor()
!CALL Calculate_vbias()
!CALL Calculate_Prob()
PRINT*,"METAD is ON"
CALL ReadHills(hillfile,hill,width,height,mtdsteps,periodic)
!DO i=1,mtdsteps
!WRITE(*,*) hill(i), height(i), width(i)
!ENDDO
!
CALL Calculate_VBias(cv,hill,width,height,hill_freq,cv_freq,mdsteps,ncolumn,mtdsteps,mtd_on_whichCV&
&,vbias,cv_temp,sys_temp,biasfactor)
DO i=1,mdsteps
WRITE(*,*)vbias(i)
ENDDO

endif

ENDPROGRAM TASS
!---------------------!
! 
