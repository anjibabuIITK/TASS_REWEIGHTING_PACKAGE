MODULE REWEIGHT
CONTAINS

!---------------------------------------------------------!
SUBROUTINE Calculate_VBias(cv,hill,width,height,w_hill,w_cv,mdsteps,ncol,mtd_steps,which_CV&
&,vbias,cv_temp,sys_temp,biasfactor)
IMPLICIT NONE
INTEGER,INTENT(IN) :: mtd_steps, mdsteps, w_cv, w_hill,ncol,which_CV
REAL*8, INTENT(IN) :: cv(mdsteps,ncol), hill(mtd_steps), height(mtd_steps), width(mtd_steps)
REAL*8, INTENT(IN) :: cv_temp, sys_temp,biasfactor
REAL*8, ALLOCATABLE, INTENT(OUT):: vbias(:)
!Local Variables
REAL*8 :: deltaT, alpha, kbT, diff_s2, ds2, ss, hh, dum  
INTEGER :: mtd_max, i,j, i_md, i_mtd, mtd_col
REAL*8, PARAMETER :: kb=1.9872041E-3 !kcal K-1 mol-1 
REAL*8, PARAMETER :: kj_to_kcal = 0.239006

ALLOCATE(vbias(mdsteps))

mtd_col=which_CV+1

! Temp in energy units
 kbT = kb*cv_temp

 deltaT = (biasfactor - 1.d0)*cv_temp
 alpha = (cv_temp + deltaT)/deltaT

!=====>>>>> Calculating vbias <<<<<======!
DO i_md=1,mdsteps
  mtd_max=(i_md*w_cv/w_hill)
  ss=cv(i_md,mtd_col)
 dum=0.d0
   DO i_mtd=1,mtd_max
    ds2=width(i_mtd)*width(i_mtd)
    hh=height(i_mtd)/alpha
    diff_s2=ss-hill(i_mtd)
    diff_s2=diff_s2*diff_s2*0.5D0
    dum=dum+hh*DEXP(-diff_s2/ds2)
   END DO
vbias(i_md)=dum
!write(21,*) i_md, vbias(i_md)
END DO

END SUBROUTINE Calculate_VBias           

!---------------------------------------------------------!
!SUBROUTINE Calculate_Ct_factor()
!



END MODULE REWEIGHT
