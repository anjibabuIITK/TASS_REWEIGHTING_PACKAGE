MODULE REWEIGHT
USE ReadFiles
CONTAINS

!---------------------------------------------------------!
SUBROUTINE Calculate_VBias(cv,hill,width,height,w_hill,w_cv,mdsteps,ncol,mtd_steps,which_CV &
     & ,vbias,cv_temp,sys_temp,biasfactor)
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
SUBROUTINE Calculate_Ct_factor(hill,width,height,mtd_steps,which_CV&
&,ct,cv_temp,sys_temp,biasfactor,gridmin2,gridmax2,gridwidth2,nbin,periodic)
IMPLICIT NONE
REAL*8, INTENT(IN) ::  gridmin2, gridmax2, gridwidth2,cv_temp,sys_temp,biasfactor
REAL*8, INTENT(IN) :: height(mtd_steps), hill(mtd_steps), width(mtd_steps)
INTEGER,INTENT(IN) :: mtd_steps,which_CV,nbin
LOGICAL, INTENT(IN):: periodic
REAL*8, INTENT(OUT), ALLOCATABLE:: ct(:)

!local variables
REAL*8 :: kbT,kTb
REAL*8 :: diff_s2, ds2, ss, hh, dum, num, den
REAL*8 :: fes1(nbin),grid(nbin)

INTEGER :: mtd_max, i_mtd, i_md
INTEGER :: i_s1, i_s2, i_s3, i_s4, nbin1, nbin2, nbin3, nbin4

REAL*8, PARAMETER :: kb=1.9872041E-3 !kcal K-1 mol-1
REAL*8, PARAMETER :: kj_to_kcal = 0.239006

ALLOCATE(ct(mtd_steps))

 kbT = kb*cv_temp
 kTb = kbT*biasfactor

WRITE(*,*) 'calculating  c(t)'

 DO i_s2=1,nbin
    grid(i_s2)=gridmin2+dfloat(i_s2-1)*gridwidth2
 END DO

        fes1=0.d0
      DO i_mtd=1,mtd_steps
        ds2=width(i_mtd)*width(i_mtd)
        ss=hill(i_mtd)
        hh = height(i_mtd)

        num=0.D0
        den=0.D0

        DO i_s2=1,nbin
           diff_s2=grid(i_s2)-ss
!---------------
!Apply periodicity
       if(periodic)  diff_s2=Apply_Piriodicity(diff_s2)
!---------------

            diff_s2=diff_s2*diff_s2*0.5D0
! fes1 = -gamma_*Vb
           fes1(i_s2)=fes1(i_s2)-hh*DEXP(-diff_s2/ds2)
! Numerator
!           num=num+DEXP(-(gamma_*fes1(i_s2))/kbT)
! Denomirator          
!           den=den+DEXP(((1.d0-gamma_)/kbT)*fes1(i_s2))

           num=num+DEXP(-fes1(i_s2)/kbT)
           den=den+DEXP(-fes1(i_s2)/kTb)
        END DO

    ct(i_mtd)=kbT*DLOG(num/den)
      END DO

END SUBROUTINE Calculate_Ct_factor

!-------------------------------------------------------------!
!SUBROUTINE Calculate_RCT(rct, )
! This subroutine is to calculate rct array.

! Call Calculate_vbias()
! Call Calculate_ct()

! Calculate RCT factor and pass it main code


!END SUBROUTINE Calculate_RCT











END MODULE REWEIGHT
