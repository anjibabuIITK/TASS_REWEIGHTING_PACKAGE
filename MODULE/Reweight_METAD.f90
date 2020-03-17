MODULE Reweight_MTD
USE ReadFiles
CONTAINS

!---------------------------------------------------------!
SUBROUTINE Calculate_VBias(cv,hill,width,height,w_hill,w_cv,mdsteps,ncol,mtd_steps,mtd_on_whichCV, &
           & vbias,cv_temp,sys_temp,biasfactor)
IMPLICIT NONE
INTEGER,INTENT(IN) :: mtd_steps, mdsteps, w_cv, w_hill,ncol,mtd_on_whichCV
REAL*8, INTENT(IN) :: cv(mdsteps,ncol), hill(mtd_steps), height(mtd_steps), width(mtd_steps)
REAL*8, INTENT(IN) :: cv_temp, sys_temp,biasfactor
REAL*8, ALLOCATABLE, INTENT(OUT):: vbias(:)
!Local Variables
REAL*8 :: deltaT, alpha, kbT, diff_s2, ds2, ss, hh, dum  
INTEGER :: mtd_max, i,j, i_md, i_mtd, mtd_col
REAL*8, PARAMETER :: kb=1.9872041E-3 !kcal K-1 mol-1 
REAL*8, PARAMETER :: kj_to_kcal = 0.239006

ALLOCATE(vbias(mdsteps))
mtd_col=mtd_on_whichCV+1

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
SUBROUTINE Calculate_Ct_factor(hill,width,height,mtd_steps,mtd_on_whichCV, & 
           & ct,cv_temp,sys_temp,biasfactor,grid,nbin,periodic)
!gridmin2,gridmax2,gridwidth2
IMPLICIT NONE
REAL*8, INTENT(IN) ::  grid(3,*),cv_temp,sys_temp,biasfactor
REAL*8, INTENT(IN) :: height(mtd_steps), hill(mtd_steps), width(mtd_steps)
INTEGER,INTENT(IN) :: mtd_steps,mtd_on_whichCV,nbin
LOGICAL, INTENT(IN):: periodic
REAL*8, INTENT(OUT), ALLOCATABLE:: ct(:)

!local variables
REAL*8 :: kbT,kTb
REAL*8 :: diff_s2, ds2, ss, hh, dum, num, den
REAL*8 :: fes1(nbin),grid1(nbin)

INTEGER :: mtd_max, i_mtd, i_md
INTEGER :: i_s1, i_s2, i_s3, i_s4, nbin1, nbin2, nbin3, nbin4

REAL*8, PARAMETER :: kb=1.9872041E-3 !kcal K-1 mol-1
REAL*8, PARAMETER :: kj_to_kcal = 0.239006

ALLOCATE(ct(mtd_steps))

 kbT = kb*cv_temp
 kTb = kbT*biasfactor

WRITE(*,*) 'calculating  c(t)'

 DO i_s2=1,nbin
!    grid(i_s2)=gridmin2+dfloat(i_s2-1)*gridwidth2
    grid1(i_s2)=grid(1,2)+dfloat(i_s2-1)*grid(3,2)
 END DO

        fes1=0.d0
      DO i_mtd=1,mtd_steps
        ds2=width(i_mtd)*width(i_mtd)
        ss=hill(i_mtd)
        hh = height(i_mtd)

        num=0.D0
        den=0.D0

        DO i_s2=1,nbin
           diff_s2=grid1(i_s2)-ss
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
SUBROUTINE Calculate_RBias(rbias,hill,width,height,cv,w_hill,w_cv,mdsteps,ncol,mtd_steps,mtd_on_whichCV, &
           & cv_temp,sys_temp,biasfactor,grid,nbin,periodic)

IMPLICIT NONE
INTEGER,INTENT(IN) :: mtd_steps, mdsteps, w_cv, w_hill, ncol, mtd_on_whichCV, nbin
REAL*8, INTENT(IN) :: cv(mdsteps,ncol), hill(mtd_steps), height(mtd_steps), width(mtd_steps)
REAL*8, INTENT(IN) :: cv_temp, sys_temp,biasfactor
REAL*8, INTENT(IN) ::  grid(3,*)
LOGICAL, INTENT(IN):: periodic

!Local Variables
INTEGER :: mtd_max, i,j, i_md, i_mtd, mtd_col
REAL*8, ALLOCATABLE:: vbias(:),ct(:)

!local variables
REAL*8 :: kbT,kTb
REAL*8 :: diff_s2, ds2, ss, hh, dum, num, den

REAL*8, ALLOCATABLE, INTENT(OUT)::rbias(:)
ALLOCATE(vbias(mdsteps),ct(mtd_steps),rbias(mdsteps))

! Call Calculate_vbias()
CALL Calculate_VBias(cv,hill,width,height,w_hill,w_cv,mdsteps,ncol,mtd_steps,mtd_on_whichCV, &
     & vbias,cv_temp,sys_temp,biasfactor)
!PRINT*,"VBIAS HERE:"

! Call Calculate_ct()
CALL Calculate_Ct_factor(hill,width,height,mtd_steps,mtd_on_whichCV,ct,cv_temp,sys_temp,biasfactor,grid,nbin,periodic)


DO i_md=1,mdsteps
   
i_mtd=(i_md*w_cv/w_hill)

      IF(i_mtd == 0) THEN
        rbias(i_md)=0.d0
      ELSE
        rbias(i_md)=vbias(i_md) - ct(i_mtd)
      ENDIF

!PRINT*,rbias(i_md)
ENDDO

! Calculate RCT factor and pass it main code
END SUBROUTINE Calculate_RBias

END MODULE Reweight_MTD
