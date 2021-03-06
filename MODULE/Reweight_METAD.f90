MODULE Reweight_MTD
USE ReadFiles
USE omp_lib
CONTAINS

!---------------------------------------------------------!
SUBROUTINE Calculate_VBias(cv,hill,width,height,w_hill,w_cv,mdsteps,ncol,mtd_steps,mtd_on_whichCV, &
           & vbias,cv_temp,sys_temp,biasfactor)
IMPLICIT NONE
INTEGER,INTENT(IN) :: mtd_steps, mdsteps, w_cv, w_hill,ncol,mtd_on_whichCV
REAL*8, INTENT(IN) :: cv(mdsteps,ncol), hill(mtd_steps), height(mtd_steps), width(mtd_steps)
REAL*8, INTENT(IN) :: cv_temp, sys_temp,biasfactor
REAL*8, ALLOCATABLE, INTENT(OUT) :: vbias(:)
!Local Variables
INTEGER :: mtd_max, i,j, i_md, i_mtd, mtd_col
REAL*8  :: deltaT, alpha, kbT, diff_s2, ds2, ss, hh, dum  
REAL*8, PARAMETER :: kb=1.9872041E-3 !kcal K-1 mol-1 
REAL*8, PARAMETER :: kj_to_kcal = 0.239006
!-----------------------------------------------------!
OPEN(21,FILE="vbias.dat",STATUS="unknown")
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
!$OMP PARALLEL
!$OMP DO
   DO i_mtd=1,mtd_max
    ds2=width(i_mtd)*width(i_mtd)
    hh=height(i_mtd)/alpha
    diff_s2=ss-hill(i_mtd)
    diff_s2=diff_s2*diff_s2*0.5D0
    dum=dum+hh*DEXP(-diff_s2/ds2)
   END DO
!$OMP END DO
!$OMP END PARALLEL
vbias(i_md)=dum
WRITE(21,101) i_md, vbias(i_md)
END DO
101 FORMAT (I10,F16.6)
END SUBROUTINE Calculate_VBias           

!---------------------------------------------------------!
SUBROUTINE Calculate_Ct_factor(hill,width,height,mtd_steps,mtd_on_whichCV, & 
           & ct,cv_temp,sys_temp,biasfactor,grid,nbin,periodic)
!gridmin2,gridmax2,gridwidth2
IMPLICIT NONE
REAL*8, INTENT(IN)  :: grid(30,3),cv_temp,sys_temp,biasfactor
REAL*8, INTENT(IN)  :: height(mtd_steps), hill(mtd_steps), width(mtd_steps)
INTEGER,INTENT(IN)  :: mtd_steps,mtd_on_whichCV,nbin
LOGICAL, INTENT(IN) :: periodic
REAL*8, INTENT(OUT), ALLOCATABLE :: ct(:)

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

OPEN(22,FILE="ct.dat",STATUS="unknown")
WRITE(*,*) 'calculating  c(t)'

 DO i_s2=1,nbin
!    grid(i_s2)=gridmin2+dfloat(i_s2-1)*gridwidth2
    grid1(i_s2)=grid(2,1)+dfloat(i_s2-1)*grid(2,3)
 END DO

        fes1=0.d0
      DO i_mtd=1,mtd_steps
        ds2=width(i_mtd)*width(i_mtd)
        ss=hill(i_mtd)
        hh = height(i_mtd)

        num=0.D0
        den=0.D0
!!$OMP PARALLEL
!!$OMP DO
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
!!$OMP END DO
!!$OMP END PARALLEL
    ct(i_mtd)=kbT*DLOG(num/den)
WRITE(22,101)i_mtd,ct(i_mtd)
      END DO
101 FORMAT (I10,F16.6)

END SUBROUTINE Calculate_Ct_factor

!-------------------------------------------------------------!
!SUBROUTINE Calculate_RBias(rbias,vbias,ct,hill,width,height,cv,w_hill,w_cv,mdsteps,ncol,mtd_steps,mtd_on_whichCV, &
!           & cv_temp,KbT,sys_temp,biasfactor,grid,nbin,periodic)
SUBROUTINE Reweight_METAD_Bias(rbias,vbias,ct,hill,width,height,cv,w_hill,w_cv,mdsteps,ncol,mtd_steps,mtd_on_whichCV, &
           & cv_temp,KbT,sys_temp,biasfactor,grid,nbin,periodic)

IMPLICIT NONE
INTEGER,INTENT(IN) :: mtd_steps, mdsteps, w_cv, w_hill, ncol, mtd_on_whichCV, nbin
REAL*8, INTENT(IN) :: cv(mdsteps,ncol), hill(mtd_steps), height(mtd_steps), width(mtd_steps)
REAL*8, INTENT(IN) :: cv_temp, sys_temp,biasfactor
REAL*8, INTENT(IN) :: grid(30,3)
REAL*8, PARAMETER  :: kb=1.9872041E-3 !kcal K-1 mol-1 
REAL*8, PARAMETER  :: kj_to_kcal = 0.239006
LOGICAL, INTENT(IN):: periodic

!Local Variables
INTEGER :: mtd_max, i,j, i_md, i_mtd, mtd_col
REAL*8, ALLOCATABLE:: vbias(:),ct(:)

!local variables
REAL*8 :: kbT,kTb
REAL*8 :: diff_s2, ds2, ss, hh, dum, num, den
REAL*8, ALLOCATABLE, INTENT(OUT) :: rbias(:)

ALLOCATE(vbias(mdsteps),ct(mtd_steps),rbias(mdsteps))

kbT = kb*cv_temp

! Call Calculate_vbias()
CALL Calculate_VBias(cv,hill,width,height,w_hill,w_cv,mdsteps,ncol,mtd_steps,mtd_on_whichCV, &
     & vbias,cv_temp,sys_temp,biasfactor)
!PRINT*,"VBIAS HERE:"

! Call Calculate_ct()
CALL Calculate_Ct_factor(hill,width,height,mtd_steps,mtd_on_whichCV,ct,cv_temp,sys_temp,biasfactor,grid,nbin,periodic)

!$OMP PARALLEL
!$OMP DO
DO i_md=1,mdsteps
   
i_mtd=(i_md*w_cv/w_hill)

      IF(i_mtd == 0) THEN
        rbias(i_md)=0.d0
      ELSE
        rbias(i_md)=vbias(i_md) - ct(i_mtd)
      ENDIF

!PRINT*,rbias(i_md)
ENDDO
!$OMP END DO
!$OMP END PARALLEL

! Calculate RCT factor and pass it main code
END SUBROUTINE Reweight_METAD_Bias
!-----------------------------------------------------------------------------------------!
SUBROUTINE Reweight_METAD(hillfile,rbias,cv,w_hill,w_cv,mdsteps,ncol,mtd_on_whichCV, &
           & cv_temp,KbT,sys_temp,biasfactor,grid,nbin,periodic,mtd_dimension)

IMPLICIT NONE
INTEGER,INTENT(IN) :: mdsteps, w_cv, w_hill, ncol, mtd_on_whichCV, nbin,mtd_dimension
REAL*8, INTENT(IN) :: cv(mdsteps,ncol)
REAL*8, INTENT(IN) :: cv_temp, sys_temp,biasfactor
REAL*8, INTENT(IN) ::  grid(30,3)
LOGICAL, INTENT(IN):: periodic
CHARACTER(len=20),INTENT(IN)::hillfile
!Local Variables
INTEGER :: mtd_max, i,j, i_md, i_mtd, mtd_col,mtd_steps
REAL*8, ALLOCATABLE:: vbias(:),ct(:),hill(:,:),width(:,:),height(:)

!local variables
REAL*8 :: kbT,kTb
REAL*8 :: diff_s2, ds2, ss, hh, dum, num, den


REAL*8, PARAMETER :: kb=1.9872041E-3 !kcal K-1 mol-1
REAL*8, PARAMETER :: kj_to_kcal = 0.239006

REAL*8, ALLOCATABLE, INTENT(OUT)::rbias(:)

ALLOCATE(vbias(mdsteps),ct(mtd_steps),rbias(mdsteps))
ALLOCATE(hill(mtd_dimension,mtd_steps),width(mtd_dimension,mtd_steps),height(mtd_steps))

kbT = kb*cv_temp


!Reading HILLS file
CALL ReadHills_new(hillfile,hill,width,height,mtd_steps,periodic,mtd_dimension)

CALL  Get_vbias(cv,hill,width,height,w_hill,w_cv,mdsteps,ncol,mtd_steps,mtd_on_whichCV, &
           & vbias,cv_temp,sys_temp,biasfactor,mtd_dimension,periodic)


!DO i=1,mdsteps
!WRITE(*,101) i, vbias(i)
!END DO
!101 FORMAT (I10,F16.6)

CALL Get_Ct_factor(hill,width,height,mdsteps,ncol,mtd_steps,mtd_on_whichCV, &
           & ct,cv_temp,sys_temp,biasfactor,grid,nbin,periodic,mtd_dimension)


!DO i=1,mtd_steps
!WRITE(*,101) i, ct(i)
!END DO

!101 FORMAT (I10,F18.6)


DO i_md=1,mdsteps

i_mtd=(i_md*w_cv/w_hill)

      IF(i_mtd == 0) THEN
        rbias(i_md)=0.d0
      ELSE
        rbias(i_md)=vbias(i_md) - ct(i_mtd)
      ENDIF

!PRINT*,rbias(i_md)
ENDDO
!print*, "TEST PASSED"
DEALLOCATE(hill,width,height,vbias,ct)

END SUBROUTINE Reweight_METAD
!-----------------------------------------------------------------------------------------------------------!
!---------------------------------------------------------!
SUBROUTINE Get_vbias(cv,hill,width,height,w_hill,w_cv,mdsteps,ncol,mtd_steps,mtd_on_whichCV, &
           & vbias,cv_temp,sys_temp,biasfactor,mtd_dimension,periodic)
IMPLICIT NONE
INTEGER,INTENT(IN) :: mtd_steps, mdsteps, w_cv, w_hill,ncol,mtd_on_whichCV,mtd_dimension
REAL*8, INTENT(IN) :: cv(mdsteps,ncol), hill(mtd_dimension,mtd_steps), height(mtd_steps), width(mtd_dimension,mtd_steps)
REAL*8, INTENT(IN) :: cv_temp, sys_temp,biasfactor
REAL*8, INTENT(OUT):: vbias(mdsteps)
LOGICAL, INTENT(IN) ::periodic
!Local Variables
REAL*8 :: deltaT, alpha, kbT, ds2, ht, dum, NM
REAL*8 :: mtd_cv1, mtd_cv2, mtd_cv3, sigma1, sigma2, sigma3, diff_s1, diff_s2, diff_s3
INTEGER :: mtd_max, i,j, i_md, i_mtd, mtd_col
REAL*8, PARAMETER :: kb=1.9872041E-3 !kcal K-1 mol-1
REAL*8, PARAMETER :: kj_to_kcal = 0.239006
!OPEN(21,FILE="vbias.dat")
mtd_col=mtd_on_whichCV

! Temp in energy units
 kbT = kb*cv_temp

 deltaT = (biasfactor - 1.d0)*cv_temp
 alpha = (cv_temp + deltaT)/deltaT
!print*, "MTD CV:",mtd_col
!print*, "CV Temp: ",cv_temp," mdsteps:", mdsteps
!=====>>>>> Calculating vbias <<<<<======!
!PRINT*,mdsteps,mtd_col
!STOP
DO i_md=1,mdsteps

   mtd_max=(i_md*w_cv/w_hill)
   dum=0.d0

!------STARTS Dimensionlity Select-----!
SELECT CASE (mtd_dimension)

 CASE(1)
!PRINT*, "Reweighting 1D MTD.."
   mtd_cv1=cv(i_md,mtd_col)
   DO i_mtd=1,mtd_max
    sigma1=width(1,i_mtd)*width(1,i_mtd)
    ht=height(i_mtd)/alpha
    diff_s1=mtd_cv1-hill(1,i_mtd)
       if(periodic)  diff_s1=Apply_Piriodicity(diff_s1)
    diff_s1=diff_s1*diff_s1*0.5D0
    dum=dum+ht*DEXP(-diff_s1/sigma1)
   END DO

 CASE(2)
!PRINT*, "Reweighting 2D MTD.."
  mtd_cv1=cv(i_md,mtd_col)
  mtd_cv2=cv(i_md,mtd_col+1)
!
  DO i_mtd=1,mtd_max
    sigma1=width(1,i_mtd)*width(1,i_mtd)
    sigma2=width(2,i_mtd)*width(2,i_mtd)
    ht=height(i_mtd)/alpha
    diff_s1=mtd_cv1-hill(1,i_mtd)
!      if (periodic) diff_s1=Apply_Periodicity(diff_s1)
    diff_s1=diff_s1*diff_s1

    diff_s2=mtd_cv2-hill(2,i_mtd)
!      if (periodic) diff_s2=Apply_Periodicity(diff_s2)
    diff_s2=diff_s2*diff_s2

    NM=((diff_s1/sigma1)+(diff_s2/sigma2))*0.5d0
    dum=dum+ht*DEXP(-NM)
  END DO

 CASE(3)
PRINT*, "Reweighting 3D MTD.."
  mtd_cv1=cv(i_md,mtd_col)
  mtd_cv2=cv(i_md,mtd_col+1)
  mtd_cv3=cv(i_md,mtd_col+2)
!
    DO i_mtd=1,mtd_max
    sigma1=width(1,i_mtd)*width(1,i_mtd)
    sigma2=width(2,i_mtd)*width(2,i_mtd)
    sigma3=width(3,i_mtd)*width(3,i_mtd)
    ht=height(i_mtd)/alpha
    diff_s1=mtd_cv1-hill(1,i_mtd)
!      if (periodic) diff_s1=Apply_Periodicity(diff_s1)
    diff_s1=diff_s1*diff_s1

    diff_s2=mtd_cv2-hill(2,i_mtd)
!      if (periodic) diff_s2=Apply_Periodicity(diff_s2)
    diff_s2=diff_s2*diff_s2

    diff_s3=mtd_cv3-hill(3,i_mtd)
!      if (periodic) diff_s3=Apply_Periodicity(diff_s3)
    diff_s3=diff_s3*diff_s3
    NM=((diff_s1/sigma1)+(diff_s2/sigma2)+(diff_s3/sigma3))*0.5d0
    dum=dum+ht*DEXP(-NM)
   ENDDO

  CASE DEFAULT
      WRITE(*,*) "MTD Dimension >3 has not implimented. "
      WRITE(*,*) "Change MTD_DIMENSION to <= 3"
      STOP
END SELECT
!------END Dimensionlity Select-----!

vbias(i_md)=dum

!WRITE(21,101) i_md, vbias(i_md)
END DO
!101 FORMAT (I10,F16.6)
!CLOSE(21)
END SUBROUTINE Get_vbias

!---------------------------------------------------------!
SUBROUTINE Get_Ct_factor(hill,width,height,mdsteps,ncol,mtd_steps,mtd_on_whichCV, &
           & ct,cv_temp,sys_temp,biasfactor,grid,nbin,periodic,mtd_dimension)

IMPLICIT NONE
INTEGER,INTENT(IN) :: mtd_steps, mdsteps, mtd_on_whichCV,mtd_dimension, nbin, ncol
REAL*8, INTENT(IN) :: hill(mtd_dimension,mtd_steps), height(mtd_steps), width(mtd_dimension,mtd_steps)
LOGICAL, INTENT(IN):: periodic
REAL*8, INTENT(IN) :: grid(30,3),cv_temp,sys_temp,biasfactor
REAL*8, INTENT(OUT), ALLOCATABLE:: ct(:)

!local variables
REAL*8 :: mtd_cv1, mtd_cv2, mtd_cv3, sigma1, sigma2, sigma3, diff_s1, diff_s2, diff_s3
REAL*8 :: kbT,kTb
REAL*8 :: ds2, ss, ht, dum, num, den, NM
REAL*8 :: fes1(nbin),grid1(mtd_dimension,nbin)

INTEGER :: i, j, mtd_max, i_mtd, i_md, mtd_col,i_s2

REAL*8, PARAMETER :: kb=1.9872041E-3 !kcal K-1 mol-1
REAL*8, PARAMETER :: kj_to_kcal = 0.239006

ALLOCATE(ct(mtd_steps))
mtd_col=mtd_on_whichCV

 kbT = kb*cv_temp
 kTb = kbT*biasfactor

!OPEN(22,FILE="ct.dat")
!WRITE(*,*) 'calculating  c(t)'

DO i=1,mtd_dimension
   DO i_s2=1,nbin
      grid1(mtd_dimension,i_s2)=grid(mtd_col,1)+dfloat(i_s2-1)*grid(mtd_col,3)
   END DO
mtd_col=mtd_col+1
END DO

        fes1=0.d0
      DO i_mtd=1,mtd_steps

!>>>>>>>>> STARTS CASE SELECTS <<<<<<<<<<!
  SELECT CASE(mtd_dimension)

 CASE(1)
     sigma1=width(1,i_mtd)*width(1,i_mtd)
     ht=height(i_mtd)

        num=0.D0
        den=0.D0

        DO i_s2=1,nbin
           diff_s1=grid1(1,i_s2)-hill(1,i_mtd)

!Apply periodicity
!       if(periodic)  diff_s1=Apply_Piriodicity(diff_s1)
       diff_s1=diff_s1*diff_s1*0.5D0
       fes1(i_s2)=fes1(i_s2)-ht*DEXP(-diff_s1/sigma1)
! Numerator
           num=num+DEXP(-fes1(i_s2)/kbT)
! Denomirator
           den=den+DEXP(i-fes1(i_s2)/kTb)
        END DO
!
 CASE (2)
     sigma1=width(1,i_mtd)*width(1,i_mtd)
     sigma2=width(2,i_mtd)*width(2,i_mtd)
     ht=height(i_mtd)

        num=0.D0
        den=0.D0

        DO i_s2=1,nbin
           diff_s1=grid1(1,i_s2)-hill(1,i_mtd)
           diff_s2=grid1(2,i_s2)-hill(2,i_mtd)

!       if(periodic)  diff_s1=Apply_Piriodicity(diff_s1)
!       if(periodic)  diff_s2=Apply_Piriodicity(diff_s2)

            diff_s1=diff_s1*diff_s1
            diff_s2=diff_s2*diff_s2

            NM=((diff_s1/sigma1)+(diff_s2/sigma2))*0.5d0

           fes1(i_s2)=fes1(i_s2)-ht*DEXP(-NM)

           num=num+DEXP(-fes1(i_s2)/kbT)
           den=den+DEXP(-fes1(i_s2)/kTb)

        END DO
!
 CASE (3)
     sigma1=width(1,i_mtd)*width(1,i_mtd)
     sigma2=width(2,i_mtd)*width(2,i_mtd)
     sigma3=width(3,i_mtd)*width(3,i_mtd)
     ht=height(i_mtd)

        num=0.D0
        den=0.D0

        DO i_s2=1,nbin
           diff_s1=grid1(1,i_s2)-hill(1,i_mtd)
           diff_s2=grid1(2,i_s2)-hill(2,i_mtd)
           diff_s3=grid1(3,i_s2)-hill(3,i_mtd)

!       if(periodic)  diff_s1=Apply_Piriodicity(diff_s1)
!       if(periodic)  diff_s2=Apply_Piriodicity(diff_s2)
!       if(periodic)  diff_s3=Apply_Piriodicity(diff_s3)

            diff_s1=diff_s1*diff_s1
            diff_s2=diff_s2*diff_s2
            diff_s3=diff_s3*diff_s3

            NM=((diff_s1/sigma1)+(diff_s2/sigma2)+(diff_s3/sigma3))*0.5d0

           fes1(i_s2)=fes1(i_s2)-ht*DEXP(-NM)

           num=num+DEXP(-fes1(i_s2)/kbT)
           den=den+DEXP(-fes1(i_s2)/kTb)

        END DO
!
  CASE DEFAULT
      WRITE(*,*) "MTD Dimension >3 has not implimented. "
      WRITE(*,*) "Change MTD_DIMENSION to <= 3"
      STOP
END SELECT
!>>>>>>>>> ENDS CASE SELECT <<<<<<<<<<!
        ct(i_mtd)=kbT*DLOG(num/den)
    END DO

END SUBROUTINE Get_Ct_factor

!-------------------------------------------------------------!
END MODULE Reweight_MTD
