MODULE probability
USE Prepare_Inputs
USE ReadFiles
USE Reweight_MTD 
CONTAINS
!----------------------------------------------------------------------!
SUBROUTINE histogram(nbin,ncv,mdsteps,mtd_steps,vbias,ct,w_cv,w_hill,grid,cv,KbT)
IMPLICIT NONE
INTEGER :: nbin,ncv,index1,i_md,i_mtd,i_s1,s1,ncol,mdsteps,mtd_steps,w_cv,w_hill
REAL*8 :: dum,grid(30,3),cv(mdsteps,*),KbT
REAL*8 :: vbias(mdsteps),ct(mtd_steps),den

OPEN(30,FILE="PROB.dat")
ALLOCATE (prob(nbin))
WRITE(6,*)"READING v_bias FILE"

prob = 0.0d0
DO i_md=1,mdsteps
index1 = NINT((cv(i_md,2) - grid(2,1))/grid(2,3))

i_mtd=(i_md*w_cv/w_hill)
    IF (i_mtd .eq. 0)THEN
        dum = 0.0d0
    ELSE
        dum = vbias(i_md) - ct(i_mtd)
    ENDIF
prob(index1) = prob(index1) + DEXP(dum/KbT)
den = den + DEXP(dum/KbT)
ENDDO
dum = den*grid(2,3)
den = 1.0d0/dum

DO i_s1=1,NBIN
  s1 = DFLOAT(i_s1-1)*grid(2,3)+grid(2,1)
   prob(i_s1) = prob(i_s1)*den
WRITE(30,101)((grid(2,3)*i_s1)+grid(2,1)),prob(i_s1)
!WRITE(6,*)i_s1,grid(2,3),grid(2,1),((grid(2,3)*i_s1)+grid(2,1)),prob(i_s1)
ENDDO
101 FORMAT (F16.4,2X,F16.8)
END SUBROUTINE histogram
!----------------------------------------------------------------------!
SUBROUTINE Probability_1D(nbin,ncv,mdsteps,rbias,grid,cv,KbT,t_min,t_max,metad)
IMPLICIT NONE

INTEGER, INTENT(IN) :: nbin,ncv,mdsteps,t_min,t_max
REAL*8,  INTENT(IN) :: grid(30,3),cv(mdsteps,*),KbT, rbias(mdsteps)
LOGICAL, INTENT(IN) :: metad
INTEGER:: i_md,i_mtd,i_s1
INTEGER:: index1
REAL*8::dum,den,s1
REAL*8, ALLOCATABLE::prob(:)


IF(t_max.gt.mdsteps)STOP '!!ERROR: t_max > total MD steps'

ALLOCATE(prob(nbin))


OPEN(2,file="PROB_1D",status='unknown')
WRITE(*,*) 'calculating  probability'

den=0.d0
prob=0.d0

DO i_md=t_min,t_max
 IF((i_md.GT.t_min).AND.(i_md.LT.t_max))THEN
index1 = NINT((cv(i_md,2) - grid(1,1))/grid(1,3))+1

   IF(index1.gt.0.and.index1.le.nbin) then

      if(metad) then
        prob(index1) = prob(index1) + DEXP(rbias(i_md)/kbT)
        den=den+DEXP(rbias(i_md)/kbT)
      else
        prob(index1) = prob(index1)+1.d0 
        den=den+1.d0 
      endif

   END IF
     
 END IF

END DO

dum = den*grid(1,3)
  den=1.d0/dum


DO i_s1=1,nbin
  s1 = DFLOAT(i_s1-1)*grid(1,3)+grid(1,1)

           prob(i_s1)=prob(i_s1)*den

           WRITE(2,'(2E16.8)')s1, prob(i_s1)
 END DO
 WRITE(*,'(A)')'Unbiased distribution written in PROB_1D'

close(2)
DEALLOCATE(prob)

ENDSUBROUTINE Probability_1D
!---------------------------------------------------------------------!
SUBROUTINE Probability_2D(nbin,ncv,mdsteps,rbias,grid,cv,KbT,t_min,t_max,metad)
IMPLICIT NONE

INTEGER, INTENT(IN) :: nbin,ncv,mdsteps,t_min, t_max
REAL*8,  INTENT(IN) :: grid(30,3),cv(mdsteps,*),KbT, rbias(mdsteps)
LOGICAL, INTENT(IN) :: metad
INTEGER:: i_md,i_mtd,i_s1,i_s2
INTEGER:: index1, index2
REAL*8::dum,den,s1,s2
REAL*8, ALLOCATABLE::prob(:,:)


IF(t_max.gt.mdsteps)STOP '!!ERROR: t_max > total MD steps'

ALLOCATE(prob(nbin,nbin))


OPEN(2,file="PROB_2D",status='unknown')
WRITE(*,*) 'calculating  probability'

den=0.d0
prob=0.d0

DO i_md=t_min,t_max
 IF((i_md.GT.t_min).AND.(i_md.LT.t_max))THEN
index1 = NINT((cv(i_md,2) - grid(1,1))/grid(1,3))+1
index2 = NINT((cv(i_md,3) - grid(2,1))/grid(2,3))+1

     IF(index1.gt.0.and.index2.gt.0.and.index1.le.&
        nbin.and.index2.le.nbin) then

       if(metad) then
          prob(index1,index2) = prob(index1,index2) + DEXP(rbias(i_md)/kbT)
          den=den+DEXP(rbias(i_md)/kbT)
       else
        prob(index1,index2) = prob(index1,index2) + 1.d0
        den=den+1.d0 
       endif

     END IF
     
END IF

END DO

dum = den*grid(1,3)*grid(2,3)
  den=1.d0/dum


DO i_s1=1,nbin
  s1 = DFLOAT(i_s1-1)*grid(1,3)+grid(1,1)
   DO i_s2=1,nbin
  s2 = DFLOAT(i_s2-1)*grid(2,3)+grid(2,1)

           prob(i_s1,i_s2)=prob(i_s1,i_s2)*den

           WRITE(2,'(3E16.8)')s1, s2, prob(i_s1,i_s2)
   END DO
      WRITE(2,*)
END DO
 WRITE(*,'(A)')'Unbiased distribution written in PROB_2D'

close(2)
DEALLOCATE(prob)

ENDSUBROUTINE Probability_2D
!---------------------------------------------------------------------!
SUBROUTINE Probability_3D(nbin,ncv,mdsteps,rbias,grid,cv,KbT,t_min,t_max,metad)
IMPLICIT NONE

INTEGER, INTENT(IN) :: nbin,ncv,mdsteps,t_min, t_max
REAL*8,  INTENT(IN) :: grid(30,3),cv(mdsteps,*),KbT,rbias(mdsteps)
LOGICAL, INTENT(IN) :: metad
INTEGER:: i_md,i_mtd,i_s1,i_s2, i_s3
INTEGER:: index1, index2, index3
REAL*8::dum,den,s1,s2,s3
REAL*8, ALLOCATABLE::prob(:,:,:)


IF(t_max.gt.mdsteps)STOP '!!ERROR: t_max > total MD steps'

ALLOCATE(prob(nbin,nbin,nbin))

OPEN(2,file="PROB_3D",status='unknown')

WRITE(*,*) 'calculating  3D probability'

den=0.d0
prob=0.d0

DO i_md=t_min,t_max
 IF((i_md.GT.t_min).AND.(i_md.LT.t_max))THEN
index1 = NINT((cv(i_md,2) - grid(1,1))/grid(1,3))+1
index2 = NINT((cv(i_md,3) - grid(2,1))/grid(2,3))+1
index3 = NINT((cv(i_md,4) - grid(3,1))/grid(3,3))+1

     IF(index1.gt.0.and.index2.gt.0.and.index3.gt.0.and.index1.le.&
        nbin.and.index2.le.nbin.and.index3.le.nbin) then

       if(metad) then
          prob(index1,index2,index3) = prob(index1,index2,index3) + DEXP(rbias(i_md)/kbT)
          den=den+DEXP(rbias(i_md)/kbT)
       else
          prob(index1,index2,index3) = prob(index1,index2,index3) + 1.d0
          den=den+1.d0 
       endif

     END IF
     
END IF

END DO

dum = den*grid(1,3)*grid(2,3)*grid(3,3)
  den=1.d0/dum


DO i_s1=1,nbin
  s1 = DFLOAT(i_s1-1)*grid(1,3)+grid(1,1)
   DO i_s2=1,nbin
  s2 = DFLOAT(i_s2-1)*grid(2,3)+grid(2,1)
   DO i_s3=1,nbin
  s3 = DFLOAT(i_s3-1)*grid(3,3)+grid(3,1)

           prob(i_s1,i_s2,i_s3)=prob(i_s1,i_s2,i_s3)*den

           WRITE(2,'(E16.8)') prob(i_s1,i_s2,i_s3)
   END DO
   END DO
END DO
 WRITE(*,'(A)')'Unbiased distribution written in PROB_3D'

close(2)
DEALLOCATE(prob)

ENDSUBROUTINE Probability_3D
!---------------------------------------------------------------------!
SUBROUTINE Probability_4D(nbin,ncv,mdsteps,rbias,grid,cv,KbT,t_min,t_max,metad)
IMPLICIT NONE

INTEGER, INTENT(IN) :: nbin,ncv,mdsteps,t_min, t_max
REAL*8,  INTENT(IN) :: grid(30,3),cv(mdsteps,*),KbT, rbias(mdsteps)
LOGICAL, INTENT(IN) :: metad
INTEGER:: i_md,i_mtd,i_s1,i_s2, i_s3,i_s4
INTEGER:: index1, index2, index3, index4
REAL*8::dum,den,s1,s2,s3,s4
REAL*8, ALLOCATABLE::prob(:,:,:,:)


IF(t_max.gt.mdsteps)STOP '!!ERROR: t_max > total MD steps'

ALLOCATE(prob(nbin,nbin,nbin,nbin))

OPEN(2,file="PROB_4D",status='unknown')

WRITE(*,*) 'calculating  4D probability'

den=0.d0
prob=0.d0

DO i_md=t_min,t_max
 IF((i_md.GT.t_min).AND.(i_md.LT.t_max))THEN
index1 = NINT((cv(i_md,2) - grid(1,1))/grid(1,3))+1
index2 = NINT((cv(i_md,3) - grid(2,1))/grid(2,3))+1
index3 = NINT((cv(i_md,4) - grid(3,1))/grid(3,3))+1
index4 = NINT((cv(i_md,5) - grid(4,1))/grid(4,3))+1
     IF(index1.gt.0.and.index2.gt.0.and.index3.gt.0.and.index4.gt.0.and.&
       index1.le.nbin.and.index2.le.nbin.and.index3.le.nbin.and.index4.le.nbin) then

       if(metad) then
         prob(index1,index2,index3,index4) = prob(index1,index2,index3,index4) + DEXP(rbias(i_md)/kbT)
         den=den+DEXP(rbias(i_md)/kbT)
       else
         prob(index1,index2,index3,index4) = prob(index1,index2,index3,index4) + 1.d0
          den=den+1.d0
       endif

     END IF
     
END IF

END DO

dum = den*grid(1,3)*grid(2,3)*grid(3,3)*grid(4,3)
  den=1.d0/dum


DO i_s1=1,nbin
  s1 = DFLOAT(i_s1-1)*grid(1,3)+grid(1,1)
   DO i_s2=1,nbin
  s2 = DFLOAT(i_s2-1)*grid(2,3)+grid(2,1)
   DO i_s3=1,nbin
  s3 = DFLOAT(i_s3-1)*grid(3,3)+grid(3,1)
   DO i_s4=1,nbin
  s4 = DFLOAT(i_s4-1)*grid(4,3)+grid(4,1)

           prob(i_s1,i_s2,i_s3,i_s4)=prob(i_s1,i_s2,i_s3,i_s4)*den

           WRITE(2,'(E16.8)') prob(i_s1,i_s2,i_s3,i_s4)
     END DO
    END DO
   END DO
  END DO
 WRITE(*,'(A)')'Unbiased distribution written in PROB_4D'

close(2)
DEALLOCATE(prob)

ENDSUBROUTINE Probability_4D
!---------------------------------------------------------------------!
SUBROUTINE Calculate_Probability(nbin,ncv,mdsteps,rbias,grid,cv,KbT,t_min,t_max,prob_dimension,metad)
IMPLICIT NONE

INTEGER, INTENT(IN) :: nbin,ncv,mdsteps, prob_dimension,t_min, t_max
REAL*8,  INTENT(IN) :: grid(30,3),cv(mdsteps,*),KbT, rbias(mdsteps)
LOGICAL, INTENT(IN) :: metad


IF(t_max.gt.mdsteps)STOP '!!ERROR: t_max > total MD steps'

!WRITE(*,*) 'Calculating  Probability'

!------------------------------------!
SELECT CASE (prob_dimension)

CASE(1)
   CALL Probability_1D(nbin,ncv,mdsteps,rbias,grid,cv,KbT,t_min,t_max,metad)

CASE(2)
   CALL Probability_2D(nbin,ncv,mdsteps,rbias,grid,cv,KbT,t_min,t_max,metad)

CASE(3)
   CALL Probability_3D(nbin,ncv,mdsteps,rbias,grid,cv,KbT,t_min,t_max,metad)

CASE(4)
   CALL Probability_4D(nbin,ncv,mdsteps,rbias,grid,cv,KbT,t_min,t_max,metad)

CASE(12)
   CALL Probability_1D(nbin,ncv,mdsteps,rbias,grid,cv,KbT,t_min,t_max,metad)
   CALL Probability_2D(nbin,ncv,mdsteps,rbias,grid,cv,KbT,t_min,t_max,metad)
CASE(24)
   CALL Probability_2D(nbin,ncv,mdsteps,rbias,grid,cv,KbT,t_min,t_max,metad)
   CALL Probability_4D(nbin,ncv,mdsteps,rbias,grid,cv,KbT,t_min,t_max,metad)
CASE(14)
   CALL Probability_1D(nbin,ncv,mdsteps,rbias,grid,cv,KbT,t_min,t_max,metad)
   CALL Probability_4D(nbin,ncv,mdsteps,rbias,grid,cv,KbT,t_min,t_max,metad)
CASE(13)
   CALL Probability_1D(nbin,ncv,mdsteps,rbias,grid,cv,KbT,t_min,t_max,metad)
   CALL Probability_3D(nbin,ncv,mdsteps,rbias,grid,cv,KbT,t_min,t_max,metad)

CASE DEFAULT
 WRITE(*,*)  "Probability dimension > 4 is not implimented. Pls Specify the proper dimensionalityy for Probability"
 WRITE(*,*) "prob_dimension = 1 or 2 or 3 or 4"
END SELECT
!------------------------------------!

ENDSUBROUTINE Calculate_Probability
!---------------------------------------------------------------------!
END MODULE probability
