MODULE ReadFiles
USE GetSteps
CONTAINS

!-----------------------------------!
SUBROUTINE ReadCVFile(cvfile,cv,i_cv,mdsteps,ncol,periodic_CV,periodic)
!ReadCVFile(cvfile,cv,mdsteps,ncol,periodic_CV)
IMPLICIT NONE
INTEGER,INTENT(INOUT)::mdsteps,ncol
REAL*8, INTENT(OUT), ALLOCATABLE::cv(:,:)
CHARACTER(len=20),INTENT(IN)::periodic_CV,cvfile
INTEGER::i,j
REAL*8::time

CALL SYSTEM("cp COLVAR COLVAR.old")
CALL SYSTEM('sed -i "/^#/d" COLVAR')

open(unit=11,file=cvfile,status='old')
mdsteps=NSteps(11)
ncol=Get_Columns(11)-1
ALLOCATE(cv(mdsteps,ncol))
PRINT*,"NCOL except time: ", ncol
DO i=1,mdsteps
  READ(11,*)time,cv(i,1:)
!-------------------------!
! Applying Periodicity
SELECT CASE (periodic_CV)

CASE ("NONE")
!     Print*,"No Periodic applied"
CASE ("ALL")
    cv(i,1)=Apply_Piriodicity(cv(i,1))
    cv(i,2)=Apply_Piriodicity(cv(i,2))

if(ncol > 2) then
    cv(i,3)=Apply_Piriodicity(cv(i,3))
    cv(i,4)=Apply_Piriodicity(cv(i,4))

endif
!     Print*, "All the CVs are periodic"
CASE ("CV1")
 !    Print*, "CV1 is periodic"
    cv(i,1)=Apply_Piriodicity(cv(i,1))
CASE ("CV2")
 !    Print*, "CV2 is periodic"
    cv(i,2)=Apply_Piriodicity(cv(i,2))

CASE ("CV3")
 !    Print*, "CV3 is periodic"
    cv(i,3)=Apply_Piriodicity(cv(i,3))

CASE ("CV4")
 !    Print*, "CV4 is periodic"
    cv(i,4)=Apply_Piriodicity(cv(i,4))

CASE ("CV1.and.CV2")
 !    Print*, "CV1 and CV2 are periodic"
    cv(i,1)=Apply_Piriodicity(cv(i,1))
    cv(i,2)=Apply_Piriodicity(cv(i,2))

CASE ("CV2.and.CV3")
 !    Print*, "CV2 and CV3 are periodic"
    cv(i,2)=Apply_Piriodicity(cv(i,2))
    cv(i,3)=Apply_Piriodicity(cv(i,3))
CASE ("CV3.and.CV4")
 !    Print*, "CV3 and CV4 are periodic"
    cv(i,3)=Apply_Piriodicity(cv(i,3))
    cv(i,4)=Apply_Piriodicity(cv(i,4))
CASE ("CV1.and.CV4")
 !    Print*, "CV1 and CV4 are periodic"
    cv(i,1)=Apply_Piriodicity(cv(i,1))
    cv(i,4)=Apply_Piriodicity(cv(i,4))
CASE ("CV2.and.CV4")
 !    Print*, "CV2 and CV4 are periodic"
    cv(i,2)=Apply_Piriodicity(cv(i,2))
    cv(i,4)=Apply_Piriodicity(cv(i,4))
CASE ("CV1.and.CV3")
 !    Print*, "CV1 and CV3 are periodic"
    cv(i,1)=Apply_Piriodicity(cv(i,1))
    cv(i,3)=Apply_Piriodicity(cv(i,3))

CASE ("CV1.and.CV2.and.CV3")
 !    Print*, "CV1 ,CV2 and CV3 are periodic"
    cv(i,1)=Apply_Piriodicity(cv(i,1))
    cv(i,2)=Apply_Piriodicity(cv(i,2))
    cv(i,3)=Apply_Piriodicity(cv(i,3))
CASE ("CV2.and.CV3.and.CV4")
 !    Print*, "CV2 ,CV3 and CV4 are periodic"
    cv(i,2)=Apply_Piriodicity(cv(i,2))
    cv(i,3)=Apply_Piriodicity(cv(i,3))
    cv(i,4)=Apply_Piriodicity(cv(i,4))

CASE DEFAULT
!     PRINT*,"CV > 4 is not implimented"
END SELECT
!-------------------------!


ENDDO
CLOSE(11)
END SUBROUTINE ReadCVFile
!-----------------------------------!
REAL FUNCTION Apply_Piriodicity(AnyValue)
IMPLICIT NONE
REAL*8:: AnyValue

   if (AnyValue .gt. 3.14d0 ) AnyValue=AnyValue - 6.28d0
   if (AnyValue .lt.-3.14d0 ) AnyValue=AnyValue + 6.28d0

Apply_Piriodicity=AnyValue

END FUNCTION Apply_Piriodicity
!
!-----------------------------------!
SUBROUTINE ReadHills(hillfile,hill,width,height,mtd_steps,periodic)
IMPLICIT NONE
INTEGER,INTENT(INOUT)::mtd_steps
REAL*8, INTENT(OUT), ALLOCATABLE::hill(:),width(:),height(:)
CHARACTER(len=20),INTENT(IN)::hillfile
LOGICAL, INTENT(IN)::periodic
INTEGER::i,j,i_mtd
REAL*8::time
REAL*8, PARAMETER :: kj_to_kcal = 0.239006


CALL SYSTEM('cp HILLS HILLS.old')
CALL SYSTEM('sed -i "/^#/d" HILLS')

open(unit=12,file=hillfile,status='old')
mtd_steps=NSteps(12)
ALLOCATE(hill(mtd_steps),width(mtd_steps),height(mtd_steps))

DO i_mtd=1,mtd_steps
  READ(12,*)time,hill(i_mtd),width(i_mtd),height(i_mtd)
  if(periodic)  hill(i_mtd)=Apply_Piriodicity(hill(i_mtd))
  height(i_mtd)=height(i_mtd)*kj_to_kcal

ENDDO

CLOSE(12)
END SUBROUTINE ReadHills
!-----------------------------------!
SUBROUTINE ReadHills_new(hillfile,hill,width,height,mtd_steps,periodic,mtd_dimension)
IMPLICIT NONE
INTEGER,INTENT(IN)::mtd_dimension
INTEGER,INTENT(OUT)::mtd_steps
REAL*8, INTENT(OUT), ALLOCATABLE::hill(:,:),width(:,:),height(:)
CHARACTER(len=20),INTENT(IN)::hillfile
LOGICAL, INTENT(IN)::periodic
INTEGER::i,j,i_mtd
REAL*8::time
REAL*8, PARAMETER :: kj_to_kcal = 0.239006


CALL SYSTEM('cp HILLS HILLS.old')
CALL SYSTEM('sed -i "/^#/d" HILLS')

open(unit=12,file=hillfile,status='old')
mtd_steps=NSteps(12)

ALLOCATE(hill(mtd_dimension,mtd_steps),width(mtd_dimension,mtd_steps),height(mtd_steps))

DO i_mtd=1,mtd_steps

        READ(12,*)time,hill(1:mtd_dimension, i_mtd),width(1:mtd_dimension,i_mtd),height(i_mtd)
       
        if(periodic) then
            DO i=1,mtd_dimension
               hill(i,i_mtd)=Apply_Piriodicity(hill(i,i_mtd))
            ENDDO
        endif

        height(i_mtd)=height(i_mtd)*kj_to_kcal

ENDDO

CLOSE(12)
END SUBROUTINE ReadHills_new
!-----------------------------------!
END MODULE ReadFiles
