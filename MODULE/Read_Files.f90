MODULE ReadFiles
USE GetSteps
CONTAINS
!---------------------------------------------------------------------------!
SUBROUTINE ReadCVFile(cvfile,cv,i_cv,mdsteps,ncol,periodic_CV,periodic)
IMPLICIT NONE
INTEGER :: i,j,k,n,it,mdsteps,ncol,i_cv(30)
REAL*8,ALLOCATABLE :: cv(:,:)
CHARACTER(len=20)   :: periodic_CV,cvfile
LOGICAL, INTENT(IN) :: periodic

CALL SYSTEM("cp COLVAR COLVAR.old")
CALL SYSTEM('sed -i "/^#/d" COLVAR')
open(unit=11,file=cvfile,status='old')
mdsteps=NSteps(11)
ncol=Get_Columns(11)

ALLOCATE(cv(mdsteps,ncol))
IF (periodic) THEN
!-------------Check Given Input Type (Character/Integer)--------------------!
it = ICHAR(periodic_CV(1:1))           !Convet into ASCII character set	    !
IF (it .ge. 48 .and. it .le. 57)  THEN !Check If Integer (48 - 57 is Integer)
READ(periodic_CV,'(I5)') n             !IF yes then read that as in integer !
!---------------------------------------------------------------------------!
! Applying Periodicity
DO i = 1,mdsteps
READ(11,*,END=100) cv(i,1:)
  DO k = 1,n
    j = i_cv(k)
    cv(i,j)=Apply_Piriodicity(cv(i,j))
  ENDDO
ENDDO

ELSE  ! If not found as an Integer then do the following 

DO i=1,mdsteps
  READ(11,*)cv(i,1:)
!---------------------------------------------------------------------------!
! Applying Periodicity
IF (periodic_CV .eq. 'NONE') THEN
cv(i,1:) = cv (i,1:)
ELSEIF (periodic_CV .eq. 'ALL') THEN
   DO j = 2,ncol
    cv(i,j)=Apply_Piriodicity(cv(i,j))
   ENDDO
ENDIF
ENDDO ; ENDIF
ELSE
DO i = 1,mdsteps
READ(11,*,END=100) cv(i,1:)
ENDDO
ENDIF
100 CLOSE(11)
END
!---------------------------------------------------------------------------!
REAL FUNCTION Apply_Piriodicity(AnyValue)
IMPLICIT NONE
REAL*8 :: AnyValue
   if (AnyValue .gt. 3.14d0 ) AnyValue=AnyValue - 6.28d0
   if (AnyValue .lt.-3.14d0 ) AnyValue=AnyValue + 6.28d0

Apply_Piriodicity=AnyValue

END FUNCTION Apply_Piriodicity
!
!---------------------------------------------------------------------------!
SUBROUTINE ReadHills(hillfile,hill,width,height,mtd_steps,periodic)
IMPLICIT NONE
INTEGER :: i,j,i_mtd
INTEGER,INTENT(INOUT) :: mtd_steps
REAL*8 :: time
REAL*8, PARAMETER :: kj_to_kcal = 0.239006
REAL*8, INTENT(OUT), ALLOCATABLE :: hill(:),width(:),height(:)
CHARACTER(len=20),INTENT(IN) :: hillfile
LOGICAL, INTENT(IN) :: periodic


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
!---------------------------------------------------------------------------!
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

        READ(12,*,END=100)time,hill(1:mtd_dimension, i_mtd),width(1:mtd_dimension,i_mtd),height(i_mtd)
!        DO j = 1,mtd_dimension
!         READ(12,*,END=100)time,hill(j,i_mtd)!,width(1:mtd_dimension,i_mtd),height(i_mtd)
!WRITE(6,101)time,hill(j,i_mtd)!,width(1:mtd_dimension,i_mtd),height(i_mtd)
!        ENDDO
        if(periodic) then
            DO i=1,mtd_dimension
               hill(i,i_mtd)=Apply_Piriodicity(hill(i,i_mtd))
            ENDDO
        endif

        height(i_mtd)=height(i_mtd)*kj_to_kcal

ENDDO
101 FORMAT (F16.4,F16.4,F16.4,F16.4)
100 CLOSE(12)
!STOP
END SUBROUTINE ReadHills_new
!-----------------------------------!
END MODULE ReadFiles
