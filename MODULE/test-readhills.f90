Program test

!-----------------------------------!
!SUBROUTINE ReadHills_new(hillfile,hill,width,height,mtd_steps,periodic,mtd_dimension)
IMPLICIT NONE
INTEGER::mtd_steps,mtd_dimension
REAL*8,  ALLOCATABLE::hill(:,:),width(:,:),height(:)
CHARACTER(len=20)::hillfile
!LOGICAL::periodic
INTEGER::i,j,i_mtd
REAL*8::time,dummy
REAL*8, PARAMETER :: kj_to_kcal = 0.239006

mtd_dimension=3
mtd_steps=100
hillfile="HILLS_3D"

CALL SYSTEM('cp HILLS_3D HILLS_3D.old')
CALL SYSTEM('sed -i "/^#/d" HILLS_3D')

open(unit=12,file=hillfile,status='old')
!mtd_steps=NSteps(12)

ALLOCATE(hill(mtd_dimension,mtd_steps),width(mtd_dimension,mtd_steps),height(mtd_steps))

DO i_mtd=1,mtd_steps

        READ(12,*)time,hill(1:mtd_dimension, i_mtd),width(1:mtd_dimension,i_mtd),height(i_mtd),dummy
       
!        if(periodic) then
!            DO i=1,mtd_dimension
!               hill(i,i_mtd)=Apply_Piriodicity(hill(i,i_mtd))
!            ENDDO
!        endif

!        height(i_mtd)=height(i_mtd)*kj_to_kcal
 
        WRITE(*,*)time,hill(1:mtd_dimension, i_mtd),width(1:mtd_dimension,i_mtd),height(i_mtd),dummy
      
ENDDO

CLOSE(12)
!END SUBROUTINE ReadHills_new
!-----------------------------------!
end program test
