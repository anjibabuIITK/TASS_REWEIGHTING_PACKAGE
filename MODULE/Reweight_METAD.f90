MODULE REWEIGHT
CONTAINS

!---------------------------------------------------------!
!SUBROUTINE Calculate_VBias()
!SUBROUTINE Calculate_VBias(cv,hill,width,height,w_hill,w_cv,mdsteps,mtd_steps,which_CV,vbias )
!IMPLICIT NONE
!!REAL*8 :: diff_s2, ds2, ss, hh, dum, num, gamma_,alpha
!REAL*8, ALLOCATABLE,INTENT(IN) :: cv1(:,:), hill(:), height(:), vbias(:), width(:)
!INTEGER,INTENT(IN) :: mtd_steps, md_steps, w_cv, w_hill
!CHARACTER (len=20), INTENT(IN)::which_CV
!INTEGER :: mtd_max, i,j, i_md, i_mtd
!INTEGER :: i_s1, i_s2, i_s3, i_s4
!
!
!ALLOCATE(cv(md_steps),cv2(md_steps))
!ALLOCATE(cv3(md_steps),cv4(md_steps))
!ALLOCATE(vbias(md_steps))
!ALLOCATE(ht(mtd_steps))
!ALLOCATE(hill(mtd_steps),width(mtd_steps))
!
!
!DO i_md=1,md_steps
! READ(11,*) dum,cv1(i_md),cv2(i_md),cv3(i_md),cv4(i_md)
! READ(11,*) dum,cv1(i_md),cv2(i_md)
!     IF( cv1(i_md) .gt.  3.14d0)  cv1(i_md) = cv1(i_md) - 6.28d0
!     IF( cv1(i_md) .lt. -3.14d0 ) cv1(i_md) = cv1(i_md) + 6.28d0
!     IF( cv2(i_md) .gt.  3.14d0)  cv2(i_md) = cv2(i_md) - 6.28d0
!     IF( cv2(i_md) .lt. -3.14d0 ) cv2(i_md) = cv2(i_md) + 6.28d0
!     IF( cv3(i_md) .gt.  3.14d0)  cv3(i_md) = cv3(i_md) - 6.28d0
!     IF( cv3(i_md) .lt. -3.14d0 ) cv3(i_md) = cv3(i_md) + 6.28d0
!     IF( cv4(i_md) .gt.  3.14d0)  cv4(i_md) = cv4(i_md) - 6.28d0
!     IF( cv4(i_md) .lt. -3.14d0 ) cv4(i_md) = cv4(i_md) + 6.28d0
!END DO

!nbin1 = NINT((gridmax1-gridmin1)/griddiff1)+1
!nbin2 = NINT((gridmax2-gridmin2)/griddiff2)+1
!nbin3 = NINT((gridmax3-gridmin3)/griddiff3)+1
!nbin4 = NINT((gridmax4-gridmin4)/griddiff4)+1

!write(*,*) nbin1, nbin2, nbin3, nbin4
!write(*,*) nbin1, nbin2

!DO i_mtd=1,mtd_steps
! READ(12,*) dum,hill(i_mtd),width(i_mtd),ht(i_mtd)
!      IF( hill(i_mtd) .gt.  3.14d0) hill(i_mtd) = hill(i_mtd) - 6.28d0
!      IF( hill(i_mtd) .lt. -3.14d0 )hill(i_mtd) = hill(i_mtd) + 6.28d0
!       ht(i_mtd)=ht(i_mtd)*kj_to_kcal
!END DO
!
! write(*,*) 'calculating vbias'
!DO i_md=1,md_steps
!  mtd_max=(i_md*w_cv/w_hill)
!ss=cv2(i_md)
! dum=0.d0
!   DO i_mtd=1,mtd_max
!    ds2=width(i_mtd)*width(i_mtd)
!    hh=ht(i_mtd)/alpha
!    diff_s2=ss-hill(i_mtd)
!     if (diff_s2 .gt. 3.14d0 ) diff_s2 =diff_s2 - 6.28d0
!     if (diff_s2 .lt.-3.14d0 ) diff_s2 =diff_s2 + 6.28d0
  ! ! diff_s2=diff_s2*diff_s2*0.5D0
!    dum=dum+hh*DEXP(-diff_s2/ds2)
!   END DO
!vbias(i_md)=dum
!write(21,*) i_md, vbias(i_md)
!END DO

!write(*,*) " vbias written in data_vbias.dat file. "

!close(1)
!close(11)
!close(21)
!close(12)

!DEALLOCATE(cv2, ht, vbias, hill)
!DEALLOCATE(cv3, cv4, width, cv1)
!DEALLOCATE(width, cv1)

!END SUBROUTINE Calculate_VBias(           )


!---------------------------------------------------------!
!SUBROUTINE Calculate_Ct_factor()
!



END MODULE REWEIGHT
