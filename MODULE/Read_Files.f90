MODULE ReadFiles
CONTAINS

!-----------------------------------!
SUBROUTINE ReadCVFile(iunit,cv,mdsteps,ncol,periodic_CV)
IMPLICIT NONE
INTEGER,INTENT(IN)::iunit,mdsteps,ncol
REAL*8, INTENT(OUT)::cv(mdsteps,ncol)
CHARACTER(len=20),INTENT(IN)::periodic_CV
INTEGER::i,j
REWIND(iunit)
DO i=1,mdsteps
  READ(iunit,*)cv(i,1:)


SELECT CASE (periodic_CV)

CASE ("NONE")
     Print*,"No Periodic applied"
CASE ("ALL")
     IF( cv(i,2) .gt.  3.14d0)  cv(i,2) = cv(i,2) - 6.28d0
     IF( cv(i,2) .lt. -3.14d0 ) cv(i,2) = cv(i,2) + 6.28d0
     IF( cv(i,3) .gt.  3.14d0)  cv(i,3) = cv(i,3) - 6.28d0
     IF( cv(i,3) .lt. -3.14d0 ) cv(i,3) = cv(i,3) + 6.28d0
     IF( cv(i,4) .gt.  3.14d0)  cv(i,4) = cv(i,4) - 6.28d0
     IF( cv(i,4) .lt. -3.14d0 ) cv(i,4) = cv(i,4) + 6.28d0
     IF( cv(i,5) .gt.  3.14d0)  cv(i,5) = cv(i,5) - 6.28d0
     IF( cv(i,5) .lt. -3.14d0 ) cv(i,5) = cv(i,5) + 6.28d0
     Print*, "All the CVs are periodic"
CASE ("CV1")
     Print*, "CV1 is periodic"
     IF( cv(i,2) .gt.  3.14d0)  cv(i,2) = cv(i,2) - 6.28d0
     IF( cv(i,2) .lt. -3.14d0 ) cv(i,2) = cv(i,2) + 6.28d0
CASE ("CV2")
     Print*, "CV2 is periodic"
     IF( cv(i,3) .gt.  3.14d0)  cv(i,3) = cv(i,3) - 6.28d0
     IF( cv(i,3) .lt. -3.14d0 ) cv(i,3) = cv(i,3) + 6.28d0
CASE ("CV3")
     Print*, "CV3 is periodic"
     IF( cv(i,4) .gt.  3.14d0)  cv(i,4) = cv(i,4) - 6.28d0
     IF( cv(i,4) .lt. -3.14d0 ) cv(i,4) = cv(i,4) + 6.28d0
CASE ("CV4")
     Print*, "CV4 is periodic"
     IF( cv(i,5) .gt.  3.14d0)  cv(i,5) = cv(i,5) - 6.28d0
     IF( cv(i,5) .lt. -3.14d0 ) cv(i,5) = cv(i,5) + 6.28d0
CASE ("CV1.and.CV2")
     Print*, "CV1 and CV2 are periodic"
     IF( cv(i,2) .gt.  3.14d0)  cv(i,2) = cv(i,2) - 6.28d0
     IF( cv(i,2) .lt. -3.14d0 ) cv(i,2) = cv(i,2) + 6.28d0
     IF( cv(i,3) .gt.  3.14d0)  cv(i,3) = cv(i,3) - 6.28d0
     IF( cv(i,3) .lt. -3.14d0 ) cv(i,3) = cv(i,3) + 6.28d0
CASE ("CV2.and.CV3")
     Print*, "CV2 and CV3 are periodic"
     IF( cv(i,3) .gt.  3.14d0)  cv(i,3) = cv(i,3) - 6.28d0
     IF( cv(i,3) .lt. -3.14d0 ) cv(i,3) = cv(i,3) + 6.28d0
     IF( cv(i,4) .gt.  3.14d0)  cv(i,4) = cv(i,4) - 6.28d0
     IF( cv(i,4) .lt. -3.14d0 ) cv(i,4) = cv(i,4) + 6.28d0
CASE ("CV3.and.CV4")
     Print*, "CV3 and CV4 are periodic"
     IF( cv(i,4) .gt.  3.14d0)  cv(i,4) = cv(i,4) - 6.28d0
     IF( cv(i,4) .lt. -3.14d0 ) cv(i,4) = cv(i,4) + 6.28d0
     IF( cv(i,5) .gt.  3.14d0)  cv(i,5) = cv(i,5) - 6.28d0
     IF( cv(i,5) .lt. -3.14d0 ) cv(i,5) = cv(i,5) + 6.28d0
CASE ("CV1.and.CV4")
     Print*, "CV1 and CV4 are periodic"
     IF( cv(i,2) .gt.  3.14d0)  cv(i,2) = cv(i,2) - 6.28d0
     IF( cv(i,2) .lt. -3.14d0 ) cv(i,2) = cv(i,2) + 6.28d0
     IF( cv(i,5) .gt.  3.14d0)  cv(i,5) = cv(i,5) - 6.28d0
     IF( cv(i,5) .lt. -3.14d0 ) cv(i,5) = cv(i,5) + 6.28d0
CASE ("CV2.and.CV4")
     Print*, "CV2 and CV4 are periodic"
     IF( cv(i,3) .gt.  3.14d0)  cv(i,3) = cv(i,3) - 6.28d0
     IF( cv(i,3) .lt. -3.14d0 ) cv(i,3) = cv(i,3) + 6.28d0
     IF( cv(i,5) .gt.  3.14d0)  cv(i,5) = cv(i,5) - 6.28d0
     IF( cv(i,5) .lt. -3.14d0 ) cv(i,5) = cv(i,5) + 6.28d0
CASE ("CV1.and.CV3")
     Print*, "CV1 and CV3 are periodic"
     IF( cv(i,2) .gt.  3.14d0)  cv(i,2) = cv(i,2) - 6.28d0
     IF( cv(i,2) .lt. -3.14d0 ) cv(i,2) = cv(i,2) + 6.28d0
     IF( cv(i,4) .gt.  3.14d0)  cv(i,4) = cv(i,4) - 6.28d0
     IF( cv(i,4) .lt. -3.14d0 ) cv(i,4) = cv(i,4) + 6.28d0
CASE ("CV1.and.CV2.and.CV3")
     Print*, "CV1 ,CV2 and CV3 are periodic"
     IF( cv(i,2) .gt.  3.14d0)  cv(i,2) = cv(i,2) - 6.28d0
     IF( cv(i,2) .lt. -3.14d0 ) cv(i,2) = cv(i,2) + 6.28d0
     IF( cv(i,3) .gt.  3.14d0)  cv(i,3) = cv(i,3) - 6.28d0
     IF( cv(i,3) .lt. -3.14d0 ) cv(i,3) = cv(i,3) + 6.28d0
     IF( cv(i,4) .gt.  3.14d0)  cv(i,4) = cv(i,4) - 6.28d0
     IF( cv(i,4) .lt. -3.14d0 ) cv(i,4) = cv(i,4) + 6.28d0
CASE ("CV2.and.CV3.and.CV4")
     Print*, "CV2 ,CV3 and CV4 are periodic"
     IF( cv(i,3) .gt.  3.14d0)  cv(i,3) = cv(i,3) - 6.28d0
     IF( cv(i,3) .lt. -3.14d0 ) cv(i,3) = cv(i,3) + 6.28d0
     IF( cv(i,4) .gt.  3.14d0)  cv(i,4) = cv(i,4) - 6.28d0
     IF( cv(i,4) .lt. -3.14d0 ) cv(i,4) = cv(i,4) + 6.28d0
     IF( cv(i,5) .gt.  3.14d0)  cv(i,5) = cv(i,5) - 6.28d0
     IF( cv(i,5) .lt. -3.14d0 ) cv(i,5) = cv(i,5) + 6.28d0

CASE DEFAULT
     PRINT*,"CV > 4 is not implimented"
END SELECT


ENDDO
REWIND(iunit)
END SUBROUTINE ReadCVFile
!-----------------------------------!
 ! SUBROUTINE ReadHills()
!
!-----------------------------------!
END MODULE ReadFiles
