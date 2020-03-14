MODULE GetSteps
CONTAINS
SUBROUTINE get_steps(iunit,nsteps)
IMPLICIT NONE
INTEGER iunit, nsteps
INTEGER ios
nsteps=0
REWIND(iunit)
Read_Loop: DO
   READ(iunit,*,IOSTAT=ios)
   IF(ios.ne.0)EXIT Read_Loop
   nsteps=nsteps+1
END DO Read_Loop
REWIND(iunit)
END
!-----------------------------------!
INTEGER FUNCTION NSteps(iunit)
IMPLICIT NONE
INTEGER iunit,nstep
INTEGER ios
nstep=0
REWIND(iunit)
Read_Loop: DO
   READ(iunit,*,IOSTAT=ios)
   IF(ios.ne.0)EXIT Read_Loop
   nstep=nstep+1
END DO Read_Loop
REWIND(iunit)
NSteps=nstep
END FUNCTION NSteps
!-----------------------------------!
! Get Number of Columns
INTEGER FUNCTION Get_Columns(iunit)
IMPLICIT NONE
INTEGER, parameter :: max_clmns=30
CHARACTER (len=1000)::line
INTEGER:: i,io,ncol, iunit
REAL*8, DIMENSION(max_clmns)::Count_Array  !Count_Array(max_clmns)

REWIND(iunit)
!Get first line of the file
DO
READ(iunit,'(A)',iostat=io) line
IF (io/=0) THEN
WRITE(*,*) "Error in reading file."
stop
ENDIF
exit !exit loop
ENDDO

ncol=0
! Count number of columns 
DO i=1,max_clmns
 READ(line,*,iostat=io) Count_Array(1:i)
IF (io==-1) exit
ncol=ncol+1
ENDDO
!WRITE(*,*) "No. of Columns: ",ncolumn
REWIND(iunit)
Get_Columns=ncol
END FUNCTION Get_Columns 
!-----------------------------------!
! Get Number of Columns
SUBROUTINE GetColumns(iunit,ncol)
IMPLICIT NONE
INTEGER, parameter :: max_clmns=30
CHARACTER (len=1000)::line
INTEGER:: i,io,ncol, iunit
REAL*8, DIMENSION(max_clmns)::Count_Array  !Count_Array(max_clmns)

REWIND(iunit)
!Get first line of the file
DO
READ(iunit,'(A)',iostat=io) line
IF (io/=0) THEN
WRITE(*,*) "Error in reading file."
stop
ENDIF
exit !exit loop
ENDDO

ncol=0
! Count number of columns 
DO i=1,max_clmns
 READ(line,*,iostat=io) Count_Array(1:i)
IF (io==-1) exit
ncol=ncol+1
ENDDO
!WRITE(*,*) "No. of Columns: ",ncolumn
REWIND(iunit)
RETURN
ENDSUBROUTINE 
!-----------------------------------!
! Get gridwidth for given min, max, nbin
SUBROUTINE Get_gridwidth(gridmin,gridmax,nbin,gridwidth)
IMPLICIT NONE
REAL*8, INTENT(IN)::gridmin,gridmax
REAL*8, INTENT(OUT)::gridwidth
INTEGER, INTENT(IN)::nbin

gridwidth=(gridmax-gridmin)/DFLOAT(nbin-1)


ENDSUBROUTINE Get_gridwidth
!-----------------------------------!
! Function to Get gridwidth for given min, max, nbin
REAL FUNCTION GridWidth(gridmin,gridmax,nbin)
IMPLICIT NONE
REAL*8, INTENT(IN)::gridmin,gridmax
REAL*8::griddiff
INTEGER, INTENT(IN)::nbin

griddiff=(gridmax-gridmin)/DFLOAT(nbin-1)

! Function returns the value by its name so
GridWidth = griddiff

END FUNCTION GridWidth
!-----------------------------------!
!
!-----------------------------------!

END MODULE GetSteps
