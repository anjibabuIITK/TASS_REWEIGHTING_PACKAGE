MODULE Prepare_Inputs
IMPLICIT NONE
CHARACTER(len=20)::KEYWORD, cvfile, hillfile,dummy, dummy2
CHARACTER(len=1000)::text
LOGICAL::metad=.TRUE.
REAL*8::cv_temp, sys_temp,biasfactor
INTEGER::i,n, k, io_stat 
INTEGER::ncv, uscv, mtdcv,hill_freq,cv_freq


!--------------------------------------------!
!Initializing all the Variables
cv_temp=300.d0; sys_temp=300.d0
ncv=2; uscv=1; mtdcv=1;hill_freq=500;cv_freq=10
cvfile="COLVAR";hillfile="HILLS"
metad=.TRUE.
!--------------------------------------------!
CONTAINS
!--------------------------------------------!
!Fortran program to read input from a file
SUBROUTINE Get_Inputs(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq)

CHARACTER(len=20),INTENT(INOUT)::cvfile, hillfile
LOGICAL,INTENT(INOUT)::metad=.TRUE.
REAL*8,INTENT(INOUT)::cv_temp, sys_temp,biasfactor
INTEGER,INTENT(INOUT)::ncv, uscv, mtdcv,hill_freq,cv_freq
CHARACTER(len=20)::KEYWORD, dummy, dummy2
CHARACTER(len=1000)::text
INTEGER::i,n, k, io_stat 

!Initializing all the Variables
!CALL Initialize(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad)

! Check inputfiles 
!call Is_File_Exist('input.inp ')
!call Is_File_Exist('plumed.dat')

! Reading Plumed inputfile
!CALL Read_Plumed_Input()

!OPEN(unit=10, file='input.inp', status='unknown')
OPEN(unit=10, file='admin_input.inp', status='unknown')
REWIND(10)
DO
READ(10, '(a)', iostat=io_stat) text
if(io_stat .ne. 0) exit
READ(text,*) KEYWORD
SELECT CASE (KEYWORD)

    CASE ('CVFILE')
      READ(text,*) KEYWORD,dummy,cvfile
    CASE ('HILLSFILE')
      READ(text,*) KEYWORD,dummy,hillfile
    CASE ('NCV')
      READ(text,*) KEYWORD,dummy,ncv
    CASE ('NMTDCV')
      READ(text,*) KEYWORD,dummy,mtdcv
    CASE ('USCV')
      READ(text,*) KEYWORD,dummy,uscv
    CASE ('PACE')
      READ(text,*) KEYWORD,dummy,hill_freq
    CASE ('CV_TEMP')
      READ(text,*) KEYWORD,dummy,cv_temp
    CASE ('SYSTEM_TEMP')
      READ(text,*) KEYWORD,dummy,sys_temp
    CASE ('STRIDE')
      READ(text,*) KEYWORD,dummy,cv_freq
    CASE ('BIASFACTOR')
      READ(text,*) KEYWORD,dummy,biasfactor
    CASE ('METAD')
      READ(text,*) KEYWORD,dummy,dummy2
      IF(dummy2 == "OFF") metad=.FALSE.
    CASE DEFAULT
!      READ(text,*) KEYWORD,dummy,dummy2

END SELECT
ENDDO
! Print the Data
!CALL Print_Data(cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq)
CLOSE(10)
END SUBROUTINE 
!-----------------------------------------!
! Subroutine to check existance of given file
SUBROUTINE Is_File_Exist(givenfile)
IMPLICIT NONE
CHARACTER(len=10)::givenfile
LOGICAL::fexist

inquire (file=givenfile,exist=fexist)
if(fexist) then
         write(*,*) " The file"," ",givenfile," exists!"
       else
         write(*,*) " The file"," ",givenfile," does not exist!"
         stop
       end if
END SUBROUTINE 
!-----------------------------------------!
! subroutine to initialize all the variables
SUBROUTINE Initialize(cvfile,hillfile,cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq,metad)
IMPLICIT NONE
REAL*8, INTENT(INOUT):: cv_temp, sys_temp
INTEGER, INTENT(INOUT)::ncv, uscv, mtdcv,hill_freq,cv_freq
CHARACTER (len=20), INTENT(INOUT)::cvfile,hillfile
LOGICAL, INTENT(INOUT)::metad

cv_temp=300.d0; sys_temp=300.d0
ncv=2; uscv=1; mtdcv=1;hill_freq=500;cv_freq=10
cvfile="COLVAR";hillfile="HILLS"
metad=.TRUE.

END SUBROUTINE

!------------------------------------------!
SUBROUTINE Read_Plumed_Input()
IMPLICIT NONE

CALL SYSTEM("sh Get_Plumed_input.sh")
CALL SYSTEM("cat input.inp >> admin_input.inp")

END SUBROUTINE
!-----------------------------------------!
SUBROUTINE Print_Data(cv_temp, sys_temp,ncv, uscv, mtdcv,hill_freq,cv_freq)
IMPLICIT NONE
REAL*8, INTENT(INOUT):: cv_temp, sys_temp
INTEGER, INTENT(INOUT)::ncv, uscv, mtdcv,hill_freq,cv_freq

WRITE(*,100)cv_temp
WRITE(*,200)sys_temp
WRITE(*,300)ncv
WRITE(*,400)mtdcv
WRITE(*,500)hill_freq
WRITE(*,600)cv_freq

100 FORMAT("CV TEMP ",5X,"=",2X,F10.2,2X)
200 FORMAT("SYSTEM TEMP ",5X,"=",2X,F10.2,2X)
300 FORMAT("NO. of CVs",5X,"=",2X,I5,2X)
400 FORMAT("METAD CVs",5X,"=",2X,I5,2X)
500 FORMAT("METAD PACE",5X,"=",2X,I5,2X)
600 FORMAT("METAD CV STRIDE",5X,"=",2X,I5,2X)

END SUBROUTINE
!-----------------------------------------!
END MODULE Prepare_Inputs
!ref:https://stackoverflow.com/questions/29125581/fortran-find-string-in-txt-file
