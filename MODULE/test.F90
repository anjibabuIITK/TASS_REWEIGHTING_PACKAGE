IMPLICIT NONE
CHARACTER(len=20) ::cvfile, hillfile
LOGICAL ::metad,periodic
REAL*8 ::cv_temp, sys_temp,biasfactor,gridmin1,gridmin2,gridmax1,gridmax2,gridmin3,gridmax3,gridmin4,gridmax4
REAL*8 ::gridwidth1,gridwidth2,gridwidth3,gridwidth4
INTEGER ::ncv, uscv, mtdcv,hill_freq,cv_freq,nbin,t_min,t_max
CHARACTER(len=20)::KEYWORD, dummy, dummy2
CHARACTER(len=1000)::text
INTEGER::i,n, k, io_stat 
REAL*8, ALLOCATABLE :: grid1(:,:),gridmin(:),gridmax(:)
CHARACTER*20,ALLOCATABLE :: grid(:)

ncv = 4
ALLOCATE (grid(ncv))
ALLOCATE (gridmin(ncv), gridmax(ncv))
ALLOCATE (grid1(3,ncv))
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
    CASE ('PERIODICITY')
      READ(text,*) KEYWORD,dummy,dummy2
      IF(dummy2 == "ON") periodic=.TRUE.
    CASE ('NBIN')
      READ(text,*) KEYWORD,dummy,nbin
    CASE ('GRID_INFORMATION')
      READ(text,*) KEYWORD
        GOTO 200
    CASE ('T_MIN')
      READ(text,*) KEYWORD,dummy,t_min
    CASE ('T_MAX')
      READ(text,*) KEYWORD,dummy,t_max
    CASE DEFAULT

END SELECT
ENDDO
100 FORMAT (A4,I1,2X,A,4F16.2)
200 CONTINUE
nbin=101
IF (KEYWORD .eq. 'GRID_INFORMATION') THEN
   DO i = 1,NCV
     READ(10,*) grid(i),dummy,grid1(1:2,i)!gridmin(i),gridmax(i)
     grid1(3,i) = (grid1(2,i) - grid1(1,i))/nbin
     PRINT*,nbin
   ENDDO
   DO i = 1,NCV
     WRITE(6,100) grid(i),(i),dummy,grid1(1:3,i)!gridmin(i),gridmax(i)
  ENDDO
  ELSE
    PRINT*,'!!ERROR IN THE INPUT FILE NEAR GRID INFORMATION :-> STOPPING...'
ENDIF
CLOSE(10)
END 
