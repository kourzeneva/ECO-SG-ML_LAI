PROGRAM GlobVisu 
! PURPOSE: Visualisation of ECO-SG_ML_LAI dataset util
! AUTHOR:  Ekaterina Kourzeneva, 
!          FMI, 08.2025
! MODIFICATIONS:
!
! 08.2025 Ekaterina Kourzeneva, FMI: small updates to visualize the albedo fiels as well
!

 USE Bitmap_ML_LAI, ONLY : &
  NlonB_gov_out, &                     ! Number of longitude pixels of ML and the output LAI/ALB and bitmaps
  NlatB_gov_out, &                     ! Number of latitude pixels of the LAI/ALB and ML bitmaps
  NlonB, &                             ! Number of longitude pixels of the specific bitmap
  NlatB, &                             ! Number of latitude pixels of the specific bitmap
  WestLim_gov_out, &                   ! Western boundary of the ML and output LAI/ALB bitmaps
  NorthLim_gov_out, &                  ! Northern boundary of the ML and output LAI/ALB and bitmaps 
  EastLim_gov_out, &                   ! Eastern boundary of the ML and output LAI/ALB bitmaps
  SouthLim_gov_out, &                  ! Southern boundary of the ML and output LAI/ALB bitmaps
  WestLim,  &                          ! Western boundary of the current bitmap
  NorthLim, &                          ! Northern boundary of the current bitmap
  EastLim,  &                          ! Eastern boundary of the current bitmap
  SouthLim, &                          ! Southern boundary of the current bitmap 
  PixSize_gov_out, &                   ! Pixel size of the LAI/ALB and ML bitmaps in seconds of arc(s)
  DPixSize_gov_out, &                  ! Pixel size decimal of the ML and output map
  DPixSize, &                          ! Pixel size decimal of the specific bitmap
  BP_gov, &                            ! Number of bytes per pixel, for the ML map
  Factor_out_all                       ! Multiplication factor for LAI/ALB
  

 IMPLICIT NONE

 LOGICAL, PARAMETER :: LRUC=.FALSE. ! If we want to read uncompressed data
                                    ! Here, we make a choice between reading compressed and uncompressed data.
                                    ! The default is reading the compressed data.
 
 REAL(KIND=8) :: North, & ! Boundaries of visualised region in deg.
         South, & ! 
         West,  & ! 
         East     !
 INTEGER :: FV    ! Field version: 1 is LAI, 2 is albedo

 CHARACTER(6), PARAMETER :: BitmapFile='bitmap'

 INTEGER :: NorthPix, SouthPix, WestPix, EastPix ! Boundaries of visualised region in pixels
 INTEGER :: Vol ! Size of visualised region

 REAL, DIMENSION(:,:), ALLOCATABLE :: Region ! Region (piece of the bitmap), format for GRADs
 INTEGER(1), DIMENSION(:), ALLOCATABLE :: LonPixDat1   ! Pixels of data along the latitude
 REAL, DIMENSION(:), ALLOCATABLE :: WLonPixDat         ! Working array

 CHARACTER(LEN=4), DIMENSION(:), ALLOCATABLE :: LonDimsChar
 INTEGER(4), DIMENSION(:), ALLOCATABLE :: LonDimsInt
 CHARACTER(LEN=2), DIMENSION(:), ALLOCATABLE :: LonPixDatCompChar
 INTEGER(2), DIMENSION(:), ALLOCATABLE :: LonPixDatCompInt
 INTEGER, DIMENSION(:), ALLOCATABLE :: IWLonPixDat

 INTEGER :: ilat, ilon, iargc
 INTEGER :: ErCode
 INTEGER :: ilat_lai, ilon_lai, ilon_s, ilon_0
 INTEGER :: klon_0
 INTEGER(8) :: iPos
 CHARACTER *80 :: CHIN

!--------------------------------------------------------------------------------------------

 IF(iargc().EQ.0) THEN
  WRITE(*,*) 'NO BOUNDARIES RECIEVED!'
  STOP
 END IF

 CALL GETARG(1, CHIN)
 READ(CHIN, '(F5.2)') North
 CALL GETARG(2, CHIN)
 READ(CHIN, '(F5.2)') South
 CALL GETARG(3, CHIN)
 READ(CHIN, '(F5.2)') West
 CALL GETARG(4, CHIN)
 READ(CHIN, '(F5.2)') East
 CALL GETARG(5, CHIN)
 READ(CHIN,'(I4)') FV
                   
 SELECT CASE(FV)
    CASE(1)
       WRITE(*,*) 'Bitmap is LAI_*_c.dir'
    CASE(2)
       WRITE(*,*) 'Bitmap is AL_*_c.dir'
 END SELECT

 WRITE(*,*) 'North= ', North, ' South:= ', South
 WRITE(*,*) 'West= ', West, ' East= ', East

 IF((North-South).LT.0._8.OR.(East-West).LT.0._8) THEN
  WRITE(*,*) 'Wrong boundaries of the region!'
  STOP  
 END IF

 DPixSize_gov_out=PixSize_gov_out/3600._8
 EastLim_gov_out=WestLim_gov_out+NlonB_gov_out*DPixSize_gov_out
 SouthLim_gov_out=NorthLim_gov_out-NlatB_gov_out*DPixSize_gov_out

 NlonB=NlonB_gov_out
 NlatB=NlatB_gov_out
 WestLim=WestLim_gov_out
 NorthLim=NorthLim_gov_out
 EastLim=EastLim_gov_out
 SouthLim=SouthLim_gov_out
 DPixSize=DPixSize_gov_out
  
 CALL Coor2Num_ML_LAI(North, 2, NorthPix, ErCode)
 IF(ErCode.NE.0) STOP
 CALL Coor2Num_ML_LAI(South, 2, SouthPix, ErCode)
 IF(ErCode.NE.0) STOP
 CALL Coor2Num_ML_LAI(West,  1, WestPix,  ErCode)
 IF(ErCode.NE.0) STOP
 CALL Coor2Num_ML_LAI(East,  1, EastPix,  ErCode)
 IF(ErCode.NE.0) STOP

 write(*,*) 'NorthPix= ', NorthPix, ' SouthPix= ', SouthPix
 write(*,*) 'WestPix= ' , WestPix,  ' EastPix= ' , EastPix

 Vol = (SouthPix-NorthPix+1)*(EastPix-WestPix+1)
 IF(Vol.GT.750000) WRITE(*,*) 'WARNING!!!!!: Too many grid points for GrADS!!!'

 ALLOCATE(Region(WestPix:EastPix,NorthPix:SouthPix))
 IF(SIZE(Region).NE.Vol) STOP 'Wrong size of the region !!!'

 ALLOCATE(LonPixDat1(NlonB_gov_out))
 ALLOCATE(WLonPixDat(NlonB_gov_out))
 ALLOCATE(IWLonPixDat(NlonB_gov_out))

 IF(LRUC) THEN ! Uncompressed data
    
    OPEN(1,file=TRIM(BitmapFile), FORM='unformatted', ACCESS='direct', RECL=NlonB_gov_out*BP_gov)
    
    DO ilat=NorthPix,SouthPix
       
      READ(1,REC=ilat) LonPixDat1
      WLonPixDat=LonPixDat1     

      DO ilon=WestPix, EastPix
        Region(ilon,(NorthPix+SouthPix)-ilat)=WLonPixDat(ilon)    
      END DO
  
    END DO

 ELSE ! Compressed data

    OPEN(1,file=TRIM(BitmapFile), FORM='unformatted', ACCESS='stream')

    ALLOCATE(LonDimsChar(NlatB_gov_out))
    ALLOCATE(LonDimsInt(NlatB_gov_out))

 ! Read governing info for the LAI map: line of dimensions in lon
    READ(1) LonDimsChar
    LonDimsInt=TRANSFER(LonDimsChar,1_4,NlatB_gov_out)
    
! Move the "cursor" to the initial position on the LAI map
    iPos=NLatB_gov_out*4+1
    DO ilat_lai=1,NorthPix-1
       iPos=iPos+LonDimsInt(ilat_lai)*2
    END DO

    DO ilat_lai=NorthPix,SouthPix

       ! Read the LAI data    
       ALLOCATE(LonPixDatCompChar(LonDimsInt(ilat_lai)))
       ALLOCATE(LonPixDatCompInt(LonDimsInt(ilat_lai)))
       
       READ(1,pos=iPos) LonPixDatCompChar
       LonPixDatCompInt=TRANSFER(LonPixDatCompChar,1_2,LonDimsInt(ilat_lai))
       ilon_lai=1
       DO ilon_s=1,LonDimsInt(ilat_lai)
          
          IF(LonPixDatCompInt(ilon_s).LT.4000) THEN
             WLonPixDat(ilon_lai)=LonPixDatCompInt(ilon_s)
             IWLonPixDat(ilon_lai)=FLOOR(WLonPixDat(ilon_lai)/100.)
             WLonPixDat(ilon_lai)=MOD(WLonPixDat(ilon_lai),100.)
             ilon_lai=ilon_lai+1
          ELSE
             klon_0=LonPixDatCompInt(ilon_s)-4000
             DO ilon_0=1,klon_0
                IF(ilon_lai.GT.NLonB_gov_out) THEN
                   write(*,*) 'Too many values:', ilon_lai, ilon_s, ilon_0, klon_0, LonDimsInt(ilat_lai), ilat_lai
                   STOP
                END IF
                WLonPixDat(ilon_lai)=0.
                IWLonPixDat(ilon_lai)=0
                ilon_lai=ilon_lai+1
             END DO
          END IF
       END DO

       DO ilon=WestPix, EastPix     
         Region(ilon,(NorthPix+SouthPix)-ilat_lai)=WLonPixDat(ilon)     
       END DO      
    
       iPos=iPos+LonDimsInt(ilat_lai)*2

       DEALLOCATE(LonPixDatCompChar)
       DEALLOCATE(LonPixDatCompInt)
       
    END DO

    DEALLOCATE(LonDimsChar)
    DEALLOCATE(LonDimsInt)
   
 END IF
 
 Region=Region/Factor_out_all(FV)
 
 WRITE(*,*) MINVAL(Region), MAXVAL(Region)

 OPEN(3, file='Visu.dat', FORM='unformatted', ACCESS='direct', RECL=Vol*4)
 WRITE(3, REC=1) Region

 OPEN(4, file='Visu.ctl')
 WRITE(4,'(A14)') 'dset ^Visu.dat'
 WRITE(4,'(A21)') 'options little_endian'
 WRITE(4,'(A14)') 'undef -99999.9'
 WRITE(4,'(A21)') 'title Landscape Types'
! WRITE(4,'(A4,I8,A8,2F13.7)') 'xdef',(EastPix-WestPix+1), 'linear', West, PixSize/3600.
! WRITE(4,'(A4,I8,A8,2F13.7)') 'ydef',(SouthPix-NorthPix+1), 'linear', South, PixSize/3600.
 WRITE(4,'(A4,I8,A8,2F13.7)') 'xdef',(EastPix-WestPix+1), 'linear', West, PixSize_gov_out/3600._8
 WRITE(4,'(A4,I8,A8,2F13.7)') 'ydef',(SouthPix-NorthPix+1), 'linear', South, PixSize_gov_out/3600._8
 WRITE(4,'(A18)') 'zdef 1 levels 1000'
 WRITE(4,'(A33)') 'tdef  99 linear 00z21dec1978 12hr'
 WRITE(4,'(A6)') 'vars 1'
 WRITE(4,'(A24)') 'Type 0 99 Landscape Type'
 WRITE(4,'(A7)') 'endvars'
 
 CLOSE(1)
 CLOSE(3)
 CLOSE(4)

 DEALLOCATE(Region)
 DEALLOCATE(LonPixDat1)
 DEALLOCATE(WLonPixDat)
 DEALLOCATE(IWLonPixDat)

END PROGRAM GlobVisu
!-------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------
