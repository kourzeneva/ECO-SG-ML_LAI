PROGRAM GlobVisu 
! PURPOSE: Visualisation of ECO-SG_ML_LAI dataset util
! AUTHOR:  Ekaterina Kourzeneva, 
!          FMI, 08.2025

 USE Bitmap_ML_LAI, ONLY : &
  NlonB_all, &                         ! Number of longitude pixels of all bitmaps
  NlatB_all, &                         ! Number of latitude pixels of all bitmaps
  WestLim_all, &                       ! Western boundaries of all bitmaps
  NorthLim_all, &                      ! Northern boundaries of all bitmaps
  EastLim_all, &                       ! Eastern boundaries of all bitmaps
  SouthLim_all, &                      ! Southern boundaries of all bitmaps
  PixSize_all, &                       ! Pixel size of the bitmap in seconds of arc of all bitmaps
  DPixSize_all, &                      ! Pixel size decimal of all bitmaps
  BP_all                               ! Number of bytes per pixel of all bitmaps
  

 IMPLICIT NONE

 LOGICAL, PARAMETER :: LRUC=.FALSE. ! If we want to read uncompressed data
                                    ! Here, we make a choice between reading compressed and uncompressed data.
                                    ! The default is reading the compressed data.
 
 REAL(KIND=8) :: North, & ! Boundaries of visualised region in deg.
         South, & ! 
         West,  & ! 
         East     ! 

 INTEGER, PARAMETER :: BV = 2 ! Bitmap version
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
                   
 WRITE(*,*) 'Bitmap is ECO-SG-ML_LAI!'

 WRITE(*,*) 'North= ', North, ' South:= ', South
 WRITE(*,*) 'West= ', West, ' East= ', East

 IF((North-South).LT.0._8.OR.(East-West).LT.0._8) THEN
  WRITE(*,*) 'Wrong boundaries of the region!'
  STOP  
 END IF

 DPixSize_all=PixSize_all/3600._8
 EastLim_all=WestLim_all+NlonB_all*DPixSize_all
 SouthLim_all=NorthLim_all-NlatB_all*DPixSize_all
  
 CALL Coor2Num_ML_LAI(BV, North, 2, NorthPix, ErCode)
 IF(ErCode.NE.0) STOP
 CALL Coor2Num_ML_LAI(BV, South, 2, SouthPix, ErCode)
 IF(ErCode.NE.0) STOP
 CALL Coor2Num_ML_LAI(BV, West,  1, WestPix,  ErCode)
 IF(ErCode.NE.0) STOP
 CALL Coor2Num_ML_LAI(BV, East,  1, EastPix,  ErCode)
 IF(ErCode.NE.0) STOP

 write(*,*) 'NorthPix= ', NorthPix, ' SouthPix= ', SouthPix
 write(*,*) 'WestPix= ' , WestPix,  ' EastPix= ' , EastPix

 Vol = (SouthPix-NorthPix+1)*(EastPix-WestPix+1)
 IF(Vol.GT.750000) WRITE(*,*) 'WARNING!!!!!: Too many grid points for GrADS!!!'

 ALLOCATE(Region(WestPix:EastPix,NorthPix:SouthPix))
 IF(SIZE(Region).NE.Vol) STOP 'Wrong size of the region !!!'

 ALLOCATE(LonPixDat1(NlonB_all(BV)))
 ALLOCATE(WLonPixDat(NlonB_all(BV)))
 ALLOCATE(IWLonPixDat(NlonB_all(BV)))

 IF(LRUC) THEN ! Uncompressed data
    
    OPEN(1,file=TRIM(BitmapFile), FORM='unformatted', ACCESS='direct', RECL=NlonB_all(BV)*BP_all(BV))
    
    DO ilat=NorthPix,SouthPix
       
      READ(1,REC=ilat) LonPixDat1
      WLonPixDat=LonPixDat1     

      DO ilon=WestPix, EastPix
        Region(ilon,(NorthPix+SouthPix)-ilat)=WLonPixDat(ilon)    
      END DO
  
    END DO

 ELSE ! Compressed data

    OPEN(1,file=TRIM(BitmapFile), FORM='unformatted', ACCESS='stream')

    ALLOCATE(LonDimsChar(NlatB_all(BV)))
    ALLOCATE(LonDimsInt(NlatB_all(BV)))

 ! Read governing info for the LAI map: line of dimensions in lon
    READ(1) LonDimsChar
    LonDimsInt=TRANSFER(LonDimsChar,1_4,NlatB_all(BV))
    
! Move the "cursor" to the initial position on the LAI map
    iPos=NLatB_all(BV)*4+1
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
                IF(ilon_lai.GT.NLonB_all(BV)) THEN
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

 Region=Region/10.
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
 WRITE(4,'(A4,I8,A8,2F13.7)') 'xdef',(EastPix-WestPix+1), 'linear', West, PixSize_all(BV)/3600._8
 WRITE(4,'(A4,I8,A8,2F13.7)') 'ydef',(SouthPix-NorthPix+1), 'linear', South, PixSize_all(BV)/3600._8
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
