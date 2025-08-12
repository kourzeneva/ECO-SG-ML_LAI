PROGRAM LAI2ML 
! PURPOSE: Converting ECO-SG adjusted LAI to ECO-SG_ML adjusted one
! AUTHOR:  Ekaterina Kourzeneva, 
!          FMI, 07.2025

 USE Bitmap_ML_LAI, ONLY : &
  NlonB_all, &                     ! Number of longitude pixels of the bitmaps
  NlatB_all, &                     ! Number of latitude pixels of the bitmaps
  PixSize_all, &                   ! Pixel size of the LAI and ML bitmaps in seconds of arc(s)
  BitmapFile_all, &                ! The initial bitmap file name
  BP_all, &                        ! Number of bytes per pixel in the bitmaps
  WestLim_all, &                   ! Western boundaries of the LAI and ML bitmaps
  NorthLim_all, &                  ! Northern boundaries of the LAI and ML bitmaps
  EastLim_all, &                   ! Eastern boundaries of the LAI and ML bitmaps
  SouthLim_all, &                  ! Southern boundaries of the LAI and ML bitmaps
  DPixSize_all, &                  ! Pixel size decimal of the LAI and ML bitmaps
  LSea_all,  &                     ! Legends for sea, lake, river for different maps
  LLake_all, &
  LRiver_all 

 IMPLICIT NONE

 INTERFACE
   SUBROUTINE CHAR_TO_REAL(KOUT,HSTR)
     CHARACTER(LEN=*),DIMENSION(:),INTENT(IN)  :: HSTR
     REAL,DIMENSION(SIZE(HSTR)),   INTENT(OUT) :: KOUT
   END SUBROUTINE CHAR_TO_REAL
 END INTERFACE

 CHARACTER(13), PARAMETER :: Bitmap_file_out='bitmap_lai_ml' ! Output file name, uncompressed
 CHARACTER(18), PARAMETER :: Bitmap_file_out_comp='bitmap_lai_ml_comp' ! Output file name, compressed
 LOGICAL, PARAMETER :: LWUC=.FALSE. ! If we want to write uncompressed data.
                                    ! Here, we make a choice between writing compressed and uncompressed data.
                                    ! The default is writing the compressed data.
                                    ! This logical key affects only writing!
                                    ! Compression is performed anyway, although the results are not written.
                                    ! This is not optimal of course and may be changed in future.
                                    ! Note that the output file names differ for the compressed and uncompressed cases:
                                    ! bitmap_lai_ml and bitmap_lai_ml_comp, see above.
 
 INTEGER(1), PARAMETER :: L_UDEF=90 ! Undefined value for LAI

 INTEGER :: NorthPix , SouthPix , WestPix , EastPix ! Boundaries of the ML map in pixels of the LAI map

 CHARACTER(LEN=4), DIMENSION(:), ALLOCATABLE :: LonDimsChar, LonDimsChar_ML
 INTEGER(4), DIMENSION(:), ALLOCATABLE :: LonDimsInt, LonDimsInt_ML
 CHARACTER(LEN=2), DIMENSION(:), ALLOCATABLE :: LonPixDatCompChar, LonPixDatCompChar_ML
 REAL, DIMENSION(:), ALLOCATABLE :: LonPixDatCompReal 
 INTEGER(2), DIMENSION(:), ALLOCATABLE :: LonPixDatCompInt_ML

 REAL, DIMENSION(:), ALLOCATABLE :: WLonPixDat_LAI  ! Working array for LAI
 INTEGER, DIMENSION(:), ALLOCATABLE :: IWLonPixDat_LAI
 REAL, DIMENSION(:,:), ALLOCATABLE :: WLonPixDat_LAI_ML ! Working array for LAI_ML
 INTEGER(1), DIMENSION(:,:), ALLOCATABLE :: IWLonPixDat_LAI_ML
 INTEGER(2), DIMENSION(:,:), ALLOCATABLE :: IWLonPixDat_LAI_ML_out

 INTEGER(1), DIMENSION(:,:), ALLOCATABLE :: IWLonPixDat_ML

 REAL(8) :: LatPixN_LAI, LatPixS_LAI, LatPix_ML, LonPixW_LAI, LonPixE_LAI, LonPix_ML

 INTEGER :: ilat_lai, ilon_lai, ilon_s, ilon_0, ilat_ml, ilon_ml
 INTEGER :: irec_ml, irec_lai_ml
 INTEGER :: ilat_lai_ml, ilon_lai_ml
 INTEGER :: ilat_w, ilon_w
 INTEGER :: klon_0, klat_ml
 INTEGER(8) :: iPos, iiPos
 INTEGER :: ErCode
 INTEGER(1) :: LAI_Found
 LOGICAL :: LatDone, LonDone, LFound, L0, L00

 !--------------------------------------------------------------------------------------------

 ! Calculate all the parameters of the LAI and ML bitmaps

 DPixSize_all=PixSize_all/3600._8
 EastLim_all=WestLim_all+NlonB_all*DPixSize_all
 SouthLim_all=NorthLim_all-NlatB_all*DPixSize_all

 WRITE(*,*) 'Parameters of maps are (LAI, ML):'
 WRITE(*,*) 'DPixSize: ', DPixSize_all(1), DPixSize_all(2)
 WRITE(*,*) 'WestLim: ', WestLim_all(1), WestLim_all(2)
 WRITE(*,*) 'EastLim: ', EastLim_all(1), EastLim_all(2)
 WRITE(*,*) 'NorthLim: ', NorthLim_all(1), NorthLim_all(2)
 WRITE(*,*) 'SouthLim: ', SouthLim_all(1), SouthLim_all(2)

 ! Calculate boundaries of the ML map in pixel space of the LAI map

 CALL Coor2Num_ML_LAI(1, NorthLim_all(2), 2, NorthPix, ErCode)
 IF(ErCode.NE.0) STOP
 CALL Coor2Num_ML_LAI(1, SouthLim_all(2), 2, SouthPix, ErCode)
 IF(ErCode.NE.0) STOP
 CALL Coor2Num_ML_LAI(1, WestLim_all(2),  1, WestPix,  ErCode)
 IF(ErCode.NE.0) STOP
 CALL Coor2Num_ML_LAI(1, EastLim_all(2),  1, EastPix,  ErCode)
 IF(ErCode.NE.0) STOP

 WRITE(*,*) 'Boundaries the ML map in pixel space of the LAI map:'
 WRITE(*,*) 'North_Pix= ', NorthPix, ' SouthPix= ', SouthPix
 WRITE(*,*) 'WestPix= ' , WestPix,  ' EastPix= ' , EastPix

 ! Open files for reading and writing

 OPEN(1,file=TRIM(BitmapFile_all(1)), FORM='unformatted', ACCESS='stream')
 OPEN(2,file=TRIM(BitmapFile_all(2)), FORM='unformatted', ACCESS='direct', RECL=NlonB_all(2)*BP_all(2))
 IF(LWUC) THEN
    OPEN(3,file=TRIM(Bitmap_file_out), FORM='unformatted', ACCESS='direct', RECL=NlonB_all(2)*BP_all(2))
 ELSE
    OPEN(4,file=TRIM(Bitmap_file_out_comp), FORM='unformatted', ACCESS='stream')   
 END IF

 ALLOCATE(LonDimsChar(NlatB_all(1)))
 ALLOCATE(LonDimsInt(NlatB_all(1)))
 ALLOCATE(WLonPixDat_LAI(NlonB_all(1)))
 ALLOCATE(IWLonPixDat_LAI(NlonB_all(1)))
 ALLOCATE(LonDimsChar_ML(NlatB_all(2))) 
 ALLOCATE(LonDimsInt_ML(NlatB_all(2)))

 ! Read governing info for the LAI map: line of dimensions in lon

 READ(1) LonDimsChar
 LonDimsInt=TRANSFER(LonDimsChar,1_4,NlatB_all(1)) 

! Move the "cursor" to the initial position on the LAI map
 iPos=NLatB_all(1)*4+1
 DO ilat_lai=1,NorthPix-1
    iPos=iPos+LonDimsInt(ilat_lai)*2
 END DO

! Move the "cursor" to the initial position of the LAI_ML map,
! leaving space for the governing info
 iiPos=NLatB_all(2)*4+1
 
 ilat_ml=1
 irec_lai_ml=1
 irec_ml=1
 ilat_lai_ml=1
 LatDone=.FALSE.
 
 DO ilat_lai=NorthPix,SouthPix

    klat_ml=0
    
    LatPixN_LAI=NorthLim_all(1)-(ilat_lai-1)*DPixSize_all(1) ! Calculate coordinates of the boundaries of the LAI pixel
    LatPixS_LAI=LatPixN_LAI-DPixSize_all(1)

    DO
       LatPix_ML=NorthLim_all(2)-(ilat_ml-1)*DPixSize_all(2)
       IF(LatPix_ML.LE.LatPixN_LAI.AND.LatPix_ML.GT.LatPixS_LAI) THEN
          klat_ml=klat_ml+1
          ilat_ml=ilat_ml+1
          IF(ilat_ml.GT.NLatB_all(2)) THEN
             LatDone=.TRUE.
             EXIT
          END IF
       ELSE
          EXIT
       END IF
    END DO

    ALLOCATE(WLonPixDat_LAI_ML(NLonB_all(2),klat_ml))
    ALLOCATE(IWLonPixDat_LAI_ML(NLonB_all(2),klat_ml))
    ALLOCATE(IWLonPixDat_LAI_ML_out(NLonB_all(2),klat_ml))
    ALLOCATE(IWLonPixDat_ML(NLonB_all(2),klat_ml))
        
! Read the LAI data    
    ALLOCATE(LonPixDatCompChar(LonDimsInt(ilat_lai)))
    ALLOCATE(LonPixDatCompReal(LonDimsInt(ilat_lai)))
    READ(1,pos=iPos) LonPixDatCompChar
    DO ilon_s=1,LonDimsInt(ilat_lai)
       CALL CHAR_TO_REAL(LonPixDatCompReal(ilon_s:ilon_s),LonPixDatCompChar(ilon_s:ilon_s))
       IF(LonPixDatCompReal(ilon_s).LT.0.) THEN
          LonPixDatCompReal(ilon_s)= NINT(32768*2.+LonPixDatCompReal(ilon_s))
       END IF
    END DO
    ilon_lai=1
    DO ilon_s=1,LonDimsInt(ilat_lai)
       IF(LonPixDatCompReal(ilon_s).LT.4000.) THEN
          WLonPixDat_LAI(ilon_lai)=LonPixDatCompReal(ilon_s)
          IWLonPixDat_LAI(ilon_lai)=FLOOR(WLonPixDat_LAI(ilon_lai)/100.)
          WLonPixDat_LAI(ilon_lai)=MOD(WLonPixDat_LAI(ilon_lai),100.)
          ilon_lai=ilon_lai+1
       ELSE
          klon_0=NINT(LonPixDatCompReal(ilon_s)-4000.)
          DO ilon_0=1,klon_0
             IF(ilon_lai.GT.NLonB_all(1)) THEN
                write(*,*) 'Too many values:', ilon_lai, ilon_s, klon_0, ilat_lai  
                STOP
             END IF
             WLonPixDat_LAI(ilon_lai)=0.
             IWLonPixDat_LAI(ilon_lai)=0
             ilon_lai=ilon_lai+1
          END DO
       END IF
    END DO
    
    iPos=iPos+LonDimsInt(ilat_lai)*2

    DEALLOCATE(LonPixDatCompChar)
    DEALLOCATE(LonPixDatCompReal)

    write(*,*)  ilat_lai, MAXVAL(WLonPixDat_LAI), MINVAL(WLonPixDat_LAI), 'LAI line read ... '

    ! Interpolate LAI to the finer resolution with the nearest neighbour metod (just repeating values)
    
    ilon_ml=1
    LonDone=.FALSE.

    DO ilon_lai=WestPix, EastPix
       
      LonPixW_LAI=WestLim_all(1)+(ilon_lai-1)*DPixSize_all(1) ! Calculate coordinates of the boundaries of the LAI pixel
      LonPixE_LAI=LonPixW_LAI+DPixSize_all(1)
      DO
         LonPix_ML=WestLim_all(2)+(ilon_ml-1)*DPixSize_all(2)
         IF(LonPix_ML.GE.LonPixW_LAI.AND.LonPix_ML.LT.LonPixE_LAI) THEN
           WLonPixDat_LAI_ML(ilon_ml,:) = WLonPixDat_LAI(ilon_lai)
           ilon_ml=ilon_ml+1
           IF(ilon_ml.GT.NLonB_all(2)) THEN
             LonDone=.TRUE.
             EXIT
           END IF
         ELSE
           EXIT
         END IF
      END DO
      IF (LonDone) THEN
         EXIT
      END IF
    END DO
   
    IWLonPixDat_LAI_ML=WLonPixDat_LAI_ML   

    !Read the ECO-ML covers
    DO ilat_w=1,klat_ml
      READ(2,REC=irec_ml) IWLonPixDat_ML(:,ilat_w) 
      irec_ml=irec_ml+1
    END DO

    ! Adjust LAI to the ML Covers
    DO ilat_w=1,klat_ml
       DO ilon_w=1,NLonB_all(2)
          IF ((IWLonPixDat_ML(ilon_w,ilat_w).EQ.LSea_all(2)).OR. &
               (IWLonPixDat_ML(ilon_w,ilat_w).EQ.LLake_all(2)).OR. &
               (IWLonPixDat_ML(ilon_w,ilat_w).EQ.LRiver_all(2))) THEN
             IWLonPixDat_LAI_ML(ilon_w,ilat_w) = 0
          ELSE
             IF ((IWLonPixDat_LAI_ML(ilon_w,ilat_w).EQ.LSea_all(1)).OR. &
                  (IWLonPixDat_LAI_ML(ilon_w,ilat_w).EQ.LLake_all(1)).OR. &
                  (IWLonPixDat_LAI_ML(ilon_w,ilat_w).EQ.LRiver_all(1))) THEN
                IWLonPixDat_LAI_ML(ilon_w,ilat_w) = L_UDEF
             END IF
          END IF
       END DO
    END DO

    ! Fast extrapolation of LAI to the points where it was water, but now it is not
    ! Extrapolation algorithm may be improved in future, although works satisfactory for this task
    DO ilat_w=1,klat_ml
       LFOUND=.FALSE.
       LAI_Found=L_UDEF
       DO ilon_w=1,NLonB_all(2)
          IF((IWLonPixDat_LAI_ML(ilon_w,ilat_w).NE.0).AND.(IWLonPixDat_LAI_ML(ilon_w,ilat_w).NE.L_UDEF)) THEN
             LFOUND=.TRUE.
             LAI_Found=IWLonPixDat_LAI_ML(ilon_w,ilat_w)
          END IF
          IF((IWLonPixDat_LAI_ML(ilon_w,ilat_w).EQ.L_UDEF).AND.LFOUND) THEN
             IWLonPixDat_LAI_ML(ilon_w,ilat_w)=LAI_Found
          END IF
       END DO
       LFOUND=.FALSE.
       LAI_Found=L_UDEF
       DO ilon_w=NLonB_all(2),1,-1
          IF((IWLonPixDat_LAI_ML(ilon_w,ilat_w).NE.0).AND.(IWLonPixDat_LAI_ML(ilon_w,ilat_w).NE.L_UDEF)) THEN
             LFOUND=.TRUE.
             LAI_Found=IWLonPixDat_LAI_ML(ilon_w,ilat_w)
          END IF
          IF((IWLonPixDat_LAI_ML(ilon_w,ilat_w).EQ.L_UDEF).AND.LFOUND) THEN
             IWLonPixDat_LAI_ML(ilon_w,ilat_w)=LAI_Found
          END IF
       END DO
    END DO

    ! Combining the new LAI with the new covers (from ECO_ML) 
    DO ilat_w=1,klat_ml
       DO ilon_w=1,NLonB_all(2)
          IF(IWLonPixDat_LAI_ML(ilon_w,ilat_w).EQ.0) THEN
             IWLonPixDat_LAI_ML_out(ilon_w,ilat_w) = 0
          ELSE
             IWLonPixDat_LAI_ML_out(ilon_w,ilat_w) = 100*IWLonPixDat_ML(ilon_w,ilat_w)+IWLonPixDat_LAI_ML(ilon_w,ilat_w)
          END IF
       END DO
    END DO

    ! Compressing the new LAI
    
    DO ilat_w=1,klat_ml

       ! 1 . Calculating the size of the new compressed lines

      ilon_lai_ml=0
      L0=.TRUE.
      L00=.FALSE.
      klon_0=0
      DO ilon_w=1,NLonB_all(2)
         IF(IWLonPixDat_LAI_ML(ilon_w,ilat_w).EQ.0) THEN
            klon_0=klon_0+1
            IF(L0) THEN
               ilon_lai_ml=ilon_lai_ml+1
               L0=.FALSE.
            END IF
            IF(klon_0.LT.28766) THEN ! 28767=32767-4000-1
               L00=.TRUE.
            ELSE
               ilon_lai_ml=ilon_lai_ml+1
               klon_0=0
               L00=.FALSE.               
            END IF
         ELSE
            IF(L00) THEN
               klon_0=0
               L00=.FALSE.
            END IF
            ilon_lai_ml=ilon_lai_ml+1
            L0=.TRUE.
         END IF
      END DO
      
      LonDimsInt_ML(ilat_lai_ml)=ilon_lai_ml

      
     ! 2. Compressing data

      ALLOCATE(LonPixDatCompInt_ML(LonDimsInt_ML(ilat_lai_ml)))
      ALLOCATE(LonPixDatCompChar_ML(LonDimsInt_ML(ilat_lai_ml)))

      ilon_lai_ml=1
      L0=.TRUE.
      L00=.FALSE.
      klon_0=0
      DO ilon_w=1,NLonB_all(2)
         IF(IWLonPixDat_LAI_ML(ilon_w,ilat_w).EQ.0) THEN
            klon_0=klon_0+1
            IF(L0) THEN
               ilon_lai_ml=ilon_lai_ml+1
               L0=.FALSE.
            END IF
            IF(klon_0.LT.28766) THEN ! 28767=32767-4000-1
               L00=.TRUE.
            ELSE
               LonPixDatCompInt_ML(ilon_lai_ml-1) = klon_0+4000 
               ilon_lai_ml=ilon_lai_ml+1
               klon_0=0
               L00=.FALSE.               
            END IF
         ELSE
            IF(L00) THEN
               LonPixDatCompInt_ML(ilon_lai_ml-1) = klon_0+4000
               klon_0=0
               L00=.FALSE.
            END IF
            LonPixDatCompInt_ML(ilon_lai_ml) = IWLonPixDat_LAI_ML_out(ilon_w,ilat_w)
            ilon_lai_ml=ilon_lai_ml+1
            L0=.TRUE.
         END IF
      END DO
      IF(IWLonPixDat_LAI_ML(NLonB_all(2),ilat_w).EQ.0) THEN ! "Boundary condition"
         LonPixDatCompInt_ML(LonDimsInt_ML(ilat_lai_ml)) = klon_0+4000
      END IF

      IF(.NOT.LWUC) THEN
         write(*,*) ilat_lai_ml, LonDimsInt_ML(ilat_lai_ml), MINVAL(LonPixDatCompInt_ML), MAXVAL(LonPixDatCompInt_ML)
      END IF
      
    ! Writing data
      LonPixDatCompChar_ML=TRANSFER(LonPixDatCompInt_ML,'aa',LonDimsInt_ML(ilat_lai_ml))
      IF(.NOT.LWUC) THEN
         WRITE(4,pos=iiPos) LonPixDatCompChar_ML
      END IF
            
      DEALLOCATE(LonPixDatCompInt_ML)
      DEALLOCATE(LonPixDatCompChar_ML)
      
      iiPos=iiPos+LonDimsInt_ML(ilat_lai_ml)*2
      ilat_lai_ml=ilat_lai_ml+1
      
      IF(ilat_lai_ml.GT.NLatB_all(2)) THEN
         LatDone=.TRUE.
         EXIT
      END IF
      
    END DO

    DO ilat_w=1,klat_ml
       IF(LWUC) THEN
          WRITE(3,REC=irec_lai_ml) IWLonPixDat_LAI_ML(:,ilat_w)
       END IF
       irec_lai_ml=irec_lai_ml+1
    END DO
        
    DEALLOCATE(WLonPixDat_LAI_ML)
    DEALLOCATE(IWLonPixDat_LAI_ML)
    DEALLOCATE(IWLonPixDat_LAI_ML_out)
    DEALLOCATE(IWLonPixDat_ML)

    IF (LatDone) THEN
      EXIT
    END IF
    
 END DO

 ! Writing governing info
 LonDimsChar_ML=TRANSFER(LonDimsInt_ML,'aaaa',NlatB_all(2))
 IF(.NOT.LWUC) THEN
    WRITE(4,pos=1) LonDimsChar_ML
 END IF
 
 IF(LWUC) THEN
    write(*,*) irec_lai_ml-1 , 'Lines are written'
 ELSE
    write(*,*) ilat_lai_ml-1 , 'Lines are written'   
 END IF
! 
 CLOSE(1)
 CLOSE(2)
 IF(LWUC) THEN
    CLOSE(3)
 ELSE
    CLOSE(4)
 END IF
!
 DEALLOCATE(LonDimsChar)
 DEALLOCATE(LonDimsInt)
 DEALLOCATE(WLonPixDat_LAI)
 DEALLOCATE(IWLonPixDat_LAI)
 DEALLOCATE(LonDimsChar_ML)
 DEALLOCATE(LonDimsInt_ML)

END PROGRAM LAI2ML
!-------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------
