PROGRAM LAI2ML 
! PURPOSE: Converting ECO-SG adjusted LAI to ECO-SG_ML adjusted one
! AUTHOR:  Ekaterina Kourzeneva, 
!          FMI, 07.2025
!  
! MODIFICATIONS:
!
! 08.2025 Ekaterina Kourzeneva, FMI: small updates to process the albedo fiels as well
! 09.2025 Ekaterina Kourzeneva, FMI: corrections to make the list of masked covers to be more general
! 09.2025 Ekaterina Kourzeneva, FMI: more general structure ti implement TH; implementing TH
  
 USE Bitmap_ML_LAI, ONLY : &
  NlonB_in, &                      ! Number of longitude pixels of the specific input bitmap
  NlatB_in, &                      ! Number of latitude pixels of the specific input bitmap
  NlonB_gov_out, &                 ! Number of longitude pixels of ML and the output LAI/ALB/TH and bitmaps
  NlatB_gov_out, &                 ! Number of latitude pixels of the LAI/ALB/TH and ML bitmaps
  NlonB, &                         ! Number of longitude pixels of the specific bitmap
  NlatB, &                         ! Number of latitude pixels of the specific bitmap 
  PixSize_in, &                    ! Pixel size of the specific input bitmap
  PixSize_gov_out, &               ! Pixel size of the LAI/ALB/TH and ML bitmaps in seconds of arc(s)
  BitmapFile_in, &                 ! The input LAI/ALB/TH file name
  BitmapFile_gov, &                ! The ML bitmap file name
  BP_gov, &                        ! Number of bytes per pixel, for the ML map
  WestLim_in,  &                   ! Western boundary of the current input bitmap
  NorthLim_in, &                   ! Northern boundary of the current input bitmap
  EastLim_in,  &                   ! Eastern boundary of the current input bitmap
  SouthLim_in, &                   ! Southern boundary of the current input bitmap
  WestLim_gov_out , &              ! Western boundary of the ML and output LAI/ALB/TH bitmaps
  NorthLim_gov_out, &              ! Northern boundary of the ML and output LAI/ALB/TH and bitmaps
  EastLim_gov_out, &               ! Eastern boundary of the ML and output LAI/ALB/TH bitmaps
  SouthLim_gov_out, &              ! Southern boundary of the ML and output LAI/ALB/TH bitmaps
  WestLim,  &                      ! Western boundary of the current bitmap
  NorthLim, &                      ! Northern boundary of the current bitmap
  EastLim,  &                      ! Eastern boundary of the current bitmap
  SouthLim, &                      ! Southern boundary of the current bitmap 
  DPixSize_in, &                   ! Pixel size decimal of the specific input map
  DPixSize_gov_out, &              ! Pixel size decimal of the ML and output map
  DPixSize, &                      ! Pixel size decimal of the specific bitmap
  NMaskedCovers, &                 ! Number of masked covers
  MaskedCovers, &                  ! List of covers to be masked
  InitField                        ! To choose a field

 IMPLICIT NONE

 INTERFACE
   SUBROUTINE CHAR_TO_REAL(KOUT,HSTR)
     CHARACTER(LEN=*),DIMENSION(:),INTENT(IN)  :: HSTR
     REAL,DIMENSION(SIZE(HSTR)),   INTENT(OUT) :: KOUT
   END SUBROUTINE CHAR_TO_REAL
 END INTERFACE

 INTEGER :: FV    ! Field version: 1 is LAI, 2 is albedo

 CHARACTER(13) :: Bitmap_file_out ! Output file name, uncompressed
 CHARACTER(18) :: Bitmap_file_out_comp ! Output file name, compressed
 LOGICAL, PARAMETER :: LWUC=.FALSE. ! If we want to write uncompressed data.
                                    ! Here, we make a choice between writing compressed and uncompressed data.
                                    ! The default is writing the compressed data.
                                    ! This logical key affects only writing!
                                    ! Compression is performed anyway, although the results are not written.
                                    ! This is not optimal of course and may be changed in future.
                                    ! Note that the output file names differ for the compressed and uncompressed cases:
                                    ! bitmap_*_ml and bitmap_*_ml_comp, see the code below.
 
 INTEGER(1), PARAMETER :: L_UDEF=-90 ! Undefined value for LAI/ALB/TH

 INTEGER :: NorthPix , SouthPix , WestPix , EastPix ! Boundaries of the ML map in pixels of the LAI/ALB/TH map

 CHARACTER(LEN=4), DIMENSION(:), ALLOCATABLE :: LonDimsChar, LonDimsChar_ML
 INTEGER(4), DIMENSION(:), ALLOCATABLE :: LonDimsInt, LonDimsInt_ML
 CHARACTER(LEN=2), DIMENSION(:), ALLOCATABLE :: LonPixDatCompChar, LonPixDatCompChar_ML
 REAL, DIMENSION(:), ALLOCATABLE :: LonPixDatCompReal 
 INTEGER(2), DIMENSION(:), ALLOCATABLE :: LonPixDatCompInt_ML

 REAL, DIMENSION(:), ALLOCATABLE :: WLonPixDat_LAI  ! Working array for LAI/ALB/TH
 INTEGER, DIMENSION(:), ALLOCATABLE :: IWLonPixDat_LAI
 REAL, DIMENSION(:,:), ALLOCATABLE :: WLonPixDat_LAI_ML ! Working array for LAI_ML/ALB_ML
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
 INTEGER :: icov
 INTEGER :: ErCode
 CHARACTER *80 :: CHIN
 INTEGER(1) :: LAI_Found
 LOGICAL :: LatDone, LonDone, LFOUND, L0, L00, LMask

 !--------------------------------------------------------------------------------------------

! Is it LAI or ALB?
 
 IF(iargc().EQ.0) THEN
    WRITE(*,*) 'IS IT LAI OR ALB FIELD? PLEASE SPECIFY!'
    STOP
 END IF
 CALL GETARG(1, CHIN)
 READ(CHIN,'(I4)') FV

 SELECT CASE(FV)
    CASE(1)
       WRITE(*,*) 'Bitmap is LAI_*_c.dir'
       Bitmap_file_out='bitmap_lai_ml'
       Bitmap_file_out_comp='bitmap_lai_ml_comp'
    CASE(2)
       WRITE(*,*) 'Bitmap is AL_*_c.dir'
       Bitmap_file_out='bitmap_alb_ml'
       Bitmap_file_out_comp='bitmap_alb_ml_comp'
    CASE(3)
       WRITE(*,*) 'Bitmap is new_ht_c.dir'
       Bitmap_file_out='bitmap_th_ml'
       Bitmap_file_out_comp='bitmap_th_ml_comp'
    CASE DEFAULT
       WRITE(*,*) 'Which field to process? Numbers between 1 and 3 are possible'
       STOP
 END SELECT

 CALL InitField(FV)

 ! Calculate all the parameters of the LAI/ALB/TH and ML bitmaps
 
 DPixSize_in=PixSize_in/3600._8
 SouthLim_in=NorthLim_in-NlatB_in*DPixSize_in
 EastLim_in=WestLim_in+NlonB_in*DPixSize_in

 DPixSize_gov_out=PixSize_gov_out/3600._8
 SouthLim_gov_out=NorthLim_gov_out-NlatB_gov_out*DPixSize_gov_out
 EastLim_gov_out = WestLim_gov_out+NlonB_gov_out*DPixSize_gov_out

 WRITE(*,*) 'Parameters of maps are (LAI/ALB/TH, ML):'
 WRITE(*,*) 'DPixSize: ', DPixSize_in, DPixSize_gov_out
 WRITE(*,*) 'WestLim: ', WestLim_in, WestLim_gov_out
 WRITE(*,*) 'EastLim: ', EastLim_in, EastLim_gov_out
 WRITE(*,*) 'NorthLim: ', NorthLim_in, NorthLim_gov_out
 WRITE(*,*) 'SouthLim: ', SouthLim_in, SouthLim_gov_out

 ! Calculate boundaries of the ML map in pixel space of the LAI/ALB/TH map

 NlonB=NlonB_in
 NlatB=NlatB_in
 WestLim=WestLim_in
 NorthLim=NorthLim_in
 EastLim=EastLim_in
 SouthLim=SouthLim_in
 DPixSize=DPixSize_in
 CALL Coor2Num_ML_LAI(NorthLim_gov_out, 2, NorthPix, ErCode)
 IF(ErCode.NE.0) STOP
 CALL Coor2Num_ML_LAI(SouthLim_gov_out, 2, SouthPix, ErCode)
 IF(ErCode.NE.0) STOP
 CALL Coor2Num_ML_LAI(WestLim_gov_out,  1, WestPix,  ErCode)
 IF(ErCode.NE.0) STOP
 CALL Coor2Num_ML_LAI(EastLim_gov_out,  1, EastPix,  ErCode)
 IF(ErCode.NE.0) STOP

 WRITE(*,*) 'Boundaries the ML map in pixel space of the LAI/ALB/TH map:'
 WRITE(*,*) 'North_Pix= ', NorthPix, ' SouthPix= ', SouthPix
 WRITE(*,*) 'WestPix= ' , WestPix,  ' EastPix= ' , EastPix

 ! Open files for reading and writing

 OPEN(1,file=TRIM(BitmapFile_in), FORM='unformatted', ACCESS='stream')
 OPEN(2,file=TRIM(BitmapFile_gov), FORM='unformatted', ACCESS='direct', RECL=NlonB_gov_out*BP_gov)
 IF(LWUC) THEN
    OPEN(3,file=TRIM(Bitmap_file_out), FORM='unformatted', ACCESS='direct', RECL=NlonB_gov_out*BP_gov)
 ELSE
    OPEN(4,file=TRIM(Bitmap_file_out_comp), FORM='unformatted', ACCESS='stream')   
 END IF

 ALLOCATE(LonDimsChar(NlatB_in))
 ALLOCATE(LonDimsInt(NlatB_in))
 ALLOCATE(WLonPixDat_LAI(NlonB_in))
 ALLOCATE(IWLonPixDat_LAI(NlonB_in))
 ALLOCATE(LonDimsChar_ML(NlatB_gov_out)) 
 ALLOCATE(LonDimsInt_ML(NlatB_gov_out))

 ! Read governing info for the LAI/ALB/TH map: line of dimensions in lon

 READ(1) LonDimsChar
 LonDimsInt=TRANSFER(LonDimsChar,1_4,NlatB_in) 

! Move the "cursor" to the initial position on the LAI/ALB/TH map
 iPos=NLatB_in*4+1
 DO ilat_lai=1,NorthPix-1
    iPos=iPos+LonDimsInt(ilat_lai)*2
 END DO

! Move the "cursor" to the initial position of the LAI_ML/ALB_ML map,
! leaving space for the governing info
 iiPos=NLatB_gov_out*4+1
 
 ilat_ml=1
 irec_lai_ml=1
 irec_ml=1
 ilat_lai_ml=1
 LatDone=.FALSE.
 
 DO ilat_lai=NorthPix,SouthPix

    klat_ml=0
    
    LatPixN_LAI=NorthLim_in-(ilat_lai-1)*DPixSize_in ! Calculate coordinates of the boundaries of the LAI/ALB/TH pixel
    LatPixS_LAI=LatPixN_LAI-DPixSize_in

    DO
       LatPix_ML=NorthLim_gov_out-(ilat_ml-1)*DPixSize_gov_out
       IF(LatPix_ML.LE.LatPixN_LAI.AND.LatPix_ML.GT.LatPixS_LAI) THEN
          klat_ml=klat_ml+1
          ilat_ml=ilat_ml+1
          IF(ilat_ml.GT.NLatB_gov_out) THEN
             LatDone=.TRUE.
             EXIT
          END IF
       ELSE
          EXIT
       END IF
    END DO

    ALLOCATE(WLonPixDat_LAI_ML(NLonB_gov_out,klat_ml))
    ALLOCATE(IWLonPixDat_LAI_ML(NLonB_gov_out,klat_ml))
    ALLOCATE(IWLonPixDat_LAI_ML_out(NLonB_gov_out,klat_ml))
    ALLOCATE(IWLonPixDat_ML(NLonB_gov_out,klat_ml))
        
! Read the LAI/ALB/TH data    
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
             IF(ilon_lai.GT.NLonB_in) THEN
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

    write(*,*)  ilat_lai, MAXVAL(WLonPixDat_LAI), MINVAL(WLonPixDat_LAI), 'LAI/ALB/TH line read ... '

    ! Interpolate LAI/ALB/TH to the finer resolution with the nearest neighbour metod (just repeating values)
    
    ilon_ml=1
    LonDone=.FALSE.

    DO ilon_lai=WestPix, EastPix
       
      LonPixW_LAI=WestLim_in+(ilon_lai-1)*DPixSize_in ! Calculate coordinates of the boundaries of the LAI/ALB/TH pixel
      LonPixE_LAI=LonPixW_LAI+DPixSize_in
      DO
         LonPix_ML=WestLim_gov_out+(ilon_ml-1)*DPixSize_gov_out
         IF(LonPix_ML.GE.LonPixW_LAI.AND.LonPix_ML.LT.LonPixE_LAI) THEN
           WLonPixDat_LAI_ML(ilon_ml,:) = WLonPixDat_LAI(ilon_lai)
           ilon_ml=ilon_ml+1
           IF(ilon_ml.GT.NLonB_gov_out) THEN
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

    ! Adjust LAI/ALB/TH to the ML Covers
    DO ilat_w=1,klat_ml
       DO ilon_w=1,NLonB_gov_out

          LMask=.FALSE.
          DO icov=1,NMaskedCovers
             IF (IWLonPixDat_ML(ilon_w,ilat_w).EQ.MaskedCovers(icov)) THEN
                IWLonPixDat_LAI_ML(ilon_w,ilat_w) = 0
                LMask=.TRUE.
                EXIT
             END IF
          END DO
          IF (.NOT.LMask) THEN
             IF (IWLonPixDat_LAI_ML(ilon_w,ilat_w).EQ.0) THEN
                 IWLonPixDat_LAI_ML(ilon_w,ilat_w) = L_UDEF
             END IF
          END IF
         
       END DO
    END DO

    ! Fast extrapolation of LAI/ALB/TH to the points where it was water, but now it is not
    ! Extrapolation algorithm may be improved in future, although works satisfactory for this task
    DO ilat_w=1,klat_ml
       LFOUND=.FALSE.
       LAI_Found=L_UDEF
       DO ilon_w=1,NLonB_gov_out
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
       DO ilon_w=NLonB_gov_out,1,-1
          IF((IWLonPixDat_LAI_ML(ilon_w,ilat_w).NE.0).AND.(IWLonPixDat_LAI_ML(ilon_w,ilat_w).NE.L_UDEF)) THEN
             LFOUND=.TRUE.
             LAI_Found=IWLonPixDat_LAI_ML(ilon_w,ilat_w)
          END IF
          IF((IWLonPixDat_LAI_ML(ilon_w,ilat_w).EQ.L_UDEF).AND.LFOUND) THEN
             IWLonPixDat_LAI_ML(ilon_w,ilat_w)=LAI_Found
          END IF
       END DO
    END DO

    ! Combining the new LAI/ALB/TH with the new covers (from ECO_ML) 
    DO ilat_w=1,klat_ml
       DO ilon_w=1,NLonB_gov_out
          IF(IWLonPixDat_LAI_ML(ilon_w,ilat_w).EQ.0) THEN
             IWLonPixDat_LAI_ML_out(ilon_w,ilat_w) = 0
          ELSE
             IWLonPixDat_LAI_ML_out(ilon_w,ilat_w) = 100*IWLonPixDat_ML(ilon_w,ilat_w)+IWLonPixDat_LAI_ML(ilon_w,ilat_w)
          END IF
       END DO
    END DO

    ! Compressing the new LAI/ALB/TH collocated with ML Covers
    
    DO ilat_w=1,klat_ml

       ! 1 . Calculating the size of the new compressed lines

      ilon_lai_ml=0
      L0=.TRUE.
      L00=.FALSE.
      klon_0=0
      DO ilon_w=1,NLonB_gov_out
         IF(IWLonPixDat_LAI_ML(ilon_w,ilat_w).EQ.0) THEN
            klon_0=klon_0+1
            IF(L0) THEN
               ilon_lai_ml=ilon_lai_ml+1
               L0=.FALSE.
            END IF
            IF(klon_0.LT.28766) THEN ! 28766=32767-4000-1
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
      DO ilon_w=1,NLonB_gov_out
         IF(IWLonPixDat_LAI_ML(ilon_w,ilat_w).EQ.0) THEN
            klon_0=klon_0+1
            IF(L0) THEN
               ilon_lai_ml=ilon_lai_ml+1
               L0=.FALSE.
            END IF
            IF(klon_0.LT.28766) THEN ! 28766=32767-4000-1
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
      IF(IWLonPixDat_LAI_ML(NLonB_gov_out,ilat_w).EQ.0) THEN ! "Boundary condition"
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
      
      IF(ilat_lai_ml.GT.NLatB_gov_out) THEN
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
 LonDimsChar_ML=TRANSFER(LonDimsInt_ML,'aaaa',NlatB_gov_out)
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
 DEALLOCATE(MaskedCovers)

END PROGRAM LAI2ML
!-------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------
