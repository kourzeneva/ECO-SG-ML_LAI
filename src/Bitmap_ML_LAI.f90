MODULE Bitmap_ML_LAI
  
! PURPOSE: Set the parameters of the bitmaps of ECO_SG_ML and LAI
! AUTHOR:  Ekaterina Kourzeneva, 
!          FMI, 07.2025
!  
! MODIFICATIONS:
!
! 08.2025 Ekaterina Kourzeneva, FMI: small updates to process the albedo fiels as well
! 09.2025 Ekaterina Kourzeneva, FMI: corrections to make the list of masked covers to be more general 
!  
  IMPLICIT NONE
  
 INTEGER, PARAMETER, DIMENSION(3) :: NlonB_in_all = (/ 129600, 129600, 129600 /) , & ! Number of longitude pixels of the input LAI/ALB/TH bitmaps
                                     NlatB_in_all = (/ 50400,  50400, 64800 /)      ! Number of latitude pixels of the input LAI/ALB/TH bitmaps

 INTEGER, PARAMETER :: NlonB_gov_out = 137600 , & ! Number of longitude pixels of ML and the output LAI/ALB/TH and bitmaps
                       NlatB_gov_out = 96600      ! Number of latitude pixels of the LAI/ALB/TH and ML bitmaps
 
 REAL(8), PARAMETER, DIMENSION(3) :: PixSize_in_all = (/ 10.0_8, 10.0_8, 10.0_8 /)  ! Pixel size of the input LAI/ALB/TH and bitmaps
                                                                                    ! in seconds of arc(s)
 REAL(8), PARAMETER :: PixSize_gov_out = 1.94036112_8  ! Pixel size of the output LAI/ALB/TH and ML bitmaps in seconds of arc(s)

 CHARACTER(17), PARAMETER :: BitmapFile_in = 'bitmap_LAI_ALB_TH' ! The input LAI/ALB/TH file name
 CHARACTER(9), PARAMETER :: BitmapFile_gov = 'bitmap_ML'  ! The ML bitmap file name

 INTEGER, PARAMETER, DIMENSION(3) :: NMaskedCovers_all = (/ 6, 3, 23 /)  ! Number of masked covers for all fields.
                                                                         ! Lists of covers to be masked are defined in InitMask
 
 REAL, PARAMETER, DIMENSION(3) :: Factor_out_all = (/ 10., 100., 1. /) ! Multiplication factor for LAI/ALB/TH

 INTEGER, PARAMETER :: BP_gov = 1 ! Number of bytes per pixel, for the ML map (and output LAI/ALB/TH maps, for uncompressed output)
  
 INTEGER :: NMaskedCovers ! Number of masked covers
 INTEGER, DIMENSION(:), ALLOCATABLE :: MaskedCovers ! List of covers to be masked, for the chosen field

 REAL(KIND=8), DIMENSION(3), PARAMETER :: WestLim_in_all = (/ -180.0_8, -180.0_8, -180.0_8 /), & ! Western boundaries of the LAI/ALB/TH bitmaps
                                          NorthLim_in_all = (/ 80.0_8, 80.0_8, 90.0_8 /)         ! Northern boundaries of the LAI/ALB/TH bitmaps
 REAL(KIND=8), PARAMETER :: WestLim_gov_out = -31.9997305054148_8 , & ! Western boundary of the ML and output LAI/ALB/TH bitmaps
                            NorthLim_gov_out = 72.0660843734148_8     ! Northern boundary of the ML and output LAI/ALB/TH and bitmaps 
 
 REAL(KIND=8)  :: WestLim_in,  & ! Western boundary of the current input bitmap
                  NorthLim_in, & ! Northern boundary of the current input bitmap
                  EastLim_in,  & ! Eastern boundary of the current input bitmap
                  SouthLim_in    ! Southern boundary of the current input bitmap

 REAL(KIND=8)  :: WestLim,  & ! Western boundary of the current bitmap
                  NorthLim, & ! Northern boundary of the current bitmap
                  EastLim,  & ! Eastern boundary of the current bitmap
                  SouthLim    ! Southern boundary of the current bitmap 

 REAL(KIND=8) :: EastLim_gov_out, & ! Eastern boundary of the ML and output LAI/ALB bitmaps
                 SouthLim_gov_out   ! Southern boundary of the ML and output LAI/ALB bitmaps

 REAL(KIND=8) :: DPixSize_in      ! Pixel size decimal of the specific input map
 REAL(KIND=8) :: DPixSize_gov_out ! Pixel size decimal of the ML and output map
 REAL(KIND=8) :: DPixSize         ! Pixel size decimal of the specific map

 INTEGER :: NlonB_in, & ! Number of longitude pixels of the specific input bitmap
            NlatB_in    ! Number of latitude pixels of the specific input bitmap

 INTEGER :: NlonB, & ! Number of longitude pixels of the specific bitmap
            NlatB    ! Number of latitude pixels of the specific bitmap
 
 REAL(8) :: PixSize_in ! Pixel size of the specific input bitmap
 
CONTAINS

 SUBROUTINE InitField(FV) ! To choose a field and a corresponding map
   IMPLICIT NONE
   TYPE MaskedCoversType
      INTEGER, DIMENSION(:), ALLOCATABLE :: MaskedCovers_field ! Lists of covers to be masked, for all fields
   END TYPE MaskedCoversType
   INTEGER, INTENT(IN) :: FV ! Bitmap version
   TYPE(MaskedCoversType), DIMENSION(3) :: MaskedCovers_all
   INTEGER :: ifield

   ! Covers to be masked for different fields are defined here
   DO ifield=1,3
      ALLOCATE(MaskedCovers_all(ifield)%MaskedCovers_field(NMaskedCovers_all(ifield)))
   END DO
   MaskedCovers_all(1)%MaskedCovers_field = (/1, 2, 3, 4, 5, 6/) ! For LAI: sea, lake, river, bare land, bare rock, permanent snow
   MaskedCovers_all(2)%MaskedCovers_field = (/1, 2, 3/) ! For ALB: sea, lake, river
   MaskedCovers_all(3)%MaskedCovers_field = (/1, 2, 3, 4, 5, 6, 16, 17, 18, 19, 20, 21, 23, &
                                              24, 25, 26, 27, 28, 29, 30, 31, 32, 33/) ! For TH: all except forest types

   ! Choice of the particular field
   NMaskedCovers = NMaskedCovers_all(FV)
   ALLOCATE(MaskedCovers(NMaskedCovers))
   MaskedCovers=MaskedCovers_all(FV)%MaskedCovers_field

   DO ifield=1,3
      DEALLOCATE(MaskedCovers_all(ifield)%MaskedCovers_field)
   END DO

   PixSize_in=PixSize_in_all(FV)
   NlonB_in=NlonB_in_all(FV)
   NlatB_in=NlatB_in_all(FV)
   NorthLim_in = NorthLim_in_all(FV)
   WestLim_in = WestLim_in_all(FV)

 END SUBROUTINE InitField
 
END MODULE Bitmap_ML_LAI
