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

 INTEGER, PARAMETER, DIMENSION(2) :: NlonB_all = (/ 129600, 137600 /) , & ! Number of longitude pixels of the LAI/ALB and ML bitmaps
                                     NlatB_all = (/ 50400,  96600 /)      ! Number of latitude pixels of the LAI/ALB and ML bitmaps
 REAL(8), PARAMETER, DIMENSION(2) :: PixSize_all = (/ 10.0_8, 1.94036112_8 /)      ! Pixel size of the LAI/ALB and ML bitmaps in seconds of arc(s)

 CHARACTER(14), PARAMETER, DIMENSION(2) :: BitmapFile_all = (/'bitmap_LAI_ALB','bitmap_ML     '/) ! The LAI/ALB and ML bitmap file names

 INTEGER, PARAMETER, DIMENSION(2) :: NMaskedCovers_all = (/6, 3/)  ! Number of masked covers for all fields. Lists of covers to be masked are defined in InitMask
  
 INTEGER :: NMaskedCovers ! Number of masked covers
 INTEGER, DIMENSION(:), ALLOCATABLE :: MaskedCovers ! List of covers to be masked, for the chosen field
 
 INTEGER, PARAMETER, DIMENSION(2) :: BP_all = (/ 2, 1 /) ! Number of bytes per pixel, for different maps

 REAL(KIND=8), DIMENSION(2), PARAMETER :: WestLim_all = (/ -180.0_8, -31.9997305054148_8 /), & ! Western boundaries of the LAI/ALB and ML bitmaps
                                          NorthLim_all = (/ 80.0_8, 72.0660843734148_8 /)      ! Northern boundaries of the LAI/ALB and ML bitmaps
      
 REAL(KIND=8), DIMENSION(2) :: EastLim_all, & ! Eastern boundaries of the LAI/ALB and ML bitmaps
                               SouthLim_all   ! Southern boundaries of the LAI/ALB and ML bitmaps

 REAL(KIND=8), DIMENSION(2) :: DPixSize_all ! Pixel size decimal
 
 INTEGER :: NlonB, & ! Number of longitude pixels of the specific bitmap
            NlatB    ! Number of latitude pixels of the specific bitmap

 REAL(8) :: PixSize ! Pixel size of the specific bitmap

 INTEGER :: BP ! Number of bytes per pixel of the specific bitmap
 
CONTAINS

 SUBROUTINE InitMap(BV) ! To choose a map
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: BV ! Bitmap version
   
   NlonB=NlonB_all(BV)
   NlatB=NlatB_all(BV)
   PixSize=PixSize_all(BV)
   BP=BP_all(BV)
   
 END SUBROUTINE InitMap

 SUBROUTINE InitField(FV) ! To choose a field
   IMPLICIT NONE
   TYPE MaskedCoversType
      INTEGER, DIMENSION(:), ALLOCATABLE :: MaskedCovers_field ! Lists of covers to be masked, for all fields
   END TYPE MaskedCoversType
   INTEGER, INTENT(IN) :: FV ! Bitmap version
   TYPE(MaskedCoversType), DIMENSION(3) :: MaskedCovers_all
   INTEGER :: ifield

   ! Covers to be masked for different fields are defined here
   DO ifield=1,2
      ALLOCATE(MaskedCovers_all(ifield)%MaskedCovers_field(NMaskedCovers_all(ifield)))
   END DO
   MaskedCovers_all(1)%MaskedCovers_field = (/1, 2, 3, 4, 5, 6/) ! For LAI: sea, lake, river, bare land, bare rock, permanent snow
   MaskedCovers_all(2)%MaskedCovers_field = (/1, 2, 3/) ! For ALB: sea, lake, river

   ! Choice of the particular field
   NMaskedCovers = NMaskedCovers_all(FV)
   ALLOCATE(MaskedCovers(NMaskedCovers))
   MaskedCovers=MaskedCovers_all(FV)%MaskedCovers_field

   DO ifield=1,2
      DEALLOCATE(MaskedCovers_all(ifield)%MaskedCovers_field)
   END DO
 END SUBROUTINE InitField
 
END MODULE Bitmap_ML_LAI
