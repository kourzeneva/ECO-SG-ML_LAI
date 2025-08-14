MODULE Bitmap_ML_LAI
  
! PURPOSE: Set the parameters of the bitmaps of ECO_SG_ML and LAI
! AUTHOR:  Ekaterina Kourzeneva, 
!          FMI, 07.2025
!  
! MODIFICATIONS:
!
! 08.2025 Ekaterina Kourzeneva, FMI: small updates to process the albedo fiels as well
!  
 IMPLICIT NONE
 
 INTEGER, PARAMETER, DIMENSION(2) :: NlonB_all = (/ 129600, 137600 /) , & ! Number of longitude pixels of the LAI/ALB and ML bitmaps
                                     NlatB_all = (/ 50400,  96600 /)      ! Number of latitude pixels of the LAI/ALB and ML bitmaps
 REAL(8), PARAMETER, DIMENSION(2) :: PixSize_all = (/ 10.0_8, 1.94036112_8 /)      ! Pixel size of the LAI/ALB and ML bitmaps in seconds of arc(s)

 CHARACTER(14), PARAMETER, DIMENSION(2) :: BitmapFile_all = (/'bitmap_LAI_ALB','bitmap_ML     '/) ! The LAI/ALB and ML bitmap file names

 INTEGER, PARAMETER, DIMENSION(2) :: LSea_all = (/ 0, 1 /),  &   !  Legends for sea, lake, river for different maps
                                     LLake_all = (/ 0, 2 /), &
                                     LRiver_all = (/ 0, 3 /)
 
 INTEGER, PARAMETER, DIMENSION(2) :: BP_all = (/ 2, 1 /) ! Number of bytes per pixel, for different maps

 REAL(KIND=8), DIMENSION(2), PARAMETER :: WestLim_all = (/ -180.0_8, -31.9997305054148_8 /), & ! Western boundaries of the LAI/ALB and ML bitmaps
                                          NorthLim_all = (/ 80.0_8, 72.0660843734148_8 /)      ! Northern boundaries of the LAI/ALB and ML bitmaps
      
 REAL(KIND=8), DIMENSION(2) :: EastLim_all, & ! Eastern boundaries of the LAI/ALB and ML bitmaps
                               SouthLim_all   ! Southern boundaries of the LAI/ALB and ML bitmaps

 REAL(KIND=8), DIMENSION(2) :: DPixSize_all ! Pixel size decimal
 
 INTEGER :: NlonB, & ! Number of longitude pixels of the specific bitmap
            NlatB    ! Number of latitude pixels of the specific bitmap

 REAL(8) :: PixSize
! INTEGER :: PixSize ! Pixel size of the specific bitmap

 INTEGER :: LSea,  & !  Legends for sea, lake, river of the specific bitmap
             LLake, &
             LRiver

 INTEGER :: BP ! Number of bytes per pixel of the specific bitmap
 
CONTAINS

 SUBROUTINE InitMap(BV) ! To choose a map
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: BV ! Bitmap version
   
   NlonB=NlonB_all(BV)
   NlatB=NlatB_all(BV)
   PixSize=PixSize_all(BV)
   LSea=LSea_all(BV)
   LLake=LLake_all(BV)
   LRiver=LRiver_all(BV)
   BP=BP_all(BV)
   
 END SUBROUTINE InitMap
 
END MODULE Bitmap_ML_LAI
