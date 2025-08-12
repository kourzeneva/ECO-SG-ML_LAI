SUBROUTINE Coor2Num_ML_LAI(BV,Coord, Axis, Num, ErCode) 
! PURPOSE: To convert coordinate in degrees to number of nearest pixel of the bitmap
! AUTHOR:  Ekaterina Kourzeneva, 
!          RSHU
!          Modified 7.2005, FMI:
!          possibility to use different bitmaps is added

 USE Bitmap_ML_LAI, ONLY : &
  NlonB_all,    &  ! Number of longitude pixels of the LAI and ML bitmaps
  NlatB_all,    &  ! Number of latitude pixels of the LAI and ML bitmaps
  WestLim_all,  &  ! Western boundaries of the LAI and ML bitmaps
  NorthLim_all, &  ! Northern boundaries of the LAI and ML bitmaps
  EastLim_all,  &  ! Eastern boundaries of the LAI and ML bitmaps
  SouthLim_all, &  ! Southern boundaries of the LAI and ML bitmaps 
  DPixSize_all     ! Pixel size decimal


 IMPLICIT NONE

 INTEGER, INTENT(IN) :: BV ! Bitmap version
 REAL(KIND=8), INTENT(IN) :: Coord ! The coordinate in DEGREES (with decimal digits!!!)
 INTEGER, INTENT(IN) :: Axis ! The axis: 1 - longitude, degrees from -180 to 180
                             !           2 - latitude, degrees from -90 to 90
 INTEGER, INTENT(OUT) :: Num ! The number of the nearest pixel of the bitmap
 INTEGER, INTENT(OUT) :: ErCode ! Code of error = 0 if no error
                                !               = 1 if error
  
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

 SELECT CASE(Axis) ! Choose axis
  CASE (1)
   IF(Coord.LT.WestLim_all(BV).OR.Coord.GT.EastLim_all(BV)) THEN
    Num=0
    write(*,*) 'Longitude should be within the interval of ', WestLim_all(BV), ' and ', EastLim_all(BV)
    ErCode = 1
    RETURN
   END IF
!!   Num=NINT((180.+MIN(Coord,(180.-DPixSize)))*3600/PixSize)+1
!!   Num=INT((180.+MIN(Coord,179.999))*3600/PixSize)+1
!   Num=NINT((Coord-WestLim)*3600/PixSize)+1
   !   Num=NINT((Coord-WestLim_all(BV))*3600._8/PixSize_all(BV))+1
   Num=INT((Coord-WestLim_all(BV))/DPixSize_all(BV))+1
   ErCode = 0
  CASE (2)
   IF(Coord.LT.SouthLim_all(BV).OR.Coord.GT.NorthLim_all(BV)) THEN
    Num=0
    write(*,*) 'Latitude should be within the interval of', SouthLim_all(BV), ' and ', NorthLim_all(BV)
    ErCode = 1
    RETURN
   END IF
!!    Num=NlatB-NINT((90.+MAX(Coord,(-90.+DPixSize)))*3600/PixSize)+1
!!    Num=NlatB-(INT((90.+MAX(Coord,-89.999))*3600/PixSize)+1)+1 
!!    Num=NlatB-(INT((60.+MIN(Coord,79.999))*3600/PixSize)+1)+1
!    Num=NlatB-(NINT((Coord-SouthLim)*3600/PixSize)+1)+1
 !    Num=NlatB_all(BV)-(NINT((Coord-SouthLim_all(BV))*3600._8/PixSize_all(BV))+1)+1
   Num=NlatB_all(BV)-(INT((Coord-SouthLim_all(BV))/DPixSize_all(BV))+1)+1
!!    Num=NINT((Coord-SouthLim)*3600/PixSize)+1
    ErCode = 0
  CASE DEFAULT
   Num=0
   write(*,*) 'Latitude or Longitude? Wrong number of axis!'
   ErCode = 1
   RETURN
 END SELECT ! Choose axis

END SUBROUTINE Coor2Num_ML_LAI
! --------------------------------------------------------------------------------

