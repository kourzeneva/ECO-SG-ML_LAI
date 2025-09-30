SUBROUTINE Coor2Num_ML_LAI(Coord, Axis, Num, ErCode) 
! PURPOSE: To convert coordinate in degrees to number of nearest pixel of the bitmap
! AUTHOR:  Ekaterina Kourzeneva, 
!          RSHU
!          Modified 7.2005, FMI:
!          possibility to use different bitmaps is added

 USE Bitmap_ML_LAI, ONLY : &
  NlonB, &                         ! Number of longitude pixels of the specific bitmap
  NlatB, &                         ! Number of latitude pixels of the specific bitmap
  WestLim,  &                      ! Western boundary of the current bitmap
  NorthLim, &                      ! Northern boundary of the current bitmap
  EastLim,  &                      ! Eastern boundary of the current bitmap
  SouthLim, &                      ! Southern boundary of the current bitmap
  DPixSize                         ! Pixel size decimal of the specific bitmap

 IMPLICIT NONE

 REAL(KIND=8), INTENT(IN) :: Coord ! The coordinate in DEGREES (with decimal digits!!!)
 INTEGER, INTENT(IN) :: Axis ! The axis: 1 - longitude, degrees from -180 to 180
                             !           2 - latitude, degrees from -90 to 90
 INTEGER, INTENT(OUT) :: Num ! The number of the nearest pixel of the bitmap
 INTEGER, INTENT(OUT) :: ErCode ! Code of error = 0 if no error
                                !               = 1 if error
  
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

 SELECT CASE(Axis) ! Choose axis
  CASE (1)
   IF(Coord.LT.WestLim.OR.Coord.GT.EastLim) THEN
    Num=0
    write(*,*) 'Longitude should be within the interval of ', WestLim, ' and ', EastLim
    ErCode = 1
    RETURN
   END IF
!!   Num=NINT((180.+MIN(Coord,(180.-DPixSize)))*3600/PixSize)+1
!!   Num=INT((180.+MIN(Coord,179.999))*3600/PixSize)+1
!   Num=NINT((Coord-WestLim)*3600/PixSize)+1
   !   Num=NINT((Coord-WestLim_all(BV))*3600._8/PixSize_all(BV))+1
   Num=INT((Coord-WestLim)/DPixSize)+1
   ErCode = 0
  CASE (2)
   IF(Coord.LT.SouthLim.OR.Coord.GT.NorthLim) THEN
    Num=0
    write(*,*) 'Latitude should be within the interval of', SouthLim, ' and ', NorthLim
    ErCode = 1
    RETURN
   END IF
!!    Num=NlatB-NINT((90.+MAX(Coord,(-90.+DPixSize)))*3600/PixSize)+1
!!    Num=NlatB-(INT((90.+MAX(Coord,-89.999))*3600/PixSize)+1)+1 
!!    Num=NlatB-(INT((60.+MIN(Coord,79.999))*3600/PixSize)+1)+1
!    Num=NlatB-(NINT((Coord-SouthLim)*3600/PixSize)+1)+1
 !    Num=NlatB_all(BV)-(NINT((Coord-SouthLim_all(BV))*3600._8/PixSize_all(BV))+1)+1
   Num=NlatB-(INT((Coord-SouthLim)/DPixSize)+1)+1
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

