module NestedTypes
   TYPE WindOb
      integer :: windDir 
      real    :: windMps
   END TYPE WindOb

   TYPE WeatherOb
      real :: tempK
      real :: humidity
      real :: precip
      TYPE (WindOb) :: wind
   END TYPE WeatherOb
end module
