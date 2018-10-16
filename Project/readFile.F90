      program readFile

      character(len=50) :: fname           ! file name
      character(len=90) :: line, line2
      integer           :: funit = 45      ! file unit number
      integer           :: numRecords      ! number of records in the file
      integer           :: i
      ! Non-time dependent variables
      character(len=30) :: stationName
      integer           :: begYear, begMonth, begDay
      integer           :: begHour, begMinute
      integer           :: freqMinute
      real              :: latitude, longitude
      ! Time dependent variables
      real, allocatable :: temperature(:)
      real, allocatable :: humidity(:)
      real, allocatable :: wind(:)
      real, allocatable :: radiation(:)
      real, allocatable :: precipitation(:)

      fname = 'Blodgett_DataFile.txt'

      ! First reading of the file to get the non-time dependent variables
      ! and determine the number of records in the file
      !------------------------------------------------------------------
      open(unit=funit, file=fname, status='old')

      ! Read station name
      Read( funit, '( a14, a )') line, stationName
      
      ! Read date/time
      Read( funit, '( a14, a )') line2, line
      line = TRIM(line)
      READ(line(1:2),   '(i2)') begMonth
      READ(line(4:5),   '(i2)') begDay
      READ(line(7:10),  '(i4)') begYear
      READ(line(14:15), '(i2)') begHour
      READ(line(17:18), '(i2)') begMinute
      
      ! Read frequency
      Read( funit, '( a14, a )') line2, line
      READ(line, '( i2 )') freqMinute

      ! Read latitude
      Read( funit, '( a14, a )') line2, line
      READ(line, '( f8.6 )') latitude

      ! Read longitude
      Read( funit, '( a14, a )') line2, line
      READ(line, '( f10.6 )') longitude

      !Read blank/comments
      Read( funit, '( a )') line
      Read( funit, '( a )') line
      Read( funit, '( a )') line
      Read( funit, '( a )') line

      !Determine the number of records
      numRecords = 0
      Do
         Read( funit, '( a )', End = 200 ) line
         numRecords = numRecords + 1
      End Do

200   close(funit)


      ! Allocate the time dependent variables
      !--------------------------------------
      ALLOCATE(temperature(numRecords))
      ALLOCATE(humidity(numRecords))
      ALLOCATE(wind(numRecords))
      ALLOCATE(radiation(numRecords))
      ALLOCATE(precipitation(numRecords))

      ! Second reading of the file to get all the time dependent variables
      !-------------------------------------------------------------------
      open(unit=funit, file=fname, status='old')

      DO i = 1, 9
         Read( funit, '( a )') line
      ENDDO

      DO i = 1, numRecords
         Read( funit, *) temperature(i), humidity(i), &
                                 wind(i), radiation(i), precipitation(i)
      ENDDO
      close(funit)

      print*, "Station Name:      ", TRIM(stationName)
      PRINT*, "Date/Time:         ", begYear, begMonth, begDay,'-', begHour, begMinute
      print*, "Frequency:         ", freqMinute , " minutes"
      print*, "Latitude:          ", latitude
      print*, "Longitude:         ", longitude
      print*, 'Number of records: ', numRecords
      print*, 'Temperature:       ', minval(temperature), maxval(temperature)
      print*, 'Humidity:          ', minval(humidity), maxval(humidity)
      print*, 'Wind:              ', minval(wind), maxval(wind)
      print*, 'Radition:          ', minval(radiation), maxval(radiation)
      print*, 'Precipitation:     ', minval(precipitation), maxval(precipitation)

      DEALLOCATE(temperature)
      DEALLOCATE(humidity)
      DEALLOCATE(wind)
      DEALLOCATE(radiation)
      DEALLOCATE(precipitation)

      end program readFile
