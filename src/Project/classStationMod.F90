      module classStationMod
      
      USE definedConstantsMod

      IMPLICIT NONE

      private

      type, public :: obsData
           character(len=maxLengthChar) :: stationFileName
           integer                      :: totalNumRecords    ! number of records in the file
           ! Non-time dependent variables
           character(len=maxLengthChar) :: stationName
           integer                      :: refYear
           integer                      :: refMonth
           integer                      :: refDay
           integer                      :: refHour
           integer                      :: refMinute
           integer                      :: freqMinute
           real(kind=4)                 :: latitude
           real(kind=4)                 :: longitude
           ! Time dependent variables
           real(kind=4), allocatable    :: temperature(:)
           real(kind=4), allocatable    :: humidity(:)
           real(kind=4), allocatable    :: wind(:)
           real(kind=4), allocatable    :: radiation(:)
           real(kind=4), allocatable    :: precipitation(:)
           CONTAINS
              procedure :: allocateVars  => allocateVariables
              procedure :: printVars     => printStationData
              procedure :: readConstants => readConstantVars
              procedure :: readVariables => readNonConstantVars
              procedure :: ref_nymd      => get_refDate
              procedure :: ref_nhms      => get_refTime
              procedure :: freq          => get_freqMinute
              procedure :: recs          => get_totalNumRecords
      end type obsData

      INTERFACE obsData
         module procedure init_obsData
      END INTERFACE

!------------------------------------------------------------------------------
       CONTAINS
!------------------------------------------------------------------------------
          type(obsData) function init_obsData (stationFileName)
            character(len=*), intent(in) :: stationFileName
            init_obsData%stationFileName = TRIM(stationFileName)
          end function init_obsData

          subroutine readConstantVars(self)
            class(obsData),  intent(inOut) :: self

            integer           :: funit = 45      ! file unit number
            character(len=maxLengthChar) :: line, line2

            open(unit=funit, file=self%stationFileName, status='old')

            ! Read station name
            Read( funit, '( a14, a )') line, self%stationName

            ! Read date/time
            Read( funit, '( a14, a )') line2, line
            line = TRIM(line)
            READ(line(1:2),   '(i2)') self%refMonth
            READ(line(4:5),   '(i2)') self%refDay
            READ(line(7:10),  '(i4)') self%refYear
            READ(line(14:15), '(i2)') self%refHour
            READ(line(17:18), '(i2)') self%refMinute

            ! Read frequency
            Read( funit, '( a14, a )') line2, line
            READ(line, '( i2 )') self%freqMinute

            ! Read latitude
            Read( funit, '( a14, a )') line2, line
            READ(line, '( f8.6 )') self%latitude

            ! Read longitude
            Read( funit, '( a14, a )') line2, line
            READ(line, '( f10.6 )') self%longitude

            !Read blank/comments
            Read( funit, '( a )') line
            Read( funit, '( a )') line
            Read( funit, '( a )') line
            Read( funit, '( a )') line

            !Determine the number of records
            self%totalNumRecords = 0
            Do
               Read( funit, '( a )', End = 200 ) line
               self%totalNumRecords = self%totalNumRecords + 1
            End Do

200         close(funit)

          end subroutine readConstantVars

!------------------------------------------------------------------------------
          subroutine readNonConstantVars(self)
            class(obsData),  intent(inOut) :: self

            integer           :: i
            integer           :: funit = 45      ! file unit number
            character(len=maxLengthChar) :: line
            ! Second reading of the file to get all the time dependent variables
            !-------------------------------------------------------------------
            open(unit=funit, file=self%stationFileName, status='old')

            DO i = 1, 9
               Read( funit, '( a )') line
            ENDDO

            DO i = 1, self%totalNumRecords
               Read( funit, *) self%temperature(i), self%humidity(i), &
                               self%wind(i), self%radiation(i), &
                               self%precipitation(i)
            ENDDO
            close(funit)

          end subroutine readNonConstantVars

!------------------------------------------------------------------------------
          subroutine printStationData(self)
            class(obsData),  intent(inOut) :: self

            WRITE(*,*)'------------------------------------------------'
            WRITE(*,*)'-------->  Summary of the station file <--------'
            WRITE(*,*)'------------------------------------------------'
            WRITE(*,'(a22,1x,a)') "Station Name:", TRIM(self%stationName)
            WRITE(*,'(a22,1x,i12)') 'Number of records:', self%totalNumRecords
            WRITE(*,'(a22,1x,i4,a1,i2,a1,i2,a1,i2,a1,i2)') "Date/Time:", &
                self%refYear,'/',self%refMonth,'/',self%refDay,'-', self%refHour,':',self%refMinute
            WRITE(*,'(a22,1x,i3,1x,a)') "Frequency:", self%freqMinute, &
                                                       "minutes"
            WRITE(*,'(a22,1x,f12.5)') "Latitude:", self%latitude
            WRITE(*,'(a22,1x,f12.5)') "Longitude:", self%longitude
            WRITE(*,'(a22,1x,2f12.5)') 'Temperature range:', minval(self%temperature), maxval(self%temperature)
            WRITE(*,'(a22,1x,2f12.5)') 'Humidity range:', minval(self%humidity), maxval(self%humidity)
            WRITE(*,'(a22,1x,2f12.5)') 'Wind range:', minval(self%wind), maxval(self%wind)
            WRITE(*,'(a22,1x,2f12.5)') 'Radiation range:', minval(self%radiation), maxval(self%radiation)
            WRITE(*,'(a22,1x,2f12.5)') 'Precipitation range:', minval(self%precipitation), maxval(self%precipitation)
            WRITE(*,*)'------------------------------------------------'
          end subroutine printStationData

!------------------------------------------------------------------------------
          subroutine allocateVariables(self)
            class(obsData),  intent(inOut) :: self
            call allocate_wind(self)
            call allocate_humidity(self)
            call allocate_radiation(self)
            call allocate_temperature(self)
            call allocate_precipitation(self)
          end subroutine allocateVariables
!------------------------------------------------------------------------------
          function get_refDate(self) result(nymd)
            class(obsData),  intent(in) :: self
            integer :: nymd
            nymd = 10000*self%refYear + 100*self%refMonth + self%refDay
          end function get_refDate
!------------------------------------------------------------------------------
          function get_refTime(self) result(nhms)
            class(obsData),  intent(in) :: self
            integer :: nhms
            nhms = 10000*self%refHour + 100*self%refMinute 
          end function get_refTime
!------------------------------------------------------------------------------
!---------stationName
          subroutine get_stationName(self, stationName)
            character(len=*), intent(out) :: stationName
            class(obsData),  intent(inOut) :: self
            stationName = self%stationName
          end subroutine get_stationName

          subroutine set_stationName(self, stationName)
            character(len=*),  intent(in) :: stationName
            class(obsData),  intent(inOut) :: self
            self%stationName = stationName
          end subroutine set_stationName

!---------totalNumRecords
          function get_totalNumRecords(self) result(totalNumRecords)
            class(obsData),  intent(in) :: self
            integer                     :: totalNumRecords
            totalNumRecords = self%totalNumRecords
          end function get_totalNumRecords

          subroutine set_totalNumRecords(self, totalNumRecords)
            integer,           intent(in) :: totalNumRecords
            class(obsData),  intent(inOut) :: self
            self%totalNumRecords = totalNumRecords
          end subroutine set_totalNumRecords

!---------refYear
          subroutine get_refYear(self, refYear)
            integer,          intent(out) :: refYear
            class(obsData),  intent(inOut) :: self
            refYear = self%refYear
          end subroutine get_refYear

          subroutine set_refYear(self, refYear)
            integer,           intent(in) :: refYear
            class(obsData),  intent(inOut) :: self
            self%refYear = refYear
          end subroutine set_refYear

!---------refMonth
          subroutine get_refMonth(self, refMonth)
            integer,          intent(out) :: refMonth
            class(obsData),  intent(inOut) :: self
            refMonth = self%refMonth
          end subroutine get_refMonth

          subroutine set_refMonth(self, refMonth)
            integer,           intent(in) :: refMonth
            class(obsData),  intent(inOut) :: self
            self%refMonth = refMonth
          end subroutine set_refMonth

!---------refDay
          subroutine get_refDay(self, refDay)
            integer,          intent(out) :: refDay
            class(obsData),  intent(inOut) :: self
            refDay = self%refDay
          end subroutine get_refDay

          subroutine set_refDay(self, refDay)
            integer,           intent(in) :: refDay
            class(obsData),  intent(inOut) :: self
            self%refDay = refDay
          end subroutine set_refDay

!---------refHour
          subroutine get_refHour(self, refHour)
            integer,          intent(out) :: refHour
            class(obsData),  intent(inOut) :: self
            refHour = self%refHour
          end subroutine get_refHour

          subroutine set_refHour(self, refHour)
            integer,           intent(in) :: refHour
            class(obsData),  intent(inOut) :: self
            self%refHour = refHour
          end subroutine set_refHour

!---------refMinute
          subroutine get_refMinute(self, refMinute)
            integer,          intent(out) :: refMinute
            class(obsData),  intent(inOut) :: self
            refMinute = self%refMinute
          end subroutine get_refMinute

          subroutine set_refMinute(self, refMinute)
            integer,           intent(in) :: refMinute
            class(obsData),  intent(inOut) :: self
            self%refMinute = refMinute
          end subroutine set_refMinute

!---------freqMinute
          function get_freqMinute(self) result(freqMinute)
            integer                        :: freqMinute
            class(obsData),  intent(in) :: self
            freqMinute = self%freqMinute
          end function get_freqMinute

          subroutine set_freqMinute(self, freqMinute)
            integer,           intent(in) :: freqMinute
            class(obsData),  intent(inOut) :: self
            self%freqMinute = freqMinute
          end subroutine set_freqMinute

!---------Latitude
          subroutine get_latitude(self, latitude)
            real(kind=4),     intent(out) :: latitude
            class(obsData),  intent(inOut) :: self
            latitude = self%latitude
          end subroutine get_latitude

          subroutine set_latitude(self, latitude)
            real(kind=4),      intent(in) :: latitude
            class(obsData),  intent(inOut) :: self
            self%latitude = latitude
          end subroutine set_latitude

!---------Longitude
          subroutine get_longitude(self, longitude)
            real(kind=4),     intent(out) :: longitude
            class(obsData),  intent(inOut) :: self
            longitude = self%longitude
          end subroutine get_longitude

          subroutine set_longitude(self, longitude)
            real(kind=4),      intent(in) :: longitude
            class(obsData),  intent(inOut) :: self
            self%longitude = longitude
          end subroutine set_longitude

!---------Precipitation
          subroutine get_temperature(self, temperature)
            real(kind=4),     intent(out) :: temperature(:)
            class(obsData),  intent(inOut) :: self
            temperature(:) = self%temperature(:)
          end subroutine get_temperature

          subroutine set_temperature(self, temperature)
            real(kind=4),      intent(in) :: temperature(:)
            class(obsData),  intent(inOut) :: self
            self%temperature(:) = temperature(:)
          end subroutine set_temperature

          subroutine allocate_temperature(self)
            class(obsData),  intent(inOut) :: self
            ALLOCATE(self%temperature(self%totalNumRecords))
          end subroutine allocate_temperature

!---------Humidity
          subroutine get_humidity(self, humidity)
            real(kind=4),     intent(out) :: humidity(:)
            class(obsData),  intent(inOut) :: self
            humidity(:) = self%humidity(:)
          end subroutine get_humidity

          subroutine set_humidity(self, humidity)
            real(kind=4),      intent(in) :: humidity(:)
            class(obsData),  intent(inOut) :: self
            self%humidity(:) = humidity(:)
          end subroutine set_humidity

          subroutine allocate_humidity(self)
            class(obsData),  intent(inOut) :: self
            ALLOCATE(self%humidity(self%totalNumRecords))
          end subroutine allocate_humidity

!---------Wind
          subroutine get_wind(self, wind)
            real(kind=4),     intent(out) :: wind(:)
            class(obsData),  intent(inOut) :: self
            wind(:) = self%wind(:)
          end subroutine get_wind

          subroutine set_wind(self, wind)
            real(kind=4),      intent(in) :: wind(:)
            class(obsData),  intent(inOut) :: self
            self%wind(:) = wind(:)
          end subroutine set_wind

          subroutine allocate_wind(self)
            class(obsData),  intent(inOut) :: self
            ALLOCATE(self%wind(self%totalNumRecords))
          end subroutine allocate_wind

!---------Radiation
          subroutine get_radiation(self, radiation)
            real(kind=4),     intent(out) :: radiation(:)
            class(obsData),  intent(inOut) :: self
            radiation(:) = self%radiation(:)
          end subroutine get_radiation

          subroutine set_radiation(self, radiation)
            real(kind=4),      intent(in) :: radiation(:)
            class(obsData),  intent(inOut) :: self
            self%radiation(:) = radiation(:)
          end subroutine set_radiation

          subroutine allocate_radiation(self)
            class(obsData),  intent(inOut) :: self
            ALLOCATE(self%radiation(self%totalNumRecords))
          end subroutine allocate_radiation

!---------Precipitation
          subroutine get_precipitation(self, precipitation)
            real(kind=4),     intent(out) :: precipitation(:)
            class(obsData),  intent(inOut) :: self
            precipitation(:) = self%precipitation(:)
          end subroutine get_precipitation

          subroutine set_precipitation(self, precipitation)
            real(kind=4),     intent(in)  :: precipitation(:)
            class(obsData),  intent(inOut) :: self
            self%precipitation(:) = precipitation(:)
          end subroutine set_precipitation

          subroutine allocate_precipitation(self)
            class(obsData),  intent(inOut) :: self
            ALLOCATE(self%precipitation(self%totalNumRecords))
          end subroutine allocate_precipitation

      end module classStationMod
