      module calendarMod

      use  definedConstantsMod

      IMPLICIT NONE
      
      private
      public  :: convertRecordToDateTime
      public  :: getSecondsFromRefDate

!*********************************************************************************
CONTAINS
!*********************************************************************************
! convertRecordToDateTime - Given a record number, convert it to a new date
!                           and a new time with respect to the reference
!                           date and time.
!*********************************************************************************
      subroutine convertRecordToDateTime (nrecs, refDate, refTime, &
                                                  newDate, newTime)
      integer, intent(in)  :: nrecs     ! record number
      integer, intent(in)  :: refDate   ! year/month/day (YYYYMMDD)
      integer, intent(in)  :: refTime   ! hour/min/Sec (HHMMSS)
      integer, intent(out) :: newDate   ! year/month/day (YYYYMMDD)
      integer, intent(out) :: newTime   ! hour/min/Sec (HHMMSS)

      integer              :: ndays, totalSecs
      integer              :: oldTime, ndy, i

      ! number of seconds from the first record
      totalSecs = 30*secsPerMin*(nrecs - 1) 

      ndays     = totalSecs / secsPerDay ! number of days
      oldTime   =  convertSecondstoTime (totalSecs - ndays*secsPerDay)

      call addTwoTimes (refTime, oldTime, ndy, newTime)

      ndays   = ndays + ndy
      newDate = refDate
      do i = 1, ndays
         newDate = incrementDate(newDate, 1)
      enddo

      end subroutine convertRecordToDateTime
!*********************************************************************************
! addTwoTime - Add two times and determine the numder of days and the new time
!*********************************************************************************
      subroutine addTwoTimes (oldTime1, oldTime2, nday, newTime)
      integer, intent(in)  :: oldTime1   ! hour/min/Sec (HHMMSS)
      integer, intent(in)  :: oldTime2   ! hour/min/Sec (HHMMSS)
      integer, intent(out) :: nday       ! number of day (0 or 1)
      integer, intent(out) :: newTime   ! hour/min/Sec (HHMMSS)
      
      integer              :: nh1, nm1, ns1
      integer              :: nh2, nm2, ns2
      integer              :: nh , nm , ns 

      nday = 0

      call splitDateTime(oldTime1, nh1, nm1, ns1)
      call splitDateTime(oldTime2, nh2, nm2, ns2)

      nh = nh1 + nh2
      nm = nm1 + nm2
      ns = ns1 + ns2

      if (ns .GE. 60) then
         ns = ns - 60
         nm = nm + 1
      endif

      if (nm .GE. 60) then
         nm = nm - 60
         nh = nh + 1
      endif

      if (nh .GE. 24) then
         nh = nh - 24
         nday = nday + 1
      endif

      newTime = 10000*nh + 100*nm + ns

      end subroutine addTwoTimes
!*********************************************************************************
! incrementDate - Increments/decrements a date (nymd) by one day
!*********************************************************************************
      function incrementDate (nymd, dayinc) result(newDate)
      integer, intent(in) :: nymd     ! year/month/day (YYYYMMDD)
      integer, intent(in) :: dayinc   ! the day increment/decrement (1 or -1)
      integer             :: newDate  ! nymd + dayinc
   
      logical :: is_ldy
      logical :: is_lyr
      integer :: ndy, nmon, nyr

      if ((dayinc .NE. -1) .and. (dayinc .NE. 1)) then
         PRINT*, 'dayinc range error in incrementDate.'
         STOP
      end if

      call splitDateTime(nymd, nyr, nmon, ndy)
      ndy  = ndy + dayinc

      if (ndy == 0) then
         ! Return to previous month
         nmon = nmon - 1

         if (nmon == 0) then
            nmon = monthsPerYear
            nyr = nyr - 1
         end if

         is_lyr = IsLeapYear (nyr)

         if (is_lyr) then
            ndy = daysPerMonthLeapYear(nmon)
         else
            ndy = daysPerMonthLeapYear(nmon)
         end if
      else
         is_lyr = IsLeapYear (nyr)

         if (is_lyr .and. (nmon == 2) .and. (ndy == daysPerMonthLeapYear(nmon))) then
            is_ldy = .true.
         else
            is_ldy = .false.
         end if

         if (.not. is_ldy) then
            if (ndy > daysPerMonth(nmon)) then
               ! Proceed to next month
               ndy  = 1
               nmon = nmon + 1

               if (nmon > monthsPerYear) then
                  nmon = 1
                  nyr = nyr + 1
               end if
            end if
         end if
      end if

      newDate = (nyr * 10000) + (nmon * 100) + ndy


      end function incrementDate
!*********************************************************************************
! getSecondsFromRefDate - Return the offset in seconds from reference date/time, 
!                         given any nymd/nhms
!*********************************************************************************
      function getSecondsFromRefDate (ref_nymd, ref_nhms, nymd, nhms) result(nsecs)
      integer, intent(in) :: ref_nymd  ! reference year/month/day (YYYYMMDD)
      integer, intent(in) :: ref_nhms  ! reference hour/min/sec   (HHMMSS)
      integer, intent(in) :: nymd      ! year/month/day (YYYYMMDD)
      integer, intent(in) :: nhms      ! hour/min/sec   (HHMMSS)
      integer             :: nsecs     ! seconds from refrence time (s)

      integer             :: i
      integer             :: ref_year, year, month, day

      call splitDateTime (ref_nymd, ref_year, month, day)
      call splitDateTime (    nymd,     year, month, day)

      nsecs = ConvertDateToSeconds (nymd) + ConvertTimeToSeconds (nhms) - &
              ConvertDateToSeconds (ref_nymd) - ConvertTimeToSeconds (ref_nhms)

      if (ref_year .LT. year) then
         do i = ref_year, year-1
            if (isLeapYear(i)) then
               nsecs = nsecs + secsPerLeapYear
            else
               nsecs = nsecs + secsPerYear
            endif
         enddo
      endif

      return

      end function getSecondsFromRefDate
!*********************************************************************************
! getSecondsFromJanuary1 - Return the offset in seconds from Jan. 1st, given any nymd/nhms
!*********************************************************************************
      function getSecondsFromJanuary1 (nymd, nhms) result(nsec_jan1)
      integer, intent(in) :: nymd      ! year/month/day (YYYYMMDD)
      integer, intent(in) :: nhms      ! hour/min/sec   (HHMMSS)
      integer             :: nsec_jan1 ! seconds from Jan. 1st (s)

      nsec_jan1 = ConvertDateToSeconds (nymd) + ConvertTimeToSeconds (nhms)

      return

      end function getSecondsFromJanuary1
!*********************************************************************************
! getDaysFromJanuary1 - Return the the number of days from Jan. 1st given nymd
!*********************************************************************************
      function getDaysFromJanuary1 (nymd) result(nday_jan1)
      integer, intent(in) :: nymd           ! year/month/day (YYYYMMDD)
      integer             :: nday_jan1      ! days from Jan. 1st (days)

      nday_jan1 = (ConvertDateToSeconds (nymd) / secsPerDay) + 1

      return

      end function getDaysFromJanuary1
!*********************************************************************************
! convertTimeToSeconds - convert nhms to total seconds
!*********************************************************************************
      function convertTimeToSeconds (nhms) result(nsecs)
      integer, intent(in) :: nhms
      integer             :: nsecs
      nsecs = (nhms / 10000) * secsPerHour +              &
              (Mod (nhms, 10000) / 100) * secsPerMin +   &
               Mod (nhms, 100)
      end function convertTimeToSeconds
!*********************************************************************************
! convertSecondstoTime - convert total seconds to hour/min/sec
!*********************************************************************************
      function convertSecondstoTime (nsecs) result(nhms)
      integer, intent(in) :: nsecs
      integer             :: nhms

      if (nsecs .GE. secsPerDay) then
         write(*,*) 'nsecs too big in ConvertSecondstoTime.'
         STOP
      endif
      nhms = (nsecs / secsPerHour) * 10000 +                 &
             (Mod (nsecs, secsPerHour) / secsPerMin) * 100 +     &
             Mod (nsecs, secsPerMin)
      end function convertSecondstoTime
!*********************************************************************************
! convertDateToSeconds - Converts nymd to seconds
!*********************************************************************************
      function convertDateToSeconds (nymd) result(nsecs)
      integer, intent(in) :: nymd   ! year/month/day (YYYYMMDD)
      integer             :: nsecs

      integer             :: ndy, nmon, nyr, idays

      call SplitDateTime(nymd, nyr, nmon, ndy)

      if (IsLeapYear (nyr)) then
         idays  = (startDayOfMonthLeapYear(nmon) - 1) + (ndy - 1)
      else
         idays  = (startDayOfMonth(nmon)    - 1) + (ndy - 1)
      end if

      nsecs = idays * secsPerDay
      end function ConvertDateToSeconds
!*********************************************************************************
! splitDateTime - Extract the three fields from either nymd 
!                 (year, month, day) or nhms (hours, minutes, seconds).
!*********************************************************************************
       subroutine splitDateTime (nlmr, left2, middle2, right2)
       integer, intent(in)  :: nlmr    ! nymd or nhms ("lmr" = "left" "middle" "right")
                                        ! (YYYYMMDD or HHMMSS)
       integer, intent(out) :: left2   ! left   field (year  or hours)
       integer, intent(out) :: middle2 ! middle field (month or minutes)
       integer, intent(out) :: right2  ! right  field (day   or seconds)

       left2   = nlmr / 10000
       middle2 = Mod (nlmr, 10000) / 100
       right2  = Mod (nlmr, 100)

       return
       end subroutine splitDateTime
!*********************************************************************************
!  isLeapYear - Determine whether a given year is a leap year.
!*********************************************************************************

      FUNCTION isLeapYear (YEAR) RESULT (LEAP_FLAG)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: YEAR
      LOGICAL :: LEAP_FLAG

      LEAP_FLAG = .FALSE.
      IF (MOD(YEAR,4) .EQ. 0)   LEAP_FLAG = .TRUE.
      IF (MOD(YEAR,100) .EQ. 0) LEAP_FLAG = .FALSE.
      IF (MOD(YEAR,400) .EQ. 0) LEAP_FLAG = .TRUE.

      RETURN

      END FUNCTION isLeapYear

      end module calendarMod
