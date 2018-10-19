      module definedConstantsMod

      IMPLICIT NONE

      integer, parameter :: maxLengthChar   = 100
      
      integer, parameter :: minsPerHours    =  60
      integer, parameter :: hoursPerDay     =  24
      integer, parameter :: daysPerWeek     =   7
      integer, parameter :: daysPerYear     = 365
      integer, parameter :: daysPerLeapYear = 366

      integer, parameter :: monthsPerYear   = 12
      integer, parameter :: secsPerMin      = 60
      integer, parameter :: secsPerHour     = secsPerMin *60
      integer, parameter :: secsPerDay      = secsPerHour*24
      integer, parameter :: secsPerWeek     = secsPerDay * 7
      integer, parameter :: secsPerYear     = secsPerDay * daysPerYear
      integer, parameter :: secsPerLeapYear = secsPerDay * daysPerLeapYear

      INTEGER, parameter :: daysPerMonth            (monthsPerYear) = (/      &
         31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31  /)
      INTEGER, parameter :: daysPerMonthLeapYear    (monthsPerYear) = (/      &
         31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31  /)
      INTEGER, parameter :: startDayOfMonth         (monthsPerYear) = (/      &
         1,  32,  60,  91, 121, 152, 182, 213, 244, 274, 305, 335 /)
      INTEGER, parameter :: startDayOfMonthLeapYear (monthsPerYear) = (/      &
         1,  32,  61,  92, 122, 153, 183, 214, 245, 275, 306, 336 /)

      end module definedConstantsMod
