      program testingCharacter
      implicit none
      integer, parameter :: maxLengthChar = 50
      character (len = maxLengthChar) :: stationName
      character (len = maxLengthChar) :: welcomeMsg
      character (len = maxLengthChar) :: location
      character                       :: oneChar

      welcomeMsg = '       Welcome to the Fortran Tutorial    '
      location = 'Hampton, Virginia'
      stationName = 'Blodgett'
      oneChar     = 'T'
      print *, 'Message - 1:     ', welcomeMsg
      print *, 'Message - 2:     ', TRIM(welcomeMsg)
      print *, 'Message - 3:     ', TRIM(welcomeMsg)//' in '//TRIM(location)
      print *, 'Name of station: ', stationName
      print *, oneChar
      end program testingCharacter
