      program classStationProgram
      
      USE calendarMod
      USE definedConstantsMod
      USE classStationMod

      implicit none

      type(obsData) :: Blodgett
      
      integer :: myRec     = 1
      integer :: myDate, myTime, mySecs
      integer :: myRefDate, myRefTime
      integer :: recs, freq

      Blodgett = obsData('Blodgett_DataFile.txt')
      call Blodgett%readConstants
      call Blodgett%allocateVars
      call Blodgett%readVariables
      call Blodgett%printVars

      myRefDate = Blodgett%ref_nymd()
      myRefTime = Blodgett%ref_nhms()
      recs      = Blodgett%recs()
      freq      = Blodgett%freq()

      ! Get the date/time of the last record
      call convertRecordToDateTime (recs, myRefDate, myRefTime,  myDate, myTime)
      WRITE(*,'(a29,1x,i8,a1,i6)') 'Last Record Date/Time:', myDate, '-', myTime

      ! Verification of the last record
      mySecs = getSecondsFromRefDate (myRefDate, myRefTime, myDate, myTime)
      WRITE(*,'(a29,1x,i8)') 'Last record verification:', mySecs/(freq*secsPerMin) + 1

      PRINT*
      ! Find the record number of an abitrary date
      myDate = 20031025
      myTime =   143000
      mySecs = getSecondsFromRefDate (myRefDate, myRefTime, myDate, myTime)
      WRITE(*,'(a29,1x,i8,a1,i6,1x,a2,1x,i7)') 'Record corresponding to', myDate,'-', myTime, &
             'is:', mySecs/(freq*secsPerMin) + 1

      end program classStationProgram
