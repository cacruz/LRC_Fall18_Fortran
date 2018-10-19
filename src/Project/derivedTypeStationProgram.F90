      program derivedTypeStationProgram
      
      USE calendarMod
      USE definedConstantsMod
      USE derivedTypeStationMod

      implicit none

      type(obsData) :: Blodgett
      
      integer :: myRec , recs, freq
      integer :: myDate, myTime, mySecs
      integer :: myRefDate, myRefTime

      call set_stationFileName(Blodgett, 'Blodgett_DataFile.txt')
      call readConstantVars(Blodgett)
      call allocateVariables(Blodgett)
      call readNonConstantVars(Blodgett)
      call printStationData(Blodgett)

      myRefDate = get_refDate(Blodgett)
      myRefTime = get_refTime(Blodgett)
      recs      = get_totalNumRecords(Blodgett)
      freq      = get_freqMinute(Blodgett)

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

      end program derivedTypeStationProgram
