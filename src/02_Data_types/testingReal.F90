      program testingReal     
      implicit none
      
      real(kind=4):: real4Res  ! Define single precision real variable
      real(kind=8):: real8Res ! Define double precision real variable
      integer ::  intRes    ! Define integer variables
      
      ! floating point division 
      real4Res = 2.0/3.0
      real8Res = 2.0/3.0 
      intRes = 2/3
      
      print *, 'Single precision division:', real4Res
      print *, 'Double precision division:', real8Res
      print *, 'Integer division:', intRes

      end program testingReal

