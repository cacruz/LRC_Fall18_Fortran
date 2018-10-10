      program testingInteger  
      implicit none
      
      integer(kind =  2) :: shortval          !two byte integer
      integer(kind =  4) :: longval           !four byte integer
      integer(kind =  8) :: verylongval       ! eight byte integer
      integer(kind = 16) :: veryverylongval   ! sixteen byte integer
      integer            :: defval            ! default integer 
        
      print *, huge(shortval)
      print *, huge(longval)
      print *, huge(verylongval)
      print *, huge(veryverylongval)
      print *, huge(defval)
      
      end program testingInteger
