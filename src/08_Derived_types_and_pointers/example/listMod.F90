      MODULE listMod
  
      implicit none

      private
      public  :: ListElem

      TYPE ListElem
       REAL                    :: value
       TYPE(ListElem), POINTER :: next
      END TYPE ListElem

     END MODULE listMod
