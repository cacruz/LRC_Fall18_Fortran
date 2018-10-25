   PROGRAM listProgram
    USE listMod

    INTERFACE
      SUBROUTINE PrintList(head)
       USE ListMod
       type( ListElem ), pointer :: head
      END SUBROUTINE

      FUNCTION InsertList(head, elem)
       USE ListMod
       type( ListElem ), pointer :: head, elem
       type( ListElem ), pointer :: InsertList
      END FUNCTION 

      FUNCTION DeleteList(head)
       USE ListMod
       type( ListElem ), pointer :: head
       type( ListElem ), pointer :: DeleteList
      END FUNCTION 
    END INTERFACE

    ! ---------------------------------------------         
    ! Start of main program
    ! ---------------------------------------------

    type( ListElem ), pointer :: head
    type( ListElem ), pointer :: newElem
    integer, parameter :: N = 4

    NULLIFY( head )   ! Empty list

    ! ---------------------------------------------         
    ! Add the N elements
    ! ---------------------------------------------
    DO i = 1, N
      ALLOCATE( newElem )

      CALL random_number( newElem%value )

      head => InsertList(head, newElem)
      CALL PrintList(head)
    END DO

   print *,
   print *, "----> Deleting  head from list...."

   head => DeleteList(head)
   CALL PrintList(head)

   END PROGRAM listProgram
