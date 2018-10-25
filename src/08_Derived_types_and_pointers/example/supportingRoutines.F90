
!------
! Print
!------
   SUBROUTINE PrintList(head)
    USE ListMod
    IMPLICIT NONE

    type( ListElem ), pointer :: head
    type( ListElem ), pointer :: ptr

    ptr => head

    print *, "The list is: "
    DO WHILE ( associated(ptr) )
      print *, ptr%value
      ptr => ptr%next
    END DO
    print *
   END SUBROUTINE

!------------------------
! Insert at head function
!------------------------

   FUNCTION InsertList(head, elem)
    USE ListMod
    IMPLICIT NONE

    type( ListElem ), pointer :: head, elem
    type( ListElem ), pointer :: InsertList                 

    elem%next => head

    InsertList => elem
   END FUNCTION

!---------------------------------------
! "delete at start of the list" function
!---------------------------------------

   FUNCTION DeleteList(head)
    USE ListMod
    IMPLICIT NONE

    type( ListElem ), pointer :: head
    type( ListElem ), pointer :: DeleteList                       

    type( ListElem ), pointer :: h

    IF ( associated( head ) ) THEN
       h    => head			! Use to deallocate
       head => head%next		! 2nd elem is now head
       deallocate(h)			! Deallocate old first elem       
    END IF

    DeleteList => head
   END FUNCTION
