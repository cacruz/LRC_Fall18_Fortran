program LinkedList
    implicit none

    TYPE node
       integer :: value
       TYPE (node), POINTER :: next
    END TYPE node

    integer :: num, status
    TYPE (node), POINTER :: head, current

    ! build up the list; initially nullify head 
    NULLIFY(head)
    print *, "Enter a list of integers, one at a time, ending with 0"
    do
       read *, num        ! read num from keyboard
       if (num == 0) EXIT ! until 0 is entered
       ALLOCATE(current)  ! create new node
       current%value = num
       current%next => head ! point to previous one
       head => current      ! update head of list
    end do

    ! traverse the list & print the values
    current => head     ! start at head
    do
       ! exit if null pointer--end of list
       if (.NOT. ASSOCIATED(current)) EXIT
       print*, current%value

       ! make current alias of next node
       current => current%next
    end do
end program LinkedList
