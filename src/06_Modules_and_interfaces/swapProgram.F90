PROGRAM swapProgram

       USE swapMod, only : SWAP

       IMPLICIT NONE
       INTEGER          :: I, J, K, L
       REAL             :: A, B, X, Y
       CHARACTER        :: C, D, E, F

       !--------------
       ! Swap integers
       !--------------
       I = 1   
       J = 2         
       K = 100 
       L = 200
       WRITE (*,'(a,4(x,i4))') "Integer before swap:", I, J, K, L
       CALL SWAP (I, J)  
       CALL SWAP (K, L)
       WRITE (*,'(a,4(x,i4))') "Integer after  swap:", I, J, K, L
       WRITE (*,*) 

       !------------------
       ! Swap real numbers
       !------------------
       A = 7.1 
       B = 10.9      
       X = 11.1
       Y = 17.0
       WRITE (*,'(a,4(x,f5.2))') "Real numbers before swap:", A, B, X, Y
       CALL SWAP (A, B)  
       CALL SWAP (X, Y)
       WRITE (*,'(a,4(x,f5.2))') "Real numbers after  swap:", A, B, X, Y
       WRITE (*,*) 

       !------------------
       ! Swap characters
       !------------------
       C = 'a' 
       d = 'b'       
       E = '1' 
       F = '"'
       WRITE (*,'(a,4(x,a))') "Characters before swap: ", C, D, E, F
       CALL SWAP (C, D)  
       CALL SWAP (E, F)
       WRITE (*,'(a,4(x,a))') "Characters after  swap: ", C, D, E, F

END PROGRAM swapProgram
