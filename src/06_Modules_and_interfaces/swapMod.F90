MODULE swapMod

       implicit none

       INTERFACE SWAP
          MODULE PROCEDURE SWAP_R
          MODULE PROCEDURE SWAP_I
          MODULE PROCEDURE SWAP_C
       END INTERFACE
CONTAINS

       SUBROUTINE SWAP_R(A, B)
       IMPLICIT NONE
       REAL, INTENT (INOUT)      :: A, B
       REAL                      :: TEMP
               TEMP = A ; A = B ; B = TEMP
       END SUBROUTINE SWAP_R

       SUBROUTINE SWAP_I(A, B)
       IMPLICIT NONE
       INTEGER, INTENT (INOUT)   :: A, B
       INTEGER                   :: TEMP
               TEMP = A ; A = B ; B = TEMP
       END SUBROUTINE SWAP_I

       SUBROUTINE SWAP_C(A, B)
       IMPLICIT NONE
       CHARACTER, INTENT (INOUT) :: A, B
       CHARACTER                 :: TEMP
                  TEMP = A ; A = B ; B = TEMP
       END SUBROUTINE SWAP_C
END MODULE swapMod
