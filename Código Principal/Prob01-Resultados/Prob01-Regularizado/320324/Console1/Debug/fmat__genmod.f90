        !COMPILER-GENERATED INTERFACE MODULE: Wed Feb 15 14:36:50 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE FMAT__genmod
          INTERFACE 
            SUBROUTINE FMAT(X,Y,G,H,FI,DFI,KODE,NOP,IDUP,A,B,CB,XX,YY,  &
     &XXC,YYC,NX,OMEGA)
              INTEGER(KIND=4) :: NX
              REAL(KIND=8) :: X(NX)
              REAL(KIND=8) :: Y(NX)
              REAL(KIND=8) :: G(NX,NX)
              REAL(KIND=8) :: H(NX,NX)
              REAL(KIND=8) :: FI(NX)
              REAL(KIND=8) :: DFI(NX)
              INTEGER(KIND=4) :: KODE(NX)
              INTEGER(KIND=4) :: NOP(NX,3)
              INTEGER(KIND=4) :: IDUP(NX)
              REAL(KIND=8) :: A(NX,NX)
              REAL(KIND=8) :: B(NX,NX)
              REAL(KIND=8) :: CB(NX)
              REAL(KIND=8) :: XX(NX)
              REAL(KIND=8) :: YY(NX)
              REAL(KIND=8) :: XXC(NX)
              REAL(KIND=8) :: YYC(NX)
              REAL(KIND=8) :: OMEGA
            END SUBROUTINE FMAT
          END INTERFACE 
        END MODULE FMAT__genmod
