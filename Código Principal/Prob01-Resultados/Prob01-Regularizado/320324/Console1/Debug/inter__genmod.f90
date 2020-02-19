        !COMPILER-GENERATED INTERFACE MODULE: Wed Feb 15 14:36:50 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INTER__genmod
          INTERFACE 
            SUBROUTINE INTER(FI,DFI,KODE,XXC,YYC,X,Y,SOL,NOP,DSOLX,DSOLY&
     &,LOCC,A,B,CB,XX,YY,NX)
              INTEGER(KIND=4) :: NX
              REAL(KIND=8) :: FI(NX)
              REAL(KIND=8) :: DFI(NX)
              INTEGER(KIND=4) :: KODE(NX)
              REAL(KIND=8) :: XXC(NX)
              REAL(KIND=8) :: YYC(NX)
              REAL(KIND=8) :: X(NX)
              REAL(KIND=8) :: Y(NX)
              REAL(KIND=8) :: SOL(NX)
              INTEGER(KIND=4) :: NOP(NX,3)
              REAL(KIND=8) :: DSOLX(NX)
              REAL(KIND=8) :: DSOLY(NX)
              INTEGER(KIND=4) :: LOCC(NX)
              REAL(KIND=8) :: A(NX,NX)
              REAL(KIND=8) :: B(NX,NX)
              REAL(KIND=8) :: CB(NX)
              REAL(KIND=8) :: XX(NX)
              REAL(KIND=8) :: YY(NX)
            END SUBROUTINE INTER
          END INTERFACE 
        END MODULE INTER__genmod
