        !COMPILER-GENERATED INTERFACE MODULE: Tue Feb 21 13:00:37 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTPUT__genmod
          INTERFACE 
            SUBROUTINE OUTPUT(X,Y,FI,DFI,XXC,YYC,SOL,DSOLX,DSOLY,NX,SAI,&
     &TEMPO,OMEGA)
              INTEGER(KIND=4) :: NX
              REAL(KIND=8) :: X(NX)
              REAL(KIND=8) :: Y(NX)
              REAL(KIND=8) :: FI(NX)
              REAL(KIND=8) :: DFI(NX)
              REAL(KIND=8) :: XXC(NX)
              REAL(KIND=8) :: YYC(NX)
              REAL(KIND=8) :: SOL(NX)
              REAL(KIND=8) :: DSOLX(NX)
              REAL(KIND=8) :: DSOLY(NX)
              CHARACTER(LEN=20) :: SAI
              REAL(KIND=8) :: TEMPO
              REAL(KIND=8) :: OMEGA
            END SUBROUTINE OUTPUT
          END INTERFACE 
        END MODULE OUTPUT__genmod
