        !COMPILER-GENERATED INTERFACE MODULE: Tue Feb 21 13:00:36 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INPUT__genmod
          INTERFACE 
            SUBROUTINE INPUT(XXC,YYC,X,Y,KODE,FI,NOP,LOCC,IDUP,NX,SAI,  &
     &ARQENT,OMEGA)
              INTEGER(KIND=4) :: NX
              REAL(KIND=8) :: XXC(NX)
              REAL(KIND=8) :: YYC(NX)
              REAL(KIND=8) :: X(NX)
              REAL(KIND=8) :: Y(NX)
              INTEGER(KIND=4) :: KODE(NX)
              REAL(KIND=8) :: FI(NX)
              INTEGER(KIND=4) :: NOP(NX,3)
              INTEGER(KIND=4) :: LOCC(NX)
              INTEGER(KIND=4) :: IDUP(NX)
              CHARACTER(LEN=20) :: SAI
              CHARACTER(LEN=20) :: ARQENT
              REAL(KIND=8) :: OMEGA
            END SUBROUTINE INPUT
          END INTERFACE 
        END MODULE INPUT__genmod
