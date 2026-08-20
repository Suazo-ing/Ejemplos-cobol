      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALARIO.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-NOMBRE            PIC X(20).
       01  WS-EDAD              PIC 9(2).
       01  WS-SALARIO           PIC $$$,$$$,$$9.99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "INGRESA TU NOMBRE".
            ACCEPT WS-NOMBRE.

            DISPLAY "INGRESA TU EDAD".
            ACCEPT WS-EDAD.

            DISPLAY "INGRESA TU SALARIO".
            ACCEPT WS-SALARIO.

            DISPLAY "HOLA" WS-NOMBRE.
            DISPLAY "TU EDAD ES" WS-EDAD "AÑOS".
            DISPLAY "TU SALARIO ES" WS-SALARIO "PESOS".
            STOP RUN.
