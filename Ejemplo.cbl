      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJEMPLO.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 TABLA-MESES.
               05 MES-ITEM PIC X(10) OCCURS 12.
       PROCEDURE DIVISION.
           MOVE 'ENERO' TO MES-ITEM(1).
           MOVE'FEBRERO' TO MES-ITEM(2).
           MOVE'MARZO' TO MES-ITEM(3).
           MOVE'ABRIL' TO MES-ITEM(4).
           MOVE'MAYO' TO MES-ITEM(5).
           DISPLAY TABLA-MESES.
       END PROGRAM EJEMPLO.
