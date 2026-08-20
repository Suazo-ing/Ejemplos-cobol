      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATRICES.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-TABLA1.
           05 FILA-A OCCURS 10 TIMES.
              10 COL-A                   PIC 9(2) OCCURS 10 TIMES.

       01  WS-TABLA2.
           05 FILA-B OCCURS 10 TIMES.
              10 COL-B                   PIC 9(2) OCCURS 10 TIMES.

       01  WS-TABLA3.
           05 FILA-C OCCURS 10 TIMES.
              10 COL-C                   PIC 9(2) OCCURS 10 TIMES.

       01  INDICES.
           05 FILA                       PIC 9 VALUE ZERO.
           05 COLUMNA                    PIC 9 VALUE ZERO.
           05 ESC                        PIC 9 VALUE ZERO.

       PROCEDURE DIVISION.
       INICIO.
       PERFORM 1000-CAPTURA-MATRIZ-A
       PERFORM 2000-CAPTURA-MATRIZ-B
       PERFORM 3000-MULTIPLICA-MATRICES
       PERFORM 4000-MOSTRAR-MATRIZ-C
           STOP RUN.

       1000-CAPTURA-MATRIZ-A.
           DISPLAY "INGRESA VALORES PARA TU MATRIZ A"
       PERFORM VARYING FILA FROM 1 BY 1 UNTIL FILA > 2
           PERFORM VARYING COLUMNA FROM 1 BY 1 UNTIL COLUMNA > 2
           DISPLAY "A(" FILA "," COLUMNA "): "
           ACCEPT COL-A(FILA,COLUMNA)
           END-PERFORM
       END-PERFORM.

       2000-CAPTURA-MATRIZ-B.
           DISPLAY "INGRESA VALORES PARA TU MATRIZ B"
       PERFORM VARYING FILA FROM 1 BY 1 UNTIL FILA > 2
           PERFORM VARYING COLUMNA FROM 1 BY 1 UNTIL COLUMNA > 2
           DISPLAY "B(" FILA "," COLUMNA "): "
           ACCEPT COL-B(FILA,COLUMNA)
           END-PERFORM
       END-PERFORM.

       3000-MULTIPLICA-MATRICES.
       PERFORM VARYING FILA FROM 1 BY 1 UNTIL FILA > 2
        PERFORM VARYING COLUMNA FROM 1 BY 1 UNTIL COLUMNA > 2
         PERFORM VARYING ESC FROM 1 BY 1 UNTIL ESC > 2
           COMPUTE COL-C(FILA,COLUMNA) = COL-C(FILA,COLUMNA) +
                   (COL-A(FILA,ESC) * COL-B(ESC,COLUMNA))
         END-PERFORM
        END-PERFORM
       END-PERFORM.

       4000-MOSTRAR-MATRIZ-C.
           DISPLAY "========================="
           DISPLAY "---MATRIZ RESULTANTE C---"
           DISPLAY "========================="
       PERFORM VARYING FILA FROM 1 BY 1 UNTIL FILA > 2
           DISPLAY "( " COL-C(FILA,1) ", " COL-C(FILA,2) " )"
       END-PERFORM.
       END PROGRAM MATRICES.
