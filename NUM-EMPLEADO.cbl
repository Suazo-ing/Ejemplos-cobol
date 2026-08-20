      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTE-EMPLEADOS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-EMPLEADOS ASSIGN TO "EMPL.TXT"
                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-TRABAJO-EMP ASSIGN TO "TEMP_EMPL.TMP"
                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-EMP-ORD ASSIGN TO "EMPL_ORDENADO.TXT"
                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-NUM ASSIGN TO "NUM_EMPL.TXT"
                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-TRABAJO-NUM ASSIGN TO "TEMP_NUM_EM.TMP"
                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-NUM-ORD ASSIGN TO "NUM_EMPL_ORD.TXT"
                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-REPORTE ASSIGN TO "REPORTE2.TXT"
                 ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  ARCHIVO-EMPLEADOS.
       01  REG-EMPLEADO.
           05 EMP-NUMERO             PIC X(10).
           05 EMP-NOMBRES            PIC X(15).
           05 EMP-AP-PATERNO         PIC X(13).
           05 EMP-AP-MATERNO         PIC X(14).

       SD  ARCHIVO-TRABAJO-EMP.
       01  REG-TRABAJO-EMP.
           05 TRAB-EMP-NUMERO        PIC X(10).
           05 TRAB-EMP-RESTO         PIC X(42).

       FD  ARCHIVO-EMP-ORD.
       01  REG-EMP-ORD.
           05 EMPO-NUMERO            PIC X(10).
           05 EMPO-NOMBRES           PIC X(15).
           05 EMPO-AP-PATERNO        PIC X(13).
           05 EMPO-AP-MATERNO        PIC X(14).

       FD  ARCHIVO-NUM.
       01  REG-DEPTO.
           05 NUM-EMP                 PIC X(9).
           05 NUM-TEL                 PIC X(13).

       SD  ARCHIVO-TRABAJO-NUM.
       01  REG-TRABAJO-DEPTO.
           05 TRAB-NUM-EMPL           PIC X(9).
           05 TRAB-NUM-TEL            PIC X(13).

       FD  ARCHIVO-NUM-ORD.
       01  REG-DEPTO-ORD.
           05 NUMO-EMPL               PIC X(9).
           05 NUMO-TEL                PIC X(13).

       FD  ARCHIVO-REPORTE.
       01  LINEA-REPORTE              PIC X(160).

       WORKING-STORAGE SECTION.
       01 WS-EOF-EMP                 PIC X(1) VALUE "N".
       01 WS-EOF-NUM                 PIC X(1) VALUE "N".
       01 WS-NOMBRE-COMPLETO         PIC X(60).
       01 WS-TELEFONOS-EMPLEADO      PIC X(100).
       01 WS-TELEFONOS-TEMP          PIC X(100).

       01 WS-TABLA-TELEFONOS.
           05 WS-TEL-OCU OCCURS 20 TIMES  PIC X(13).
       01 WS-NUM-TELEFONOS           PIC 9(2) VALUE ZERO.
       01 WS-SUB                     PIC 9(2) VALUE ZERO.

       01 WS-CONTADORES.
           05 WS-RELACIONADOS        PIC 9(4) VALUE ZERO.
           05 WS-TEL-SIN-EMPLEADO    PIC 9(4) VALUE ZERO.
           05 WS-TELS-RELACIONADOS   PIC 9(4) VALUE ZERO.

       01 WS-CONT-FORMATO            PIC ZZZ9.
       01 WS-EMPLEADO-ENCONTRADO     PIC X VALUE "N".
       01 WS-TEL-FORMATO             PIC ZZZ9.

       PROCEDURE DIVISION.
       INICIO.
           SORT ARCHIVO-TRABAJO-EMP
             ON ASCENDING KEY TRAB-EMP-NUMERO
             USING ARCHIVO-EMPLEADOS
             GIVING ARCHIVO-EMP-ORD.

           SORT ARCHIVO-TRABAJO-NUM
             ON ASCENDING KEY TRAB-NUM-EMPL
             USING ARCHIVO-NUM
             GIVING ARCHIVO-NUM-ORD.

           OPEN INPUT  ARCHIVO-EMP-ORD.
           OPEN INPUT  ARCHIVO-NUM-ORD.
           OPEN OUTPUT ARCHIVO-REPORTE.

           MOVE "--REPORTE DE EMPLEADOS Y TELEFONOS--"
               TO LINEA-REPORTE
           WRITE LINEA-REPORTE.

           MOVE SPACES TO LINEA-REPORTE

           STRING
               "EMPLEADO             NOMBRE COMPLETO"
               "                         TELEFONO"
               DELIMITED BY SIZE
               INTO LINEA-REPORTE
           END-STRING

           WRITE LINEA-REPORTE.

           PERFORM LEER-EMPLEADO.
           PERFORM LEER-NUMERO.

           PERFORM UNTIL WS-EOF-EMP = "S"
               PERFORM PROCESAR-EMPLEADO
           END-PERFORM.

           PERFORM REPORTE-TELEFONOS-SIN-EMPLEADO.

           PERFORM CONTADORES.

           CLOSE ARCHIVO-EMP-ORD
                 ARCHIVO-NUM-ORD
                 ARCHIVO-REPORTE.

           DISPLAY "REPORTE GENERADO".

           STOP RUN.

           LEER-EMPLEADO.
               READ ARCHIVO-EMP-ORD
                   AT END
                       MOVE "S" TO WS-EOF-EMP
               END-READ.

           LEER-NUMERO.
               READ ARCHIVO-NUM-ORD
                   AT END
                       MOVE "S" TO WS-EOF-NUM
               END-READ.

           PROCESAR-EMPLEADO.
               MOVE "N" TO WS-EMPLEADO-ENCONTRADO.
               MOVE SPACES TO WS-NOMBRE-COMPLETO
               MOVE ZERO   TO WS-NUM-TELEFONOS
               STRING
                   EMPO-NOMBRES        DELIMITED BY SPACE
                   " "                 DELIMITED BY SIZE
                   EMPO-AP-PATERNO     DELIMITED BY SPACE
                   " "                 DELIMITED BY SIZE
                   EMPO-AP-MATERNO     DELIMITED BY SPACE
                   INTO WS-NOMBRE-COMPLETO
               END-STRING.

               PERFORM UNTIL WS-EOF-NUM = "S"
                   EVALUATE TRUE

                       WHEN NUMO-EMPL = EMPO-NUMERO
                           PERFORM CARGAR-TELEFONO-ARREGLO
                           MOVE "S" TO WS-EMPLEADO-ENCONTRADO
                           PERFORM LEER-NUMERO

                       WHEN NUMO-EMPL < EMPO-NUMERO
                           PERFORM LEER-NUMERO

                       WHEN OTHER
                           EXIT PERFORM

                   END-EVALUATE
               END-PERFORM.

               IF WS-EMPLEADO-ENCONTRADO = "S"
                   ADD 1 TO WS-RELACIONADOS
                   PERFORM ESCRIBIR-COMBINADO
               END-IF.

               PERFORM LEER-EMPLEADO.

           CARGAR-TELEFONO-ARREGLO.
               IF WS-NUM-TELEFONOS < 20
                   ADD 1 TO WS-NUM-TELEFONOS
                   MOVE NUMO-TEL TO WS-TEL-OCU(WS-NUM-TELEFONOS)
               END-IF
               ADD 1 TO WS-TELS-RELACIONADOS.

           ESCRIBIR-COMBINADO.
               MOVE SPACES TO WS-TELEFONOS-EMPLEADO

               PERFORM VARYING WS-SUB FROM 1 BY 1
                       UNTIL WS-SUB > WS-NUM-TELEFONOS

                   IF WS-SUB = 1
                       STRING WS-TEL-OCU(WS-SUB) DELIMITED BY SPACE
                           INTO WS-TELEFONOS-EMPLEADO
                       END-STRING
                   ELSE
                       MOVE WS-TELEFONOS-EMPLEADO TO WS-TELEFONOS-TEMP
                       MOVE SPACES TO WS-TELEFONOS-EMPLEADO
                       STRING FUNCTION TRIM(WS-TELEFONOS-TEMP)
                                  DELIMITED BY SIZE
                              ", "                 DELIMITED BY SIZE
                              WS-TEL-OCU(WS-SUB)   DELIMITED BY SPACE
                           INTO WS-TELEFONOS-EMPLEADO
                       END-STRING
                   END-IF

               END-PERFORM.

               MOVE SPACES TO LINEA-REPORTE
               STRING EMPO-NUMERO         DELIMITED BY SIZE
                      " | "               DELIMITED BY SIZE
                      WS-NOMBRE-COMPLETO  DELIMITED BY SIZE
                      " | "               DELIMITED BY SIZE
                      FUNCTION TRIM(WS-TELEFONOS-EMPLEADO)
                          DELIMITED BY SIZE
                      INTO LINEA-REPORTE
               END-STRING
               WRITE LINEA-REPORTE.

           REPORTE-TELEFONOS-SIN-EMPLEADO.
               MOVE SPACES TO LINEA-REPORTE
               WRITE LINEA-REPORTE.
               MOVE
           "-----------------------------------------------------------"
                   TO LINEA-REPORTE
               WRITE LINEA-REPORTE.

               MOVE "TELEFONOS SIN EMPLEADO ASIGNADO"
                   TO LINEA-REPORTE
               WRITE LINEA-REPORTE.

               MOVE
           "-----------------------------------------------------------"
                   TO LINEA-REPORTE
               WRITE LINEA-REPORTE.

               PERFORM UNTIL WS-EOF-NUM = "S"
                   IF NUMO-EMPL = "ZZZZZZZZ"
                       MOVE SPACES TO LINEA-REPORTE
                       STRING NUMO-TEL DELIMITED BY SIZE
                           INTO LINEA-REPORTE
                       END-STRING
                       WRITE LINEA-REPORTE
                       ADD 1 TO WS-TEL-SIN-EMPLEADO
                   END-IF
                   PERFORM LEER-NUMERO
               END-PERFORM.

           CONTADORES.
               MOVE "ESTADISTICAS" TO LINEA-REPORTE.
               WRITE LINEA-REPORTE.

               MOVE WS-RELACIONADOS
                   TO WS-CONT-FORMATO.
               MOVE SPACES TO LINEA-REPORTE
               STRING "EMPLEADOS CON TELEFONO: "  DELIMITED BY SIZE
                       WS-CONT-FORMATO            DELIMITED BY SIZE
                   INTO LINEA-REPORTE
               END-STRING
               WRITE LINEA-REPORTE.

               MOVE WS-TELS-RELACIONADOS
                   TO WS-TEL-FORMATO.
               MOVE SPACES TO LINEA-REPORTE
               STRING   "TELEFONOS RELACIONADOS : " DELIMITED BY SIZE
                         WS-TEL-FORMATO             DELIMITED BY SIZE
                   INTO LINEA-REPORTE
               END-STRING
               WRITE LINEA-REPORTE.

               MOVE WS-TEL-SIN-EMPLEADO
                   TO WS-TEL-FORMATO.
               MOVE SPACES TO LINEA-REPORTE
               STRING  "TELEFONOS SIN EMPLEADO : " DELIMITED BY SIZE
                        WS-TEL-FORMATO             DELIMITED BY SIZE
                   INTO LINEA-REPORTE
               END-STRING
               WRITE LINEA-REPORTE.
