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
           SELECT ARCHIVO-EMPLEADOS ASSIGN TO "EMPLEADOS.TXT"
                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-TRABAJO-EMP ASSIGN TO "TEMP_EMP.TMP"
                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-EMP-ORD ASSIGN TO "EMP_ORDENADO.TXT"
                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-DEPTO ASSIGN TO "DEPTO.TXT"
                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-TRABAJO-DEPTO ASSIGN TO "TEMP_DEPTO.TMP"
                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-DEPTO-ORD ASSIGN TO "DEPTO_ORDENADO.TXT"
                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-REPORTE ASSIGN TO "REPORTE.TXT"
                 ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  ARCHIVO-EMPLEADOS.
       01  REG-EMPLEADO.
           05 EMP-NUMERO             PIC 9(6).
           05 EMP-ID-DEPTO           PIC 9(6).
           05 EMP-NOMBRES            PIC X(15).
           05 EMP-AP-PATERNO         PIC X(14).
           05 EMP-AP-MATERNO         PIC X(13).
           05 EMP-EDAD               PIC 9(4).
           05 EMP-SEXO               PIC X(8).
           05 EMP-UNIVERSIDAD        PIC X(17).

       SD  ARCHIVO-TRABAJO-EMP.
       01  REG-TRABAJO-EMP.
           05 TRAB-EMP-NUMERO        PIC 9(6).
           05 TRAB-EMP-ID-DEPTO      PIC 9(6).
           05 TRAB-EMP-RESTO         PIC X(77).


       FD  ARCHIVO-EMP-ORD.
       01  REG-EMP-ORD.
           05 EMPO-NUMERO            PIC 9(6).
           05 EMPO-ID-DEPTO          PIC 9(6).
           05 EMPO-NOMBRE            PIC X(15).
           05 EMPO-AP-PATERNO        PIC X(14).
           05 EMPO-AP-MATERNO        PIC X(13).
           05 EMPO-EDAD              PIC 9(4).
           05 EMPO-SEXO              PIC X(8).
           05 EMPO-UNIVERSIDAD       PIC X(17).

       FD  ARCHIVO-DEPTO.
       01  REG-DEPTO.
           05 DEP-ID                 PIC 9(6).
           05 DEP-DEPARTAMENTO       PIC X(23).
           05 DEP-CIUDAD             PIC X(30).
           05 DEP-SALARIO            PIC 9(5)V99.

       SD  ARCHIVO-TRABAJO-DEPTO.
       01  REG-TRABAJO-DEPTO.
           05 TRAB-DEP-ID            PIC 9(6).
           05 TRAB-DEP-RESTO         PIC X(60).

       FD  ARCHIVO-DEPTO-ORD.
       01  REG-DEPTO-ORD.
           05 DEPO-ID                 PIC 9(6).
           05 DEPO-DEPARTAMENTO       PIC X(23).
           05 DEPO-CIUDAD             PIC X(30).
           05 DEPO-SALARIO            PIC 9(5)V99.

       FD  ARCHIVO-REPORTE.
       01  LINEA-REPORTE              PIC X(160).

       WORKING-STORAGE SECTION.
       01 WS-EOF-EMP                 PIC X(1) VALUE "N".
       01 WS-EOF-DEPTO               PIC X(1) VALUE "N".
       01 WS-SALARIO-FORMATO         PIC $$$,$$$,$$9.99.
       01 WS-NOMBRE-COMPLETO         PIC X(43).

       01 WS-CONTADORES.
           05 WS-RELACIONADOS        PIC 9(4) VALUE ZERO.

       01 WS-CONT-FORMATO            PIC ZZZ9.


       PROCEDURE DIVISION.
       INICIO.
           SORT ARCHIVO-TRABAJO-EMP
             ON ASCENDING KEY TRAB-EMP-ID-DEPTO
             USING ARCHIVO-EMPLEADOS
             GIVING ARCHIVO-EMP-ORD.

           SORT ARCHIVO-TRABAJO-DEPTO
             ON ASCENDING KEY TRAB-DEP-ID
             USING ARCHIVO-DEPTO
             GIVING ARCHIVO-DEPTO-ORD.

           OPEN INPUT ARCHIVO-EMP-ORD.
           OPEN INPUT ARCHIVO-DEPTO-ORD.
           OPEN OUTPUT ARCHIVO-REPORTE.

           MOVE "--REPORTE RELACIONADO--"
           TO LINEA-REPORTE
           WRITE LINEA-REPORTE.

           MOVE SPACES TO LINEA-REPORTE
           STRING
              "------------------EMPLEADO------------------|"
              "-EDAD-|"
              "----UNIVERSIDAD----|"
              "-------DEPARTAMENTO------|"
              "-------------CIUDAD-------------|"
              "----SALARIO----|"       DELIMITED BY SIZE
           INTO LINEA-REPORTE
           END-STRING
           WRITE LINEA-REPORTE.

           PERFORM LEER-EMPLEADO.
           PERFORM LEER-DEPTO.

           PERFORM UNTIL WS-EOF-EMP = "S" OR WS-EOF-DEPTO = "S"

            EVALUATE TRUE

             WHEN EMPO-ID-DEPTO = DEPO-ID
                 PERFORM ESCRIBIR-COMBINADO
                 PERFORM LEER-EMPLEADO

              WHEN EMPO-ID-DEPTO < DEPO-ID
                 PERFORM LEER-EMPLEADO

              WHEN EMPO-ID-DEPTO > DEPO-ID
                 PERFORM LEER-DEPTO


             END-EVALUATE
           END-PERFORM.

           PERFORM CONTADORES.

           CLOSE ARCHIVO-EMP-ORD.
           CLOSE ARCHIVO-DEPTO-ORD.
           CLOSE ARCHIVO-REPORTE.

           DISPLAY "REPORTE GENERADO".

           STOP RUN.

           LEER-EMPLEADO.
               READ ARCHIVO-EMP-ORD
               AT END
                 MOVE "S" TO WS-EOF-EMP
                 END-READ.

           LEER-DEPTO.
               READ ARCHIVO-DEPTO-ORD
               AT END
                 MOVE "S" TO WS-EOF-DEPTO
                 END-READ.

           ESCRIBIR-COMBINADO.
               MOVE DEPO-SALARIO TO WS-SALARIO-FORMATO.
               STRING EMPO-NOMBRE       DELIMITED BY SIZE
                      "  "              DELIMITED BY SIZE
                      EMPO-AP-PATERNO   DELIMITED BY SIZE
                      "  "              DELIMITED BY SIZE
                      EMPO-AP-MATERNO   DELIMITED BY SIZE
                      "  "              DELIMITED BY SIZE
                      INTO WS-NOMBRE-COMPLETO
                END-STRING


                STRING WS-NOMBRE-COMPLETO DELIMITED BY SIZE
                       " | "              DELIMITED BY SIZE
                       EMPO-EDAD          DELIMITED BY SIZE
                       " | "               DELIMITED BY SIZE
                       EMPO-UNIVERSIDAD   DELIMITED BY SIZE
                       " | "               DELIMITED BY SIZE
                       DEPO-DEPARTAMENTO  DELIMITED BY SIZE
                       " | "              DELIMITED BY SIZE
                       DEPO-CIUDAD        DELIMITED BY SIZE
                       " | "              DELIMITED BY SIZE
                       WS-SALARIO-FORMATO DELIMITED BY SIZE
                       INTO LINEA-REPORTE
                END-STRING
                WRITE LINEA-REPORTE.

                ADD 1 TO WS-RELACIONADOS.

            CONTADORES.
                MOVE "ESTADISTICAS" TO LINEA-REPORTE.
                WRITE LINEA-REPORTE.

       MOVE WS-RELACIONADOS TO WS-CONT-FORMATO.
       STRING "EMPLEADOS COMBINADOS CORRECTAMENTE:  " DELIMITED BY SIZE
                       WS-CONT-FORMATO DELIMITED BY SIZE
                       INTO LINEA-REPORTE
                END-STRING
                WRITE LINEA-REPORTE.
