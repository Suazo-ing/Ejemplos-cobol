      ******************************************************************
      * Author: CARLOS-SUAZO
      * Date: 14/08/2026
      * Purpose: ACADEMIC
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTE-EMPRESAS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-CURP ASSIGN TO "CURP.TXT"
                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-RFC ASSIGN TO "RFC.TXT"
                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-REPORTE ASSIGN TO "REPORTE_EMPRESAS.TXT"
                 ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARCHIVO-CURP.
       01  REG-CURP.
           05 CURP-CODIGO             PIC X(18).
           05 FILLER                  PIC X(1).
           05 CURP-NOMBRE             PIC X(13).
           05 CURP-APELLIDO           PIC X(14).
           05 CURP-EDAD               PIC X(2).
           05 FILLER                  PIC X(1).
           05 CURP-PUESTO             PIC X(17).
           05 CURP-RFC-LISTA          PIC X(45).

       FD  ARCHIVO-RFC.
       01  REG-RFC.
           05 RFC-CODIGO              PIC X(12).
           05 FILLER                  PIC X(1).
           05 RFC-NOMBRE              PIC X(20).
           05 RFC-TIPO                PIC X(20).
           05 RFC-PAIS                PIC X(18).
           05 RFC-GIRO                PIC X(20).

       FD  ARCHIVO-REPORTE.
       01  LINEA-REPORTE              PIC X(200).

       WORKING-STORAGE SECTION.
       01 WS-BANDERAS.
           05 WS-EOF-CURP             PIC X(1) VALUE "N".
           05 WS-EOF-RFC              PIC X(1) VALUE "N".

       01 WS-TABLA-EMPLEADOS.
           05 WS-EMPLEADO OCCURS 50 TIMES.
               10 WS-EMP-CURP         PIC X(18).
               10 WS-EMP-NOMBRE-COMP  PIC X(30).
               10 WS-EMP-NUM-RFC      PIC 9(2) VALUE ZERO.
               10 WS-EMP-RFC-TABLA OCCURS 5 TIMES
                                      PIC X(12).

       01 WS-NUM-EMPLEADOS            PIC 9(3) VALUE ZERO.

       01 WS-INDICES.
           05 WS-I                    PIC 9(3).
           05 WS-J                    PIC 9(2).

       01 WS-EMPLEADOS-EMPRESA        PIC X(200).
       01 WS-EMPLEADOS-EMPRESA-TEMP   PIC X(200).
       01 WS-NUM-EMP-EMPRESA          PIC 9(2) VALUE ZERO.

       01 WS-CONTADORES.
           05 WS-TOTAL-EMPRESAS       PIC 9(4) VALUE ZERO.
           05 WS-EMPRESAS-SIN-EMP     PIC 9(4) VALUE ZERO.
           05 WS-TOTAL-RELACIONES     PIC 9(4) VALUE ZERO.

       01 WS-CONT-FORMATO             PIC ZZZ9.
       01 WS-CONT-EMPRESA             PIC 99 VALUE ZERO.

       01 WS-DETALLE.
           05 WS-LINEA-DOBLE          PIC X(78) VALUE ALL "=".
           05 WS-LINEA-SIMPLE         PIC X(78) VALUE ALL "-".

       PROCEDURE DIVISION.
       INICIO.
           OPEN INPUT ARCHIVO-CURP.
           OPEN INPUT  ARCHIVO-RFC.
           OPEN OUTPUT ARCHIVO-REPORTE.

       PERFORM UNTIL WS-EOF-CURP = "S"
           READ ARCHIVO-CURP
           AT END
           MOVE "S" TO WS-EOF-CURP
           NOT AT END
       PERFORM AGREGAR-EMPLEADO-TABLA
           END-READ
       END-PERFORM.
           CLOSE ARCHIVO-CURP.

           MOVE WS-LINEA-DOBLE TO LINEA-REPORTE
           WRITE LINEA-REPORTE.
           MOVE "          REPORTE DE EMPRESAS Y EMPLEADOS RELACIONADOS"
               TO LINEA-REPORTE
           WRITE LINEA-REPORTE.
           MOVE WS-LINEA-DOBLE TO LINEA-REPORTE
           WRITE LINEA-REPORTE.
           MOVE SPACES TO LINEA-REPORTE
           WRITE LINEA-REPORTE.

       PERFORM LEER-RFC.
       PERFORM UNTIL WS-EOF-RFC = "S"
       PERFORM PROCESAR-EMPRESA
       PERFORM LEER-RFC
       END-PERFORM.

       PERFORM CONTADORES.

           CLOSE ARCHIVO-RFC
                 ARCHIVO-REPORTE.

           DISPLAY "REPORTE GENERADO".
           STOP RUN.

       LEER-RFC.
           READ ARCHIVO-RFC
           AT END
           MOVE "S" TO WS-EOF-RFC
           END-READ.

       AGREGAR-EMPLEADO-TABLA.
           ADD 1 TO WS-NUM-EMPLEADOS

           MOVE CURP-CODIGO TO WS-EMP-CURP(WS-NUM-EMPLEADOS)

           MOVE SPACES TO WS-EMP-NOMBRE-COMP(WS-NUM-EMPLEADOS)
           STRING FUNCTION TRIM(CURP-NOMBRE)   DELIMITED BY SIZE
                  " "                          DELIMITED BY SIZE
           FUNCTION TRIM(CURP-APELLIDO) DELIMITED BY SIZE
           INTO WS-EMP-NOMBRE-COMP(WS-NUM-EMPLEADOS)
           END-STRING

           UNSTRING CURP-RFC-LISTA DELIMITED BY ","
           INTO WS-EMP-RFC-TABLA(WS-NUM-EMPLEADOS, 1)
                WS-EMP-RFC-TABLA(WS-NUM-EMPLEADOS, 2)
                WS-EMP-RFC-TABLA(WS-NUM-EMPLEADOS, 3)
                WS-EMP-RFC-TABLA(WS-NUM-EMPLEADOS, 4)
                WS-EMP-RFC-TABLA(WS-NUM-EMPLEADOS, 5)
           TALLYING IN WS-EMP-NUM-RFC(WS-NUM-EMPLEADOS)
           END-UNSTRING.

       PROCESAR-EMPRESA.
           ADD 1 TO WS-CONT-EMPRESA
           MOVE SPACES TO WS-EMPLEADOS-EMPRESA
           MOVE ZERO   TO WS-NUM-EMP-EMPRESA

       PERFORM VARYING WS-I FROM 1 BY 1
           UNTIL WS-I > WS-NUM-EMPLEADOS

       PERFORM VARYING WS-J FROM 1 BY 1
           UNTIL WS-J > WS-EMP-NUM-RFC(WS-I)

           IF WS-EMP-RFC-TABLA(WS-I, WS-J) = RFC-CODIGO
       PERFORM AGREGAR-EMPLEADO-A-LISTA
           ADD 1 TO WS-NUM-EMP-EMPRESA
           ADD 1 TO WS-TOTAL-RELACIONES
       EXIT PERFORM
           END-IF

       END-PERFORM

       END-PERFORM.

           ADD 1 TO WS-TOTAL-EMPRESAS
           IF WS-NUM-EMP-EMPRESA = 0
           ADD 1 TO WS-EMPRESAS-SIN-EMP
           END-IF

       PERFORM ESCRIBIR-EMPRESA.

       AGREGAR-EMPLEADO-A-LISTA.
           IF WS-EMPLEADOS-EMPRESA = SPACES
              STRING FUNCTION TRIM(WS-EMP-NOMBRE-COMP(WS-I))
              DELIMITED BY SIZE
              INTO WS-EMPLEADOS-EMPRESA
              END-STRING
           ELSE
              MOVE WS-EMPLEADOS-EMPRESA
                   TO WS-EMPLEADOS-EMPRESA-TEMP
              MOVE SPACES TO WS-EMPLEADOS-EMPRESA
              STRING FUNCTION TRIM(WS-EMPLEADOS-EMPRESA-TEMP)
              DELIMITED BY SIZE
              ", "                       DELIMITED BY SIZE
              FUNCTION TRIM(WS-EMP-NOMBRE-COMP(WS-I))
              DELIMITED BY SIZE
              INTO WS-EMPLEADOS-EMPRESA
              END-STRING
           END-IF.

       ESCRIBIR-EMPRESA.
           MOVE SPACES TO LINEA-REPORTE
           STRING "["                    DELIMITED BY SIZE
                  WS-CONT-EMPRESA        DELIMITED BY SIZE
                  "] "                   DELIMITED BY SIZE
           FUNCTION TRIM(RFC-NOMBRE)     DELIMITED BY SIZE
                 "  (RFC: "              DELIMITED BY SIZE
                 RFC-CODIGO              DELIMITED BY SIZE
                 ")"                     DELIMITED BY SIZE
           INTO LINEA-REPORTE
           END-STRING
           WRITE LINEA-REPORTE.

           MOVE SPACES TO LINEA-REPORTE
           STRING "     Tipo : "         DELIMITED BY SIZE
           FUNCTION TRIM(RFC-TIPO)       DELIMITED BY SIZE
                  "     Pais : "         DELIMITED BY SIZE
           FUNCTION TRIM(RFC-PAIS)       DELIMITED BY SIZE
                  "     Giro : "         DELIMITED BY SIZE
           FUNCTION TRIM(RFC-GIRO)       DELIMITED BY SIZE
           INTO LINEA-REPORTE
           END-STRING
           WRITE LINEA-REPORTE.

           MOVE SPACES TO LINEA-REPORTE
           IF WS-NUM-EMP-EMPRESA = 0
           STRING "     Empleados : (sin relacionados)"
           DELIMITED BY SIZE
           INTO LINEA-REPORTE
           END-STRING
           ELSE
           STRING "     Empleados ("   DELIMITED BY SIZE
                  WS-NUM-EMP-EMPRESA   DELIMITED BY SIZE
                  ") : "               DELIMITED BY SIZE
           FUNCTION TRIM(WS-EMPLEADOS-EMPRESA)
                                       DELIMITED BY SIZE
           INTO LINEA-REPORTE
           END-STRING
           END-IF
           WRITE LINEA-REPORTE.

           MOVE SPACES TO LINEA-REPORTE
           WRITE LINEA-REPORTE.
           MOVE WS-LINEA-SIMPLE TO LINEA-REPORTE
           WRITE LINEA-REPORTE.
           MOVE SPACES TO LINEA-REPORTE
           WRITE LINEA-REPORTE.

       CONTADORES.
           MOVE WS-LINEA-DOBLE TO LINEA-REPORTE
           WRITE LINEA-REPORTE.
           MOVE "          ESTADISTICAS" TO LINEA-REPORTE.
           WRITE LINEA-REPORTE.
           MOVE WS-LINEA-DOBLE TO LINEA-REPORTE
           WRITE LINEA-REPORTE.
           MOVE SPACES TO LINEA-REPORTE
           WRITE LINEA-REPORTE.

           MOVE WS-TOTAL-EMPRESAS TO WS-CONT-FORMATO
           MOVE SPACES TO LINEA-REPORTE
           STRING "     Empresas procesadas   : "
                                       DELIMITED BY SIZE
                  WS-CONT-FORMATO      DELIMITED BY SIZE
           INTO LINEA-REPORTE
           END-STRING
           WRITE LINEA-REPORTE.

           MOVE WS-EMPRESAS-SIN-EMP TO WS-CONT-FORMATO
           MOVE SPACES TO LINEA-REPORTE
           STRING "     Empresas sin empleados: "
                                      DELIMITED BY SIZE
                  WS-CONT-FORMATO     DELIMITED BY SIZE
           INTO LINEA-REPORTE
           END-STRING
           WRITE LINEA-REPORTE.

           MOVE WS-TOTAL-RELACIONES TO WS-CONT-FORMATO
           MOVE SPACES TO LINEA-REPORTE
           STRING "     Relaciones empleado-empresa: "
                                     DELIMITED BY SIZE
                  WS-CONT-FORMATO    DELIMITED BY SIZE
           INTO LINEA-REPORTE
           END-STRING
           WRITE LINEA-REPORTE.
       END-PROGRAM.
