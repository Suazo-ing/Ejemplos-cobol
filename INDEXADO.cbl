      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INDEXADO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT ARCHIVO-PRODUCTOS ASSIGN TO "PRODUCT_19082026.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-ESTADO-TXT.

       SELECT ARCHIVO-INDEXADO ASSIGN TO "PRODUCTOS-INDEX.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CODPRODU
           FILE STATUS IS WS-ESTADO-ARCHIVO.

       DATA DIVISION.
       FILE SECTION.

       FD ARCHIVO-PRODUCTOS.
       01 PRODUCTOS.
           05 CODPRODU1          PIC X(30).
           05 FECPAGO1           PIC X(10).
           05 BASECALC1          PIC X(20).
           05 FRECPAG1           PIC X(20).
           05 TXTCUPON1          PIC X(50).
           05 MULTFASE1          PIC XX.
           05 TIPOPCIO1          PIC X(8).
           05 FECHOPCI1          PIC X(22).
           05 PERPETUA1          PIC XX.
           05 PORCENCU1          PIC X(7) DISPLAY.
           05 PORCENTI1          PIC X(7) DISPLAY.


       FD ARCHIVO-INDEXADO.
       01 PRODUCTO.
           05 CODPRODU                PIC X(30).
           05 FECINI                  PIC X(10).
           05 HORINI                  PIC X(8).
           05 HORFIN                  PIC X(8).
           05 FECPAGO                 PIC X(10).
           05 IMPMINSU                PIC X(50).
           05 IMPMAXSU                PIC X(50).
           05 TIPCUPON                PIC XX.
           05 BASECALC                PIC X(20).
           05 FRECPAG                 PIC X(20).
           05 CURVAFLO                PIC X(25).
           05 DESTEMI                 PIC X(150).
           05 TXTCUPON                PIC X(50).
           05 ORDIRREV                PIC XX.
           05 MULTFASE                PIC XX.
           05 NUMFASE                 PIC XX.
           05 TIPOPCIO                PIC X(8).
           05 FECHOPCI                PIC X(20).
           05 PERPETUA                PIC XX.
           05 MAKWHOLE                PIC XX.
           05 FECMAWHO                PIC X(20).
           05 PORCENCU                PIC S9(4)V9(3) DISPLAY.
           05 PORCENTI                PIC S9(4)V9(3) DISPLAY.

       WORKING-STORAGE SECTION.
       01  BUSQUEDA.
           05 WS-LLAVE-BUSQUEDA       PIC X(30).

       01 BANDERAS.

           05 WS-MENU-OPCION      PIC 9 VALUE ZERO.
           05 WS-ESTADO-ARCHIVO   PIC XX VALUE "00".
           05 WS-ESTADO-TXT       PIC XX VALUE "00".
           05 WS-FIN-PROG         PIC X VALUE "S".
           05 WS-FIN-LEC          PIC X VALUE "N".
           05 WS-FIN-TXT          PIC X VALUE "N".

       01 CONTADORES.
           05 WS-REGISTROS-LEIDOS       PIC 9(5) VALUE ZERO.
           05 WS-REGISTROS-ACTUALIZADOS PIC 9(5) VALUE ZERO.
           05 WS-NO-ENCONTRADOS         PIC 9(5) VALUE ZERO.

       01 FORMATO.
           05 WS-LINEA-REPORTE        PIC X(120).
           05 WS-PORCENTAJE1          PIC S9(4)V9(3).
           05 WS-PORCENTAJE2          PIC S9(4)V9(3).
           05 WS-PORC-CUPON-EDITADO   PIC ZZZ9.999.
           05 WS-PORC-TIR-EDITADO     PIC ZZZ9.999.

       PROCEDURE DIVISION.
       INICIO.
           OPEN I-O ARCHIVO-INDEXADO

           IF WS-ESTADO-ARCHIVO NOT = "00"
               DISPLAY "ERROR AL CREAR EL ARCHIVO."
               DISPLAY "STATUS: " WS-ESTADO-ARCHIVO
               STOP RUN
           END-IF.

           PERFORM UNTIL WS-MENU-OPCION = 4
             DISPLAY "MENU DEL SISTEMA"
             DISPLAY "SELECCIONE LA ACCION"
             DISPLAY "1. AGREGAR PRODUCTO"
             DISPLAY "2. VER LOS PRODUCTOS"
             DISPLAY "3. ACTUALIZAR PRODUCTO"
             DISPLAY "4. SALIR"
            ACCEPT WS-MENU-OPCION
             IF WS-MENU-OPCION = 1
                PERFORM CAPTURAR-PRODUCTO
             ELSE IF WS-MENU-OPCION = 2
                 PERFORM MOSTRAR-PRODUCTO
             ELSE IF WS-MENU-OPCION = 3
                 PERFORM ACTUALIZAR-PRODUCTOS
             ELSE IF WS-MENU-OPCION = 4
                 DISPLAY "SALIENDO DEL PROGRAMA..."
             ELSE
                 DISPLAY "OPCION INVALIDA, SELECCION 1,2,3 o 4"
             END-IF
           END-PERFORM

           CLOSE ARCHIVO-INDEXADO.

           DISPLAY " "
           DISPLAY "Proceso terminado."
           DISPLAY "Productos registrados: " WS-REGISTROS-LEIDOS

           STOP RUN.

       CAPTURAR-PRODUCTO.
           DISPLAY "=========================================="
           DISPLAY "          CAPTURA DE PRODUCTO"
           DISPLAY "=========================================="

           DISPLAY "Codigo de producto (llave): "
           ACCEPT CODPRODU

           READ ARCHIVO-INDEXADO
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   DISPLAY " "
                  DISPLAY "ERROR: ya existe un producto con ese codigo."
                  DISPLAY "Ingrese un codigo diferente."
       PERFORM CAPTURAR-PRODUCTO
                   EXIT PARAGRAPH
           END-READ


           DISPLAY "Fecha inicio (AAAA-MM-DD): "
           ACCEPT FECINI

           DISPLAY "Hora inicio (HH:MM:SS): "
           ACCEPT HORINI

           DISPLAY "Hora fin (HH:MM:SS): "
           ACCEPT HORFIN

           DISPLAY "Fecha pago cupon (AAAA-MM-DD): "
           ACCEPT FECPAGO

           DISPLAY "Importe minimo suscripcion: "
           ACCEPT IMPMINSU

           DISPLAY "Importe maximo suscripcion: "
           ACCEPT IMPMAXSU

           DISPLAY "Tipo cupon: "
           ACCEPT TIPCUPON

           DISPLAY "Base de calculo: "
           ACCEPT BASECALC

           DISPLAY "Frecuencia pago cupon: "
           ACCEPT FRECPAG

           DISPLAY "Curva flotante: "
           ACCEPT CURVAFLO

           DISPLAY "Destinatario de la emision: "
           ACCEPT DESTEMI

           DISPLAY "Texto pago cupon / descripcion: "
           ACCEPT TXTCUPON

           DISPLAY "Orden irrevocable (S/N): "
           ACCEPT ORDIRREV

           DISPLAY "Multifase (S/N): "
           ACCEPT MULTFASE

           DISPLAY "Numero de fase (bono multifase): "
           ACCEPT NUMFASE

           DISPLAY "Tipo de opcion: "
           ACCEPT TIPOPCIO

           DISPLAY "Fecha de opcion: "
           ACCEPT FECHOPCI

           DISPLAY "Perpetua (S/N): "
           ACCEPT PERPETUA

           DISPLAY "Make whole (S/N): "
           ACCEPT MAKWHOLE

           DISPLAY "Fecha make whole: "
           ACCEPT FECMAWHO

           DISPLAY "Porcentaje cupon: "
           ACCEPT WS-PORCENTAJE1

           MOVE WS-PORCENTAJE1 TO PORCENCU

           DISPLAY "Porcentaje TIR: "
           ACCEPT WS-PORCENTAJE2

           MOVE WS-PORCENTAJE2 TO PORCENTI

           WRITE PRODUCTO
               INVALID KEY
                   DISPLAY " "
                   DISPLAY "ERROR: no se pudo guardar el producto."
                   DISPLAY "STATUS: " WS-ESTADO-ARCHIVO
               NOT INVALID KEY
                   ADD 1 TO WS-REGISTROS-LEIDOS
                   DISPLAY " "
                   DISPLAY "Producto guardado correctamente."
           END-WRITE.

       MOSTRAR-PRODUCTO.
           MOVE ZERO TO WS-REGISTROS-LEIDOS
           MOVE "N" TO WS-FIN-LEC

           CLOSE ARCHIVO-INDEXADO
           OPEN INPUT ARCHIVO-INDEXADO

           IF WS-ESTADO-ARCHIVO NOT = "00"
           DISPLAY "ERROR AL ABRIR EL ARCHIVO."
           DISPLAY "STATUS: " WS-ESTADO-ARCHIVO
           EXIT PARAGRAPH
           END-IF


       PERFORM UNTIL WS-FIN-LEC = "S"
           READ ARCHIVO-INDEXADO NEXT RECORD AT END
                MOVE "S" TO WS-FIN-LEC
            NOT AT END ADD 1 TO WS-REGISTROS-LEIDOS

           DISPLAY "=============================================="
           DISPLAY "          DATOS DEL PRODUCTO"
           DISPLAY "=============================================="
           DISPLAY "Codigo producto:       " CODPRODU
           DISPLAY "Fecha inicio:          " FECINI
           DISPLAY "Hora inicio:           " HORINI
           DISPLAY "Hora fin:              " HORFIN
           DISPLAY "Fecha pago:            " FECPAGO
           DISPLAY "Importe minimo:         " IMPMINSU
           DISPLAY "Importe maximo:         " IMPMAXSU
           DISPLAY "Tipo cupon:             " TIPCUPON
           DISPLAY "Base calculo:           " BASECALC
           DISPLAY "Frecuencia pago:        " FRECPAG
           DISPLAY "Curva flotante:         " CURVAFLO
           DISPLAY "Destinatario emision:   " DESTEMI
           DISPLAY "Texto cupon:            " TXTCUPON
           DISPLAY "Orden irrevocable:      " ORDIRREV
           DISPLAY "Multifase:              " MULTFASE
           DISPLAY "Numero fase:            " NUMFASE
           DISPLAY "Tipo opcion:            " TIPOPCIO
           DISPLAY "Fecha opcion:           " FECHOPCI
           DISPLAY "Perpetua:               " PERPETUA
           DISPLAY "Make whole:             " MAKWHOLE
           DISPLAY "Fecha make whole:       " FECMAWHO
           DISPLAY "Porcentaje cupon:       " PORCENCU
           DISPLAY "Porcentaje TIR:         " PORCENTI
           DISPLAY "=============================================="

           END-READ
       END-PERFORM

           IF WS-REGISTROS-LEIDOS = 0
            DISPLAY "No hay productos registrados."
            ELSE
            DISPLAY " "
            DISPLAY "Total de productos: " WS-REGISTROS-LEIDOS
           END-IF

           CLOSE ARCHIVO-INDEXADO
           OPEN I-O ARCHIVO-INDEXADO.

       ACTUALIZAR-PRODUCTOS.
           MOVE ZERO TO WS-REGISTROS-LEIDOS
           MOVE ZERO TO WS-REGISTROS-ACTUALIZADOS
           MOVE ZERO TO WS-NO-ENCONTRADOS
           MOVE "N" TO WS-FIN-TXT

           OPEN INPUT ARCHIVO-PRODUCTOS
           IF WS-ESTADO-TXT NOT = "00"
           DISPLAY "ERROR AL ABRIR EL ARCHIVO SECUENCIAL."
           DISPLAY "STATUS: " WS-ESTADO-TXT
           EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-FIN-TXT = "S"
           READ ARCHIVO-PRODUCTOS  AT END
                MOVE "S" TO WS-FIN-TXT
            NOT AT END ADD 1 TO WS-REGISTROS-LEIDOS
                MOVE CODPRODU1 TO CODPRODU
                READ ARCHIVO-INDEXADO
                    INVALID KEY
                        ADD 1 TO WS-NO-ENCONTRADOS
                    NOT INVALID KEY
                        MOVE FECPAGO1  TO FECPAGO
                        MOVE BASECALC1 TO BASECALC
                        MOVE FRECPAG1  TO FRECPAG
                        MOVE TXTCUPON1 TO TXTCUPON
                        MOVE MULTFASE1 TO MULTFASE
                        MOVE TIPOPCIO1 TO TIPOPCIO
                        MOVE FECHOPCI1 TO FECHOPCI
                        MOVE PERPETUA1 TO PERPETUA
                        MOVE PORCENCU1 TO PORCENCU
                        MOVE PORCENTI1 TO PORCENTI

               REWRITE PRODUCTO
               INVALID KEY
               DISPLAY "ERROR AL ACTUALIZAR: " CODPRODU
               NOT INVALID KEY
                ADD 1 TO WS-REGISTROS-ACTUALIZADOS
               END-REWRITE
                END-READ
           END-READ
           END-PERFORM

           CLOSE ARCHIVO-PRODUCTOS
           CLOSE ARCHIVO-INDEXADO

           DISPLAY " "
           DISPLAY "=========================================="
           DISPLAY "       ESTADISTICAS DEL PROCESO"
           DISPLAY "=========================================="
           DISPLAY "Registros leidos:       "
            WS-REGISTROS-LEIDOS
           DISPLAY "Registros actualizados: "
            WS-REGISTROS-ACTUALIZADOS
           DISPLAY "No encontrados:         "
            WS-NO-ENCONTRADOS
           DISPLAY "==========================================".
           END PROGRAM INDEXADO.
