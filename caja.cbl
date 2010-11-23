	IDENTIFICATION DIVISION.
	PROGRAM-ID. TPFINAL.
	AUTHOR. GRUPO3TM.
	ENVIRONMENT DIVISION.
	CONFIGURATION SECTION.
	SOURCE-COMPUTER. PC.
	OBJECT-COMPUTER. PC.
	SPECIAL-NAMES.
				DECIMAL-POINT IS COMMA.
	INPUT-OUTPUT SECTION.
	FILE-CONTROL.
		SELECT OPTIONAL CLIENTES ASSIGN 
                TO DISK  "CLIENTES.DAT"
		ORGANIZATION IS INDEXED
		ACCESS MODE IS DYNAMIC 
		RECORD KEY IS DNI
		FILE STATUS IS SK.

		SELECT OPTIONAL CUENTAS ASSIGN
                TO DISK  "CUENTAS.DAT"
		ORGANIZATION IS INDEXED
		ACCESS MODE IS DYNAMIC 
		RECORD KEY IS NRO
		ALTERNATE RECORD KEY IS DNI-CLI
		FILE STATUS IS SK.

		SELECT OPTIONAL POLITICAS ASSIGN TO "POLITICAS.DAT"
		FILE STATUS IS SK.

		SELECT OPTIONAL OPERACIONES ASSIGN
                TO DISK  "OPERACIONES.DAT"
		ORGANIZATION IS INDEXED
		ACCESS MODE IS DYNAMIC 
		RECORD KEY IS NRO-OP
		ALTERNATE RECORD KEY IS FECHA-OP 
		WITH DUPLICATES
		FILE STATUS IS SK.
	
	DATA DIVISION.
	FILE SECTION.
        FD CLIENTES
		LABEL RECORD IS STANDARD
		DATA RECORD IS REG-CLIENTE.
      * El DNI se guarda en el forma: 23234567
      * El nombre y el apellido se guardan en mayusculas, para mejor comparacion.
        01 REG-CLIENTE.
        	02 DNI PIC 9(8).
			02 APELLIDO PIC A(20).
			02 NOMBRE PIC A(20).
			02 DOMICILIO PIC X(20).
			02 TELEFONO PIC X(20).
      *ESTADO DEL CLIENTE PUEDE SER:
      *"A" ->ACTIVO
      *"B" -> DADO DE BAJA
			02 ESTADO PIC A.
        FD CUENTAS
		LABEL RECORD IS STANDARD
		DATA RECORD IS REG-CUENTA.
        01 REG-CUENTA.
        	02 NRO PIC 9(8).
        	02 DNI-CLI PIC 9(8).
      * SALDO tiene formato 9999999,99
                02 SALDO PIC 9(7)v99.
      * CON FORMATO AAAAMMDD
		02 FECHA-CREACION PIC 9(8).

	FD POLITICAS DATA RECORD IS REG-POLITICAS.
	01 REG-POLITICAS.
		02 PORC-COMISION PIC 9(2).
                02 PORC-INTERES PIC 9(2).

	FD OPERACIONES DATA RECORD IS REG-OPERACIONES.
	01 REG-OPERACIONES.
		02 NRO-OP PIC 9(8).
                02 NRO-CUENTA PIC 9(8).
                02 T-OPERACION PIC 9.
                02 IMPORTE PIC S9(7)v99.
                02 CTA-ORIGEN PIC 9(8).
                02 FECHA-OP PIC 9(8).

	WORKING-STORAGE SECTION.
        77 DNI-LOGIN PIC 9(8).
        77 CUENTA-DEST PIC 9(8).
        77 CUENTA-ORIG PIC 9(8).
	77 RAYA PIC X(70) VALUE ALL "-".
	77 CORTE PIC A.
	77 SK PIC XX VALUE SPACES.
	77 opt PIC 9.
	77 optc PIC 9.
	77 busca-cli PIC 9(8).
	77 action PIC A.
	77 SALDO-ED PIC $(7)9,99.
        77 SALDO-ORIG PIC $(7)9,99.
	77 DNI-ED PIC z(8).
      * Usado como bandera para operacion de busqueda.
	77 find-code PIC A.
	77 MAX-CUENTA PIC 9(8) VALUE 99999999.
	77 NOMBRE-COMPLETO PIC A(40).
	77 TITULO PIC X(60).
	01 FECHA-ED.
		02 ANIO PIC 9(4).
		02 MES PIC 9(2).
		02 DIA PIC 9(2).
      *full fecha tiene formato DD/MM/AAAA
	77 FULL-FECHA PIC X(10).
      *TIPOS DE OPERACIONES:
      * Extraccion
      * Deposito
      * Deposito cheque
      * Transferencia de Fondos
      * Acreditación de intereses
      * Debito Comisión
	01 TIPO-OPERACION.
         02 T-OP OCCURS 7 TIMES.
               05 DESCRIPCION PIC X(35).
	77 COMISION PIC 99.
	77 linact PIC 99.
	77 IMPORTE-COM PIC 9(7)v99.
	77 BLANCO PIC X(70) VALUE ALL SPACES.
	77 IMPORTE-OP PIC 9(7)v99.
	
      * parte de declaracion de ventanas
	01 WCB.
		03 WCB-HANDLE		PIC 999 BINARY VALUE 0.
		03 WCB-NUM-ROWS		PIC 999 BINARY.
		03 WCB-NUM-COLS		PIC 999 BINARY.
		03 WCB-LOCATION-REFERENCE		PIC X.
			88 WCB-LOCATION-SCREEN-RELATIVE	VALUE "S".
			88 WCB-LOCATION-WINDOW-RELATIVE	VALUE "W".
		03 WCB-BORDER-SWITCH	PIC X.
			88 WCB-BORDER-ON	VALUE "Y" WHEN FALSE "N".

		03 WCB-BORDER-TYPE		PIC 9.
		03 WCB-BORDER-CHAR		PIC X.
		03 WCB-FILL-SWITCH		PIC X.
			88 WCB-FILL-ON		VALUE "Y" WHEN FALSE "N".

		03 WCB-FILL-CHAR			PIC X.
		03 WCB-TITLE-LOCATION	PIC X.
			88 WCB-TITLE-TOP		VALUE "T".
			88 WCB-TITLE-BOTTOM		VALUE "B".
		03 WCB-TITLE-POSITION	PIC X.
			88 WCB-TITLE-CENTER		VALUE "C".
			88 WCB-TITLE-LEFT			VALUE "L".
			88 WCB-TITLE-RIGHT		VALUE "R".
		03 WCB-TITLE-LENGHT		PIC 999 BINARY.
		03 WCB-TITLE				PIC X(64).
        
	SCREEN SECTION.
	01 SC-DNI.
		02 FILLER  PIC 9(8)
				TO DNI LINE 5 COL 16 
                REQUIRED.

	01 SC-CABECERA.
        02 FILLER  PIC X(70)
				FROM RAYA LINE 1 COL 2.
        02 FILLER  PIC X(70)
				FROM TITULO LINE 2 COL 2.
        02 FILLER  PIC X(70)
				FROM RAYA LINE 3 COL 2.

	01 SC-DESCCLI.
    	02 FILLER  PIC X(70)
				FROM RAYA LINE 1 COL 2.
        02 FILLER  PIC X(70)
				FROM TITULO LINE 2 COL 2.
        02 FILLER  PIC X(70)
				FROM RAYA LINE 3 COL 2.
        02 FILLER  PIC A(9)
				FROM "Cliente: " LINE 5 COL 2.
        02 FILLER  PIC A(40)
				FROM NOMBRE-COMPLETO LINE 5 COL 11.

	01 SC-LINEA.
        02 FILLER  PIC 9(8)
				FROM NRO-OP LINE linact COL 2.
        02 FILLER  PIC X(10)
				FROM FULL-FECHA LINE linact COL 12.
        02 FILLER  PIC X(35)
				FROM DESCRIPCION(T-OPERACION) LINE linact COL 26.
        02 FILLER  PIC $(7)9,99+
				FROM IMPORTE LINE linact COL 57.

	PROCEDURE DIVISION.
	INICIO.
    		PERFORM INICIAR-VENTANA.
		DISPLAY WCB LINE 2 COL 2 LOW ERASE
            	CONTROL "WINDOW-CREATE".
		PERFORM CARGA-OPERACIONES.
                OPEN I-O CLIENTES.
                OPEN I-O CUENTAS.
                PERFORM LOGIN-CLIENTE.
                CLOSE CLIENTES.
                CLOSE CUENTAS.
		STOP RUN.

        LOGIN-CLIENTE.
                DISPLAY SPACES ERASE LINE 1 LOW.
		MOVE  "Ingreso de Usuario:"  TO TITULO.
		DISPLAY SC-CABECERA.
      * Fuerzo el valor 0 para el DNI.
		MOVE 0 TO DNI.
		PERFORM UNTIL DNI > 0
		DISPLAY "Ingrese DNI: "  LINE 5 COL 3 LOW
		ACCEPT SC-DNI
		PERFORM BUSCAR
		IF find-code IS = "F" THEN
                DISPLAY "NO SE ENCONTRO UN CLIENTE CON DNI " LINE 22 
                COL 3 LOW
                MOVE DNI TO DNI-ED
                DISPLAY DNI-ED LINE 22 COL 41 LOW
                ACCEPT CORTE NO BEEP LINE 22
                DISPLAY BLANCO LINE 22 COL 0 LOW
                MOVE 0 TO DNI
                END-IF
		END-PERFORM.
		MOVE DNI TO DNI-CLI.
		START CUENTAS KEY IS = DNI-CLI.
		READ CUENTAS NEXT RECORD.
                PERFORM MENU-CLIENTE UNTIL opt = 5.

	BUSCAR.
      * forzar un codigo para siempre realizar la busqueda.
		MOVE "B" TO find-code.
		START CLIENTES INVALID KEY MOVE "F" TO find-code  
		NOT INVALID KEY PERFORM LEER.

	LEER.
      *  Control de errores: verificamos no procesar el EOF, si estamos en el ultimo
      * registro entonces terminamos el proceso mostrando que la busqueda no tuvo exito
		READ CLIENTES RECORD INTO REG-CLIENTE KEY IS DNI.
		MOVE "T" TO find-code.

	STUB.
		DISPLAY "NO IMPLEMENTADO".
		DISPLAY "Presione una tecla para continuar... ".
		ACCEPT CORTE NO BEEP.

      *###############################
      * CARGA-OPERACIONES
      * Inicializo el array con las operaciones.
      * El indice se usa como ID para las operaciones.
      *###############################
	CARGA-OPERACIONES.
		MOVE 1 TO opt.
      * ID = 1
		MOVE "Extraccion" TO DESCRIPCION(opt).
		ADD 1 TO opt.
      * ID = 2
		MOVE "Deposito" TO DESCRIPCION(opt).
		ADD 1 TO opt.
      * ID = 3
		MOVE "Deposito cheque" TO DESCRIPCION(opt).
		ADD 1 TO opt.
      * ID = 4
		MOVE "Transferencia de Fondos" TO DESCRIPCION(opt).
		ADD 1 TO opt.
      * ID = 5
		MOVE "Acreditacion de intereses" TO DESCRIPCION(opt).
		ADD 1 TO opt.
      * ID = 6
		MOVE "Debito Comision" TO DESCRIPCION(opt).
                ADD 1 TO opt.
      * ID = 7
		MOVE "Acreditacion Transferencia" TO DESCRIPCION(opt).

      *  MENU-CLIENTE
      * Muestra el menu con las acciones que puede realizar un cliente
      * La opcion 5 sale del programa.
	 MENU-CLIENTE.
		DISPLAY SPACES ERASE LINE 1 LOW.
		MOVE "Bienvenido"  TO TITULO.
		DISPLAY SC-CABECERA.
		DISPLAY "MENU:"  COL 2 LOW.
		DISPLAY "1) Depositar"  COL 2 LOW.
		DISPLAY "2) Realizar Extracion"  COL 2 LOW.
		DISPLAY "3) Realizar Transferencia"  COL 2 LOW.
		DISPLAY "4) Liquidacion Mensual"  COL 2 LOW.
		DISPLAY "5) Salir"  COL 2 LOW.
		ACCEPT opt  LINE 10 COL 2 LOW NO BEEP.
		EVALUATE opt 
                WHEN 1
                PERFORM MENU-DEPOSITAR
                WHEN 2
                PERFORM EXTRACION
                WHEN 3
                PERFORM TRANSFERENCIA
                WHEN 4
                PERFORM  LIQUIDACION-MENSUAL.

	MENU-DEPOSITAR.
		DISPLAY SPACES ERASE LINE 1 LOW.
		MOVE "DEPOSITO DE FONDOS"  TO TITULO.
		DISPLAY SC-CABECERA.
		DISPLAY "SELECCIONE EL TIPO DE DEPOSITO:"   COL 2 LOW.
		DISPLAY "1) Efectivo"   COL 2 LOW.
		DISPLAY "2) Cheque"   COL 2 LOW.
		DISPLAY "3) Salir"   COL 2 LOW.
		MOVE 0 TO optc.
		PERFORM UNTIL optc > 0 AND optc < 4
		ACCEPT optc NO BEEP
		END-PERFORM.
		EVALUATE optc
                WHEN 1
      * para efectivo
                MOVE 0 TO COMISION
                WHEN 2
                OPEN INPUT POLITICAS
                READ POLITICAS AT END MOVE 0 TO PORC-COMISION
                END-READ
                MOVE PORC-COMISION TO COMISION
                CLOSE POLITICAS.
		OPEN I-O OPERACIONES.
      *para cheque
		PERFORM DEPOSITO.
		CLOSE OPERACIONES.
        
      * DEPOSITO
      * Da de alta un nuevo deposito.
      * Por los cheques aplica un porcentaje de comision.
      * luego de realizada la operacion actualiza.
      * el saldo de la cuenta.
	DEPOSITO.
		DISPLAY SPACES ERASE LINE 1 LOW.
		MOVE "DEPOSITO DE FONDOS"  TO TITULO.
		DISPLAY SC-CABECERA.
      * BUSCO el ultimo numero de operacion y le sumo 1.
		MOVE MAX-CUENTA TO NRO-OP.
		START OPERACIONES KEY IS LESS NRO-OP
		INVALID KEY MOVE 0 TO NRO-OP END-START.
		IF NOT NRO-OP IS = 0 THEN
		READ OPERACIONES NEXT RECORD AT END MOVE 0 TO NRO-OP
                END-IF.
		ADD 1 TO NRO-OP.
                PERFORM FECHA-ACTUAL.
		MOVE NRO TO NRO-CUENTA.
		IF optc = 1 THEN 
		MOVE 2 TO T-OPERACION
		ELSE
		MOVE 3 TO T-OPERACION
		END-IF.
		MOVE 0 TO CTA-ORIGEN.
      *cargo importe
		MOVE 0 TO IMPORTE.
		PERFORM UNTIL IMPORTE > 0
		DISPLAY "Ingrese Importe a Depositar:" LINE 6 COL 3 LOW
		ACCEPT IMPORTE LINE 6 COL 32 NO BEEP LOW
		END-PERFORM.
		MOVE 0 TO IMPORTE-COM.
		COMPUTE IMPORTE-COM = IMPORTE * COMISION / 100.
		COMPUTE SALDO = SALDO + IMPORTE.
		MOVE IMPORTE TO IMPORTE-OP.
		REWRITE REG-CUENTA.
		WRITE REG-OPERACIONES.
      *si la comision es mayor a cero entonces tengo que crear una nueva operacion
      *de descuento de comisiciones.
		IF NOT IMPORTE-COM = 0 THEN
                PERFORM	DEBITAR-COMISION-DEPOSITO
                END-IF.
		PERFORM CONFIRMAR-DEPOSITO.
		ACCEPT find-code NO BEEP.
        
	DEBITAR-COMISION-DEPOSITO.
		ADD 1 TO NRO-OP.
                PERFORM FECHA-ACTUAL.
		MOVE NRO TO NRO-CUENTA.
		MOVE 6 TO T-OPERACION.
		MOVE 0 TO CTA-ORIGEN.
      *cargo importe
		MOVE IMPORTE-COM TO IMPORTE.
		COMPUTE IMPORTE = 0 - IMPORTE.
		COMPUTE SALDO = SALDO - IMPORTE-COM.
		REWRITE REG-CUENTA.
		WRITE REG-OPERACIONES.

	CONFIRMAR-DEPOSITO.
		DISPLAY SPACES ERASE LINE 1 LOW.
		MOVE "DEPOSITO DE FONDOS"  TO TITULO.
		DISPLAY SC-CABECERA.
		MOVE IMPORTE-OP TO SALDO-ED.
		DISPLAY "Se depositaron:"  LINE 5 COL 3 LOW.
		DISPLAY SALDO-ED LINE 5 COL 21 LOW.
		MOVE SALDO TO SALDO-ED.
		DISPLAY "Su saldo es de: " LINE 6 COL 3 LOW.
		DISPLAY SALDO-ED LINE 6 COL 21 LOW.

      * une los campos de nombre y apellido
	CREAR-NOMBRE-COMPLETO.
		MOVE SPACE TO  NOMBRE-COMPLETO.
		STRING NOMBRE DELIMITED BY "  "
		", " DELIMITED BY SIZE
		APELLIDO DELIMITED BY "  "
		INTO NOMBRE-COMPLETO.
        
	LIQUIDACION-MENSUAL.
		DISPLAY SPACES ERASE LINE 1.
		PERFORM CREAR-NOMBRE-COMPLETO.
		MOVE "LIQUIDACION DE CUENTA" TO TITULO.
		MOVE DNI TO DNI-ED.
		DISPLAY SC-DESCCLI.
		DISPLAY SPACE.
		DISPLAY "Cuenta: " LINE 4 COL 2 LOW.
		DISPLAY NRO LINE 4 COL 11 LOW.
      *transformo la fecha del formato AAAAMMDD
      *al formato DD/MM/AAAA
		MOVE FECHA-CREACION TO FECHA-ED.
		STRING DIA DELIMITED BY SIZE
		"/" DELIMITED BY SIZE
		MES DELIMITED BY SIZE 
		"/" DELIMITED BY SIZE
		ANIO DELIMITED BY SIZE
		INTO FULL-FECHA.
		DISPLAY "Fecha Alta: " LINE 7 COL 2 LOW.
		DISPLAY FULL-FECHA LINE 7 COL 14 LOW.
		MOVE SALDO TO SALDO-ED.
		DISPLAY "Saldo Cuenta: " LINE 6 COL 2 LOW.
		DISPLAY SALDO-ED LINE 6 COL 15 LOW.
		DISPLAY RAYA LINE 8 COL 2 LOW.
		DISPLAY "NRO" LINE 9 COL 2 LOW.
		DISPLAY "FECHA" LINE 9 COL 12 LOW.
		DISPLAY "DESCRIPCION" LINE 9 COL 26 LOW.
		DISPLAY "IMPORTE" LINE 9 COL 57 LOW.
		DISPLAY RAYA LINE 10 COL 2 LOW.
		PERFORM LIQUIDAR-OPERACIONES.
		ACCEPT find-code NO BEEP.
        
        FECHA-ACTUAL.
		ACCEPT FECHA-OP FROM DATE.
      *como no el compilador parece no devolver una fecha 
      *con anios de 4 digitos la creamos
      *esto hace que se pierdan fechas anteriores a 2000
                ADD 20000000 TO FECHA-OP.

	LIQUIDAR-OPERACIONES.
		OPEN INPUT OPERACIONES.
                PERFORM FECHA-ACTUAL.
		MOVE FECHA-OP TO FECHA-ED.
		MOVE 1 TO DIA.
		MOVE FECHA-ED TO FECHA-OP.
                MOVE 1 TO NRO-OP.
		START OPERACIONES KEY IS >= FECHA-OP
		INVALID KEY MOVE 0 TO NRO-OP 
		END-START.
		PERFORM DISPLAY-OPERACIONES UNTIL NRO-OP = 0.
		CLOSE OPERACIONES.
        
	DISPLAY-OPERACIONES.
		MOVE 10 TO linact
		PERFORM DISPLAY-LINEA UNTIL NRO-OP = 0 OR linact = 20.
		IF NOT NRO-OP = 0 THEN
                DISPLAY "Presione una tecla para continuar... " 
                LINE 21 COL 2 LOW
		ACCEPT find-code NO BEEP
		ELSE
		IF NOT linact = 20 THEN
                PERFORM UNTIL linact = 21
                ADD 1 TO linact
		DISPLAY BLANCO LINE linact
                END-PERFORM
                END-IF
                END-IF.
    
	DISPLAY-LINEA.
		READ OPERACIONES NEXT RECORD
                AT END MOVE 0 TO NRO-OP.
		IF NOT NRO-OP = 0 THEN
                IF NRO-CUENTA IS =  NRO THEN
                ADD 1 TO linact
      *transformo la fecha del formato AAAAMMDD
      *al formato DD/MM/AAAA
		MOVE FECHA-OP TO FECHA-ED
		STRING DIA DELIMITED BY SIZE
		"/" DELIMITED BY SIZE
		MES DELIMITED BY SIZE 
		"/" DELIMITED BY SIZE
		ANIO DELIMITED BY SIZE
		INTO FULL-FECHA
		DISPLAY SC-LINEA
                END-IF
		END-IF.

	EXTRACION.
		OPEN I-O OPERACIONES.
		DISPLAY SPACES ERASE LINE 1.
		PERFORM CREAR-NOMBRE-COMPLETO.
		DISPLAY SC-DESCCLI.
      * BUSCO el ultimo numero de operacion y le sumo 1.
		MOVE MAX-CUENTA TO NRO-OP.
		START OPERACIONES KEY IS LESS NRO-OP
		INVALID KEY MOVE 0 TO NRO-OP END-START
		IF NOT NRO-OP IS = 0 THEN
		READ OPERACIONES NEXT RECORD AT END MOVE 0 TO NRO-OP
                END-IF
		ADD 1 TO NRO-OP.
                PERFORM FECHA-ACTUAL.
		MOVE NRO TO NRO-CUENTA.
		MOVE 1 TO T-OPERACION.
		MOVE 0 TO CTA-ORIGEN.
		MOVE SALDO TO SALDO-ED.
		DISPLAY "Saldo Cuenta: " LINE 6 COL 2 LOW.
		DISPLAY SALDO-ED LINE 6 COL 15 LOW.
      *cargo importe
		MOVE 0 TO IMPORTE.
		PERFORM UNTIL IMPORTE > 0
		DISPLAY "Ingrese Importe a Extraer:" LINE 7 COL 2 LOW
		ACCEPT IMPORTE LINE 7 COL 29 NO BEEP LOW
		IF SALDO - IMPORTE < 0 THEN
		DISPLAY "EL MONTO DE LA EXTRACION SUPERA SU SALDO" 
		LINE 8 COL 2 LOW
		MOVE 0 TO IMPORTE
                ACCEPT CORTE NO BEEP
                DISPLAY BLANCO LINE 7 COL 2 LOW
                DISPLAY BLANCO LINE 8 COL 2 LOW
		END-IF
		END-PERFORM
                MOVE IMPORTE TO SALDO-ED.
		DISPLAY "MONTO A EXTRAER DE SU CUENTA: " LINE 8 COL 2 LOW.
		DISPLAY SALDO-ED LINE 8 COL 33 LOW.
		MOVE "A" TO CORTE.
		PERFORM UNTIL CORTE IS = "Y" OR CORTE IS = "N"
		DISPLAY "DESEA REALIZAR ESTA OPERACION?" LINE 9 COL 2 LOW
		ACCEPT CORTE NO BEEP LINE 9 COL 33 LOW
		INSPECT CORTE CONVERTING "yn" TO "YN"
		END-PERFORM.
                IF CORTE IS = "Y" THEN
		MOVE IMPORTE TO IMPORTE-OP
		COMPUTE IMPORTE = 0 - IMPORTE
		COMPUTE SALDO = SALDO + IMPORTE
		REWRITE REG-CUENTA
		WRITE REG-OPERACIONES
		PERFORM CONFIRMAR-EXTRACION
		ACCEPT find-code NO BEEP
                END-IF.
		CLOSE OPERACIONES.

	CONFIRMAR-EXTRACION.
		DISPLAY SPACES ERASE LINE 1 LOW.
		MOVE "EXTRACION DE FONDOS"  TO TITULO.
		DISPLAY SC-CABECERA.
		MOVE IMPORTE-OP TO SALDO-ED.
		DISPLAY "Se extrajeron:"  LINE 5 COL 3 LOW.
		DISPLAY SALDO-ED LINE 5 COL 21 LOW.
		MOVE SALDO TO SALDO-ED.
		DISPLAY "Su saldo es de: " LINE 6 COL 3 LOW.
		DISPLAY SALDO-ED LINE 6 COL 21 LOW.
        
        TRANSFERENCIA.
                OPEN I-O OPERACIONES.
		DISPLAY SPACES ERASE LINE 1 LOW.
		MOVE "EXTRACION DE FONDOS"  TO TITULO.
		DISPLAY SC-CABECERA.
                MOVE 0 TO CUENTA-DEST.
		PERFORM UNTIL CUENTA-DEST > 0
		DISPLAY "Ingrese Cuenta Destino:" LINE 7 COL 2 LOW
		ACCEPT CUENTA-DEST LINE 7 COL 27 NO BEEP LOW
                MOVE CUENTA-DEST TO NRO
                START CUENTAS KEY IS = NRO 
                INVALID KEY MOVE 0 TO CUENTA-DEST END-START
                IF NOT CUENTA-DEST IS = 0 THEN
                READ CUENTAS NEXT RECORD
                END-IF
		END-PERFORM.
                MOVE DNI TO DNI-CLI.
                START CUENTAS KEY IS = DNI-CLI.
                READ CUENTAS NEXT RECORD.
      * BUSCO el ultimo numero de operacion y le sumo 1.
		MOVE MAX-CUENTA TO NRO-OP.
		START OPERACIONES KEY IS LESS NRO-OP
		INVALID KEY MOVE 0 TO NRO-OP END-START
		IF NOT NRO-OP IS = 0 THEN
		READ OPERACIONES NEXT RECORD AT END MOVE 0 TO NRO-OP
                END-IF
		ADD 1 TO NRO-OP.
                PERFORM FECHA-ACTUAL.
		MOVE NRO TO NRO-CUENTA.
		MOVE 4 TO T-OPERACION.
		MOVE CUENTA-DEST TO CTA-ORIGEN.
                MOVE NRO TO CUENTA-ORIG.
		MOVE SALDO TO SALDO-ED.
		DISPLAY "Saldo Cuenta: " LINE 8 COL 2 LOW.
		DISPLAY SALDO-ED LINE 8 COL 15 LOW.
      *cargo importe
		MOVE 0 TO IMPORTE.
		PERFORM UNTIL IMPORTE > 0
		DISPLAY "Ingrese Importe a Transferir:" LINE 9 COL 2 LOW
		ACCEPT IMPORTE LINE 9 COL 32 NO BEEP LOW
		IF SALDO - IMPORTE < 0 THEN
		DISPLAY "EL MONTO DE LA TRANSFERENCIA SUPERA SU SALDO" 
		LINE 10 COL 2 LOW
                ACCEPT CORTE NO BEEP
                DISPLAY BLANCO LINE 10 COL 2 LOW
                DISPLAY BLANCO LINE 9 COL 2 LOW
		MOVE 0 TO IMPORTE
		END-IF
		END-PERFORM
                MOVE IMPORTE TO SALDO-ED.
		DISPLAY "MONTO A TRANSFERIR DE SU CUENTA: " LINE 10 COL 2 LOW.
		DISPLAY SALDO-ED LINE 10 COL 40 LOW.
		MOVE "A" TO CORTE.
		PERFORM UNTIL CORTE IS = "Y" OR CORTE IS = "N"
		DISPLAY "DESEA REALIZAR ESTA OPERACION?" LINE 11 COL 2 LOW
		ACCEPT CORTE NO BEEP LINE 11 COL 33 LOW
		INSPECT CORTE CONVERTING "yn" TO "YN"
		END-PERFORM.
                IF CORTE IS = "Y" THEN
		MOVE IMPORTE TO IMPORTE-OP
		COMPUTE IMPORTE = 0 - IMPORTE
		COMPUTE SALDO = SALDO + IMPORTE
                MOVE SALDO TO SALDO-ORIG
		REWRITE REG-CUENTA
		WRITE REG-OPERACIONES
                MOVE CUENTA-DEST TO NRO
                START CUENTAS KEY IS = NRO
                READ CUENTAS NEXT RECORD
                ADD 1 TO NRO-OP
		MOVE NRO TO NRO-CUENTA
		MOVE 7 TO T-OPERACION
		MOVE CUENTA-ORIG TO CTA-ORIGEN
                MOVE IMPORTE-OP TO IMPORTE
                COMPUTE SALDO = SALDO + IMPORTE
                REWRITE REG-CUENTA
		WRITE REG-OPERACIONES
      *confirmar transferencia
                PERFORM CONFIRMAR-TRANSFERENCIA
		ACCEPT find-code NO BEEP
                END-IF.
                MOVE DNI TO DNI-CLI.
                START CUENTAS KEY IS = DNI-CLI.
                READ CUENTAS NEXT RECORD.
		CLOSE OPERACIONES.

	CONFIRMAR-TRANSFERENCIA.
		DISPLAY SPACES ERASE LINE 1 LOW.
		MOVE "TRANSFERENCIA DE FONDOS"  TO TITULO.
		DISPLAY SC-CABECERA.
		MOVE IMPORTE-OP TO SALDO-ED.
		DISPLAY "Se Transfirieron:"  LINE 5 COL 3 LOW.
		DISPLAY SALDO-ED LINE 5 COL 21 LOW.
		DISPLAY "Su saldo es de: " LINE 6 COL 3 LOW.
		DISPLAY SALDO-ORIG LINE 6 COL 21 LOW.

	INICIAR-VENTANA.
		DISPLAY SPACE ERASE CONTROL "FCOLOR=WHITE,BCOLOR=BLUE".
		MOVE 23 TO WCB-NUM-ROWS.
		MOVE 75 TO WCB-NUM-COLS.
		MOVE "S" TO WCB-LOCATION-REFERENCE.
		MOVE "Y" TO  WCB-BORDER-SWITCH.
		MOVE 0 TO WCB-BORDER-TYPE.
		MOVE "*" TO WCB-BORDER-CHAR.
		MOVE "N" TO WCB-FILL-SWITCH.
		MOVE " " TO WCB-FILL-CHAR.
		MOVE "T" TO WCB-TITLE-LOCATION.
		MOVE "C" TO WCB-TITLE.
		MOVE 5 TO WCB-TITLE-LENGHT.
		MOVE "SAOCA"
            TO WCB-TITLE.