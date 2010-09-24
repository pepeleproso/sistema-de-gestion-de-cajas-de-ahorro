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
			02 CANT-TRANSACIONES PIC 9(8).

	WORKING-STORAGE SECTION.
	01 RAYA PIC X(40) VALUE ALL "-".
	01 CORTE PIC A.
	77 SK PIC XX VALUE SPACES.
	77 opt PIC 9.
	77 busca-cli PIC 9(8).
	77 action PIC A.
	77 SALDO-ED PIC $(7)9,99.
	77 DNI-ED PIC z(8).
      * Usado como bandera para operacion de busqueda.
	77 find-code PIC A.
	77 MAX-CUENTA PIC 9(8) VALUE 99999999.
	77 NOMBRE-COMPLETO PIC A(40).
	01 FECHA-ED.
		02 ANIO PIC 9(4).
		02 MES PIC 9(2).
		02 DIA PIC 9(2).
      *full fecha tiene formato DD/MM/AAAA
	77 FULL-FECHA PIC X(10).
	PROCEDURE DIVISION.
	INICIO.
		PERFORM MENU-ADMINISTRADOR UNTIL opt = 5.
		STOP RUN.
      *  MENU-ADMINISTRADOR
      * Muestra el menu con las acciones que puede realizar un usuario administrador
      * Estas acciones no puede ser realizadas por el cliente.
      * La opcion 5 sale del programa.

      * #TODO:: FALTA OBTENER EL NOMBRE DEL USUARIO, 
      * para mostrar en el menu
	MENU-ADMINISTRADOR.
		DISPLAY SPACES ERASE LINE 1.
		DISPLAY "SISTEMA DE GESTION DE CAJA DE AHORRO".
		DISPLAY RAYA.
		DISPLAY "Bienvenido Juan, Perez".
		DISPLAY RAYA.
		DISPLAY "MENU:".
		DISPLAY "1) Alta de Cliente".
		DISPLAY "2) Activacion/Desactivacion de Cliente".
		DISPLAY "3) Consultar Cliente".
		DISPLAY "4) Estadisticas".
		DISPLAY "5) Salir".
		ACCEPT opt LINE 13 NO BEEP.
		EVALUATE opt 
            WHEN 1
			MOVE "A" TO CORTE
			OPEN I-O CLIENTES
			OPEN I-O CUENTAS
			PERFORM 	ALTA-CLIENTE UNTIL CORTE = "N"
			CLOSE CLIENTES
			CLOSE CUENTAS
			WHEN 2
			OPEN I-O CLIENTES
			PERFORM BAJA
			CLOSE CLIENTES
			WHEN 3
			OPEN I-O CLIENTES
			OPEN I-O CUENTAS
			PERFORM CONSULTA
			CLOSE CLIENTES
			CLOSE CUENTAS
			WHEN 4
			PERFORM STUB.

      *###########################################
      * ALTA-CLIENTE
      * Da de alta un nuevo cliente, se ingresan los datos y se validan
      * Si todo esta OK se graban en el archivo corresponiente y
      * se da de alta la cuenta en el archivo correpondiente
      * La carga se realiza hasta que se ingrese no.
      *###########################################
	ALTA-CLIENTE.
		DISPLAY SPACES ERASE LINE 1.
		DISPLAY "SISTEMA DE GESTION DE CAJA DE AHORRO".
		DISPLAY RAYA.
		DISPLAY "ALTA DE NUEVO CLIENTE".
		DISPLAY RAYA.
      * Fuerzo el valor 0 para el DNI.
		MOVE 0 TO DNI.
		PERFORM UNTIL DNI > 0
		DISPLAY "Ingrese DNI: " WITH NO ADVANCING
		ACCEPT DNI NO BEEP
		END-PERFORM.
      * Busco si ya esta cargado.
		PERFORM BUSCAR.
		IF find-code IS = "T" THEN
		PERFORM ERROR-CLIENTE-EXISTE
		ELSE
		PERFORM ALTA-DAT-PERS
      * Dar de alta la nueva cuenta
		PERFORM	ALTA-CUENTA
		END-IF.
		DISPLAY SPACES ERASE.
		MOVE "A" TO CORTE.
		PERFORM UNTIL CORTE IS = "S" OR CORTE IS = "N"
		DISPLAY "Desea cargar otro cliente? (S/N) "
		ACCEPT CORTE NO BEEP
		END-PERFORM.
	
	ALTA-DAT-PERS.
      * Leo el nombre del teclado hasta que sea <> de ESPACIO.
		MOVE SPACE TO NOMBRE.
		PERFORM UNTIL NOT NOMBRE IS = SPACE
		DISPLAY "Ingrese Nombre: " WITH NO ADVANCING
		ACCEPT NOMBRE NO BEEP
		END-PERFORM.
      * Convierto a mayusculas antes de guardar para una mejor comparacion.
		INSPECT NOMBRE CONVERTING "asdfgñlkjhqwertpoiuyzxcvmnb" 
	TO "ASDFGÑLKJHQWERTPOIUYZXCVMNB"
		MOVE SPACE TO APELLIDO.
		PERFORM UNTIL NOT APELLIDO IS = SPACE
		DISPLAY "Ingrese Apellido: " WITH NO ADVANCING
		ACCEPT APELLIDO  NO BEEP
		END-PERFORM.
		INSPECT APELLIDO CONVERTING "asdfgñlkjhqwertpoiuyzxcvmnb" 
	TO "ASDFGÑLKJHQWERTPOIUYZXCVMNB".
		MOVE SPACE TO DOMICILIO.
		PERFORM UNTIL NOT DOMICILIO IS = SPACE
		DISPLAY "Ingrese Domicilio: " WITH NO ADVANCING
		ACCEPT DOMICILIO  NO BEEP
		END-PERFORM.
      * El Telefono no es un campo obligatorio por eso no se valida.
		DISPLAY "Ingrese Telefono: " WITH NO ADVANCING.
		ACCEPT TELEFONO NO BEEP.
		MOVE "A" TO ESTADO.
      * Guardo el registro en caso de error notifico al usuario
		WRITE REG-CLIENTE INVALID KEY 
			PERFORM ERROR-CLIENTE-EXISTE.
      *###########################################
      * ALTA-CUENTA
      * Da de alta una nueva cuenta, y la asocia al cliente actual
      *###########################################
	ALTA-CUENTA.
      * BUSCO el ultimo numero de cuenta y le sumo 1.
		MOVE MAX-CUENTA TO NRO,
		START CUENTAS KEY IS LESS NRO
		INVALID KEY MOVE 0 TO NRO END-START
		IF NOT NRO IS = 0 THEN
		READ CUENTAS NEXT RECORD AT END MOVE 0 TO NRO
      	END-IF
		ADD 1 TO NRO.
		MOVE DNI TO DNI-CLI.
		MOVE 0 TO SALDO.
		MOVE 0 TO CANT-TRANSACIONES.
      * leo la fecha de creacion desde el sistema
		ACCEPT FECHA-CREACION FROM DATE.
		ACCEPT find-code.
		WRITE REG-CUENTA INVALID KEY PERFORM ERROR-CUENTA-EXISTE.
		
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

	BAJA.
		DISPLAY SPACES ERASE LINE 1.
		DISPLAY "SISTEMA DE GESTION DE CAJA DE AHORRO".
		DISPLAY RAYA.
		DISPLAY "ACTIVACION / DESACTIVACION DE CLIENTES".
		DISPLAY RAYA.
      * Fuerzo el valor 0 para el DNI.
		MOVE 0 TO DNI.
		PERFORM UNTIL DNI > 0
		DISPLAY "Ingrese DNI: " WITH NO ADVANCING
		ACCEPT DNI NO BEEP
		END-PERFORM.
		PERFORM BUSCAR.
		IF find-code IS = "F" THEN
		PERFORM ERROR-NOT-FOUND
		ELSE
		MOVE "A" TO action
		IF ESTADO IS = "A" THEN
		PERFORM PREG-BAJA UNTIL action is = "Y" 
	or action is = "N"
		IF action IS = "Y" THEN
		MOVE "B" TO ESTADO
		REWRITE REG-CLIENTE
		DISPLAY "La operacion se realizo con exito"
		END-IF
		ELSE
		PERFORM PREG-REHABILITAR UNTIL action is = "Y" 
	or action is = "N"
		IF action IS = "Y" THEN
		MOVE "A" TO ESTADO
		REWRITE REG-CLIENTE
		DISPLAY "La operacion se realizo con exito"
		END-IF.

	PREG-BAJA.
		STRING NOMBRE DELIMITED BY "  "
		", " DELIMITED BY SIZE
		APELLIDO DELIMITED BY "  "
		INTO NOMBRE-COMPLETO
		DISPLAY "¿Desea dar de baja al Cliente: " NOMBRE-COMPLETO "? Y/N".
		ACCEPT action NO BEEP.
	PREG-REHABILITAR.
		STRING NOMBRE DELIMITED BY "  "
		", " DELIMITED BY SIZE
		APELLIDO DELIMITED BY "  "
		INTO NOMBRE-COMPLETO
		DISPLAY "¿Desea rehabilitar el Cliente: " NOMBRE-COMPLETO "? Y/N".
		ACCEPT action NO BEEP.
      * CONSULTA
      * Muestra los datos del cliente y de su cuenta
	CONSULTA.
		DISPLAY SPACES ERASE LINE 1.
		DISPLAY "SISTEMA DE GESTION DE CAJA DE AHORRO".
		DISPLAY RAYA.
		DISPLAY "CONSULTA DE CLIENTES".
		DISPLAY RAYA.
      * Fuerzo el valor 0 para el DNI.
		MOVE 0 TO DNI.
		PERFORM UNTIL DNI > 0
		DISPLAY "Ingrese DNI: " WITH NO ADVANCING
		ACCEPT DNI NO BEEP
		END-PERFORM.
		PERFORM BUSCAR.
		IF find-code IS = "F" THEN
		PERFORM ERROR-NOT-FOUND
		ELSE
		MOVE DNI TO DNI-CLI
		START CUENTAS KEY IS = DNI-CLI
		READ CUENTAS NEXT RECORD
		DISPLAY SPACES ERASE LINE 1
		DISPLAY "SISTEMA DE GESTION DE CAJA DE AHORRO"
		DISPLAY RAYA
		DISPLAY "CONSULTA DE CLIENTES"
		DISPLAY RAYA
		DISPLAY "Cliente: " WITH NO ADVANCING
		STRING NOMBRE DELIMITED BY "  "
		", " DELIMITED BY SIZE
		APELLIDO DELIMITED BY "  "
		INTO NOMBRE-COMPLETO
		DISPLAY NOMBRE-COMPLETO
		MOVE DNI TO DNI-ED
		DISPLAY "DNI: " DNI-ED
		DISPLAY "Domicilio: " DOMICILIO
		DISPLAY "Telefono: " TELEFONO
		DISPLAY SPACE
		DISPLAY "Cuenta: " NRO
      *transformo la fecha del formato AAAAMMDD
      *al formato DD/MM/AAAA
		MOVE FECHA-CREACION TO FECHA-ED
      *como no el complador parece no devolver una fecha 
      *con anios de 4 digitos la creamos
      *esto hace que se pierdan fechas anterioes a 2000
		ADD 2000 TO ANIO
		STRING DIA DELIMITED BY SIZE
		"/" DELIMITED BY SIZE
		MES DELIMITED BY SIZE 
		"/" DELIMITED BY SIZE
		ANIO DELIMITED BY SIZE
		INTO FULL-FECHA
		DISPLAY "Fecha Alta: " FULL-FECHA
		MOVE SALDO TO SALDO-ED
		DISPLAY "Saldo Cuenta: " SALDO-ED
		DISPLAY RAYA
		ACCEPT find-code NO BEEP
		END-IF.

	STUB.
		DISPLAY "NO IMPLEMENTADO".
		DISPLAY "Presione una tecla para continuar... ".
		ACCEPT CORTE NO BEEP.

      * MENSAJES DE ERROR
	ERROR-CLIENTE-EXISTE.
		DISPLAY "EL CLIENTE YA EXISTE".

	ERROR-CUENTA-EXISTE.
		DISPLAY "EL CLIENTE YA TIENE UNA CUENTA".

	ERROR-NOT-FOUND.
		DISPLAY "NO SE ENCONTRO UN CLIENTE CON DNI = " DNI.
