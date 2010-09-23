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
			02 FECHA-BAJA PIC 9(8).
			02 CANT-TRANSACIONES PIC 9(8).

	WORKING-STORAGE SECTION.
	01 RAYA PIC X(40) VALUE ALL "-".
	01 CORTE PIC A.
	77 SK PIC XX VALUE SPACES.
	77 opt PIC 9.
	77 busca-cli PIC 9(8).
      * Usado como bandera para operacion de busqueda.
	77 find-code PIC A.
	77 MAX-CUENTA PIC 9(8) VALUE 99999999.
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
		DISPLAY "2) Baja de Cliente".
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
			PERFORM STUB
			WHEN 3
			PERFORM STUB
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
      * Leo el nombre del teclado hasta que sea <> de ESPACIO.
		MOVE SPACE TO NOMBRE
		PERFORM UNTIL NOT NOMBRE IS = SPACE
		DISPLAY "Ingrese Nombre Cliente: " WITH NO ADVANCING
		ACCEPT NOMBRE NO BEEP
		END-PERFORM
      * Convierto a mayusculas antes de guardar para una mejor comparacion.
		INSPECT NOMBRE CONVERTING "asdfgñlkjhqwertpoiuyzxcvmnb" 
	TO "ASDFGÑLKJHQWERTPOIUYZXCVMNB"
		MOVE SPACE TO APELLIDO
		PERFORM UNTIL NOT APELLIDO IS = SPACE
		DISPLAY "Ingrese Apellido: " WITH NO ADVANCING
		ACCEPT APELLIDO  NO BEEP
		END-PERFORM
		INSPECT APELLIDO CONVERTING "asdfgñlkjhqwertpoiuyzxcvmnb" 
	TO "ASDFGÑLKJHQWERTPOIUYZXCVMNB"
      * El Telefono no es un campo obligatorio por eso no se valida.
		DISPLAY "Ingrese Telefono: " WITH NO ADVANCING
		ACCEPT TELEFONO NO BEEP
      * Guardo el registro en caso de error notifico al usuario.
		WRITE REG-CLIENTE INVALID KEY PERFORM ERROR-CLIENTE-EXISTE
      * Dar de alta la nueva cuenta
		PERFORM ALTA-CUENTA
		END-IF.
		DISPLAY SPACES ERASE.
		MOVE "A" TO CORTE.
		PERFORM UNTIL CORTE IS = "S" OR CORTE IS = "N"
		DISPLAY "Desea cargar otro cliente? (S/N) "
		ACCEPT CORTE NO BEEP
		END-PERFORM.
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
		MOVE 0 TO FECHA-BAJA.
		MOVE 0 TO CANT-TRANSACIONES.
      * leo la fecha de creacion desde el sistema
		ACCEPT FECHA-CREACION FROM DATE.
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