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

	WORKING-STORAGE SECTION.
	01 RAYA PIC X(40) VALUE ALL "-".
	01 CORTE PIC A.
	77 SK PIC XX VALUE SPACES.
	77 opt PIC 9.
    
	PROCEDURE DIVISION.
	INICIO.
		PERFORM MENU-ADMINISTRADOR UNTIL opt = 6.
		STOP RUN.
      *  MENU-ADMINISTRADOR
      * Muestra el menu con las acciones que puede realizar un usuario administrador
      * Estas acciones no puede ser realizadas por el cliente.
      * La opcion 6 sale del programa.

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
		DISPLAY "2) Alta de Cuenta".
		DISPLAY "3) Baja de Cuenta".
		DISPLAY "4) Consultar Cliente".
		DISPLAY "5) Estadisticas".
		DISPLAY "6) Salir".
		ACCEPT opt LINE 13 NO BEEP.
		EVALUATE opt 
            WHEN 1
			MOVE "A" TO CORTE
			OPEN OUTPUT CLIENTES
			PERFORM 	ALTA-CLIENTE UNTIL CORTE = "N"
			CLOSE CLIENTES
			WHEN 2
			PERFORM STUB
			WHEN 3
			PERFORM STUB
			WHEN 4
			PERFORM STUB
			WHEN 5
			PERFORM STUB.
      * ALTA-CLIENTE
      * Da de alta un nuevo cliente, se ingresan los datos y se validan
      * Si todo esta OK se graban en el archivo corresponiente.
      * La carga se realiza hasta que se ingrese no.
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
      * Leo el nombre del teclado hasta que sea <> de ESPACIO.
		MOVE SPACE TO NOMBRE.
		PERFORM UNTIL NOT NOMBRE IS = SPACE
		DISPLAY "Ingrese Nombre Cliente: " WITH NO ADVANCING
		ACCEPT NOMBRE NO BEEP
		END-PERFORM.
      * Convierto a mayusculas antes de guardar para una mejor comparacion.
		INSPECT NOMBRE CONVERTING "asdfgñlkjhqwertpoiuyzxcvmnb" 
	TO "ASDFGÑLKJHQWERTPOIUYZXCVMNB".
		MOVE SPACE TO APELLIDO.
		PERFORM UNTIL NOT APELLIDO IS = SPACE
		DISPLAY "Ingrese Apellido: " WITH NO ADVANCING
		ACCEPT APELLIDO  NO BEEP
		END-PERFORM.
		INSPECT APELLIDO CONVERTING "asdfgñlkjhqwertpoiuyzxcvmnb" 
	TO "ASDFGÑLKJHQWERTPOIUYZXCVMNB".
      * El Telefono no es un campo obligatorio por eso no se valida.
		DISPLAY "Ingrese Telefono: " WITH NO ADVANCING.
		ACCEPT TELEFONO NO BEEP.
      * Guardo el registro en caso de error notifico al usuario.
		WRITE REG-CLIENTE INVALID KEY PERFORM ERROR-CLIENTE-EXISTE.
		DISPLAY SK.
		DISPLAY SPACES ERASE.
		MOVE "A" TO CORTE.
		PERFORM UNTIL CORTE IS = "S" OR CORTE IS = "N"
		DISPLAY "Desea cargar otro cliente? (S/N) "
		ACCEPT CORTE NO BEEP
		END-PERFORM.

	ERROR-CLIENTE-EXISTE.
		DISPLAY "EL CLIENTE YA EXISTE".

	STUB.
		DISPLAY "NO IMPLEMENTADO".
		DISPLAY "Presione una tecla para continuar... ".
		ACCEPT CORTE NO BEEP.