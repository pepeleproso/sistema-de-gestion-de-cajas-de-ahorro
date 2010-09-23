	IDENTIFICATION DIVISION.
	PROGRAM-ID. TPFINAL.
	AUTHOR. GRUPO3TM.
	ENVIRONMENT DIVISION.
	CONFIGURATION SECTION.
	SOURCE-COMPUTER. PC.
	OBJECT-COMPUTER. PC.
	SPECIAL-NAMES.
				DECIMAL-POINT IS COMMA.
	DATA DIVISION.
	WORKING-STORAGE SECTION.
	01 RAYA PIC X(40) VALUE ALL "-".
	01 CORTE PIC A.
	77 opt PIC 9.
    
	PROCEDURE DIVISION.
	INICIO.
		PERFORM MENU-ADMINISTRADOR UNTIL opt = 6.
		STOP RUN.
      *  MENU-ADMINISTRADOR
      * Muestra el menu con las acciones que puede realizar un usuario administrador
      * Estas acciones no puede ser realizadas por el cliente.
      * La opcion 6 sale del programa.

      * TODO:: FALTA OBTENER EL NOMBRE DEL USUARIO, 
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
			PERFORM STUB
			WHEN 2
			PERFORM STUB
			WHEN 3
			PERFORM STUB
			WHEN 4
			PERFORM STUB
			WHEN 5
			PERFORM STUB.

	STUB.
		DISPLAY "NO IMPLEMENTADO".
		DISPLAY "Presione una tecla para continuar... ".
		ACCEPT CORTE NO BEEP.