(* 
    Instrucciones de ejecucion:

    >> Usando el interpetre de sml:
        Paso 1: abrir la terminar y dirigirse a la ruta de la capeta de Analizador.
        Paso 2: Ejecutar el el comando >> sml <<, para abrir el interprete.
        Paso 3: Ejecutar los siguiantes lineas una por una en el interprete:  
            use "ManejoArchivos.sml";
            use "FuncionesGenerales.sml";
            use "Menu.sml"; 
            use "Main.sml"
        Paso 4: Una vez cargados todo los archivos, ejecutar el comando >> main(); << para iniciar el programa.


    >> Usando el compilador mlton:
        Paso 1: abrir la terminar y dirigirse a la ruta de la capeta 'src'.
        Paso 2: Descomentar esta linea de la parte inferior del main: val _ = main ();
        Paso 3: Guardar los cambios.
        Paso 2: Compilar usando el siguiente comando: mlton mlton src/Analizador/compilar.mlb
        Paso 3: Ejecutar usando el siguiente comando: ./src/Analizador/compilar

*)
(* val _ = Menu.MenuPrincipal (); *)

fun main() =
    Menu.solicitarDatos ();

(* val _ = main (); *)