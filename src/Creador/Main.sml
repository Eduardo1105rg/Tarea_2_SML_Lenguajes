(* 
    Instrucciones de ejecucion:

    >> Usando el interpetre de sml:
        Paso 1: abrir la terminar y dirigirse a la ruta de la capeta de Creador.
        Paso 2: Ejecutar el el comando >> sml <<, para abrir el interprete.
        Paso 3: Ejecutar los siguiantes lineas una por una en el interprete:  
            use "ManejoArchivos.sml";
            use "FuncionesGenerales.sml";
            use "Menu.sml"; 
            use "Main.sml";
        Paso 4: Una vez cargados todo los archivos, ejecutar el comando >> main(); << para iniciar el programa.


    >> Usando el compilador mlton:
        Paso 1: abrir la terminar y dirigirse a la ruta de la capeta 'src'.
        Paso 2: Descomentar esta linea de la parte inferior del main: val _ = main ();
        Paso 3: Guardar los cambios.
        Paso 2: Compilar usando el siguiente comando: mlton mlton src/Creador/compilar.mlb
        Paso 3: Ejecutar usando el siguiente comando: ./src/Creador/compilar

*)

(* Funcion principal encargada de ejecutar le programa. *)
fun main() =
    Menu.MenuPrincipal ();

(* val _ = main (); *)
