structure FuncionesGenerales =
struct

    (* Funcion para permitir qie el usuariopueda ingresar texto mediante el teclado. *)
    fun entradaDeTeclado () = 
        case TextIO.inputLine TextIO.stdIn of
            NONE => 
                (print ("Entrada invalida, intentelo nuevamente\n"); entradaDeTeclado ())

            | SOME datos =>
                (* datos; *)
                (* Hay que eliminar ese \n que se pone al final de las lineas. *)
                let
                    val longitud = size datos
                in
                    (* Verifica si en el elemento final se encuntra ese salto de linea. *)
                    if longitud > 0 andalso String.substring(datos, longitud - 1, 1) = "\n" then
                        String.substring(datos, 0, longitud - 1)  
                    else
                        datos  
                end;
    (* Fin de la funcion para permitir el ingreso de texto, mediante el teclado. *)

    (* Funcion para permitir que el usuario ingresa valores numericos positivos. *)
    fun ingresarNumeros () = 
        let 
            fun ingresar () =
                let
                    val entrada = entradaDeTeclado ()
                in
                    case Int.fromString entrada of
                        NONE => (
                            print ("El dato ingresado no es un numero, por favor ingresalo nuevamente.\n");
                            ingresar ()
                        )
                    | SOME n => 
                        if n < 0 then
                            (
                                print ("El valor ingresado no puede ser menor que 0, ingreselo nuevamente.\n");
                                ingresar ()
                            )
                        else 
                            n
                end
        in
            ingresar ()
        end;
    (* Fin de la funcion para ingresar numeros. *)

    (* funcion para permitir que el usuario cargue los datos  *)
    fun cargar_datos_archivo () = 
        let
            (* Función auxiliar para cargar el registro *)
            fun cargar_registro (intentos) =
                let 
                    (* Solicitar la ruta del archivo *)
                    val () = print ("\nIngrese la ruta del archivo a usar (E.j: src/Data/libros.csv): ")
                    val rutaArchivo = entradaDeTeclado ()
                    val () = print ("\n")
                in
                    (* Intentar leer el archivo *)
                    ManejoArchivos.leerArchivo rutaArchivo
                    handle ManejoArchivos.ArchivoNoEncontrado => (
                        if intentos > 0 then (
                            print ("Error: No se pudo abrir el archivo. Intentando nuevamente...\n");
                            cargar_registro (intentos - 1)  (* Volver a intentar *)
                        ) else (
                            print ("Error: No se pudo abrir el archivo despues de varios intentos.\n");
                            raise ManejoArchivos.ErrorArchivo  (* Lanzar excepción si agota intentos *)
                        )
                    )
                end
        in
            (* Intentar cargar el archivo con hasta 3 intentos *)
            let
                val datos = cargar_registro 3 (* Cargar los datos del archivo *)
                val datos_procesados = List.map ManejoArchivos.dividirPorComas datos
            in
                datos_procesados  (* Retornar los datos procesados como una lista de listas *)
            end
        end;

    (* Fin de la funcion para cargar los datos  *)






end;