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



    fun ingresar_nuevo_registro () = ()



    (* Funcion para el borrado de los datos del archivo, para esto se solicita la ruta del archivo. *)
    fun borrar_registros () = 

        let 
            val () = print ("\nIngrese la ruta del archivo a usar (E.j: src/Data/libros.csv): ")
            val rutaArchivo = entradaDeTeclado ();
            val () = print ("\n")

            val _ = ( ManejoArchivos.resetearContenido rutaArchivo) handle ManejoArchivos.ArchivoNoEncontrado => (

                print ("Error: No se pudo abrir el archivo...\n");
                raise ManejoArchivos.ErrorArchivo
            )
        in 
            ()
        end; 









end;