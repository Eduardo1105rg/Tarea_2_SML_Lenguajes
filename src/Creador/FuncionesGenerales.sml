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

    (* Funcion para el regitros de nuevos libros en el sistema de la biblioteca. *)
    fun ingresar_nuevo_registro () = 
        let

            (* Se solicita el codigo del libro *)
            val () = print ("\nIngrese el codigo del libro. (E.j: LIB9999): ")
            val codigoLibro = entradaDeTeclado ();
            val () = print ("\n")

            (* Se solicita la fecha de publicacion del libro*)
            val () = print ("\nIngrese la fecha de publicacion del libro. (E.j: 2018-11-10): ")
            val fechaPublicacion = entradaDeTeclado ();
            val () = print ("\n")

            (* Se solicita  el nombre del autor*)
            val () = print ("\nIngrese el nombre del autor. (E.j: George Orwell): ")
            val nombreAutor = entradaDeTeclado ();
            val () = print ("\n")

            (* Se solicita el genero del libro *)
            val () = print ("\nIngrese el genero del libro (E.j: Fantasia): ")
            val generoLibro = entradaDeTeclado ();
            val () = print ("\n")

            (* Se solicita  *)
            val () = print ("\nIngrese la cantidad de copias disponibles del libro (E.j: 123): ")
            val numeroCopias = ingresarNumeros ();
            val () = print ("\n")

            (* Variable con los datos que vamos a registrar. *)
            val lineaRegistro = codigoLibro ^ "," ^ fechaPublicacion ^ "," ^ nombreAutor ^ "," ^ generoLibro ^ "," ^ Int.toString numeroCopias

            fun guardar_registro (count) =
                let 

                    (* Se solicita la ruta del archivo a usar. *)
                    val () = print ("\nIngrese la ruta del archivo a usar (E.j: src/Data/libros.csv): ")
                    val rutaArchivo = entradaDeTeclado ();
                    val () = print ("\n")

                    (* val _ = ( ManejoArchivos.escribirLinea (rutaArchivo, lineaRegistro )) handle ManejoArchivos.ArchivoNoEncontrado => (

                        print ("Error: No se pudo abrir el archivo...\n");
                        raise ManejoArchivos.ErrorArchivo
                    ) *)
                in
                    (ManejoArchivos.escribirLinea (rutaArchivo, lineaRegistro)) handle ManejoArchivos.ArchivoNoEncontrado => (
                        if count > 0 then (
                            print ("Error: No se pudo abrir el archivo. Intentando nuevamente...\n");
                            guardar_registro (count - 1)
                        ) else (
                            print ("Error: No se pudo registrar el libro despues de varios intentos.\n");
                            raise ManejoArchivos.ErrorArchivo
                        )
                    )
                end
        in
            guardar_registro 3;
            print ("Se ha registrado el nuevo contenido exitosamente.\n")
        end;

    (* <<Fin de la funcion de agregar registros. *)

    (* >>Funcion para el borrado de los datos del archivo, para esto se solicita la ruta del archivo. *)
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
            (print ("El contenido del archivo ha sido borrado y reestablecido con la linea inicial.\n"))
        end; 
    (* <<Fin de la funcion de borrar datos *)


end;