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
                    (* Verificar si el ultimo caracter es un salto de linea*)
                    val entradaSinSalto = if longitud > 0 andalso String.substring(datos, longitud - 1, 1) = "\n" then
                            String.substring(datos, 0, longitud - 1)  
                        else
                            datos

                in
                    (* Validar si la cadena ingresada no es vacia. *)
                    if String.size entradaSinSalto = 0 then
                        (print ("La entrada no puede estar vacia, intentelo nuevamente\n"); entradaDeTeclado ())
                    else
                        entradaSinSalto  
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

    (* Función para el registro de nuevos libros en el sistema de la biblioteca *)
    fun ingresar_nuevo_registro () = 
        let
            (* Función para verificar si el código ya existe *)
            fun verificarCodigoExistente codigo rutaArchivo =
                let
                    (* Leer los datos del archivo *)
                    val datos = (ManejoArchivos.leerArchivo rutaArchivo) handle ArchivoNoEncontrado => (
                        print ("Error: No se pudo abrir el archivo.\n");
                        raise ArchivoNoEncontrado
                    );
                    
                    (* Obtener los códigos de los libros *)
                    val codigosLibros = List.map (fn linea => List.nth (ManejoArchivos.dividirPorComas linea, 0)) datos
                in
                    (* Verificar si el código ya existe *)
                    List.exists (fn cod => cod = codigo) codigosLibros
                end;

            (* Solicitar el código del libro, validando que no exista *)
            fun solicitarCodigo rutaArchivo =
                let
                    val () = print ("\nIngrese el codigo del libro. (E.j: LIB9999): ")
                    val codigoLibro = entradaDeTeclado ();
                    val () = print ("\n")
                in
                    (* Validar que el código no esté duplicado *)
                    if verificarCodigoExistente codigoLibro rutaArchivo then (
                        print ("El codigo ingresado ya existe. Intente nuevamente.\n");
                        solicitarCodigo rutaArchivo
                    ) else
                        codigoLibro
                end;

            (* Se solicita la ruta del archivo antes de comenzar *)
            val () = print ("\nIngrese la ruta del archivo a usar (E.j: src/Data/libros.csv o ../Data/libros.csv): ")
            val rutaArchivo = entradaDeTeclado ();
            val () = print ("\n")

            (* Solicitar código validando duplicados *)
            val codigoLibro = solicitarCodigo rutaArchivo;

            (* Solicitar el resto de los datos *)
            val () = print ("\nIngrese la fecha de publicacion del libro. (E.j: 2018-11-10): ")
            val fechaPublicacion = entradaDeTeclado ();
            val () = print ("\n")

            val () = print ("\nIngrese el nombre del autor. (E.j: George Orwell): ")
            val nombreAutor = entradaDeTeclado ();
            val () = print ("\n")

            val () = print ("\nIngrese el genero del libro (E.j: Fantasea): ")
            val generoLibro = entradaDeTeclado ();
            val () = print ("\n")

            val () = print ("\nIngrese la cantidad de copias disponibles del libro (E.j: 123): ")
            val numeroCopias = ingresarNumeros ();
            val () = print ("\n")

            (* Crear la línea de registro *)
            val lineaRegistro = codigoLibro ^ "," ^ fechaPublicacion ^ "," ^ nombreAutor ^ "," ^ generoLibro ^ "," ^ Int.toString numeroCopias;

            (* Guardar el registro en el archivo *)
            fun guardar_registro (count) =
                (ManejoArchivos.escribirLinea (rutaArchivo, lineaRegistro)) handle ManejoArchivos.ArchivoNoEncontrado => (
                    if count > 0 then (
                        print ("Error: No se pudo abrir el archivo. Intentando nuevamente...\n");
                        guardar_registro (count - 1)
                    ) else (
                        print ("Error: No se pudo registrar el libro despues de varios intentos.\n");
                        raise ManejoArchivos.ErrorArchivo
                    )
                );
        in
            guardar_registro 3;
            print ("Se ha registrado el nuevo contenido exitosamente.\n")
        end;


    (* <<Fin de la funcion de agregar registros. *)

    (* >>Funcion para el borrado de los datos del archivo, para esto se solicita la ruta del archivo. *)
    fun borrar_registros () = 

        let 
            val () = print ("\nIngrese la ruta del archivo a usar (E.j: src/Data/libros.csv o ../Data/libros.csv): ")
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