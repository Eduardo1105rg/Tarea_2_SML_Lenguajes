structure FuncionesGenerales =
struct


    (* 
    

    
        Funciones de List.: https://smlfamily.github.io/Basis/list.html#LIST:SIG:SPEC
     *)


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


    (* funcion *)
    fun ordenamientoQS [] _ = []
        | ordenamientoQS (pivote::lista) cmp =
            let
                val menores = List.filter (fn x => cmp x pivote) lista
                val mayores = List.filter (fn x => not (cmp x pivote)) lista
            in
              ordenamientoQS mayores cmp @ [pivote] @ ordenamientoQS menores cmp
            end
    (* Fin *) 

    exception ValorNoValido;
    (* funcion para mostrar los libros que tengan un numero de copias que se encuentre dentro del rango que indica el usuario. Lis libros se imprimen de manera ascendente.*)
    fun mostrar_libros_en_rango (datos) = 
        let

            (* Se solicita  *)
            val () = print ("\nIngrese la cantidad de copias maximo: ")
            val numeroCopiasMax = ingresarNumeros ();
            val () = print ("\n")

            (* Se solicita  *)
            val () = print ("\nIngrese la cantidad de copias minimo: ")
            val numeroCopiasMin = ingresarNumeros ();
            val () = print ("\n")

            val () = if numeroCopiasMax < numeroCopiasMin then
                (print("Error: El valor minino es mayor que el maximo.\n"); raise ValorNoValido)
                else
                    ()
            (* Esto es para optener los libros en un rango de copias especifico   | Esto se parece un poco a JS. *)
            val lisbrosFiltrados = List.filter (fn libro =>
                let
                  val copiasString = List.nth (libro,4) (* Esto es para sacar una posicion especifica dentro de una lista. *)
                  val numCopias = Int.fromString copiasString 
                in
                  case numCopias of
                    NONE => false
                   | SOME n => n >= numeroCopiasMin andalso n <= numeroCopiasMax
                end
            ) datos

            (* Esto seria para odenar los libros  *)
            val librosOrdenados = ordenamientoQS (lisbrosFiltrados) (fn libroA => fn libroB =>
                let
                    val cantCopiasA = Int.fromString (List.nth (libroA, 4))
                    val cantCopiasB = Int.fromString (List.nth (libroB, 4))
                in
                  case (cantCopiasA, cantCopiasB) of
                    (SOME libA, SOME libB) => libA < libB (*Este parte de encarga del orden descendente o ascendente*)
                    | _ => false
                     
                end
            
            ) 
        in
            (* Aqui se imprime los libros libros ya ordenados. *)
            if List.length librosOrdenados = 0 then
                print ("No hay libros en el rango indicado. \n")
            else 
                print("===== Datos de los libros en el rango indicado:\n");

                (* Se aplica a los elementos de izquierda a derecha, Definicion de  *)
                (List.app (fn libro =>
                    let

                        val codigo = List.nth (libro, 0)
                        val fecha = List.nth (libro, 1)
                        val autor = List.nth (libro, 2)
                        val genero = List.nth (libro, 3)
                        val copias = List.nth (libro, 4)
                    in
                        print("\n-- Datos del libro:\n");
                        print(">> Codigo: " ^ codigo ^ "\n");
                        print(">> Fecha: " ^ fecha ^ "\n");
                        print(">> Autor: " ^ autor ^ "\n");
                        print(">> Genero: " ^ genero ^ "\n");
                        print(">> Copias: " ^ copias ^ "\n")

                    end
                ) librosOrdenados)
        end;
    (* Fin *)


    exception Fallo;
    (* Funcion *)
    fun actualizar_lista [] _ _ = raise Fallo 
    | actualizar_lista (elemento::lista) indice nuevo =
            if indice = 0 then
                nuevo::lista
            else 
                elemento::actualizar_lista lista (indice - 1) nuevo;
    (* FIN *)

    (* Funcion *)
    fun optener_indice (anterior, [], indice) = NONE
        | optener_indice (anterior, elemento::lista, indice) =
            if anterior = elemento then
                SOME indice
            else 
                optener_indice (anterior, lista, indice + 1);
    (* FIN *)


    (* Funcion *)
    fun buscar_indice elemento lista =
        optener_indice (elemento, lista, 0);

    (* FIN *)

    (* Funcion *)
    fun contar_libros_por_autor_sistema datos = 
        let
            fun contar_libros_autor ([], []) [] = ([], [])
            | contar_libros_autor (autores, []) [] = (autores, [])
            | contar_libros_autor ([], cantLibros) [] = ([], cantLibros)
            | contar_libros_autor (autores, cantLibros) [] = (autores, cantLibros)  (* Caso faltante *)
            | contar_libros_autor (autores, cantLibros) (libro::libros) = 
                    let
                        val autor = List.nth (libro, 2)  (* Columna del autor *)
                        val validarExistencia = List.find (fn autorActual => autorActual = autor) autores
                    in
                        case validarExistencia of 
                            NONE => 
                                contar_libros_autor (autor::autores, 1::cantLibros) libros
                        | SOME _ => 
                                let
                                    val indice = Option.valOf (buscar_indice autor autores)
                                    val nuevo_elemento = actualizar_lista cantLibros indice (List.nth (cantLibros, indice) + 1)
                                in
                                    contar_libros_autor (autores, nuevo_elemento) libros
                                end
                    end
        in
            contar_libros_autor ([], []) datos
        end;

    (* 
    
    Warning: src/Analizador/FuncionesGenerales.sml 209.17-225.23.
    Function is not exhaustive.
        missing pattern: ((:: _, _), nil) | ((nil, :: _), nil)
        in: fun contar_libros_autor ([], [])   ...  ento) libros end end
    Warning: src/Analizador/FuncionesGenerales.sml 236.17-241.67.
    Function is not exhaustive.
        missing pattern: ((:: _, nil), _) | ((nil, :: _), _)
        in: fun filtrado ([], []) resultado =  ...  ntidades) resultado)
    
     *)

    (* Fin *)

    (* Funcion *)
    fun filtrar_autores_con_5_libros (autores, cantLibros) =
        let
            fun filtrado ([],[]) resultado = resultado
                | filtrado ([], cantLibros) resultado = resultado
                | filtrado (autores, []) resultado = resultado
                | filtrado (autor::autores2, cant::cantidades) resultado =
                    if cant >= 5 then
                        filtrado (autores2, cantidades) (autor::resultado)
                    else
                        filtrado (autores2, cantidades) (resultado)
        in
          filtrado (autores, cantLibros) []
        end;
    (* Fin *)


    (* funcion *)
    fun mostrar_autores_5_libros (datos) = 
        let
          val (autores, cantLibros) = contar_libros_por_autor_sistema (datos);
          val autores_filtrados = filtrar_autores_con_5_libros (autores,  cantLibros);
        in
            if List.length autores_filtrados = 0 then
                print ("No hay autores con mas de 5 libros registrados.\n")
            else (
                print ("===== Autores con mas de 5 libros publicados:\n");
                List.app (fn autor => print (autor ^ "\n")) autores_filtrados
            )
        end;
    (* Fin *)

    (* funcion *)
    fun buscar_libros_por_codigo (datos) = ()
    (* Fin *)

    (* funcion *)
    fun buscar_libros_por_autor (datos) = ()
    (* Fin *)

    (* funcion *)
    fun mostrar_cant_libros_por_genero (datos) = ()
    (* Fin *)

    (* funcion *)
    fun mostrar_resumen_general (datos) = ()
    (* Fin *)


end;