(* 

    >> En este archivo se definen todas las funciones necesarias para el menu del programa.

 *)

structure Menu =
struct
  

    (* funcion para mostrar la opciones del menu de busqueda. *)
    fun mostrar_menu_busqueda () =
        let
            (* Se tuvi que meter en un let ya que solo se estaba imprimiendo la primera linea. *)
            val () = print ("\n>> Menu de busqueda.\n")
            val () = print ("Opciones disponibles...\n")
            val () = print (">> A) Buscar libro pro codigo.\n")
            val () = print (">> B) Buscar por autor.\n")

            val () = print (">> S) Salir del programa.\n")
            val () = print ("-- Ingresa una de las opciones anteriores: ")

        in
            ()
        end;
    (* Fin *)

    (* funcion para reaccionar a la opcion seleccionada por el usuario, depliega la funcion que corresponde a la opcion seleccioanda.*)
    fun menu_busqueda (registros) = (        
        mostrar_menu_busqueda ();
        case FuncionesGenerales.entradaDeTeclado () of 
            "A" => (FuncionesGenerales.buscar_libros_por_codigo (registros); menu_busqueda registros)
        | "a" => (FuncionesGenerales.buscar_libros_por_codigo (registros); menu_busqueda registros)

        | "B" => (FuncionesGenerales.buscar_libros_por_autor (registros); menu_busqueda registros)
        | "b" => (FuncionesGenerales.buscar_libros_por_autor (registros); menu_busqueda registros)
        | "S" => print ("\nSaliendo del programa...\n")
        | "s" => print ("\nSaliendo del programa...\n")
        | _ => (print ("\nLa opcion ingresada es invalida, intantelo nuevamente.\n"); menu_busqueda registros)
        );
    (* Fin *)
       
    (* >> Este seria la parte del menu de la palicacion. *)
    fun mostrarMenu () = 
        let
            (* Se tuvi que meter en un let ya que solo se estaba imprimiendo la primera linea. *)
            val () = print ("\n>> Menu principal\n")
            val () = print ("Sistema Analizador...\n")
            val () = print ("Opciones disponibles...\n")
            val () = print (">> A) Mostrar libros en un rango de copias.\n")
            val () = print (">> B) Autores con mas de 5 libros.\n")
            val () = print (">> C) Buscar libros por codigo o autor.\n")
            val () = print (">> D) Cantidad de libros por genero.\n")
            val () = print (">> E) Resumen general.\n")
            (* val () = print (">> ) .\n")
            val () = print (">> ) .\n") *)

            val () = print (">> S) Salir del programa.\n")
            val () = print ("-- Ingresa una de las opciones anteriores: ")

        in
            ()
        end;
    (* Fin *)
    
    (* Funcion del menu principal del sistema. *)
    fun MenuPrincipal registros = 
        let 
            val _ = mostrarMenu ();  
        in
            case FuncionesGenerales.entradaDeTeclado () of 
                "A" => (
                    FuncionesGenerales.mostrar_libros_en_rango registros handle FuncionesGenerales.ValorNoValido =>
                        print ("Regresando al menu principal\n");
                    MenuPrincipal registros
                )
            | "a" => (
                    FuncionesGenerales.mostrar_libros_en_rango registros handle FuncionesGenerales.ValorNoValido =>
                        print ("Regresando al menu principal\n");
                    MenuPrincipal registros
                )
            | "B" => (
                    FuncionesGenerales.mostrar_autores_5_libros registros handle ManejoArchivos.ErrorArchivo =>
                        print ("Regresando al menu principal\n");
                    MenuPrincipal registros
                )
            | "b" => (
                    FuncionesGenerales.mostrar_autores_5_libros registros handle ManejoArchivos.ErrorArchivo =>
                        print ("Regresando al menu principal\n");
                    MenuPrincipal registros
                )
            | "C" => (
                    menu_busqueda registros;
                    MenuPrincipal registros
                )
            | "c" => (
                    menu_busqueda registros;
                    MenuPrincipal registros
                )
            | "D" => (
                    FuncionesGenerales.mostrar_cant_libros_por_genero_especifico registros handle ManejoArchivos.ErrorArchivo =>
                        print ("Regresando al menu principal\n");
                    MenuPrincipal registros
                )
            | "d" => (
                    FuncionesGenerales.mostrar_cant_libros_por_genero_especifico registros handle ManejoArchivos.ErrorArchivo =>
                        print ("Regresando al menu principal\n");
                    MenuPrincipal registros
                )
            | "E" => (
                    FuncionesGenerales.mostrar_resumen_general registros handle ManejoArchivos.ErrorArchivo =>
                        print ("Regresando al menu principal\n");
                    MenuPrincipal registros
                )
            | "e" => (
                    FuncionesGenerales.mostrar_resumen_general registros handle ManejoArchivos.ErrorArchivo =>
                        print ("Regresando al menu principal\n");
                    MenuPrincipal registros
                )
            | "S" => print ("\nSaliendo del programa...\n")
            | "s" => print ("\nSaliendo del programa...\n")
            | _ => (
                    print ("\nLa opcion ingresada es invalida, intentelo nuevamente\n");
                    MenuPrincipal registros
                )
        end;
    (* Fin *)

    (* Funcion para solicitar los datos del archivo al usuario, ademas se redirige a la funcion del menu principal del sistema. *)
    fun solicitarDatos () = 
        let
            val registros = (FuncionesGenerales.cargar_datos_archivo ()) handle ManejoArchivos.ErrorArchivo => (
                print ("No se pudo cargar el archivo. Cerrando el programa...\n");
                raise ManejoArchivos.ErrorArchivo
            )
        in
            MenuPrincipal (registros)
        end;
    (* Fin *)

end;