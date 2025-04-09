(* 

    >> En este archivo se definen todas las funciones necesarias para el menu del programa.

 *)

structure Menu =
struct
  
    (* >> Este seria la parte del menu de la aplicacion. *)
    fun mostrarMenu () = 
        let
            (* Se tuvi que meter en un let ya que solo se estaba imprimiendo la primera linea. *)
            val () = print ("\n>> Menu principal\n")
            val () = print ("Sistema para el ingreso de nuevos datos en los registros del sistema...\n")
            val () = print ("Opciones disponibles...\n")
            val () = print (">> A) Agregar un nuevo libro.\n")
            val () = print (">> B) Borrar datos.\n")
            val () = print (">> S) Salir del programa.\n")
            val () = print ("-- Ingresa una de las opciones anteriores: ")

        in
            ()
        end;
    (* Fin de la funcion de mostrar la opciones del menuPrincipal. *)
    
    (* Funcion del menu principal del sistema. *)
    fun MenuPrincipal () = (

        mostrarMenu();
        case FuncionesGenerales.entradaDeTeclado () of 
            "A" => (FuncionesGenerales.ingresar_nuevo_registro () handle ErrorArchivo => print("Regresando al menu principal\n"); MenuPrincipal ())
            | "a" => (FuncionesGenerales.ingresar_nuevo_registro () handle ErrorArchivo => print("Regresando al menu principal\n"); MenuPrincipal ())

            |"B" => (FuncionesGenerales.borrar_registros () handle ErrorArchivo => print("Regresando al menu principal\n"); MenuPrincipal ())
            | "b" => (FuncionesGenerales.borrar_registros () handle ErrorArchivo => print("Regresando al menu principal\n"); MenuPrincipal ())

            | "S" => print ("\nSaliendo del programa...\n")
            | "s" => print ("\nSaliendo del programa...\n")

            | _ => (print ("\nLa opcion ingresada es invalida, intentalo nuevamente\n"); MenuPrincipal ())
    );

end;