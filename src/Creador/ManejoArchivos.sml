structure ManejoArchivos =
struct
  
    exception ArchivoNoEncontrado; (* Esto es para definir la excepcion. *)
    exception ErrorArchivo;
    (* Funcion para leer un archivo y retornar el contenido de este en forma de una lista con cada linea del archivo. *)
    fun leerArchivo ruta = 
        (* La estructura 'let', 'int', 'end', es como un bloquen, en la primera parte se crear las variables, o bueno se calculan
        para su uso, en la parte del 'in', se hace uso de estas variables y el 'end', nada mas define el final de todo. *)
        let 

            val archivo = (SOME (TextIO.openIn ruta)) handle _ => NONE (* Esto abre el archivo en modo lectura,
            ademas capturamos una excepcion en caso de que no se pueda abri. *)
            
            (* Funcion para leer las lineas del archivo. *)
            fun leerLineas archivo = 
                case TextIO.inputLine archivo of
                NONE => [] (* No hay mas lineas. *)
                | SOME linea => linea :: leerLineas (archivo) (* Agregar la linea que acabamos de leer a la lista de lineas *)
        
        in 
            (* Un poderoso case. *)
            case archivo of 
                NONE => raise ArchivoNoEncontrado (* Esto es para lanzar la excepcion. *)
                | SOME arch => 
                    let 
                        val contenido = leerLineas (arch) (* Leer todo el contenido y separarlo en lineas. *) 
                    
                    in 
                        TextIO.closeIn arch; (* Estos cierra el archivo que se abio *)
                        contenido (* Se retorna la informacion del archivo separada en una lista con cada linea del archivo. *)
                    end

        end;
    (* Fin de la funcion para leer el contenido de un archivo. *)

    (* Funcion para la escritura de los datos en un archivo *)
    fun escribirLinea (ruta, contenido) =
        let
            (* Abrir el archivo en modo 'append' para agregar nuevas líneas al final *)
            val archivo = (SOME(TextIO.openAppend ruta)) handle _ => NONE (* Lanzar el eror al no poder arbir el archivo.*)
        in
            case archivo of 
                NONE => raise ArchivoNoEncontrado
                | SOME arch => 
                    let
                        val () = TextIO.output (arch, contenido ^ "\n");
                        val () = TextIO.closeOut arch; 
                    in
                        ()
                    end
        end;
    (* Fin de la funcion para escribir contenido al final de un archivo. *)

    (* Funcion para borrar todo el contenido de un archivo, ademas escribe la primera linea con los datos generales de la estructura. *)
    fun resetearContenido (ruta) =
        let
            (* Abrir el archivo en modo 'openOut' para agregar nuevas líneas al final *)
            val archivo = (SOME(TextIO.openOut ruta)) handle _ => NONE (* Lanzar el eror al no poder arbir el archivo.*)
        in
            case archivo of 
                NONE => raise ArchivoNoEncontrado
                | SOME arch => 
                    let
                        val () = TextIO.output (arch, "codigo,fecha_publicacion,autor,genero,copias_disponibles" ^ "\n");
                        val () = TextIO.closeOut arch; 
                    in
                        ()
                    end
        end;
    (* Fin de la funcion para el borrado del contenido de un archivo. *)
    
end;