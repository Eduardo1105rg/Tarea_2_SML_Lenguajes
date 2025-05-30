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
                        val contenido = leerLineas (arch); (* Leer todo el contenido y separarlo en lineas. *) 
                        val contenidoSinEncabezado =
                            case contenido of
                                [] => [] (* Si no hay contenido, retornar lista vacía *)
                            | _ => tl contenido
                    in 
                        TextIO.closeIn arch; (* Estos cierra el archivo que se abio *)
                        contenidoSinEncabezado (* Se retorna la informacion del archivo separada en una lista con cada linea del archivo. *)
                    end

        end;
    (* Fin de la funcion para leer el contenido de un archivo. *)

    (* Funcion para la division de una lista de texto, en donde cada elementos se va a separar por un ','. *)
    fun dividirPorComas linea =
        (* String.tokens (fn c => c = #",") linea; *)
        let
            val longitud = String.size linea
            val linea_limpia = 
                if longitud > 0 andalso String.substring (linea, longitud - 1, 1) = "\n" then
                    String.substring (linea, 0, longitud - 1)  (* Eliminar el salto de línea *)
                else
                    linea
        in
            String.tokens (fn c => c = #",") linea_limpia
        end;
    (* Fin de la funcion separacion de datos. *)

    (* Funcion para el procesado del contenido del archivo. *)
    fun procesarArchivoCSV ruta =
        let
            val datos = (leerArchivo ruta) handle ArchivoNoEncontrado => (print ("Error: No se pudo abrir el archivo. Verifique la ruta.\n"); []);
        in
            List.map dividirPorComas datos
        end;
    (* Fin de la funcion para el procesado de los archivos. *)

end;