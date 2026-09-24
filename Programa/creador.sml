type libro = string * string * string * string * int;
(*codigo,autor,genero,fecha,copias *)

fun separarPorComa (linea: string) : string list = String.tokens (fn c => c = #",") linea;
(*String.tokens hace una lista de strings separando por el carácter especificado*)
(*fn es una expresión que crea una función anónima*)

fun listaALibro (partes: string list) : libro = case partes of [codigo, autor, genero, fecha, copiasStr] => (codigo, autor, genero, fecha, valOf (Int.fromString copiasStr)) | _ => raise Fail "Linea con formato invalido";
(*listaLibro convierte una lista de strings en un valor de tipo libro*)
(*ValOf convierte un valor de tipo int option en un valor de tipo int*)
(*fromString convierte un string en un valor de tipo int*)
(*case es una expresión que permite hacer coincidir un valor con patrones*)
(*raise Fail lanza una excepción con el mensaje especificado*)
(*let es una expresión que permite definir variables locales*)

fun pedirTexto (mensaje: string) : string = let val _ = print (mensaje ^ ": ") in case TextIO.inputLine TextIO.stdIn of NONE => "" | SOME linea => String.substring (linea, 0, String.size linea - 1) end;
(*pedir texto toma string *)
(*print imprime un mensaje en la consola*)
(*TextIO.inputLine lee una línea de texto desde la entrada estándar*)
(*String.substring toma un substring de un string dado un índice inicial y una longitud*)

fun pedirLibro () : libro = let val codigo = pedirTexto "Codigo del libro" val autor = pedirTexto "Autor" val genero = pedirTexto "Genero" val fecha = pedirTexto "Fecha de publicacion (YYYY-MM-DD)" val copiasStr = pedirTexto "Copias disponibles" in (codigo, autor, genero, fecha, valOf (Int.fromString copiasStr)) end;
(*pedir libro pide al usuario los datos de un libro y devuelve un valor de tipo libro*)

fun libroALinea (lib: libro) : string = let val (codigo, autor, genero, fecha, copias) = lib in codigo ^ "," ^ autor ^ "," ^ genero ^ "," ^ fecha ^ "," ^ Int.toString copias end;
(*libroALinea convierte un valor de tipo libro en un string separado por comas*)
(*Int.toString convierte un valor de tipo int en un string*)

fun agregarLibro (ruta: string, lib: libro) : unit = let val salida = TextIO.openAppend ruta in TextIO.output (salida, libroALinea lib ^ "\n"); TextIO.closeOut salida end;
(*agregarLibro agrega un libro a un archivo de texto*)
(*TextIO.openAppend abre un archivo en modo de escritura al final*)
(*TextIO.output escribe en el archivo*)
(*TextIO.closeOut cierra el archivo*)

fun limpiarCatalogo (ruta: string) : unit =
    let
        val salida = TextIO.openOut ruta
    in
        TextIO.closeOut salida
    end;

fun mostrarMenuCreador () : unit = print "\n=== CREADOR - GESTION BIBLIOTECARIA ===\n1. Agregar libro\n2. Limpiar catalogo\n3. Salir\nOpcion: ";

fun leerOpcion () : string =
    case TextIO.inputLine TextIO.stdIn of
        NONE => ""
      | SOME linea => String.substring (linea, 0, String.size linea - 1);

fun cicloCreador (ruta: string) : unit =
    let
        val _ = mostrarMenuCreador ()
        val opcion = leerOpcion ()
    in
        case opcion of
            "1" => (agregarLibro (ruta, pedirLibro ()); print "Libro agregado.\n"; cicloCreador ruta)
          | "2" => (limpiarCatalogo ruta; print "Catalogo limpiado.\n"; cicloCreador ruta)
          | "3" => print "Saliendo...\n"
          | _   => (print "Opcion invalida.\n"; cicloCreador ruta)
    end;

fun main () : unit =
    let
        val ruta = pedirTexto "Ingrese la ruta del archivo del catalogo"
    in
        cicloCreador ruta
    end;