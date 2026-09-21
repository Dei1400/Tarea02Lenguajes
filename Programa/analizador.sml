type libro = string * string * string * string * int;
(*codigo,autor,genero,fecha,copias*)

fun separarPorComa (linea: string) : string list = String.tokens (fn c => c = #",") linea;
(*separa un string en partes usando la coma*)

fun listaALibro (partes: string list) : libro = case partes of [codigo, autor, genero, fecha, copiasStr] => (codigo, autor, genero, fecha, valOf (Int.fromString copiasStr)) | _ => raise Fail "Linea con formato invalido";
(*convierte una lista de strings en un valor de tipo libro*)

fun pedirTexto (mensaje: string) : string = let val _ = print (mensaje ^ ": ") in case TextIO.inputLine TextIO.stdIn of NONE => "" | SOME linea => String.substring (linea, 0, String.size linea - 1) end;
(*pide un dato al usuario por teclado*)

fun leerOpcion () : string =
    case TextIO.inputLine TextIO.stdIn of
        NONE => ""
      | SOME linea => String.substring (linea, 0, String.size linea - 1);
(*lee la opcion del menu sin imprimir mensaje extra*)

fun leerLibros (ruta: string) : libro list =
    let
        val entrada = TextIO.openIn ruta
        fun leerTodasLasLineas () =
            case TextIO.inputLine entrada of
                NONE => []
              | SOME linea => linea :: leerTodasLasLineas ()
        val lineas = leerTodasLasLineas ()
        val _ = TextIO.closeIn entrada
        val lineasLimpias = List.map (fn l => String.substring (l, 0, String.size l - 1)) lineas
    in
        List.map (fn l => listaALibro (separarPorComa l)) lineasLimpias
    end;
(*abre el archivo, lee todas las lineas, y convierte cada una en un libro*)

fun mostrarLibro (lib: libro) : unit =
    let val (codigo, autor, genero, fecha, copias) = lib in
        print (codigo ^ " | " ^ fecha ^ " | " ^ autor ^ " | " ^ genero ^ " | copias: " ^ Int.toString copias ^ "\n")
    end;
(*imprime un libro en pantalla de forma legible*)

(* ===== opcion a: libros por rango de copias ===== *)

fun filtrarPorRango (libros: libro list, minimo: int, maximo: int) : libro list =
    List.filter (fn (_, _, _, _, copias) => copias >= minimo andalso copias <= maximo) libros;
(*deja solo los libros con copias entre minimo y maximo*)

fun insertarOrdenado (lib: libro, lista: libro list) : libro list =
    case lista of
        [] => [lib]
      | (l2 :: resto) =>
            let val (_, _, _, _, copiasLib) = lib
                val (_, _, _, _, copias2) = l2
            in
                if copiasLib >= copias2
                then lib :: lista
                else l2 :: insertarOrdenado (lib, resto)
            end;
(*inserta un libro en la posicion correcta de una lista ya ordenada de mayor a menor*)

fun ordenarDescendente (libros: libro list) : libro list =
    case libros of
        [] => []
      | (primero :: resto) => insertarOrdenado (primero, ordenarDescendente resto);
(*ordena toda la lista insertando cada libro uno por uno*)

fun opcionA (libros: libro list) : unit =
    let
        val minStr = pedirTexto "Copias minimas"
        val maxStr = pedirTexto "Copias maximas"
        val minimo = valOf (Int.fromString minStr)
        val maximo = valOf (Int.fromString maxStr)
        val filtrados = filtrarPorRango (libros, minimo, maximo)
        val ordenados = ordenarDescendente filtrados
    in
        List.app mostrarLibro ordenados
    end;
(*pide el rango, filtra, ordena y muestra el resultado*)

(* ===== opcion b: autores con al menos 5 libros ===== *)

fun buscarEIncrementar (clave: string, contador: (string * int) list) : (string * int) list =
    case contador of
        [] => [(clave, 1)]
      | ((k, c) :: resto) =>
            if k = clave
            then (k, c + 1) :: resto
            else (k, c) :: buscarEIncrementar (clave, resto);
(*busca la clave en la lista de contadores, si existe le suma 1, si no la agrega*)

fun contarPorClave (libros: libro list, obtenerClave: libro -> string) : (string * int) list =
    let
        fun procesar (lista: libro list, acumulado: (string * int) list) : (string * int) list =
            case lista of
                [] => acumulado
              | (lib :: resto) => procesar (resto, buscarEIncrementar (obtenerClave lib, acumulado))
    in
        procesar (libros, [])
    end;
(*cuenta cuantas veces aparece cada valor de la clave (autor, genero, etc) en toda la lista*)

fun opcionB (libros: libro list) : unit =
    let
        val conteoAutores = contarPorClave (libros, fn (_, autor, _, _, _) => autor)
        val conCincoOMas = List.filter (fn (_, cantidad) => cantidad >= 5) conteoAutores
    in
        List.app (fn (autor, cantidad) => print (autor ^ ": " ^ Int.toString cantidad ^ " libros\n")) conCincoOMas
    end;
(*cuenta libros por autor y muestra solo los que tienen 5 o mas*)

(* ===== opcion c: buscar por codigo o autor ===== *)

fun opcionC (libros: libro list) : unit =
    let
        val busqueda = pedirTexto "Ingrese codigo o autor a buscar"
        val encontrados = List.filter (fn (codigo, autor, _, _, _) => codigo = busqueda orelse autor = busqueda) libros
    in
        if List.length encontrados = 0
        then print "No se encontraron resultados.\n"
        else List.app mostrarLibro encontrados
    end;
(*filtra los libros donde el codigo o el autor coincida con lo buscado*)

(* ===== opcion d: cantidad de libros por genero ===== *)

fun opcionD (libros: libro list) : unit =
    let
        val genero = pedirTexto "Ingrese el genero"
        val coincidencias = List.filter (fn (_, _, g, _, _) => g = genero) libros
    in
        print ("Cantidad de libros de genero " ^ genero ^ ": " ^ Int.toString (List.length coincidencias) ^ "\n")
    end;
(*filtra por genero y cuenta cuantos hay*)

(* ===== opcion e: resumen general ===== *)

fun buscarMaximo (contador: (string * int) list) : (string * int) =
    case contador of
        [(k, c)] => (k, c)
      | ((k, c) :: resto) =>
            let val (k2, c2) = buscarMaximo resto in
                if c >= c2 then (k, c) else (k2, c2)
            end
      | [] => ("(sin datos)", 0);
(*recorre la lista de contadores y devuelve el que tiene el numero mas alto*)

fun libroConMasCopias (libros: libro list) : libro =
    case libros of
        [lib] => lib
      | (lib :: resto) =>
            let val (_, _, _, _, copiasLib) = lib
                val mejorResto = libroConMasCopias resto
                val (_, _, _, _, copiasResto) = mejorResto
            in
                if copiasLib >= copiasResto then lib else mejorResto
            end;
(*recorre la lista comparando copias y devuelve el libro con mas*)

fun obtenerMesAnio (lib: libro) : string =
    let val (_, _, _, fecha, _) = lib in
        String.substring (fecha, 0, 7)
    end;
(*saca los primeros 7 caracteres de la fecha, por ejemplo "2006-04"*)

fun opcionE (libros: libro list) : unit =
    let
        val porGenero = contarPorClave (libros, fn (_, _, genero, _, _) => genero)
        val porAutor = contarPorClave (libros, fn (_, autor, _, _, _) => autor)
        val porMes = contarPorClave (libros, obtenerMesAnio)

        val libroMasCopias = libroConMasCopias libros
        val (autorTop, cantidadAutorTop) = buscarMaximo porAutor
        val (generoTop, cantidadGeneroTop) = buscarMaximo porGenero
        val (mesTop, cantidadMesTop) = buscarMaximo porMes
    in
        print "\n=== RESUMEN GENERAL ===\n";
        print "1. Cantidad de libros por genero:\n";
        List.app (fn (g, c) => print ("   " ^ g ^ ": " ^ Int.toString c ^ "\n")) porGenero;
        print "2. Libro con mas copias disponibles:\n   ";
        mostrarLibro libroMasCopias;
        print ("3. Autor con mas libros: " ^ autorTop ^ " (" ^ Int.toString cantidadAutorTop ^ " libros)\n");
        print ("4. Genero con mas libros: " ^ generoTop ^ " (" ^ Int.toString cantidadGeneroTop ^ " libros)\n");
        print ("5. Mes-anio con mas publicaciones: " ^ mesTop ^ " (" ^ Int.toString cantidadMesTop ^ " publicaciones)\n")
    end;
(*combina todas las funciones anteriores para armar el resumen*)

(* ===== menu principal ===== *)

fun mostrarMenuAnalizador () : unit =
    print "\n=== ANALIZADOR - GESTION BIBLIOTECARIA ===\na. Libros populares por rango de copias\nb. Autores con al menos 5 libros\nc. Buscar por codigo o autor\nd. Cantidad de libros por genero\ne. Resumen general\nf. Salir\nOpcion: ";

fun cicloAnalizador (libros: libro list) : unit =
    let
        val _ = mostrarMenuAnalizador ()
        val opcion = leerOpcion ()
    in
        case opcion of
            "a" => (opcionA libros; cicloAnalizador libros)
          | "b" => (opcionB libros; cicloAnalizador libros)
          | "c" => (opcionC libros; cicloAnalizador libros)
          | "d" => (opcionD libros; cicloAnalizador libros)
          | "e" => (opcionE libros; cicloAnalizador libros)
          | "f" => print "Saliendo...\n"
          | _   => (print "Opcion invalida.\n"; cicloAnalizador libros)
    end;

fun main () : unit =
    let
        val ruta = pedirTexto "Ingrese la ruta del archivo a analizar"
        val libros = leerLibros ruta
    in
        cicloAnalizador libros
    end;