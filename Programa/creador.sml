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