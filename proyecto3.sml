(*
Instituto Tecnologico de Costa Rica
Escuela de Computacion
Curso de lenguajes de programacion
Proyecto 3
Esteban González Damazio
Alejandro Jimenez Gamboa
*)


(*
Datatypes usados en el proyecto
*)
datatype pattern =
			Wildcard
		| Variable of string
		| UnitP
		| ConstP of int
		| TupleP of pattern list
		| ConstructorP of string * pattern

datatype valu =
			Const of int
		| Unit
		| Tuple of valu list
		| Constructor of string * valu

fun g f1 f2 p =
		let
			val r = g f1 f2
		in
			case p of
				  Wildcard 							=> f1()
				| Variable x 						=> f2 x
				| TupleP ps 						=> List.foldl (fn(p, i) => (r p) + i) 0 ps
				| ConstructorP(_, p) 		=> r p
				| _ 										=> 0
		end
(* Excepcion que indica que no hay respuesta *)
exception NoAnswer

(*
1. Escriba una función only_capitals que toma una lista de string y retorna una lista
de string que contiene solamente strings en el argumento que inician con una letra
mayúscula. Se asume que todos los string contienen al menos 1 caracter. Utilice
List.filter, Char.isUpper y String.sub de la biblioteca de SML.
*)

fun checkCap s = Char.isUpper(String.sub(s,0));
fun only_capitals (xs : string list) =  List.filter checkCap xs
(* val test1 = only_capitals ["A","B","C", "lower"] *)

(*
2. Escriba una función longest_string1 que toma una lista de string y retorna el string
más grande en la lista. Si la lista está vacía retorna “”. En el caso de empates,
retorna el string más cercano al inicio de la lista. Use foldl, String.size y sin
recursividad (obviamente la recursividad está dada solamente en foldl).
*)

fun longest_stringl l =
	let
		fun longest_auxl (a, b) = (if String.size(a) > String.size(b) then a else b);
	in
		List.foldl longest_auxl "" l
	end

(* val test2 = longest_stringl ["A","bc","C"] *)

(*
3. Escriba una función longest_string2 que es exactamente como longest_string1
excepto que en el caso de empates retorna el string más cercano al final de la
lista. Use foldl y String.size.
*)

fun longest_string2 l =
	let
		fun longest_aux2 (a, b) = (if String.size(a) >= String.size(b) then a else b);
	in
		List.foldl longest_aux2 "" l
	end

(* val test2 = longest_string2 ["A","bc","C"] *)
(*
4. Escriba las funciones longest_string_helper, longest_string3 y longest_string4, tal
que:
• longest_string3 tiene el mismo comportamiento que longest_string1 y
longest_string4 tiene el mismo comportamiento que longes_string2.
• longest_string_helper es de tipo (int * int -> bool) -> string list -> string
(cabe resaltar el uso de currying). Esta función se ve como longest_string1 y
longest_string2 pero es más general porque toma una función como argumento.
• Si longest_string_helper se le es pasada a una función que se comporta como >
(entonces retorna true cuando el primer argumento es estrictamente mayor
que el segundo), entonces la función retornada tiene el mismo comportamiento
que longest_string1.
• longest_ string3 y longest_string4 son definidas con val-bindings y partial
applications de longest_string_helper.
*)

fun longest_string_helper opr =
    List.foldl (fn (x,max) => if opr (String.size(x), String.size(max))
			      then x
			      else max
			   ) ""

(* val longest_string3 = longest_string_helper (op >)

val longest_string4 = longest_string_helper (op >=)

val test4a = longest_string3 ["A","bc","C"]
val test4b = longest_string4 ["A","B","C"] *)

(*
5. Escriba una función longest_capitalized que toma una lista de string y retorna el
string más grande de la lista que inicie con mayúscula, o “” si no hay strings que
cumplan. Asuma que todos los strings tiene al menos 1 caracter. Use val-bindings y
el operador o de la biblioteca de SML para composición de funciones. Resuelva los
problemas de empates como se hizo en el ejercicio 2.
*)

val longest_capitalized  = longest_stringl o only_capitals  (* El simbolo 'o' es para la composicion de funciones *)
(* val test5 = longest_capitalized ["A","bc","Ce"] *)
(*
6. Escriba una función rev_string que toma un string y retorna ese mismo string pero
en orden inverso. Use el operador o, la función de la biblioteca de SML rev para
invertir listas y funciones del módulo String de la biblioteca de SML.
*)

val rev_string = implode o List.rev o explode (*implode function = generates the string containing the characters in the list l.
												This is equivalent to concat (List.map str l). This raises Size if the resulting \
												string would have size greater than maxSize.
												explode function = is the list of characters in the string s.
												Tomado de la pagina: http://sml-family.org/Basis/string.html
												*)

(*
7. Escriba una función first_answer de tipo ('a -> 'b option) -> 'a list -> 'b (los 2
argumentos son currying). El primer argumento debe ser aplicado a elementos del
segundo argumento en orden hasta la primera vez que retorne SOME v para algún
v, y entonces v es el resultado de llamar a first_answer. Si el primer argumento
retorna NONE para todos los elementos de la lista, entonces first_answer debe
lanzar una excepción NoAnswer.
*)

fun first_answer f lista =
    case lista of
				[] => raise NoAnswer
			| (hd::tl) => case f(hd) of
		      NONE => first_answer f tl
		    | SOME value => value;

(*
8. Escriba la función all_answers de tipo ('a -> 'b list option) -> 'a list -> 'b list
option (note que los 2 argumentos son currying). El primer argumento debe ser
aplicado a los elementos del segundo argumento. Si retorna NONE por
cualquier elemento, entonces el resultado de all_answers es NONE. Si no, la
llamada del primer argumento va a producir SOME lst1, SOME lst2, …, SOME lstn y
el resultado de all_answers es SOME lst donde lst es lst1, lst2, …, lstn anexados
(appended), en este caso el orden no importa. Use el operador @ y una nota final:
si se llama a la función all_answers f [], debe retornar SOME [].
*)

fun all_answers f lista =
    let
        fun all_answers_aux (f, lista, answers) = case lista of
                                      [] => SOME answers
                                    | hd::tl => case f(hd) of
                                                   NONE => NONE
                                                 | SOME v => all_answers_aux(f, tl, v @ answers)
    in
        all_answers_aux(f, lista, [])
    end


(*
Para el ejercicio 9 necesitará de la definición de pattern y la función g.
9. (a) Use g para definir una función count_wildcards que toma un pattern y retorna
cuantos Wildcards ese pattern contiene.
(b) Use g para definir una función count_wild_and_variable_lengths que toma un
pattern y retorna el número de Wildcards que están en pattern mas la suma de las
longitutes (lengths) de todas las variables que el pattern contenga. Utilice
String.size, lo que interesan son los nombres de las variables, el nombre de los
constructores es irrelevante.
(c) Use g para definir una función count_some_var que toma un string y un pattern
(como un pair) y retorna el número de veces que el string aparece como una
variable en el pattern. Nos interesan solamente los nombres de las variables, el
nombre de los constructores es irrelevante.
*)

fun count_wildcards lista =
	g (fn (x) => 1) (fn (x) => 0) lista


fun count_wild_and_variable_lengths lista =
	g (fn (x) => 1) (fn (x) => String.size x) lista

fun count_some_var (var, lista) =
g (fn (x) => 0) (fn (x) => if x = var then 1 else 0) lista;

(*
Para el ejercicio 10 necesitará solamente de la definición de pattern.
10. Escriba una función check_pat que toma un pattern y retorna true si todas las
variables que aparecen en el pattern son distintas unas de otras (tiene diferentes
strings). Los nombres de los constructores son irrelevantes. Programe esta función
con dos helper functions. La primera toma un pattern y retorna una lista de strings
contenidos en las variables. En este caso use foldl y el operador @. La segunda
función toma la lista de strings y verifica que no haya repetidos. En este caso use
List.exists.
*)

fun check_pat pat =
	let
		fun get_vars_from_pattern (pat, var_list) =
			 	case pat of
				  Variable x 						=> x::var_list
				| TupleP ps 						=> List.foldl (fn(p, i) => get_vars_from_pattern (p, var_list)@i) [] ps
				| ConstructorP(_, p) 		=> get_vars_from_pattern (p, var_list)
				|	_ 										=> []

		fun check_duplicates var_list =
			case var_list of
				[]       =>  false
	  	| hd::tl  =>  (List.exists (fn y => hd = y) tl) orelse (check_duplicates tl)

	in
		not (check_duplicates (get_vars_from_pattern (pat, [])))
		(* get_vars_from_pattern (pat, []) *)
	end


	(* val pattern4 = TupleP([ConstP 12, Variable "var1", Variable "var2",	ConstructorP("constr1", Wildcard)]);
	val test10_2 = check_pat pattern4;
	val test11 = match (Const(1), UnitP) = NONE
	val test11_2 = match(Unit, UnitP) = SOME [];
	val pattern_t = Tuple([Const 12, Constructor("blah", Unit), Constructor("constr1",	Tuple([]))]);
	val pattern_tp = TupleP([ConstP 12, Variable "var1", ConstructorP("constr1", Wildcard)]);
	val test11_3 = match(pattern_t, pattern_tp) = SOME [("var1", Constructor("blah", Unit))];
	val test12 = first_match Unit [UnitP] = SOME [] *)



(*
Para el ejercicio 11 necesitará de las definiciones de pattern, valu y las reglas para
matching definidas anteriormente.
11. Escriba una función match que toma un valu * pattern y retorna un (string *
valu) list option, que será NONE si el pattern no hace matching y SOME lst
donde lst es la lista de bindings si se hizo matching. En este caso el pattern
matching debe tener 7 casos (branches). El branch para tuplas usará all_answers
del ejercicio 8 y ListPair.zip de la biblioteca de SML.
*)

fun match (valu, pat) =
		case pat of
				Wildcard 							=> SOME []
			| Variable s						=> SOME [(s, valu)]
			| UnitP									=> (case valu of
																				Unit => SOME []
																			| _ => NONE)
			| ConstP cp							=> (case valu of
																				Const c => if c = cp then SOME [] else NONE
																			| _ => NONE)
			| TupleP ps 						=> (case valu of
																				Tuple vs	=>
																						(if length vs = length ps then
																								all_answers (match) (ListPair.zip (vs, ps))
																							else NONE)
																			| _ => NONE)


			| ConstructorP (s1, p)	=> (case valu of
																				Constructor (s2, v) =>
																						(if s1 = s2 then
																							let val result = match (v, p)
																							in
																								if isSome result then result else NONE
																							end
										 												else NONE)
																			| _ => NONE)
			(* | _ 										=> NONE *)

			(* Se piden 7 ramas, sin embargo, SML muestra un error ya que
			la séptima rama, comentada en la línea anterior, es redundante  *)





(*
El ejercicio 12 necesitará las definiciones de pattern y valu.
12. Escriba una función first_match que toma un valor y una lista de patterns y
retorna (string * valu) list option, que sería NONE si ningún pattern hace
matching o SOME lst donde lst es la lista de bindings para el primer pattern en la
lista que hace matching. Use la función first_answer del ejercicio 7.
*)

	fun first_match valu pattern_list =
		SOME (first_answer (fn x => match (valu, x)) pattern_list)
		handle NoAnswer => NONE














	(*  *)
