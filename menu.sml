(*
Insituto Tecnológico de Costa Rica 
Curso: Lenguajes de programación
Taller #2: Leer cuántas vocales, consonantes y signos hay en el archivo txt. Cuál es el promedio del largo por palabra y palabra más frecuente
Creado por Jean Paul Barrit
*) 

(*Lee el archivo y retorna una lista de tipo char*)
fun parse file =
let
    fun next_String input = (TextIO.inputAll input) 
    val stream = TextIO.openIn file
    val a = next_String stream
in
    explode(a)
end;
	
	
(* Cuenta la cantidad de veces que se encuentra la letra o el símbolo enviado*)
fun contarLetras(lista: char list, letra: char)=
	let
		fun aux (lista: char list, i:int,cont:int,a:char)=
			if i=0 then
				cont
			else
				if hd(lista)=a then
					aux(tl(lista),i-1,cont+1,a)
				else
					aux (tl(lista),i-1,cont,a)
	in 
		aux(lista,length(lista),0,letra)
	end;
	
(*Cuenta el promedio del largo de las palabras y lo retorna*)
fun contarPromedio(lista:char list)=
	let
		fun aux2 (lista: char list, i:int,cont:int,divisor:int,salto: char,espacio:char)=
			if i=0 then
				cont div divisor
			else
				if hd(lista)=salto orelse hd(lista)=espacio then
					aux2(tl(lista),i-1,cont+1,divisor+1,salto,espacio)
				else
					aux2(tl(lista),i-1,cont+1,divisor,salto,espacio)
	in
		aux2(lista,length(lista),0,0,#"\n",#" ")
	end;
	
(*A partir de la lista char crea las palabras y lo retorna en una lista*)	
fun formarPalabra(lista:char list)=
	let
		fun aux3 (lista: char list, i:int,cont:string,listaPalabra:string list,salto: char,espacio:char)=
			if i=0 then
				listaPalabra
			else
				if hd(lista)=salto orelse hd(lista)=espacio then
					aux3(tl(lista),i-1,"",cont::listaPalabra,salto,espacio)
				else
					aux3(tl(lista),i-1,cont^Char.toString(hd(lista)),listaPalabra,salto,espacio)
	in 
		aux3(lista,length(lista),"",[],#"\n",#" ")
	end;
		
(*Cuenta la cantidad de veces que se repite esa palabra*)	
fun contarPalabras(lista: string list, letra: string)=
	let
		fun aux4(lista: string list, i:int,cont:int,a:string)=
			if i=0 then
				cont
			else
				if hd(lista)=a then
					aux4(tl(lista),i-1,cont+1,a)
				else
					aux4(tl(lista),i-1,cont,a)
	in 
		aux4(lista,length(lista),0,letra)
	end;

(*Función que se encarga de verificar cuál es la palabra que más se repite*)
fun palabraRepetida(lista: string list)=
	let
		fun ganador(lista: string list,won: int, palabra: string)=
			if lista=[] then 
				palabra^"->"^Int.toString(won)
			else
				if contarPalabras(lista,hd(lista))>won then 
					ganador(tl(lista),contarPalabras(lista,hd(lista)),hd(lista))
				else
					ganador(tl(lista),won,palabra)
	in
		ganador(lista,0," ")
	end;
	
(*Función que se encarga de solo contar cuantas letras o símbolos hay en en la lista de char*)
fun cuentaSimbolos (lista:char list,listaP: char list)= 
	let
		fun auxi (listaP: char list,lista: char list,cont: int)=
			if listaP = [] then
				cont
			else
				auxi (tl(listaP),lista,cont+contarLetras(lista,hd(listaP)))
	in
		auxi (listaP,lista,0)
	end;
	
(*Función principal donde recibe la dirección del archivo txt*)
fun openFile(file: string)=
	let
		val lista = parse(file)
		val fn1 = cuentaSimbolos(lista,[#"a",#"e",#"i",#"o",#"u"])
		val fn2 = cuentaSimbolos(lista,[#"b",#"c",#"d",#"f",#"g",#"h",#"j",#"k",#"l",#"m",#"n",#"p",#"q",#"r",#"s",#"t",#"v",#"w",#"x",#"y",#"z"])
		val fn3 = cuentaSimbolos(lista,[#"!",#"#",#"$",#"%",#"&",#"/",#"(",#")",#"=",#"?",#".",#",",#";",#"{",#"}",#"[",#"]",#"+",#"*",#"-"])
		val fn4 = contarPromedio(lista)
		val fn5 = palabraRepetida(formarPalabra(lista))
		val vocales = "Vocales: "^Int.toString(fn1)
		val consonantes = "Consonantes: "^Int.toString(fn2)
		val signos = "Signos: "^Int.toString(fn3)
		val promed = "Promedio: "^Int.toString(fn4)
		val palabraM = "Palabra mas repetida: "^fn5
		val listaFinal = [vocales,consonantes,signos,promed,palabraM]
	in
		listaFinal
	end;