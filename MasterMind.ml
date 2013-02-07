
val lenght' = ref 0;

val myLine = TextIO.inputLine TextIO.stdIn;

val letras = [#"A",#"B",#"C",#"D",#"E",#"F",#"G",#"H",#"I",#"J",#"K",#"L",#"M",#"N",#"O",#"P",#"Q",#"R",#"S",#"T",#"U",#"V",#"W",#"X",#"Y",#"Z"];

val oldConjectures=[]; // antigua pobalcion
|1

fun length "" = 0 | length (str) = 1 + length (implode(tl(explode(str)))); // saca la longitud de la cadena

load "Random"; //variable
		
fun nextRandomChar(a)=chr(floor(Random.random(Random.newgen())*real(ord(chr(ord(a)+1))-ord(#"A")))+ord(#"A")); //internet
	
fun charComp(c1,c2)=
	if ord(c1)=ord(c2) then 0 
	else if ord(c1)>ord(c2) then 1 else ~1;

fun letraLimite("",ch)=ch
	|letraLimite(str,ch)= 
	if charComp(hd(explode(str)),ch)>=0 then letraLimite(implode(tl(explode(str))),hd(explode(str)))
	else letraLimite(implode(tl(explode(str))),ch);
	
fun lLimite(str)=
	letraLimite(str,hd(explode(str)));
	
fun letraAleatoria(str)=
	nextRandomChar(lLimite(str));
	
fun firstConjecture(str,len)=
	if len = 0 then []
	else if letraAleatoria(str) = letraAleatoria(str) then firstConjecture(str,len) else letraAleatoria(str)::firstConjecture(str,len-1);

	(*funcion que recibe una cadena y un caracter y determina el numero de veces que la cadena contiene dicho caracter*)
fun contiene("",ch)= 0 | contiene(str,ch)= if hd(explode(str))=ch then 1+contiene(implode(tl(explode(str))),ch) else contiene(implode(tl(explode(str))),ch);

fun letraEnCadena("",n)=(#" ") | letraEnCadena(str,n)= if contiene(str,hd(explode(str))) = n 
then hd(explode(str)) else letraEnCadena(implode(tl(explode(str))),n);

(*funcion que recibe una lista y un valor y retorna verdadero si el valor se encuentra entre los elementos de la lista*)
fun yaHabiaDicho([],str)= false
|yaHabiaDicho(lst,str)= 
	if hd(lst)=str then true else yaHabiaDicho(tl(lst),str);

	(*funcion que recibe una cadena y devuelve una lista tomando como separador el caracter ":"*)
fun response(str)=
	if hd(explode(str))=(#":") then (ord(hd(tl(explode(str)))) - ord(#"0"))::[]
	else (ord(hd(explode(str))) - ord(#"0"))::response(implode(tl(explode(str))));
	
fun len([])=0
| len(lst)= 1+len(tl(lst));
	
fun reordenar([])=[]
|reordenar(lst)=if len(lst)>2 then hd(tl(lst))::hd(lst)::reordenar(tl(tl(lst))) else 
	if len(lst)=2 then tl(lst)@[hd(lst)] else lst;

fun combinar([])=[]|combinar([x,y])=[y,x]
|combinar(x::y::z)=[x]@combinar(z)@[y]|combinar(x::y)=combinar(y)@[x]
|combinar([x])=[x];

fun invertir([])=[]
|invertir(x::y)=invertir(y)@[x];

fun ordenAleatorio(lst)= case floor(Random.random(Random.newgen())*7.0) of 
	0 => reordenar(lst) | 1 => invertir(lst) | 2 => combinar(lst) | 3 => (reordenar o combinar)(lst)
	| 4 => (combinar o reordenar)(lst) | 5 => (invertir o reordenar)(lst) | 6 => (combinar o invertir o reordenar)(lst);

fun lastConjecture(x::[])=x | lastConjecture([])=[]
| lastConjecture(lst)=lastConjecture(tl(lst));
	
fun nextConjecture(str,(x,y))=
	if (x+y)=length(str) then |
		if implode(ordenAleatorio(explode(str)))=implode(ordenAleatorio(explode(str))) then 
			ordenAleatorio(explode(str)) 
		else nextConjecture(str,(x,y))
	else
		if x=y then 
			letraEnCadena(str,x)::tl(lastConjecture(oldConjectures)) 
		else 
			if x>y then 
				letraEnCadena(str,x)::tl(lastConjecture(oldConjectures))
			else letraEnCadena(str,y)::tl(lastConjecture(oldConjectures));

fun genGuess(str,(x,y))=
	if yaHabiaDicho(oldConjectures,nextConjecture(str,(x,y))) then
		genGuess(str,(x,y))
	else nextConjecture(str,(x,y));
	
fun juego(str,intentos,(x,y))=
	if intentos = 0 then
		implode(firstConjecture(str,length(str)-2))
	else
		implode(genGuess(str,(x,y)));
		
fun stst("\n")= "" |stst(str)=implode(hd(explode(str))::explode(stst(implode(tl(explode(str))))));

fun toTupla([x,y])= (x,y)
|toTupla(x::y)= (x,hd(y));

	juego(stst(myLine),0,toTupla(response("0:0")));

fun jugar((x,y)::z,n)=if n=x then y else jugar(z,n);

