//Nombre: Jesús Huerta Aguilar
//Matricula: 202041509
Algoritmo identidad
	Definir n,c,r,e,i,longval,reclongval,switch Como Entero
	Definir diag,zero,val Como real
	Definir iz,dr,l,x,textval,textvalf Como Caracter
	//Instrucción inicial
	Escribir '-- DETECTOR DE MATRICES IDENTIDAD-- '
	Escribir 'NOTA: la matriz identidad es una matriz cuadrada (nxn)'
	Escribir ''
	Escribir 'Ingresa el tamaño de la matriz (n > 1)'
	Repetir
		Leer n
		Si n <= 1 Entonces
			Escribir '[!] ERROR: Verifica tu información [!]'
		FinSi
	Hasta Que n > 1
	Dimension val[n,n]
	Escribir ''
	//Ingreso de valores
	Escribir '|||| Ingresar valores para la matriz ||||'
	para r <- 1 hasta n Con Paso 1 Hacer
		Escribir '>> FILA ',r
		para c <- 1 hasta n con paso 1 hacer
			leer val[r,c]
			//longitud del valor
			longval <- Longitud(ConvertirATexto(val[r,c]))
			si longval > reclongval Entonces
				reclongval <- longval
			FinSi
			//determinar identidad
			si r = c Entonces
				Si val[r,c] = 1 Entonces
					diag <- diag + val[r,c]
				FinSi
			SiNo
				zero <- zero + val[r,c]
			FinSi
		FinPara
	FinPara
	//Imprimir matriz A o I
	//Salida identidad
	Escribir ' '
	Si diag = n y zero = 0 Entonces
		Escribir ' >> LA MATRIZ ES IDENTIDAD <<'
		i = 1
	SiNo
		Escribir ' >> LA MATRIZ NO ES IDENTIDAD <<'
		i = 2
	FinSi
	Segun i
		1: l <- 'I'
		2: l <- 'A'
	FinSegun
	para r <- 1 hasta n con paso 1 Hacer
		si r = redon(n/2) Entonces
			Escribir '    ',l,' = ' Sin Saltar
		SiNo
			Escribir '        ' Sin Saltar
		FinSi
		para c <- 1 hasta n con paso 1 Hacer
			//concatenación de espacios
			iz <- ' '; dr <- ' '; x <- ' '
			switch <- 0
			para e <- 1 hasta reclongval+2 con paso 1 Hacer
				textval <- ConvertirATexto(val[r,c])
				textvalf <- Concatenar(Concatenar(iz,textval),dr)
				e <- Longitud(textvalf)
				si e < reclongval+2 y switch = 0 Entonces
					iz <- Concatenar(iz,x)
					switch <- 1
				SiNo
					dr <- Concatenar(x,dr)
					switch <- 0
				FinSi
			FinPara
			Escribir '|',textvalf  Sin Saltar
		FinPara
		Escribir "|"
	FinPara
FinAlgoritmo