//Nombre: Jesús Huerta Aguilar
//Matricula: 202041509
Algoritmo transpuesta
	//Instrucción inicial
	Definir n,m,c,r,e,longval,reclongval,switch Como Entero
	Definir val Como real
	Definir iz,dr,x,textval,textvalf Como Caracter
	//Instrucción inicial
	Escribir '-- TRANSPOSICIÓN DE MATRICES (nxm) --'
	Escribir 'Ingresar tamaño de las matrices (n,m > 1)'
	Repetir
		//tamaño de la matriz
		Escribir ''
		Escribir 'Cantidad de filas (n)'
		Leer n
		Escribir 'Cantidad de columnas (m)'
		Leer m
		Si n <= 1 o m <= 1 Entonces
			Escribir '[!] ERROR: Verifica tu información [!]'
		FinSi
	Hasta Que n > 1 y m > 1
	Dimension val[n,m]
	//Ingreso de valores
	Escribir '|||| Ingresar valores para la matriz A ||||'
	para r <- 1 hasta n Con Paso 1 Hacer
		Escribir '>> FILA ',r
		para c <- 1 hasta m con paso 1 hacer
			leer val[r,c]
			//longitud del valor
			longval <- Longitud(ConvertirATexto(val[r,c]))
			si longval > reclongval Entonces
				reclongval <- longval
			FinSi
		FinPara
	FinPara
	//Imprimir matriz A
	Escribir ''
	para r <- 1 hasta n con paso 1 Hacer
		si r = redon(n/2) Entonces
			Escribir '    A = ' Sin Saltar
		SiNo
			Escribir '        ' Sin Saltar
		FinSi
		para c <- 1 hasta m con paso 1 Hacer
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
	//Imprimir transpuesta de matriz A
	Escribir ''
	Escribir 'La transpuesta de A es:'
	Escribir ''
	para c <- 1 hasta m con paso 1 Hacer
		si c = redon(m/2) Entonces
			Escribir '  A^t = ' Sin Saltar
		SiNo
			Escribir '        ' Sin Saltar
		FinSi
		para r <- 1 hasta n con paso 1 Hacer
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