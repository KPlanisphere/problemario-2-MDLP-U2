//Nombre: Jesús Huerta Aguilar
//Matricula: 202041509
Algoritmo suma_mtrcs
	Definir val,n,i,c,r,res,e,longval,reclongval,switch Como Entero
	Definir l,iz,dr,x,textval,textvalf Como Caracter
	Escribir '-- SUMA DE DOS MATRICES DE ENTEROS CUADRADAS (nxn) --'
	Escribir 'Ingresar tamaño de las matrices (n > 1)'
	Repetir
		Leer n
		Si n <= 1 Entonces
			Escribir '[!] ERROR: Verifica tu información [!]'
		FinSi
	Hasta Que n > 1
	Dimension val[n,n,2]; Dimension res[n,n]
	Escribir ''
	//Ingreso de valores para cada matriz
	reclongval <- 0
	Escribir '|||| Ingresar valores para la matriz A ||||'
	para i <- 1 hasta 2 con paso 1 Hacer
		Si i = 2 Entonces
			Escribir '|||| Ingresar valores para la matriz B ||||'
		FinSi
		para r <- 1 hasta n Con Paso 1 Hacer
			Escribir '>> FILA ',r
			para c <- 1 hasta n con paso 1 Hacer
				leer val[r,c,i]
				//longitud del valor
				longval <- Longitud(ConvertirATexto(val[r,c,i]))
				si longval > reclongval Entonces
					reclongval <- longval
				FinSi
			FinPara
		FinPara
		Escribir ''
	FinPara
	Escribir '    //// RESULTADOS \\\\ '
	//Creacion de matriz A y B
	para i <- 1 hasta 2 con paso 1 Hacer
		Escribir ''
		Segun i
			1: 
				l <- 'A'
			2: 
				l <- 'B'
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
					textval <- ConvertirATexto(val[r,c,i])
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
	FinPara
	//Sumar matrices A + B
	Escribir '- - - - - - - - - - - - - - - - - - - - - - -  '
	//Sumar aij + bij
	//determinar el valor mayor
	switch <- 0
	reclongval <- 0
	para r <- 1 hasta n con paso 1 Hacer
		para c <- 1 hasta n con paso 1 Hacer
			res[r,c] <- val[r,c,1] + val[r,c,2]
			//longitud del valor
			longval <- Longitud(ConvertirATexto(res[r,c]))
			si longval > reclongval Entonces
				reclongval <- longval
			FinSi
		FinPara
	FinPara
	//impresión de la matriz A+B
	para r <- 1 hasta n Con Paso 1 Hacer
		si r = redon(n/2) Entonces
			Escribir 'A + B = ' Sin Saltar
		SiNo
			Escribir '        ' Sin Saltar
		FinSi
		para c <- 1 hasta n con paso 1 Hacer
			//concatenación de espacios
			iz <- ' '; dr <- ' '; x <- ' '
			switch <- 0
			para e <- 1 hasta reclongval+2 con paso 1 Hacer
				textval <- ConvertirATexto(res[r,c])
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