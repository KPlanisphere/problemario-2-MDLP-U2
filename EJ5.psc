//Nombre: Jesús Huerta Aguilar
//Matricula: 202041509
Algoritmo mtrz_triang
	Definir n,c,r,e,longval,reclongval,switch,ginf,gsup,tinf,tsup Como Entero
	Definir val Como real
	Definir iz,dr,x,textval,textvalf Como Caracter
	//Instrucción inicial
	Escribir '-- DETECTOR DE MATRICES TRIANGULARES (nxn)-- '
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
	Escribir '|||| Ingresar valores para la matriz A ||||'
	para r <- 1 hasta n Con Paso 1 Hacer
		Escribir '>> FILA ',r
		para c <- 1 hasta n con paso 1 hacer
			leer val[r,c]
			//longitud del valor
			longval <- Longitud(ConvertirATexto(val[r,c]))
			si longval > reclongval Entonces
				reclongval <- longval
			FinSi
			//determinar traiangularidad
			si r > c Entonces
				gsup <- gsup + 1
				si val[r,c] = 0 Entonces
					tsup <- tsup + 1
				FinSi
			SiNo
				si r < c Entonces
					ginf <- ginf + 1
					si val[r,c] = 0 Entonces
						tinf <- tinf + 1
					FinSi
				FinSi
			FinSi
		FinPara
	FinPara
	//Salida traiangularidad
	Escribir ' '
	Si tsup = gsup y tinf = ginf Entonces
		Escribir ' >> LA MATRIZ ES DIAGONAL <<'
	SiNo
		Si tsup = gsup Entonces
			Escribir ' >> LA MATRIZ ES TRIANGULAR SUPERIOR <<'
		SiNo
			Si tinf = ginf Entonces
				Escribir ' >> LA MATRIZ ES TRIANGULAR INFERIOR <<'
			SiNo
				Escribir ' >> LA MATRIZ NO ES TRIANGULAR <<'
			FinSi
		FinSi
	FinSi
	//Imprimir matriz A
	para r <- 1 hasta n con paso 1 Hacer
		si r = redon(n/2) Entonces
			Escribir '    A = ' Sin Saltar
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