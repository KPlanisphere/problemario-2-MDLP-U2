//Nombre: Jesús Huerta Aguilar
//Matricula: 202041509

//Definir tamaño de la matriz
SubProceso tamMatriz(n Por Referencia,m Por Referencia)
	Escribir '-- PRIMOS EXISTENTES EN LA DIAGONAL PRINCIPAL nxm --'
	Escribir 'Ingresar tamaño de la matriz (n,m > 1)'
	Repetir
		Escribir ''
		Escribir 'Cantidad de filas (n)'
		Leer n
		Escribir 'Cantidad de columnas (m)'
		Leer m
		Si n <= 1 o m <= 1 Entonces
			Escribir '[!] ERROR: Verifica tu información [!]'
		FinSi
	Hasta Que n > 1 y m > 1
FinSubProceso

//Ingreso de valores
SubProceso ingresoMatriz(val Por Referencia,reclongval Por Referencia,n,m)
	tprim <- 0
	p <- 1
	Escribir '|||| Ingresar valores para la matriz A ||||'
	para r <- 1 hasta n Con Paso 1 Hacer
		Escribir '>> FILA ',r
		para c <- 1 hasta m con paso 1 hacer
			nprim <- 0
			leer val[r,c]
			//longitud del valor
			longval <- Longitud(ConvertirATexto(val[r,c]))
			si longval > reclongval Entonces
				reclongval <- longval
			FinSi
			primos(nprim,val,prim,tprim,r,c)
		FinPara
	FinPara
FinSubProceso

//determinar primos
SubProceso primos(nprim Por Referencia,val,prim,tprim,r,c)
	si r = c Entonces
		Para j<-1 Hasta val[r,c] Hacer
			Si val[r,c] MOD j=0 Entonces
				nprim <- nprim + 1
			FinSi
		FinPara
	FinSi
	contarPrimos(prim,tprim,p,val,prim)
FinSubProceso
// contador de numeros primos
SubProceso contarPrimos(prim Por Referencia,tprim Por Referencia,p Por Referencia, val,nprim)
	Si nprim=2 Entonces
		tprim <- tprim + 1
		prim[p] <- val[r,c]
		p <- p + 1
	FinSi	
FinSubProceso



Proceso  diagonal_primos
	//Instrucción inicial
	Definir n,m,c,r,e,p,longval,reclongval,switch,val,nprim,tprim,prim Como Entero
	Definir iz,dr,x,textval,textvalf Como Caracter
	tamMatriz(n,m)
	Dimension val[n,m]
	Dimension prim[n]
	ingresoMatriz(val,reclongval,n,m)
	
	
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
	//Salida primos
	Escribir ''
	Escribir '>> NUMEROS PRIMOS EN LA DIAGONAL PRINCIPAL <<'
	Si tprim = 0 Entonces
		Escribir 'La matriz A no tiene numeros primos en su diagonal principal'
	SiNo
		Escribir 'La matriz A tiene ',tprim, ' numeros primos en su diagonal principal: {'Sin Saltar
		para p <- 1 hasta tprim Con Paso 1 Hacer
			Escribir prim[p] Sin Saltar
			Si p <= tprim - 1 Entonces
				Escribir ',' Sin Saltar
			FinSi
		FinPara
		Escribir '}'
	FinSi
FinProceso