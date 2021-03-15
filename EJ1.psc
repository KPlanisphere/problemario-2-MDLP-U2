//Nombre: Jesús Huerta Aguilar
//Matricula: 202041509
Algoritmo temperatura
	//definir variables
	Definir op2,dmnr,dmyr,dx,recd Como Caracter
	Definir c,d,s,op Como Entero
	Definir ps,ptem,dia,mnr,myr,recs,rect Como Real
	Dimension dia[5,7]
	Dimension ps[5]
	Dimension myr[5]
	Dimension mnr[5]
	Dimension dmyr[5]
	Dimension dmnr[5]
	//establecer valores principales
	ptem <- 0
	c <- 1
	rect <- 0
	//Intrucción inicial y aviso
	Escribir '--REGISTRO DE TEMPERATURAS DURANTE EL MES DE ABRIL--'
	Escribir ''
	Escribir 'NOTA: Suponemos que el primer dia del mes es lunes.'
	Escribir ''
	//Registro de temperaturas
	//Cambio de semanas
	Para s <- 1 Hasta 5 Con Paso 1 Hacer
		mnr[s] <- 38
		Escribir '|||||||||| SEMANA ',s,' ||||||||||'
		Para d <- 1 Hasta 7 Con Paso 1 Hacer
			Si s = 5 y d = 4 Entonces
				dia[5,4] <- 0
				dia[5,5] <- 0
				dia[5,6] <- 0
				dia[5,7] <- 0
				d <- 7
			SiNo
				//Asignación de los dias
				Segun d Hacer
					1:	dx <- 'Lunes'
					2:	dx <- 'Martes'
					3:	dx <- 'Miercoles'
					4:	dx <- 'Jueves'
					5:	dx <- 'Viernes'
					6:	dx <- 'Sabado'
					7:	dx <- 'Domingo'
				Fin Segun
				//Registro diario
				Repetir
					Escribir 'Dia ',c,' | ',dx,':'
					leer dia[s,d]
					//Aviso por errores fuera del rango
					Si dia[s,d] > 38 o dia[s,d] < 7 Entonces
						Escribir '[!] ERROR: Verifica tu información [!]'
					FinSi
				Hasta Que dia[s,d]>=7 Y dia[s,d]<=38
				c <- c + 1
				//temperatura mas alta del mes
				Si dia[s,d] > rect Entonces
					rect <- dia[s,d]
					recd <- dx
					recs <- s
				FinSi
				//Definir temperaturas mayores y menores
				Si dia[s,d] > myr[s] Entonces
					myr[s] <- dia[s,d]
					dmyr[s] <- dx
				SiNo
					Si dia[s,d] <= mnr[s] Entonces
						mnr[s] <- dia[s,d]
						dmnr[s] <- dx
					FinSi
				FinSi
			FinSi
		Fin Para
	Fin Para
	//Creación de la matriz
	Escribir ''
	Escribir '      L    M    M    J    V    S    D'
	para s<-1 hasta 5 Con Paso 1 Hacer
		Escribir 'S',s,' ' Sin Saltar
		para d<-1 hasta 7 con paso 1 Hacer
			Si dia[s,d] < 10 Entonces
				Escribir ' |  ',dia[s,d]  Sin Saltar
			SiNo
				Escribir ' | ', dia[s,d]  Sin Saltar
			FinSi
			ptem <- ptem + dia[s,d]
		FinPara
		//Promedio semanal
		ps[s] <- ptem/3
		Escribir " |"
		ptem<-0
	FinPara
	//Menu
	Escribir ''
	Repetir
		Repetir
			Escribir '¿Que desea hacer?'
			Escribir '1- Obtener la temperatura mas alta y baja de cada semana y el dia en que se produjo'
			Escribir '2- Obtener la temperatura promedio de cada semana'
			Escribir '3- Obtener la temperatura mas alta del mes y el dia que se produjo'
			Leer op
			Limpiar Pantalla
			
			Segun op Hacer
				1:
					Escribir '--TEMPERATURA MAS ALTA Y BAJA DE CADA SEMANA--'
					
					Para s <- 1 hasta 5 Con Paso 1 Hacer
						Escribir ''
						Escribir '> Semana ',s
						Si myr[s] < 10 Entonces
							Escribir 'Maxima: ',myr[s],'°  | Registrada el dia: ',dmyr[s]
						SiNo
							Escribir 'Maxima: ',myr[s],'° | Registrada el dia: ',dmyr[s]
						FinSi
						Si mnr[s] < 10 Entonces
							Escribir 'Minima: ',mnr[s],'°  | Registrada el dia: ',dmnr[s]
						SiNo
							Escribir 'Minima: ',mnr[s],'° | Registrada el dia: ',dmnr[s]
						FinSi
					FinPara
					Escribir ''
					Escribir '¿Desea obtener algo mas? (S/N)'
					Leer op2
					Limpiar Pantalla
				2:
					Escribir '--PROMEDIOS SEMANALES--'
					Escribir ''
					Para s <- 1 hasta 5 Con Paso 1 Hacer
						Escribir "Semana ",s,': ', redon(ps[s]*100)/100,'°'
					FinPara
					Escribir ''
					Escribir '¿Desea obtener algo mas? (S/N)'
					Leer op2
					Limpiar Pantalla
				3:
					Escribir '--TEMPERATURA MAS ALTA DEL MES Y DIA ESPECIFICO--'
					Escribir ''
					Escribir 'El dia ',recd,' de la semana ',recs,' se registro la temperatura de ',rect,'°.'
					Escribir ''
					Escribir '¿Desea obtener algo mas? (S/N)'
					Leer op2
					Limpiar Pantalla
				De Otro Modo:
					Escribir '[!] ERROR: OPCIÓN INCORRECTA [!]'
					Escribir ''
			Fin Segun
		Hasta Que op > 0 y op <= 3
	Hasta Que op2 <> 's' y op2 <> 'S'
	Escribir ''
	Escribir 'Gracias por usar nuestros servicios =)'
FinAlgoritmo
