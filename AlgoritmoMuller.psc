Funcion fx <- f (x)
	fx <- x^3 + 2*x^2 + 10*x - 20	
Fin Funcion

Algoritmo PuntoFijo
	
	Escribir "Valor inicial x0"
	Leer x0
	Escribir "Valor inicial x1"
	Leer x1
	Escribir "Valor inicial x2"
	Leer x2
	Escribir "Epsilon"
	Leer EPS
	Escribir "Número de iteraciones"
	Leer MAXIT
	
	cont <- 0
	valfx <- 10
	
	Mientras valfx >= EPS & cont <= MAXIT Hacer
		
		fx1x0 <- (f(x1)-f(x0))/(x1-x0)
		fx2x1 <- (f(x2)-f(x1))/(x2-x1)
		fx2x1x0 <- (fx2x1-fx1x0)/(x2-x0)
		
		a2 <- fx2x1x0
		a1 <- fx2x1-(x2+x1)*a2
		a0 <- f(x2)-x2*(fx2x1-x1*a2)
		
		D1 <- -a1 + RC(a1^2-4*a0*a2)
		D2 <- -a1 - RC(a1^2-4*a0*a2)
		
		Si ABS(D1) > ABS(D2) Entonces
			x3 <- (2*a0)/D1
		SiNo
			x3 <- (2*a0)/D2
		FinSi
		
		x0 <- x1
		x1 <- x2
		x2 <- x3
		
		valfx <- abs(f(x3))		
		
		Si valfx <= EPS Entonces
			Escribir "La raíz es"
			Escribir x3
			Escribir "Iteraciones"
			Escribir cont						
		Fin Si
		
		cont <- cont + 1
		
	Fin Mientras
		
FinAlgoritmo
