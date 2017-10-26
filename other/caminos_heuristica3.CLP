;; =========================================================
;; ===     HEURISTICA				        ====
;; =========================================================

(deffacts base_hechos
	
	(persona origen A destino I bicicleta N coste 0 nivel 0)	
	
	(estaciones A B H J K M N R)	

	(camino A B coste 10 tipo BICI)
	(camino A C coste 8 tipo PIE)
	(camino A E coste 10 tipo BICI)
	(camino B A coste 10 tipo BICI)
	(camino B C coste 5 tipo PIE)
	(camino B F coste 6 tipo PIE)
	(camino C A coste 8 tipo PIE)
	(camino C B coste 5 tipo PIE)
	(camino C D coste 6 tipo PIE)
	(camino C H coste 6 tipo PIE)
	(camino C G coste 6 tipo PIE)
	(camino D C coste 6 tipo PIE)
	(camino D H coste 14 tipo BICI)
	(camino E A coste 10 tipo BICI)
	(camino E I coste 20 tipo PIE)
	(camino E J coste 9 tipo BICI)
	(camino F B coste 6 tipo PIE)
	(camino F K coste 10 tipo PIE)
	(camino F L coste 6 tipo PIE)
	(camino G C coste 6 tipo PIE)
	(camino G H coste 8 tipo PIE)
	(camino G L coste 9 tipo PIE)
	(camino G M coste 12 tipo PIE)
	(camino H C coste 6 tipo PIE)
	(camino H D coste 14 tipo BICI)
	(camino H G coste 8 tipo PIE)
	(camino H I coste 12 tipo BICI)
	(camino I H coste 12 tipo BICI)
	(camino I E coste 20 tipo PIE)
	(camino I O coste 2 tipo PIE)
	(camino J E coste 9 tipo BICI)
	(camino J O coste 7 tipo BICI)
	(camino K F coste 10 tipo PIE)
	(camino K L coste 2 tipo BICI)
	(camino L F coste 6 tipo PIE)
	(camino L K coste 2 tipo BICI)
	(camino L G coste 9 tipo PIE)
	(camino L M coste 7 tipo BICI)
	(camino L P coste 6 tipo BICI)
	(camino M G coste 12 tipo PIE)
	(camino M L coste 7 tipo BICI)
	(camino M Q coste 2 tipo PIE)
	(camino N Q coste 6 tipo PIE)
	(camino N O coste 8 tipo BICI)
	(camino O J coste 7 tipo BICI)
	(camino O I coste 2 tipo PIE)
	(camino O N coste 8 tipo BICI)
	(camino P L coste 6 tipo BICI)
	(camino P Q coste 2 tipo PIE)
	(camino P R coste 4 tipo BICI)
	(camino Q M coste 2 tipo PIE)
	(camino Q N coste 6 tipo PIE)
	(camino Q P coste 2 tipo PIE)
	(camino Q R coste 3 tipo PIE)
	(camino R Q coste 3 tipo PIE)
	(camino R P coste 4 tipo BICI)

	(barrio 1 A)
	(barrio 2 B C D E J)
	(barrio 3 F G H I O)
	(barrio 4 K L M N)
	(barrio 5 P Q)
	(barrio 6 R)

	(camino_minimo A 8)
	(camino_minimo B C 5)
	(camino_minimo D F G H N 6)
	(camino_minimo E 9)
	(camino_minimo I K L M O P Q 2)
	(camino_minimo J 7)
	(camino_minimo R 3)
)

( defglobal ?*nod-gen* = 0 ) ;Numero de nodos expandidos
( defglobal ?*f* = 1)
( defglobal ?*base_o_bici* = N)

(deffunction heuristica (?estado)
	(bind ?h 0)
	
	(if (<> (nth$ 1 ?estado) (nth$ 2 ?estado))
	 then (bind ?h (+ ?h 1))
	 else (bind ?h (+ ?h (abs(- (nth$ 1 ?estado) (nth$ 2 ?estado)))))
	)

	(bind ?h (* ?h (nth$ 3 ?estado)))

	(if (or (eq (nth$ 4 ?estado) S) (eq ?*base_o_bici* S)) 
         then  (bind ?h (div ?h 2))
	)

?h)





(deffunction control (?estado ?nivel)
    (bind ?*f* (heuristica ?estado))
    (bind ?*f* (+ ?*f* ?nivel 1))
)


(deffunction modifica_base_o_bici (?estacion)
	(if(neq ?estacion FALSE)
		then (bind ?*base_o_bici* S)
		else (bind ?*base_o_bici* N)
	)
)

(defrule ir_a_pie
		(declare (salience (- 0 ?*f*)))
	?f <-	(persona origen ?ori destino ?dest bicicleta N coste ?costActual nivel ?nivel)
		(camino ?ori ?destCamin coste ?cost tipo ?)
		(profundidad-maxima ?prof)

		(barrio ?barrio_1 $? ?destCamin $?)
		(barrio ?barrio_2 $? ?dest $?)
		(camino_minimo $? ?destCamin $? ?camMin)
		(estaciones $?esta)
		
  		(test (< ?nivel ?prof))
	
		(test (modifica_base_o_bici (member$ $destCamin $?esta)))
		(test (control (create$ ?barrio_1 ?barrio_2 ?camMin N) ?nivel))

	=>

	(assert	(persona origen ?destCamin destino ?dest bicicleta N coste (+ ?costActual ?cost) nivel (+ ?nivel 1)))
	(bind ?*nod-gen* (+ ?*nod-gen* 1))
)

(defrule ir_en_bici
		(declare (salience (- 0 ?*f*)))
	?f <-	(persona origen ?ori destino ?dest bicicleta S coste ?costActual nivel ?nivel)
		(camino ?ori ?destCamin coste ?cost tipo BICI)
		(profundidad-maxima ?prof)
	  	
		(barrio ?barrio_1 $? ?destCamin $?)
		(barrio ?barrio_2 $? ?dest $?)
		(camino_minimo $? ?destCamin $? ?camMin)
		(estaciones $?esta)
				

  		(test (< ?nivel ?prof))

		(test (modifica_base_o_bici (member$ $destCamin $?esta)))
		(test (control (create$ ?barrio_1 ?barrio_2 ?camMin S) ?nivel))

=>
	(assert	(persona origen ?destCamin destino ?dest bicicleta S coste (+ ?costActual (div ?cost 2)) nivel (+ ?nivel 1)))
	(bind ?*nod-gen* (+ ?*nod-gen* 1))
)

(defrule cojer_bici
		(declare (salience (- 0 ?*f*)))
	?f <-	(persona origen ?ori destino ?dest bicicleta N $?aux2 coste ?costActual nivel ?nivel)
		(estaciones $? ?ori $?)
		(profundidad-maxima ?prof)
	  	
		(barrio ?barrio_1 $? ?ori $?)
		(barrio ?barrio_2 $? ?dest $?)
		(camino_minimo $? ?ori $? ?camMin)		

  		(test (< ?nivel ?prof))

		(test (modifica_base_o_bici TRUE))
		(test (control (create$ ?barrio_1 ?barrio_2 ?camMin S) ?nivel))

=>
	(assert (persona origen ?ori destino ?dest bicicleta S $?aux2 coste (+ ?costActual 1) nivel (+ ?nivel 1)))
	(bind ?*nod-gen* (+ ?*nod-gen* 1))
)

(defrule dejar_bici
		(declare (salience (- 0 ?*f*)))
	?f <-	(persona origen ?ori destino ?dest bicicleta S $?aux2 coste ?costActual nivel ?nivel)
		(estaciones $? ?ori $?)
		(profundidad-maxima ?prof)
	  	
		(barrio ?barrio_1 $? ?ori $?)
		(barrio ?barrio_2 $? ?dest $?)
		(camino_minimo $? ?ori $? ?camMin)		

  		(test (< ?nivel ?prof))

		(test (modifica_base_o_bici TRUE))
		(test (control (create$ ?barrio_1 ?barrio_2 ?camMin N) ?nivel))

=>
	(assert (persona origen ?ori destino ?dest bicicleta N $?aux2 coste (+ ?costActual 1) nivel (+ ?nivel 1)))
	(bind ?*nod-gen* (+ ?*nod-gen* 1))
)


;; ========================================================
;; =========      ESTRATEGIA DE CONTROL DE BUSQUEDA    ========
;; ========================================================

(defrule solucion
	(declare (salience 100))
	?f <-	(persona origen ?ori destino ?ori bicicleta N coste ?costActual nivel ?nivel)
=>
	(printout t "SOLUCION ENCONTRADA EN EL NIVEL " ?nivel crlf)
	(printout t "COSTE " ?costActual crlf)
    	(printout t "NUMERO DE NODOS EXPANDIDOS O REGLAS DISPARADAS " ?*nod-gen* crlf)
	(halt)
)

(defrule no_solucion
	(declare (salience -99))
	(persona $?)

=>

    (printout t "SOLUCION NO ENCONTRADA" crlf)
    (halt)
)

(deffunction inicio ()
	(set-salience-evaluation when-activated)
      (reset)
	(printout t "Profundidad Maxima:= " )
	(bind ?prof (read))
	(printout t " Ejecuta run para poner en marcha el programa " crlf)
	(assert (profundidad-maxima ?prof))
)
