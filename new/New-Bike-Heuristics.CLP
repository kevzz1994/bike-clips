;; =======================================
;; =========      C R E D I T S   ========
;; =======================================
;;	Kevin Schaap
;;	Rafael Lobato
;;
;;  Thanks to Jorge Civera Saiz and ,
;;  Professors of Computer Science, Universitat Politècnica de València

(defglobal ?*nod-gen* = 0)
(defglobal ?*heuristicA* = 1)
(defglobal ?*stations* = (create$ A B J H K M R N))

(deffacts city

(path A B cost 10 method bike)
(path A C cost 8 method foot)
(path A E cost 10 method bike)
(path B A cost 10 method bike)
(path B C cost 5 method foot)
(path B F cost 6 method foot)
(path C A cost 8 method foot)
(path C B cost 5 method foot)
(path C D cost 6 method foot)
(path C H cost 6 method foot)
(path C G cost 6 method foot)
(path D C cost 6 method foot)
(path D H cost 14 method bike)
(path E A cost 10 method bike)
(path E I cost 20 method foot)
(path E J cost 9 method bike)
(path F B cost 6 method foot)
(path F K cost 10 method foot)
(path F L cost 6 method foot)
(path G C cost 6 method foot)
(path G H cost 8 method foot)
(path G L cost 9 method foot)
(path G M cost 12 method foot)
(path H C cost 6 method foot)
(path H D cost 14 method bike)
(path H G cost 8 method foot)
(path H I cost 12 method bike)
(path I H cost 12 method bike)
(path I E cost 20 method foot)
(path I O cost 2 method foot)
(path J E cost 9 method bike)
(path J O cost 7 method bike)
(path K F cost 10 method foot)
(path K L cost 2 method bike)
(path L F cost 6 method foot)
(path L K cost 2 method bike)
(path L G cost 9 method foot)
(path L M cost 7 method bike)
(path L P cost 6 method bike)
(path M G cost 12 method foot)
(path M L cost 7 method bike)
(path M Q cost 2 method foot)
(path N Q cost 6 method foot)
(path N O cost 8 method bike)
(path O J cost 7 method bike)
(path O I cost 2 method foot)
(path O N cost 8 method bike)
(path P L cost 6 method bike)
(path P Q cost 2 method foot)
(path P R cost 4 method bike)
(path Q M cost 2 method foot)
(path Q N cost 6 method foot)
(path Q P cost 2 method foot)
(path Q R cost 3 method foot)
(path R Q cost 3 method foot)
(path R P cost 4 method bike)

(neighbourhoods A1 B2 C2 D2 E2 J2 F3 G3 H3 I3 O3 K4 L4 M4 N4 P5 Q5 R6)
(arcs A8 B5 C5 D6 E9 J7 F6 G6 H6 I2 O2 K2 L2 M2 N6 P2 Q2 R3))

;; ========================================================
;; =============      H E U R I S T I C      ==============
;; ========================================================
;; calculating h(n)=|DistNeigh(n)|*min(Cam(n))*Sit(n)

(deffunction control (?currentPosition ?bike ?currentCost)
;; ================== DistNeigh(n) ==================
  ;; Get neighbourhood of current position, e.g. Value of index of ?currentPosition + 1
	(bind ?neighCurrentPos (nth$ (+ 1 (member$ ?currentPosition ?*neighbourhoods*)) ?*neighbourhoods*))
  ;; Get neighbourhood of destination, e.g. Value of index of ?des + 1
	(bind ?neighDestination (nth$ (+ 1 (member$ ?des ?*neighbourhoods*)) ?*neighbourhoods*))


	(if (eq ?neighCurrentPos ?neighDestination)
		then (bind ?*heuristicA* 1)
    ;; ---- Wat is het nut hier van? er kan nooit een decimal uitkomen
		else (bind ?*heuristicA* (abs (- ?neighDestination ?neighCurrentPos)))
	)

  ;; ==================   Cam(n) (|DistNeigh(n)|*min(Cam(n))) ==================
  ;;  Get the arc of minimum cost of the node n
  ;;  Index of ?currentPosition in ?*arcs* + 1, then get value with nth$ of that integer
  ;;  returns value of Index of previously calculated Integer
	(bind ?*heuristicA* (* ?*heuristicA* (nth$ (+ 1 (member$ ?currentPosition ?*arcs*)) ?*arcs*)))


  ;;  ===================== Sit(n) =====================
  ;;  Sit(n)=0.5 if the user has a bike or the user is at a bike point in the state n;
  ;;  otherwise, Sit(n)=1.
	(if (and (eq ?bike YES) (member$ ?currentPosition ?*stations*)))
    then (bind ?*heuristicA* (* ?*heuristicA 0.5))
    else (bind ?*heuristicA* (* ?*heuristicA 1))
	)

  ;;  Calculating f(n)= g(n) + h(n)
  ;;  ===================== g(n) =====================
  ;;  Cost of the path from the root node (initial state) to node n;
  ;;  taking into account the cost of the actions
  ;;  + the heuristic function described above
	(bind ?*heuristicA* (+ ?*heuristicA* ?currentCost))
)


;; =============== WALKING ===============
(defrule walk
(person currentPosition ?start des ?end bike NO cost ?cost level ?level)
(path ?start ?pathEnd cost ?pathCost method ?)
(max-depth ?depth)
  (test (< ?level ?depth))
;; control function here
=>
(assert (person currentPosition ?pathEnd des ?end bike NO cost (+ ?cost ?pathCost) level (+ ?level 1)))
(bind ?*nod-gen* (+ ?*nod-gen* 1)))

;; =============== CYCLING ===============

;; control function here
=>

;; =============== TAKE BIKE ===============

;; control function here
=>

;; =============== DROP BIKE ===============

;; control function here
=>

;; ========================================================
;; =========      S E A R C H   S T R A T E G Y    ========
;; ========================================================
;; The goal rule is used to detect when the goal state has been reached

(deffunction start ()
 (set-salience-evaluation when-activated)
 (reset)
 (printout t "Maximum depth:= " )
 (bind ?depth (read))
 (printout t "Where do we start?" crlf)
 (bind ?start(read))
 (printout t "Where do we end?" crlf)
 (bind ?end(read))
 (assert (destination ?end))
 (printout t " Execute run to start the program " crlf)

 (assert (person currentPosition ?start des ?end bike NO cost 0 level 0))
 (assert (max-depth ?depth)))

(defrule goal
    (declare (salience 100))
    ?f<-(person currentPosition ?start des ?start bike NO cost ?cost level ?level)
   =>
    (printout t "SOLUTION FOUND AT LEVEL: " ?level crlf)
    (printout t "COST OF REACHING GOAL STATE: " ?cost crlf)
    (printout t "NUMBER OF EXPANDED NODES OR TRIGGERED RULES: " ?*nod-gen* crlf)
    (printout t "GOAL FACT: " ?f crlf)

    (halt))
