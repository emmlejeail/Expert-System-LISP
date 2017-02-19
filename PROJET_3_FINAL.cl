; Forme regle : (nom (premisses) (conclusions))
;     Une règle est applicable si toutes ses premisses sont vraies
; Forme premisse : (operateur objet attribut valeur)
;     Liste operateurs : = (vrai s'il existe une correspondance dans *BF*), ! (vrai s'il nexiste pas de correspondance dans *BF*)
; Forme conclusion : (fonction argument)
; Forme objet, attribut, valeur : atome ou variable de type : (? . nom)

;Remarque : on n'est pas obligé de se limiter à des faits (objet attribut valeur), normalement les fonctions supportent des faits de différentes tailles

;listes des fonctions implémentées : apply_regle, ajoutFait, evalVar, rMatching, fMatching, regles_candidates, demand, maximun, recom, ajout_asso, descriptif, start

;pour démarrer le système, il suffit de taper (start) dans le listener 
;si vous voulez redémarrer le système, il suffit d'appeler (reset) dans le listener
;pour quitter un loop de la fonction demand, il faut répondre par fin
;goûts possibles à l'heure actuelle : evenementiel, astronomie, aviation, parachutisme, musique, danse, vieCampus, humanitaire, citoyennete, sport, mécanique et voyage
;si une des questions amène à créer un pattern étudiant qui ne partagera pas la valeur prise par l'attribut avec, aucune des asso de BF, aucune asso est recommandée.
;on ne recommande que la meilleure asso dans ce système, afin de lui recommander plusieurs assos il faut gérer des scores pondérés.
---------------------------------------------------------------------------------------------------------------------------
;Variables globales : 
(defvar *BF*)
(defvar *BR*)
(defvar trouve nil)
---------------------------------------------------------------------------------------------------------------------------
;Fonction de démarrage :
(defun start () (apply-regle (assoc 'Q4 *BR*) *BR* *BF*)) ;fonction qui lance la première règle candidate du programme.

;Fonction de redémarrage :
(defun reset ()
  (progn
    (setq trouve nil) ;on remet trouve à NIL
    (apply-regle (assoc 'Q4 *BR*) *BR* *BF*) ; on ré-applique start
    )
)

---------------------------------------------------------------------------------------------------------------------------
(setq *BR* '(
            (R0 ((= etudiant gout (? . gout)) (= objet (? . asso) asso) (= (? . asso) gout (? . gout))) ((ajout_asso (? . asso))))
            (R1 ((= etudiant style (? . style)) (= etudiant gout musique) (= objet (? . asso) asso) (= (? . asso) gout musique) (= (? . asso) style (? . style))) ((ajout_asso (? . asso))))
            (R2 ((= etudiant style (? . style)) (= etudiant gout danse) (= objet (? . asso) asso) (= (? . asso) style (? . style) (= (? . asso) gout danse))) ((ajout_asso (? . asso))))
            (R3 ((= etudiant instrument (? . instrument)) (= objet (? . asso) asso) (= (? . asso) instrument (? . instrument))) ((ajout_asso (? . asso))))
            (R4 ((= etudiant niveau (? . niveau)) (= etudiant instrument (? . instrument)) (= objet (? . asso) asso) (= (? . asso) instrument (? . instrument)) (= (? . asso) niveau (? . niveau))) ((ajout_asso (? . asso))))
            (R5 ((= etudiant façon_pratique (? . fp)) (= objet (? . asso) asso) (= (? . asso) façon_pratique (? . fp))) ((ajout_asso (? . asso))))
            (R6 ((= etudiant type (? . type)) (= etudiant gout evenementiel)(= objet (? . asso) asso) (= (? . asso) gout evenementiel) (= (? . asso) type (? . type))) ((ajout_asso (? . asso)))) 
            (R7 ((= etudiant lieu (? . lieu)) (= etudiant gout voyage) (= objet (? . asso) asso) (= (? . asso) gout voyage)(= (? . asso) lieu (? . lieu))) ((ajout_asso (? . asso))))
            (R8 ((= etudiant sport (? . sport)) (= objet (? . asso) asso) (= (? . asso) sport (? . sport))) ((ajout_asso (? . asso))))
            (R9 ((= etudiant domaine_méca (? . d_m)) (= objet (? . asso) asso) (= (? . asso) domaine_méca (? . d_m))) ((ajout_asso (? . asso))))
            (R10 ((= etudiant type (? . type)) (= etudiant gout vieCampus) (= objet (? . asso) asso) (= (? . asso) gout vieCampus) (= (? . asso) type (? . type))) ((ajout_asso (? . asso)))) 
            (R11 ((= etudiant lieu (? . lieu)) (= etudiant gout vieCampus) (= objet (? . asso) asso) (= (? . asso) gout vieCampus) (= (? . asso) lieu (? . lieu))) ((ajout_asso (? . asso))))
            (R12 ((= etudiant but (? . but)) (= etudiant gout vieCampus) (= objet (? . asso) asso) (= (? . asso) gout vieCampus) (= (? . asso) but (? . but))) ((ajout_asso (? . asso))))
            (R13 ((= etudiant but (? . but)) (= etudiant gout voyage) (= objet (? . asso) asso) (= (? . asso) gout voyage) (= (? . asso) but (? . but))) ((ajout_asso (? . asso))))
            (R14 ((= etudiant pratique (? . pratique)) (= objet (? . asso) asso) (= (? . asso) pratique (? . pratique))) ((ajout_asso (? . asso))))
            (R15 ((= etudiant standing (? . standing)) (= objet (? . asso) asso) (= (? . asso) standing (? . standing))) ((ajout_asso (? . asso))))

            (Q0 ((= etudiant façon_pratique (? . façon_pratique)) (= etudiant gout musique)) ((demand style)) )
            (Q1 ((= etudiant style (? . style)) (= etudiant gout danse)) ((demand niveau)))
            (Q2 ((= etudiant instrument (? . instrument)) (! etudiant instrument non)) ((demand niveau)) )
            (Q3 ((= etudiant niveau (? . niveau))(= etudiant gout musique)) ((demand façon_pratique)) )
            (Q4 ((! etudiant gout (? . gout))) ((demand gout)))
            (Q5 ((= etudiant niveau (? . niveau))(= etudiant gout danse)) ((demand pratique)))
            (Q6 ((= etudiant gout (? . gout)) (! etudiant gout astronomie) (! etudiant gout cyclisme) (! etudiant gout aviation) (! etudiant gout parachutisme)) ((demand (? . gout))))
            (Q7 ((= etudiant lieu (? . lieu))) ((demand but)))
            (Q8 ((= etudiant but sportif)) ((demand sport)))
            (Q9 ((= etudiant type (? . type)) (= etudiant gout evenementiel)) ((demand standing)))
            (Q10 ((= etudiant but (? . but)) (= etudiant gout voyage)) ((demand lieu)))
            (Q11 ((= etudiant type (? . type)) (= etudiant gout vieCampus)) ((demand but)))
             
             )
      )
;liste des assos implémentées : comet,SDF,etuville,IF,Stravaganza,Ocata,Larsen,Décibels,PianoUT,Choruts, AlaRUtc, Acoustique
;UTCIEL,VeloUTC, Orion, Club_bioméca, coincidence, PIC, Integ, Utécia, Picnrock, breakdance, UTRIP, SurfUT
;SkiUT, BreakDance, CABARET, TUC, SoleilsenInde, ToitpourleNépal, ToitPourLeNepal 
; pour avoir la liste, on peut utiliser la fonction : (fMatching '(objet (? . d) asso) *BF*)

(setq *BF* '(
             (objet comet asso)
             (comet gout evenementiel)
             (comet gout vieCampus)
             (comet type soiree)
             (comet lieu boite)
             (comet standing casual)
             (comet but divertissement)
             (objet SDF asso)
             (SDF gout evenementiel)
             (SDF gout vieCampus)
             (SDF type soiree)
             (SDF lieu Compiègne)
             (SDF standing casual)
             (objet Etuville asso)
             (Etuville gout evenementiel)
             (Etuville gout vieCampus)
             (Etuville type gala)
             (Etuville standing gros)
             (objet IF asso)
             (IF gout evenementiel)
             (IF type festival)
             (IF standing gros)
             (objet Stravaganza asso)
             (Stravaganza gout musique)
             (Stravaganza façon_pratique orchestre)
             (Stravaganza style classique)
             (Stravaganza instrument violon)
             (Stravaganza instrument alto)
             (Stravaganza instrument trompette)
             (Stravaganza instrument contrebasse)
             (Stravaganza instrument flûte)
             (Stravaganza instrument traversière)
             (Stravaganza instrument batterie)
             (Stravaganza niveau moyen)
             (Stravaganza niveau confirme)
             (objet Décibels asso)
             (Décibels gout musique)
             (Décibels gout evenementiel)
             (Décibels instrument non)
             (Décibels type concert)
             (Décibels standing gros)
             (Décibels standing casual)
             (objet Larsen asso)
             (Larsen gout musique)
             (Larsen style rock)
             (Larsen façon_pratique groupe)
             (Larsen instrument guitare)
             (Larsen instrument chant)
             (Larsen instrument batterie)
             (Larsen instrument basse) 
             (Larsen instrument piano)
             (Larsen instrument batterie)
             (Larsen niveau moyen)
             (Larsen niveau confirme)
             (objet PianoUT asso)
             (PianoUT gout musique)
             (PianoUT style classique)
             (PianoUT instrument piano)
             (PianoUT façon_pratique individuelle)
             (PianoUT niveau basique)
             (PianoUT niveau moyen)
             (PianoUT niveau confirme)
             (objet Choruts asso)
             (Choruts gout musique)
             (Choruts style variete)
             (Choruts façon_pratique chorale)
             (Choruts instrument chant)
             (Choruts niveau basique)  
             (Choruts niveau moyen)
             (Choruts niveau confirme)
             (objet Ocata asso)
             (Ocata gout musique)
             (Ocata style jazz)
             (Ocata instrument saxophone)
             (Ocata instrument piano)
             (Ocata instrument guitare)
             (Ocata instrument contrebasse)
             (Ocata instrument trompette)
             (Ocata façon_pratique groupe)
             (Ocata niveau moyen)
             (Ocata niveau confirmé)
             (objet Acoustic asso)
             (Acoustic gout musique)
             (Acoustic façon_pratique seul)
             (Acoustic façon_pratique groupe)
             (Acoustic niveau débutant)
             (Acoustic niveau moyen)
             (Acoustic niveau confirmé)
             (Acoustic instrument guitare_sèche)
             (objet AlaRUtc asso)
             (AlaRUtc gout musique)
             (AlaRUtc façon_pratique groupe)
             (AlaRUtc style fanfare)
             (AlaRUtc niveau débutant)
             (AlaRUtc niveau moyen)
             (AlaRUtc niveau confirmé)
             (AlaRUtc instrument saxophone)
             (AlaRUtc instrument trompette)
             (AlaRUtc instrument tambour)
             (AlaRUtc instrument flute)
             (objet Cabaret asso)
             (Cabaret gout vieCampus)
             (Cabaret gout evenementiel)
             (Cabaret type soirée)
             (Cabaret lieu Compiègne)
             (Cabaret standing classe)
             (Cabaret but repas)
             (objet TUC asso)
             (TUC gout citoyennete)
             (TUC gout humanitaire)
             (TUC lieu Compiègne)
             (objet SoleilEnInde asso)
             (SoleilEnInde gout humanitaire)
             (SoleilEnInde gout voyage)
             (SoleilEnInde lieu Inde)
             (SoleilEnInde but aider)
             (SoleilEnInde but humanitaire)
             (objet ToitPourLeNepal asso)
             (ToitPourLeNepal gout humanitaire)
             (ToitPourLeNepal gout voyage)
             (ToitPourLeNepal lieu Nepal)
             (ToitPourLeNepal but aider)
             (ToitPourLeNepal but humanitaire)
             (objet UTrip asso)
             (UTrip gout voyage)
             (UTrip lieu europe)
             (Utrip but touristique)
             (objet SkiUT asso)
             (SkiUT gout sport)
             (SkiUT gout voyage)
             (SkiUT lieu montagne)
             (SkiUT sport ski)
             (SkiUT but sportif)
             (objet SurfUT asso)
             (SurfUT gout sport)
             (SurfUT gout voyage)
             (SurfUT lieu mer)
             (SurfUT sport surf)
             (SurfUT but sportif)
             (objet RugbyUTC asso)
             (RugbyUTC gout sport)
             (RugbyUTC sport rugby)
             (objet Pic asso)
             (Pic gout vieCampus)
             (Pic but divertissement)
             (Pic type bar)
             (objet Integ asso)
             (Integ gout vieCampus)
             (Integ type integration)
             (Integ but intégrer)
             (objet Coincidence asso)
             (Coincidence gout danse)
             (Coincidence style modern_jazz)
             (Coincidence niveau moyen)
             (Coincidence niveau confirmé)
             (Coincidence pratique groupe)
             (objet BreakDance asso)
             (BreakDance gout danse)
             (BreakDance style breakdance)
             (BreakDance niveau débutant)
             (BreakDance niveau moyen)
             (BreakDance niveau confirmé)
             (BreakDance pratique seul)
             (BreakDance pratique groupe)
             (objet PicNRock asso)
             (PicNRock gout danse)
             (PicNRock style rock)
             (PicNRock niveau débutant)
             (PicNRock niveau moyen)
             (PicNRock niveau confirmé)
             (PicNRock pratique couple)
             (objet Utécia asso)
             (Utécia gout mécanique)
             (Utécia domaine_méca automobile)
             (objet Club_BioMéca asso)
             (Club_BioMéca gout mécanique)
             (Club_BioMéca domaine_méca prothèses)
             (objet Orion asso)
             (Orion gout astronomie)
             (objet VeloUTC asso)
             (Orion gout cyclisme)
             (objet UTCiel asso)
             (UTCiel gout aviation)
             (UTCiel gout parachutisme)
             ))

---------------------------------------------------------------------------------------------------------------------------
(defun ajout_asso (asso BR BF)
  (progn
    (if (not (member (list 'recom asso) BF :test #'equal)) ; si on a pas déjà recommandé l'asso
      (push (list 'recom asso) BF) ;on ajoute l'élément (recom asso) dans BF
    )
  (push (list 'score asso) BF) ;on ajoute l'élément (score asso) pour donner + de poids à l'asso
  )
)

;Tests unitaires : 
(ajout_asso 'PicNRock *BR* *BF*)
---------------------------------------------------------------------------------------------------------------------------
(defun demand (theme BR BF)
  (progn
  (cond 
   ((or (eq theme 'style) (eq theme 'danse)) ;si le theme de la question est style ou danse
    (format t "Quel style ?" ) 
    (setq rep (read))
    (ajoutFait (list 'etudiant 'style rep ) BR BF) ;on ajoute le fait dans la base de fait
    )
   ((eq theme 'gout)
    (loop ;on crée un loop
      (if trouve (return-from nil nil)) ;on ne s'arrête que lorsque la variable trouve est true
      (format t "Quels sont tes centres d'intérêt ? ") ;on demande les centre d'intérêt
      (setq rep (read)) ;on lit la réponse
      (if (eq rep 'fin) (return-from nil nil)) ;on sort du loop si la réponse est fin
      (ajoutFait (list 'etudiant 'gout rep) BR BF) ;sinon on ajoute le fait à la base de faits
     )
    )
   ((eq theme 'mécanique)
    (format t "Dans quel domaine de mécanique (prothèses, automobile) ?")
    (setq rep (read))
    (ajoutFait (list 'etudiant 'domaine_méca rep) BR BF)
    )
   ((eq theme 'musique) ;si le thème est musique
    (format t "Joues-tu d'un instrument ? ") ;on demande si l'utilisateur joue d'un instrument
    (setq rep (read))
    (if (eq rep 'non) ; si c'est non
        (ajoutFait (list 'etudiant 'instrument rep) BR BF) ;on ajoute le fait dans BF
        (loop ;si c'est oui, on loop sur les instruments
          (if trouve (return-from nil nil)) ;on s'arrête si trouve est à true
          (format t "Quel(s) instrument(s) joues-tu ? ")
          (setq rep (read))
          (if (eq rep 'fin) (return-from nil nil)) ;on s'arrête aussi si l'utilisateur entre fin
          (ajoutFait (list 'etudiant 'instrument rep) BR BF)
        ))
    )
   ((eq theme 'façon_pratique)
   (format t "Désires-tu jouer dans un groupe, un orchestre ... ? ") 
   (ajoutFait (list 'etudiant 'façon_pratique (read)) BR BF) 
    )
   ((eq theme 'pratique)
    (format t "Tu veux danser seul, en groupe, en couple...?")
    (ajoutFait (list 'etudiant 'pratique (read)) BR BF)
    )
    ((eq theme 'niveau)
   (format t "Quel niveau as-tu ? ") 
   (ajoutFait (list 'etudiant 'niveau (read)) BR BF)
     )
   ((eq theme 'humanitaire)
    (format t "Dans quel lieu?")
    (setq rep (read))
    (ajoutFait (list 'etudiant 'lieu rep) BR BF)
    )
   ((eq theme 'lieu)
      (loop
        (if trouve (return-from nil nil))
        (format t "Dans quel lieu?")
        (setq rep (read))
        (if (eq rep 'fin) (return-from nil nil))
        (ajoutFait (list 'etudiant 'lieu rep) BR BF)
     )
    )
   ((eq theme 'voyage)
        (format t "Dans quel but (humanitaire,sportif,touristique) ?")
        (setq rep (read))
        (ajoutFait (list 'etudiant 'but rep) BR BF)
     )
   ((eq theme 'evenementiel)
    (format t "Quel type d'évenements?")
    (setq rep (read))
    (ajoutFait (list 'etudiant 'type rep) BR BF)
    )
    ((eq theme 'vieCampus)
    (format t "De quel type?")
    (setq rep (read))
    (ajoutFait (list 'etudiant 'type rep) BR BF)
    )
   ((eq theme 'sport)
    (format t "Quel sport?")
    (setq rep (read))
    (ajoutFait (list 'etudiant 'sport rep) BR BF)
    )
   ((eq theme 'standing)
    (format t "De quel standing?")
    (setq rep (read))
    (ajoutFait (list 'etudiant 'standing rep) BR BF)
    )
   ((eq theme 'but)
    (format t "Dans quel but ?")
    (setq rep (read))
    (ajoutFait (list 'etudiant 'but rep) BR BF)
    )
   )
   BF)
  )

;Tests unitaires : 
(demand 'instrument *BR*)

---------------------------------------------------------------------------------------------------------------------------
(defun recom (BF)                   
      (let ((asso (maximum BF))) ;on récupère l'asso avec le score max grâce à la fonction maximum
        (format t "On te recommande cette association : ~S~% Cela te va ?" (car asso))
        (setq rep (read))
        (if (equal rep '?) ;si l'utilisateur veut en savoir plus
            (progn
              (descriptif (car asso)) ;on lance la fonction descriptif sur l'association
              (format t "Veux-tu t'y investir ?")
              (setq rep (read))
              )
          )
        (if (equal rep 'oui) ;si la réponse est oui alors trouve passe à true
          (setq trouve t)    ;cela permettra de sortir des loop de la fonction demand
      )
    )
  )

(defun descriptif (asso) ;donne le descriptif d'une asso seuls les descriptifs de Larsen, Décibels et SkiUT existent, il suffirait d'ajouter tous les descriptifs des assos présents dans la base de faits
  (cond 
   ((eq asso 'Larsen) (format t "Larsen, l’association des musiciens de l’UTC
Larsen permet aux musiciens de l’UTC de se rencontrer et de
former ou compléter leur groupe.
Ceux-ci répètent dans la salle prévue à cet effet, équipée du matos haut
de gamme (!) de l’asso. Larsen offre la possibilité aux groupes de participer aux
différents événements type tournée des bars, soirées concerts, Festival des Vieilles
Pipettes, PMDE, Festupic...
Environ une fois par mois, une soirée jam session dans un bar est organisée et mélange
des musiciens de tous horizons (groupes de la ville, musiciens compiégnois, jazzeux
d’Ocata, orchestre...). Si vous cherchez le groupe de vos rêves, contactez-vous !"))
  ((eq asso 'Décibels) (format t "De la scène au studio, nous offrons la possibilité aux étudiants de découvrir l’univers technique du son et de l’éclairage. 
Ouverte aux débutants autant qu’aux initiés, l’asso se présente en trois pôles :
- Pôle Sono : Des concerts Larsen dans les bars aux concerts semi-pros lors de gros
évènements, une partie importante de notre activité consiste à sonoriser les scènes de
l’UTC : montage, câblage, balances, plateau, régie, logistique. Lors des formations de base
nous apprenons aux débutants à utiliser le matériel (table de mixage, système de diffusion,
micros, etc) afin de pouvoir gérer une scène en toute autonomie. Quelques exemples
d’évènements où nous sommes présents : Utcéenne, Gala, Cabaret, concerts Larsen, PMDE,
Festupic, concerts de l’ESCOM.
- Pôle MAO : Décibels est aussi présent coté studio. Enregistrement de groupes, projet
Weeksong, ateliers MAO. Ces activités sont basées sur l’échange de savoir entre les
étudiants, ainsi il est possible d’apprendre en partant de zéro ou de partager ses connaissances (Logiciels, prise de son, expériences...). Pour mener les ateliers et développer les différents
projets de ce pôle, nous investissons le studio d’enregistrement de la nMDE.
- Pôle Light : aux côtés du pôle Sono, les lumières apportent une vraie touche pro aux
spectables de l’UTC ! Des formations techniques et artistiques dispensées tout au long
du semestre vous permettront de devenir un véritable connaisseur des lights et de vous
éclater lors des spectables utcéens ou lors de projet montés en parallèle !"))
   ((eq asso 'SkiUT) (format t "Des étudiants, de la neige, du sport et de l’ambiance pour une semaine inoubliable !
Créée en 1989 par des étudiants passionnés de montagne, Ski’UTC a pour but de rassembler, lors d’une semaine au mois de Janvier, le plus grand nombre d’étudiants de
l’UTC pour un voyage hors du commun au ski.
Il s’agit d’un événement qui promeut la cohésion et l’esprit d’équipe au sein des étudiants,
qui propose des animations sportives et festives. Tout cela à des tarifs accessibles.
Ski’UTC permet également à des étudiants qui n’en auraient pas l’occasion faute de
moyens financiers (ou habitant dans des pays sans neige) de pouvoir partir au ski. Pour certains, Ski’UTC est l’occasion de découvrir le ski.
C’est un évènement qui permet de renforcer la cohésion et l’esprit d’équipe au sein des étudiants à travers de nombreuses animations sportives et festives : organisation de
slaloms, soirées multisports sur neige, barbecue sur les pistes, restaurant en altitude..
"))
   )
  )

(defun maximum (BF)
  (let ((lst_asso (fMatching '(recom (? . d)) BF)) (maxi '(x . 0)) lst_comptage) ;on récupère une liste des asso avec un score non nul et on définit le max à un couple (x . 0)
    (dolist (asso lst_asso maxi)                               ;pour chaque asso dans lst_asso
      (setq lst_comptage (fMatching `(score ,(cdar asso)) BF)) ;on récupère une liste dont le nombre d'élément est le nombre de fois qu'apparaît le pattern (score asso)
      (setq score (cons (cdar asso) (length lst_comptage)))    ;on crée une paire pointée (asso . nb_element dans lst_comptage)
      (if (> (cdr score) (cdr maxi)) ;si le score de cette asso est supérieur au maximum déjà défini
        (setq maxi score)            ;on remplace maximum par cette nouvelle paire pointée
          )
    )
    )
  )


---------------------------------------------------------------------------------------------------------------------------

(defun apply-regle (regle BR BF )
  (let* ((pointeur (assoc (car regle) BR)) (BR_temp (remove pointeur BR)) BF_temp) ;on supprime la règle de base de règles, le résultat est stocké dans BR_temp
    ;remove renvoie en fait la base de règles en excluant la règle à supprimer mais ne met en aucun cas à jour BR
    (dolist (conclusion (caddr regle) (list BR_temp BF_temp))
      (setq BF_temp (funcall (car conclusion) (cadr conclusion) BR_temp BF)) 
       ;application des conclusions de la règle 
       ;utilisation d'un funcall car les conclusions sont basées sur des fonctions.
      )
   )
  )
;ici, il n'existe qu'une conclusion par règle mais on aurait pu en imaginer plusieurs. 

---------------------------------------------------------------------------------------------------------------------------
(defun ajoutFait (fait BR BF) ; Profondeur et chaînage avant
  (if (null trouve) 
      (if (not (member fait BF :test #'equal)) ;on teste d'abord si le fait n'appartient pas à la BF, si tel est le cas, on l'ajoute 
          (let (lst_regles regle result var) 
            (setq lst_regles (regles_candidates BR (append (list fait) BF))) ;on cherche alors les règles qui peuvent s'appliquer suite à cet ajout (qui n'étaient pas déclenchables avant sans cet ajout)
            (if lst_regles 
                (progn
                (setq BF (append (list fait) BF)) ;mise à jour de la base de faits
                (loop ;les règles sont appliquées les unes à la suite des autres par un loop
                  (setq regle (pop lst_regles)) 
                  (if regle (print regle)) ;;affiche la règle utilisée
                  (if (or (member (cons '= fait) (cadr regle) :test #'equal) (equal (caaadr regle) '!))
                      (progn
                        (setq result (apply-regle regle BR BF))
                        (setq BR (car result))
                        (setq BF (cadr result))
                        )
                    )
                  (if trouve (return-from nil trouve)) ;dans le cas où on aurait trouvé l'asso dans les appels emboîtés
                  (if (and regle (null lst_regles) (eq (car (caaddr regle)) 'ajout_asso)) (recom BF)) ;si la dernière règle appliquée est de type 1, on est arrivé à une feuille
                  ; de notre arbre de questions, on appelle recom
                  (if (null regle) (return-from nil nil)) ; si on a appliqué toutes les règles, on sort du loop
                  )  
              )
            )
      )
  )
    )
  )


---------------------------------------------------------------------------------------------------------------------------
(defun fMatching (pattern BF)
  (let (l_matchings)
    (dolist (fait BF l_matchings) ; On parcourt tous les faits
      (let ((atoms_ok t) (l_vars '()))
        (do ((l_pAtoms pattern (cdr l_pAtoms)) (l_fAtoms fait (cdr l_fAtoms))) ((not (and (or l_pAtoms l_fAtoms) atoms_ok)) atoms_ok) ; Pour chaque fait, on compare ses elements avec le pattern
          (if (and (listp (car l_pAtoms)) (equal (caar l_pAtoms) '?) (car l_fAtoms)) ; Si l'élément est une variable dans le pattern...
            (push (cons (cdar l_pAtoms) (car l_fAtoms)) l_vars) ; ...alors on enregistre cette variable et sa valeur dans le fait, et on les ajoutes à l_vars...
            (if (not (equal (car l_pAtoms) (car l_fAtoms))) ; ...sinon si l'element du fait et lelement du pattern sont différents...
              (setq atoms_ok nil) ; ...alors on rejette ce fait et on passe au suivant.
            )
          )
        )
        (if atoms_ok
          (push l_vars l_matchings) ; Si le fait correspond au pattern, on ajoute l_vars à la liste resultat
        )
      )
    )
  )
  )

---------------------------------------------------------------------------------------------------------------------------
(defun evalVar (list l_vars)
  (cons ; On combine l'evaluation du première élément, et l'évaluation du reste de la liste par recursivité
        (if (listp (car list))
          (if (equal (caar list) '?) ; Si le premier element est une variable...
            (let ((val (cdr (assoc (cdar list) l_vars)))) (if val val (car list))) ; ...l'evalue par sa valeur retourne sa valeur
            (evalVar (car list) l_vars) ; Si c'est une simple liste On retourne ; Si l'element est une simple liste, on utilise evalVar pour l'évaluer
          )
          (car list) ; Si l'élément est un simple atom, on l'évalue par lui-meme
        )
        (if (cdr list) (evalVar (cdr list) l_vars) nil) ; S'il reste des éléments dans la liste, on utilise evalVar pour les évaluer
  )
)

(evalVar '(R1 ((= (? . to) taad) (? . test)) (? . bateau)) '((test . a) (to . b)))
---------------------------------------------------------------------------------------------------------------------------
(defun rMatching (l_premisses BF)
  (let (l_matchings (l_fMatchings (fMatching (cdar l_premisses) BF))) ; On met les correspondances au pattern de la premisse dans l_fMatchings
    (case (caar l_premisses)
      (! (setq l_fMatchings (if l_fMatchings nil (list nil)))) ; Si on teste l'absence de ce fait et qu'il existe on met l_fMatchings à nil sinon on on le met à (nil)
    )
    (if (cdr l_premisses) ; Si il reste d'autre premisses...
      (dolist (fMatching l_fMatchings l_matchings) ; ...alors pour chacune des combinaisons de variables trouvée pour le pattern de la première prémisse...
        (dolist (matching (rMatching (evalVar (cdr l_premisses) fMatching) BF)) ; ...on recupere les combinaison spossibles pour les premisses restantes en utilisant rMatching récursivement.
          (push (append fMatching matching) l_matchings) ; On ajoute la combinaison des variables trouvée pour la premiere premisse et celles trouvées pour les suivantes.
        )
      )
      l_fMatchings ; Si c'était la dernière prémisse, on retourne simplement le résultat de fMatching
    )
  )
)


(rMatching '((= objet (? . asso) asso) (= (? . asso) gout (? . gout))) *BF*)
---------------------------------------------------------------------------------------------------------------------------
(defun regles_candidates (BR BF)
  (let (l_regles_c)
    (dolist (regle BR (reverse l_regles_c)) ; Pour chaque regle de BR...
      (dolist (matching (rMatching (cadr regle) BF)) ; ...on cherche les combinaisons de variables possibles...
        (push (evalVar regle matching) l_regles_c) ; ...et on ajoute l'évaluation de cette règle avec chacune de ces combinaisons.
      )
    )
  )
)

(regles_candidates *BR* *BF*)
