; Forme regle : (nom (premisses) (conclusions))
;     Une r�gle est applicable si toutes ses premisses sont vraies
; Forme premisse : (operateur objet attribut valeur)
;     Liste operateurs : = (vrai s'il existe une correspondance dans *BF*), ! (vrai s'il nexiste pas de correspondance dans *BF*)
; Forme conclusion : (fonction argument)
; Forme objet, attribut, valeur : atome ou variable de type : (? . nom)

;Remarque : on n'est pas oblig� de se limiter � des faits (objet attribut valeur), normalement les fonctions supportent des faits de diff�rentes tailles

;listes des fonctions impl�ment�es : apply_regle, ajoutFait, evalVar, rMatching, fMatching, regles_candidates, demand, maximun, recom, ajout_asso, descriptif, start

;pour d�marrer le syst�me, il suffit de taper (start) dans le listener 
;si vous voulez red�marrer le syst�me, il suffit d'appeler (reset) dans le listener
;pour quitter un loop de la fonction demand, il faut r�pondre par fin
;go�ts possibles � l'heure actuelle : evenementiel, astronomie, aviation, parachutisme, musique, danse, vieCampus, humanitaire, citoyennete, sport, m�canique et voyage
;si une des questions am�ne � cr�er un pattern �tudiant qui ne partagera pas la valeur prise par l'attribut avec, aucune des asso de BF, aucune asso est recommand�e.
;on ne recommande que la meilleure asso dans ce syst�me, afin de lui recommander plusieurs assos il faut g�rer des scores pond�r�s.
---------------------------------------------------------------------------------------------------------------------------
;Variables globales : 
(defvar *BF*)
(defvar *BR*)
(defvar trouve nil)
---------------------------------------------------------------------------------------------------------------------------
;Fonction de d�marrage :
(defun start () (apply-regle (assoc 'Q4 *BR*) *BR* *BF*)) ;fonction qui lance la premi�re r�gle candidate du programme.

;Fonction de red�marrage :
(defun reset ()
  (progn
    (setq trouve nil) ;on remet trouve � NIL
    (apply-regle (assoc 'Q4 *BR*) *BR* *BF*) ; on r�-applique start
    )
)

---------------------------------------------------------------------------------------------------------------------------
(setq *BR* '(
            (R0 ((= etudiant gout (? . gout)) (= objet (? . asso) asso) (= (? . asso) gout (? . gout))) ((ajout_asso (? . asso))))
            (R1 ((= etudiant style (? . style)) (= etudiant gout musique) (= objet (? . asso) asso) (= (? . asso) gout musique) (= (? . asso) style (? . style))) ((ajout_asso (? . asso))))
            (R2 ((= etudiant style (? . style)) (= etudiant gout danse) (= objet (? . asso) asso) (= (? . asso) style (? . style) (= (? . asso) gout danse))) ((ajout_asso (? . asso))))
            (R3 ((= etudiant instrument (? . instrument)) (= objet (? . asso) asso) (= (? . asso) instrument (? . instrument))) ((ajout_asso (? . asso))))
            (R4 ((= etudiant niveau (? . niveau)) (= etudiant instrument (? . instrument)) (= objet (? . asso) asso) (= (? . asso) instrument (? . instrument)) (= (? . asso) niveau (? . niveau))) ((ajout_asso (? . asso))))
            (R5 ((= etudiant fa�on_pratique (? . fp)) (= objet (? . asso) asso) (= (? . asso) fa�on_pratique (? . fp))) ((ajout_asso (? . asso))))
            (R6 ((= etudiant type (? . type)) (= etudiant gout evenementiel)(= objet (? . asso) asso) (= (? . asso) gout evenementiel) (= (? . asso) type (? . type))) ((ajout_asso (? . asso)))) 
            (R7 ((= etudiant lieu (? . lieu)) (= etudiant gout voyage) (= objet (? . asso) asso) (= (? . asso) gout voyage)(= (? . asso) lieu (? . lieu))) ((ajout_asso (? . asso))))
            (R8 ((= etudiant sport (? . sport)) (= objet (? . asso) asso) (= (? . asso) sport (? . sport))) ((ajout_asso (? . asso))))
            (R9 ((= etudiant domaine_m�ca (? . d_m)) (= objet (? . asso) asso) (= (? . asso) domaine_m�ca (? . d_m))) ((ajout_asso (? . asso))))
            (R10 ((= etudiant type (? . type)) (= etudiant gout vieCampus) (= objet (? . asso) asso) (= (? . asso) gout vieCampus) (= (? . asso) type (? . type))) ((ajout_asso (? . asso)))) 
            (R11 ((= etudiant lieu (? . lieu)) (= etudiant gout vieCampus) (= objet (? . asso) asso) (= (? . asso) gout vieCampus) (= (? . asso) lieu (? . lieu))) ((ajout_asso (? . asso))))
            (R12 ((= etudiant but (? . but)) (= etudiant gout vieCampus) (= objet (? . asso) asso) (= (? . asso) gout vieCampus) (= (? . asso) but (? . but))) ((ajout_asso (? . asso))))
            (R13 ((= etudiant but (? . but)) (= etudiant gout voyage) (= objet (? . asso) asso) (= (? . asso) gout voyage) (= (? . asso) but (? . but))) ((ajout_asso (? . asso))))
            (R14 ((= etudiant pratique (? . pratique)) (= objet (? . asso) asso) (= (? . asso) pratique (? . pratique))) ((ajout_asso (? . asso))))
            (R15 ((= etudiant standing (? . standing)) (= objet (? . asso) asso) (= (? . asso) standing (? . standing))) ((ajout_asso (? . asso))))

            (Q0 ((= etudiant fa�on_pratique (? . fa�on_pratique)) (= etudiant gout musique)) ((demand style)) )
            (Q1 ((= etudiant style (? . style)) (= etudiant gout danse)) ((demand niveau)))
            (Q2 ((= etudiant instrument (? . instrument)) (! etudiant instrument non)) ((demand niveau)) )
            (Q3 ((= etudiant niveau (? . niveau))(= etudiant gout musique)) ((demand fa�on_pratique)) )
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
;liste des assos impl�ment�es : comet,SDF,etuville,IF,Stravaganza,Ocata,Larsen,D�cibels,PianoUT,Choruts, AlaRUtc, Acoustique
;UTCIEL,VeloUTC, Orion, Club_biom�ca, coincidence, PIC, Integ, Ut�cia, Picnrock, breakdance, UTRIP, SurfUT
;SkiUT, BreakDance, CABARET, TUC, SoleilsenInde, ToitpourleN�pal, ToitPourLeNepal 
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
             (SDF lieu Compi�gne)
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
             (Stravaganza fa�on_pratique orchestre)
             (Stravaganza style classique)
             (Stravaganza instrument violon)
             (Stravaganza instrument alto)
             (Stravaganza instrument trompette)
             (Stravaganza instrument contrebasse)
             (Stravaganza instrument fl�te)
             (Stravaganza instrument traversi�re)
             (Stravaganza instrument batterie)
             (Stravaganza niveau moyen)
             (Stravaganza niveau confirme)
             (objet D�cibels asso)
             (D�cibels gout musique)
             (D�cibels gout evenementiel)
             (D�cibels instrument non)
             (D�cibels type concert)
             (D�cibels standing gros)
             (D�cibels standing casual)
             (objet Larsen asso)
             (Larsen gout musique)
             (Larsen style rock)
             (Larsen fa�on_pratique groupe)
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
             (PianoUT fa�on_pratique individuelle)
             (PianoUT niveau basique)
             (PianoUT niveau moyen)
             (PianoUT niveau confirme)
             (objet Choruts asso)
             (Choruts gout musique)
             (Choruts style variete)
             (Choruts fa�on_pratique chorale)
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
             (Ocata fa�on_pratique groupe)
             (Ocata niveau moyen)
             (Ocata niveau confirm�)
             (objet Acoustic asso)
             (Acoustic gout musique)
             (Acoustic fa�on_pratique seul)
             (Acoustic fa�on_pratique groupe)
             (Acoustic niveau d�butant)
             (Acoustic niveau moyen)
             (Acoustic niveau confirm�)
             (Acoustic instrument guitare_s�che)
             (objet AlaRUtc asso)
             (AlaRUtc gout musique)
             (AlaRUtc fa�on_pratique groupe)
             (AlaRUtc style fanfare)
             (AlaRUtc niveau d�butant)
             (AlaRUtc niveau moyen)
             (AlaRUtc niveau confirm�)
             (AlaRUtc instrument saxophone)
             (AlaRUtc instrument trompette)
             (AlaRUtc instrument tambour)
             (AlaRUtc instrument flute)
             (objet Cabaret asso)
             (Cabaret gout vieCampus)
             (Cabaret gout evenementiel)
             (Cabaret type soir�e)
             (Cabaret lieu Compi�gne)
             (Cabaret standing classe)
             (Cabaret but repas)
             (objet TUC asso)
             (TUC gout citoyennete)
             (TUC gout humanitaire)
             (TUC lieu Compi�gne)
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
             (Integ but int�grer)
             (objet Coincidence asso)
             (Coincidence gout danse)
             (Coincidence style modern_jazz)
             (Coincidence niveau moyen)
             (Coincidence niveau confirm�)
             (Coincidence pratique groupe)
             (objet BreakDance asso)
             (BreakDance gout danse)
             (BreakDance style breakdance)
             (BreakDance niveau d�butant)
             (BreakDance niveau moyen)
             (BreakDance niveau confirm�)
             (BreakDance pratique seul)
             (BreakDance pratique groupe)
             (objet PicNRock asso)
             (PicNRock gout danse)
             (PicNRock style rock)
             (PicNRock niveau d�butant)
             (PicNRock niveau moyen)
             (PicNRock niveau confirm�)
             (PicNRock pratique couple)
             (objet Ut�cia asso)
             (Ut�cia gout m�canique)
             (Ut�cia domaine_m�ca automobile)
             (objet Club_BioM�ca asso)
             (Club_BioM�ca gout m�canique)
             (Club_BioM�ca domaine_m�ca proth�ses)
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
    (if (not (member (list 'recom asso) BF :test #'equal)) ; si on a pas d�j� recommand� l'asso
      (push (list 'recom asso) BF) ;on ajoute l'�l�ment (recom asso) dans BF
    )
  (push (list 'score asso) BF) ;on ajoute l'�l�ment (score asso) pour donner + de poids � l'asso
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
    (loop ;on cr�e un loop
      (if trouve (return-from nil nil)) ;on ne s'arr�te que lorsque la variable trouve est true
      (format t "Quels sont tes centres d'int�r�t ? ") ;on demande les centre d'int�r�t
      (setq rep (read)) ;on lit la r�ponse
      (if (eq rep 'fin) (return-from nil nil)) ;on sort du loop si la r�ponse est fin
      (ajoutFait (list 'etudiant 'gout rep) BR BF) ;sinon on ajoute le fait � la base de faits
     )
    )
   ((eq theme 'm�canique)
    (format t "Dans quel domaine de m�canique (proth�ses, automobile) ?")
    (setq rep (read))
    (ajoutFait (list 'etudiant 'domaine_m�ca rep) BR BF)
    )
   ((eq theme 'musique) ;si le th�me est musique
    (format t "Joues-tu d'un instrument ? ") ;on demande si l'utilisateur joue d'un instrument
    (setq rep (read))
    (if (eq rep 'non) ; si c'est non
        (ajoutFait (list 'etudiant 'instrument rep) BR BF) ;on ajoute le fait dans BF
        (loop ;si c'est oui, on loop sur les instruments
          (if trouve (return-from nil nil)) ;on s'arr�te si trouve est � true
          (format t "Quel(s) instrument(s) joues-tu ? ")
          (setq rep (read))
          (if (eq rep 'fin) (return-from nil nil)) ;on s'arr�te aussi si l'utilisateur entre fin
          (ajoutFait (list 'etudiant 'instrument rep) BR BF)
        ))
    )
   ((eq theme 'fa�on_pratique)
   (format t "D�sires-tu jouer dans un groupe, un orchestre ... ? ") 
   (ajoutFait (list 'etudiant 'fa�on_pratique (read)) BR BF) 
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
    (format t "Quel type d'�venements?")
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
      (let ((asso (maximum BF))) ;on r�cup�re l'asso avec le score max gr�ce � la fonction maximum
        (format t "On te recommande cette association : ~S~% Cela te va ?" (car asso))
        (setq rep (read))
        (if (equal rep '?) ;si l'utilisateur veut en savoir plus
            (progn
              (descriptif (car asso)) ;on lance la fonction descriptif sur l'association
              (format t "Veux-tu t'y investir ?")
              (setq rep (read))
              )
          )
        (if (equal rep 'oui) ;si la r�ponse est oui alors trouve passe � true
          (setq trouve t)    ;cela permettra de sortir des loop de la fonction demand
      )
    )
  )

(defun descriptif (asso) ;donne le descriptif d'une asso seuls les descriptifs de Larsen, D�cibels et SkiUT existent, il suffirait d'ajouter tous les descriptifs des assos pr�sents dans la base de faits
  (cond 
   ((eq asso 'Larsen) (format t "Larsen, l�association des musiciens de l�UTC
Larsen permet aux musiciens de l�UTC de se rencontrer et de
former ou compl�ter leur groupe.
Ceux-ci r�p�tent dans la salle pr�vue � cet effet, �quip�e du matos haut
de gamme (!) de l�asso. Larsen offre la possibilit� aux groupes de participer aux
diff�rents �v�nements type tourn�e des bars, soir�es concerts, Festival des Vieilles
Pipettes, PMDE, Festupic...
Environ une fois par mois, une soir�e jam session dans un bar est organis�e et m�lange
des musiciens de tous horizons (groupes de la ville, musiciens compi�gnois, jazzeux
d�Ocata, orchestre...). Si vous cherchez le groupe de vos r�ves, contactez-vous !"))
  ((eq asso 'D�cibels) (format t "De la sc�ne au studio, nous offrons la possibilit� aux �tudiants de d�couvrir l�univers technique du son et de l��clairage. 
Ouverte aux d�butants autant qu�aux initi�s, l�asso se pr�sente en trois p�les :
- P�le Sono : Des concerts Larsen dans les bars aux concerts semi-pros lors de gros
�v�nements, une partie importante de notre activit� consiste � sonoriser les sc�nes de
l�UTC : montage, c�blage, balances, plateau, r�gie, logistique. Lors des formations de base
nous apprenons aux d�butants � utiliser le mat�riel (table de mixage, syst�me de diffusion,
micros, etc) afin de pouvoir g�rer une sc�ne en toute autonomie. Quelques exemples
d��v�nements o� nous sommes pr�sents : Utc�enne, Gala, Cabaret, concerts Larsen, PMDE,
Festupic, concerts de l�ESCOM.
- P�le MAO : D�cibels est aussi pr�sent cot� studio. Enregistrement de groupes, projet
Weeksong, ateliers MAO. Ces activit�s sont bas�es sur l��change de savoir entre les
�tudiants, ainsi il est possible d�apprendre en partant de z�ro ou de partager ses connaissances (Logiciels, prise de son, exp�riences...). Pour mener les ateliers et d�velopper les diff�rents
projets de ce p�le, nous investissons le studio d�enregistrement de la nMDE.
- P�le Light : aux c�t�s du p�le Sono, les lumi�res apportent une vraie touche pro aux
spectables de l�UTC ! Des formations techniques et artistiques dispens�es tout au long
du semestre vous permettront de devenir un v�ritable connaisseur des lights et de vous
�clater lors des spectables utc�ens ou lors de projet mont�s en parall�le !"))
   ((eq asso 'SkiUT) (format t "Des �tudiants, de la neige, du sport et de l�ambiance pour une semaine inoubliable !
Cr��e en 1989 par des �tudiants passionn�s de montagne, Ski�UTC a pour but de rassembler, lors d�une semaine au mois de Janvier, le plus grand nombre d��tudiants de
l�UTC pour un voyage hors du commun au ski.
Il s�agit d�un �v�nement qui promeut la coh�sion et l�esprit d��quipe au sein des �tudiants,
qui propose des animations sportives et festives. Tout cela � des tarifs accessibles.
Ski�UTC permet �galement � des �tudiants qui n�en auraient pas l�occasion faute de
moyens financiers (ou habitant dans des pays sans neige) de pouvoir partir au ski. Pour certains, Ski�UTC est l�occasion de d�couvrir le ski.
C�est un �v�nement qui permet de renforcer la coh�sion et l�esprit d��quipe au sein des �tudiants � travers de nombreuses animations sportives et festives : organisation de
slaloms, soir�es multisports sur neige, barbecue sur les pistes, restaurant en altitude..
"))
   )
  )

(defun maximum (BF)
  (let ((lst_asso (fMatching '(recom (? . d)) BF)) (maxi '(x . 0)) lst_comptage) ;on r�cup�re une liste des asso avec un score non nul et on d�finit le max � un couple (x . 0)
    (dolist (asso lst_asso maxi)                               ;pour chaque asso dans lst_asso
      (setq lst_comptage (fMatching `(score ,(cdar asso)) BF)) ;on r�cup�re une liste dont le nombre d'�l�ment est le nombre de fois qu'appara�t le pattern (score asso)
      (setq score (cons (cdar asso) (length lst_comptage)))    ;on cr�e une paire point�e (asso . nb_element dans lst_comptage)
      (if (> (cdr score) (cdr maxi)) ;si le score de cette asso est sup�rieur au maximum d�j� d�fini
        (setq maxi score)            ;on remplace maximum par cette nouvelle paire point�e
          )
    )
    )
  )


---------------------------------------------------------------------------------------------------------------------------

(defun apply-regle (regle BR BF )
  (let* ((pointeur (assoc (car regle) BR)) (BR_temp (remove pointeur BR)) BF_temp) ;on supprime la r�gle de base de r�gles, le r�sultat est stock� dans BR_temp
    ;remove renvoie en fait la base de r�gles en excluant la r�gle � supprimer mais ne met en aucun cas � jour BR
    (dolist (conclusion (caddr regle) (list BR_temp BF_temp))
      (setq BF_temp (funcall (car conclusion) (cadr conclusion) BR_temp BF)) 
       ;application des conclusions de la r�gle 
       ;utilisation d'un funcall car les conclusions sont bas�es sur des fonctions.
      )
   )
  )
;ici, il n'existe qu'une conclusion par r�gle mais on aurait pu en imaginer plusieurs. 

---------------------------------------------------------------------------------------------------------------------------
(defun ajoutFait (fait BR BF) ; Profondeur et cha�nage avant
  (if (null trouve) 
      (if (not (member fait BF :test #'equal)) ;on teste d'abord si le fait n'appartient pas � la BF, si tel est le cas, on l'ajoute 
          (let (lst_regles regle result var) 
            (setq lst_regles (regles_candidates BR (append (list fait) BF))) ;on cherche alors les r�gles qui peuvent s'appliquer suite � cet ajout (qui n'�taient pas d�clenchables avant sans cet ajout)
            (if lst_regles 
                (progn
                (setq BF (append (list fait) BF)) ;mise � jour de la base de faits
                (loop ;les r�gles sont appliqu�es les unes � la suite des autres par un loop
                  (setq regle (pop lst_regles)) 
                  (if regle (print regle)) ;;affiche la r�gle utilis�e
                  (if (or (member (cons '= fait) (cadr regle) :test #'equal) (equal (caaadr regle) '!))
                      (progn
                        (setq result (apply-regle regle BR BF))
                        (setq BR (car result))
                        (setq BF (cadr result))
                        )
                    )
                  (if trouve (return-from nil trouve)) ;dans le cas o� on aurait trouv� l'asso dans les appels embo�t�s
                  (if (and regle (null lst_regles) (eq (car (caaddr regle)) 'ajout_asso)) (recom BF)) ;si la derni�re r�gle appliqu�e est de type 1, on est arriv� � une feuille
                  ; de notre arbre de questions, on appelle recom
                  (if (null regle) (return-from nil nil)) ; si on a appliqu� toutes les r�gles, on sort du loop
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
          (if (and (listp (car l_pAtoms)) (equal (caar l_pAtoms) '?) (car l_fAtoms)) ; Si l'�l�ment est une variable dans le pattern...
            (push (cons (cdar l_pAtoms) (car l_fAtoms)) l_vars) ; ...alors on enregistre cette variable et sa valeur dans le fait, et on les ajoutes � l_vars...
            (if (not (equal (car l_pAtoms) (car l_fAtoms))) ; ...sinon si l'element du fait et lelement du pattern sont diff�rents...
              (setq atoms_ok nil) ; ...alors on rejette ce fait et on passe au suivant.
            )
          )
        )
        (if atoms_ok
          (push l_vars l_matchings) ; Si le fait correspond au pattern, on ajoute l_vars � la liste resultat
        )
      )
    )
  )
  )

---------------------------------------------------------------------------------------------------------------------------
(defun evalVar (list l_vars)
  (cons ; On combine l'evaluation du premi�re �l�ment, et l'�valuation du reste de la liste par recursivit�
        (if (listp (car list))
          (if (equal (caar list) '?) ; Si le premier element est une variable...
            (let ((val (cdr (assoc (cdar list) l_vars)))) (if val val (car list))) ; ...l'evalue par sa valeur retourne sa valeur
            (evalVar (car list) l_vars) ; Si c'est une simple liste On retourne ; Si l'element est une simple liste, on utilise evalVar pour l'�valuer
          )
          (car list) ; Si l'�l�ment est un simple atom, on l'�value par lui-meme
        )
        (if (cdr list) (evalVar (cdr list) l_vars) nil) ; S'il reste des �l�ments dans la liste, on utilise evalVar pour les �valuer
  )
)

(evalVar '(R1 ((= (? . to) taad) (? . test)) (? . bateau)) '((test . a) (to . b)))
---------------------------------------------------------------------------------------------------------------------------
(defun rMatching (l_premisses BF)
  (let (l_matchings (l_fMatchings (fMatching (cdar l_premisses) BF))) ; On met les correspondances au pattern de la premisse dans l_fMatchings
    (case (caar l_premisses)
      (! (setq l_fMatchings (if l_fMatchings nil (list nil)))) ; Si on teste l'absence de ce fait et qu'il existe on met l_fMatchings � nil sinon on on le met � (nil)
    )
    (if (cdr l_premisses) ; Si il reste d'autre premisses...
      (dolist (fMatching l_fMatchings l_matchings) ; ...alors pour chacune des combinaisons de variables trouv�e pour le pattern de la premi�re pr�misse...
        (dolist (matching (rMatching (evalVar (cdr l_premisses) fMatching) BF)) ; ...on recupere les combinaison spossibles pour les premisses restantes en utilisant rMatching r�cursivement.
          (push (append fMatching matching) l_matchings) ; On ajoute la combinaison des variables trouv�e pour la premiere premisse et celles trouv�es pour les suivantes.
        )
      )
      l_fMatchings ; Si c'�tait la derni�re pr�misse, on retourne simplement le r�sultat de fMatching
    )
  )
)


(rMatching '((= objet (? . asso) asso) (= (? . asso) gout (? . gout))) *BF*)
---------------------------------------------------------------------------------------------------------------------------
(defun regles_candidates (BR BF)
  (let (l_regles_c)
    (dolist (regle BR (reverse l_regles_c)) ; Pour chaque regle de BR...
      (dolist (matching (rMatching (cadr regle) BF)) ; ...on cherche les combinaisons de variables possibles...
        (push (evalVar regle matching) l_regles_c) ; ...et on ajoute l'�valuation de cette r�gle avec chacune de ces combinaisons.
      )
    )
  )
)

(regles_candidates *BR* *BF*)
