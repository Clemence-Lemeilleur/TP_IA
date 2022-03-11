	/*********************************
	DESCRIPTION DU JEU DU TIC-TAC-TOE
	*********************************/

	/*
	Une situation est decrite par une matrice 3x3.
	Chaque case est soit un emplacement libre (Variable LIBRE), soit contient le symbole d'un des 2 joueurs (o ou x)

	Contrairement a la convention du tp precedent, pour modeliser une case libre
	dans une matrice on n'utilise pas une constante speciale (ex : nil, 'vide', 'libre','inoccupee' ...);
	On utilise plut�t un identificateur de variable, qui n'est pas unifiee (ex : X, A, ... ou _) .
	La situation initiale est une "matrice" 3x3 (liste de 3 listes de 3 termes chacune)
	o� chaque terme est une variable libre.	
	Chaque coup d'un des 2 joureurs consiste a donner une valeur (symbole x ou o) a une case libre de la grille
	et non a deplacer des symboles deja presents sur la grille.		
	
	Pour placer un symbole dans une grille S1, il suffit d'unifier une des variables encore libres de la matrice S1,
	soit en ecrivant directement Case=o ou Case=x, ou bien en accedant a cette case avec les predicats member, nth1, ...
	La grille S1 a change d'etat, mais on n'a pas besoin de 2 arguments representant la grille avant et apres le coup,
	un seul suffit.
	Ainsi si on joue un coup en S, S perd une variable libre, mais peut continuer a s'appeler S (on n'a pas besoin de la designer
	par un nouvel identificateur).
	*/

situation_initiale([ [_,_,_],
                     [_,_,_],
                     [_,_,_] ]).

test_matrice([ 	[a,b,c],
				[d,e,f],
				[g,h,i] ]).

test_align_gg([ 	[x,x,x],
					[x,x,x],
					[x,x,x] ]).

test_gagnant_J([ 	[x,o,o],
					[o,x,o],
					[x,o,x] ]).

test_perdant_J([ 	[x,x,o],
					[o,o,o],
					[x,o,x] ]).

test_egalite([ 	[x,o,x],
				[o,x,o],
				[o,x,o] ]).

test_predicats :-	test_align_gg(S), joueur_initial(J), 
					alignement(Ali, S), alignement_gagnant(Ali, J).

joueur_initial(x).

adversaire(x,o).
adversaire(o,x).

alignement(L, Matrix) :- ligne(L,Matrix).
alignement(C, Matrix) :- colonne(C,Matrix).
alignement(D, Matrix) :- diagonale(D,Matrix).

	
ligne(L, M) :- nth1(_,M,L).

colonne(C,M) :- maplist(nth1(_), M, C).

		
diagonale(D, M) :- 
	premiere_diag(1,D,M).


diagonale(D, M) :-
	seconde_diag(3,D,M).

	
premiere_diag(_,[],[]).
premiere_diag(K,[E|D],[Ligne|M]) :-
	nth1(K,Ligne,E),
	K1 is K+1,
	premiere_diag(K1,D,M).

seconde_diag(_,[],[]).
seconde_diag(K,[E|D],[Ligne|M]) :-
	nth1(K,Ligne,E),
	K1 is K-1,
	seconde_diag(K1,D,M).


possible(  [],  _).
possible([X|L], J) :- unifiable(X,J), possible(L,J).

unifiable(X,J) :- var(X); X=J.

alignement_gagnant(Ali, J) :- possible(Ali, J), ground(Ali).

alignement_perdant(Ali, J) :- 	J=x, possible(Ali, o), ground(Ali);
								J=o, possible(Ali, x), ground(Ali).


	/* ****************************
	DEFINITION D'UN ETAT SUCCESSEUR
	****************************** */

	/* 
	Il faut definir quelle operation subit la matrice
	M representant l'Etat courant
	lorsqu'un joueur J joue en coordonnees [L,C]
	*/	

% A FAIRE
% successeur(J, Etat,[L,C]) :- ? ? ? ?  

	/**************************************
   	 EVALUATION HEURISTIQUE D'UNE SITUATION
  	 **************************************/

	/*
	1/ l'heuristique est +infini si la situation J est gagnante pour J
	2/ l'heuristique est -infini si la situation J est perdante pour J
	3/ sinon, on fait la difference entre :
	   le nombre d'alignements possibles pour J
	moins
 	   le nombre d'alignements possibles pour l'adversaire de J
*/


heuristique(J,Situation,H) :-
   H = 10000,
   alignement(Alig,Situation),
   alignement_gagnant(Alig,J), !;
   adversaire(J, Adv), H is

	
heuristique(J,Situation,H) :-		% cas 2
   H = -10000,				% grand nombre approximant -infini
   alignement(Alig,Situation),
   alignement_perdant(Alig,J), !.	


% on ne vient ici que si les cut precedents n'ont pas fonctionne,
% c-a-d si Situation n'est ni perdante ni gagnante.

% A FAIRE 					cas 3
% heuristique(J,Situation,H) :- ? ? ? ?


