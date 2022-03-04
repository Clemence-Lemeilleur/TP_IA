p%*******************************************************************************
%                                    AETOILE
%*******************************************************************************

/*
Rappels sur l'algorithme
 
- structures de donnees principales = 2 ensembles : P (etat pendants) et Q (etats clos)
- P est dedouble en 2 arbres binaires de recherche equilibres (AVL) : Pf et Pu
 
   Pf est l'ensemble des etats pendants (pending states), ordonnes selon
   f croissante (h croissante en cas d'egalite de f). Il permet de trouver
   rapidement le prochain etat a developper (celui qui a f(U) minimum).
   
   Pu est le meme ensemble mais ordonne lexicographiquement (selon la donnee de
   l'etat). Il permet de retrouver facilement n'importe quel etat pendant

   On gere les 2 ensembles de fa�on synchronisee : chaque fois qu'on modifie
   (ajout ou retrait d'un etat dans Pf) on fait la meme chose dans Pu.

   Q est l'ensemble des etats deja developpes. Comme Pu, il permet de retrouver
   facilement un etat par la donnee de sa situation.
   Q est modelise par un seul arbre binaire de recherche equilibre.

Predicat principal de l'algorithme :

   aetoile(Pf,Pu,Q)

   - reussit si Pf est vide ou bien contient un etat minimum terminal
   - sinon on prend un etat minimum U, on genere chaque successeur S et les valeurs g(S) et h(S)
	 et pour chacun
		si S appartient a Q, on l'oublie
		si S appartient a Ps (etat deja rencontre), on compare
			g(S)+h(S) avec la valeur deja calculee pour f(S)
			si g(S)+h(S) < f(S) on reclasse S dans Pf avec les nouvelles valeurs
				g et f 
			sinon on ne touche pas a Pf
		si S est entierement nouveau on l'insere dans Pf et dans Ps
	- appelle recursivement etoile avec les nouvelles valeurs NewPF, NewPs, NewQs

*/

%*******************************************************************************

:- ['avl.pl'].       % predicats pour gerer des arbres bin. de recherche
:- ['taquin.pl'].    % predicats definissant le systeme a etudier

%*******************************************************************************

main :- initial_state(S0), heuristique2(S0, H0), G0 is -1, F0 is H0+G0,
	empty(Pf), empty(Pu), empty(Q),
	insert( [[F0,H0,G0], S0], Pf, Pf),
	insert( [S0, [F0,H0,G0], nil, nil], Pu, Pu),
	aetoile(Pf, Pu, Q).

%*******************************************************************************

loop_successors([], _, _, _).
loop_successors([[S]|Ls], Pf, Pu, Q) :-
	S=[U, _, _, _], member(U, Q), loop_successors(Ls, Pf, Pu, Q).
	
	
expand(U, G, ListNoeudsSuccDirect):-
	findall( [S, [Fs, Hs, Gs], U, A], 
		(rule(A,1, U, S), heuristique(S,Hs),Gs is G+1,Fs is Gs+Hs), 
		ListNoeudsSuccDirect).

affiche_solution :- true.

aetoile(Pf, Pu, _) :-
	empty(Pf), empty(Pu),
	print("PAS de SOLUTION : L’ETAT FINAL N’EST PAS ATTEIGNABLE !").
aetoile(Pf, _, Q) :-
	suppress_min([[_, _, G], Fmin], Pf, _), final_state(Fmin), affiche_solution.
aetoile(Pf, Ps, Qs) :-
	suppress_min([_, Umin], Pf, Pf2), suppress([Umin, _, _, _], Pu, Pu2),
	expand(Umin, G, Succ), loop_successors(Succ)

