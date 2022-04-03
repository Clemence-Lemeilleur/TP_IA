%*******************************************************************************
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

main :- 
	% On fait toutes les initialisations
	initial_state3(S0), 
	heuristique(S0, H0), 
	G0 is 0, F0 is H0+G0,
	empty(Pf0), empty(Pu0), empty(Q),
	insert([[F0,H0,G0], S0], Pf0, Pf),
	insert( [S0, [F0,H0,G0], nil, nil], Pu0, Pu),
	% et on lance aetoile
	aetoile(Pf, Pu, Q).

%*******************************************************************************
% LOOP_SUCCESSORS : traiter chaque noeud successeur
%*******************************************************************************
loop_successors([], Pf, Pu, _, Pf, Pu).

loop_successors([[U, _, _, _]|Ls], Pf, Pu, Q, Newpf, Newpu) :-
	/* si S est dans Q, alors on oublie cet etat */
	belongs([U, _, _, _], Q),
	!,
	loop_successors(Ls, Pf, Pu, Q, Newpf, Newpu).

loop_successors([[U,[Fs,Hs,Gs], Peres, As] | Ls] , Pf, Pu, Q, Newpf, Newpu) :-
	/* S est connu dans Pu alors on garde le terme associe a la meilleure evaluation */
	belongs([U,[Fpu, Hpu, Gpu],Perepu, Apu], Pu), 
	(Fs < Fpu -> (
		suppress([U,[Fpu, Hpu, Gpu],Perepu, Apu], Pu, Pu2),
		suppress([[Fpu, Hpu, Gpu],U], Pf, Pf2),
		insert([U, [Fs, Hs, Gs], Peres, As], Pu2, Pu3),
		insert([[Fs, Hs, Gs],U], Pf2, Pf3),
		loop_successors(Ls, Pf3, Pu3, Q, Newpf, Newpu)
		);(
		loop_successors(Ls, Pf, Pu, Q, Newpf, Newpu)
		)
	),
	!.

loop_successors([[U,[F,H,G],Pere, Action] | Ss], Pf, Pu, Q, Newpf, Newpu) :-
	/* S est une nouvelle situation, on l'insere dans Pu et Pf */
	insert([U,[F,H,G],Pere, Action], Pu, Pu2),
	insert([[F,H,G], U], Pf, Pf2),
	loop_successors(Ss, Pf2, Pu2, Q, Newpf, Newpu).

%*******************************************************************************
% EXPAND : renvoie les successeurs d'une situation U
%*******************************************************************************
	
expand(U, G, Successeurs):-
	findall( [S, [F, H, Ga], U, A], 
	(rule(A, 1, U, S), 
	heuristique2(S,H),
	Ga is G+1,F is Ga+H), 
	Successeurs).

%*******************************************************************************
% RETOUR_SOLUTION et AFFICHAGE : renvoie le bon chemin et gere l'affichage
%*******************************************************************************

retour_solution(_, nil, []).
retour_solution(Q, U, [[U,A,H] | L]) :-
	belongs([U, [F,H,G], Pere, A], Q),
	suppress([U, [F,H,G], Pere, A], Q, Q1),
	retour_solution(Q1, Pere, L).

affichage([]).
affichage([L|Ls]):-
	write(L),
	write("\n"),
	affichage(Ls).
	
%*******************************************************************************
% AETOILE
%*******************************************************************************

aetoile(Pf, Pu, _) :-
	/* Cas trivial, Pf et Pu vide : pas de solution */
	empty(Pf), empty(Pu),
	write("PAS de SOLUTION : L’ETAT FINAL N’EST PAS ATTEIGNABLE !").

aetoile(Pf, Pu, Q) :-
	/* Si le noeud de valeur F min de Pf correspond a la situation finale, on a trouve
	la solution */
	final_state(Fmin), 
	suppress_min([[F, H, G], Fmin], Pf, _),
	suppress([Fmin, [F, H, G], Pere, A], Pu, _),
	insert([Fmin,[F,H,G], Pere, A],Q,Q1),
	retour_solution(Q1, Fmin, L),
	reverse(L, Lrev),
	length(Lrev, Long),
	Coutfinal is Long -1, %on fait -1 car dans Lrev il y a l'etat initial
	affichage(Lrev),
	write("\n"),
	write(Coutfinal),
	!.

aetoile(Pf, Pu, Q) :-
	/* cas general */
	suppress_min([[F,H,G], Umin], Pf, Pf2), 
	suppress([Umin, [F,H,G], Pere1, A], Pu, Pu2),
	expand(Umin, G, Succ), 
	loop_successors(Succ, Pf2, Pu2, Q, Newpf, Newpu),
	insert([Umin,[F,H,G],Pere1,A], Q, NewQ),
	aetoile(Newpf, Newpu, NewQ).

%************************************************************
%	TESTS
%************************************************************

%pour tester expand
testexp :-
	U=[[ a, b, c],        
       [ g, h, d],
       [vide,f, e] ],
	Result = [	
	[[	[ a, b, c],        
		[ vide, h, d],
		[ g,f, e]], _, U, up],
	[[	[ a, b, c],        
		[ g, h, d],
		[ f,vide, e]], _, U, right]],
	expand(U,0,Result).

%pour tester loop_successors
tests:-
	initial_state(S0),
	G0 is 0,
	heuristique2(S0,H0),
	F0 is G0 + H0,
	empty(Q),
	empty(Pu0),
	empty(Pf0),
	insert([[F0,H0,G0],S0], Pf0, Pf),
	insert([S0, [F0,H0,G0], nil, nil], Pu0, Pu),
	suppress_min([[F0,H0,G0],S0], Pf, Pf1),
	suppress([S0, [F0,H0,G0], nil, nil], Pu, Pu1),
	
	expand(S0,G0,L),
	write(L),
	write("Tirie\n\n"),
	put_flat(Pu),
	write("\n\n"),
	put_flat(Pf),
	write("Bairnar\n\n"),
	loop_successors(L, Pf, Pu, Q, Pf1, Pu1),
	write("Filipe\n\n"),
	put_flat(Pu1),
	write("Jan-jaq\n\n"),
	put_flat(Pf1).