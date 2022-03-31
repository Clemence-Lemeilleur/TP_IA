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

main :- initial_state2(S0), heuristique2(S0, H0), G0 is 0, F0 is H0+G0,
	empty(Pf), empty(Pu), empty(Q),
	insert( [[F0,H0,G0], S0], Pf, Pf),
	insert( [S0, [F0,H0,G0], nil, nil], Pu, Pu),
	aetoile(Pf, Pu, Q).

%*******************************************************************************

loop_successors([], Pf, Pu, _, Pf, Pu) :- write("oui").

loop_successors([[U, _, _, _]|Ls], Pf, Pu, Q, Newpf, Newpu) :-
	write("bernardoni le chauve\n"),
	/*si S est dans Q, alors on oublie cet etat*/
	belongs([U, _, _, _], Q),
	!,
	loop_successors(Ls, Pf, Pu, Q, Newpf, Newpu).

loop_successors([[U,[Fs,Hs,Gs], Peres, As] | Ls] , Pf, Pu, Q, Newpf, Newpu) :-
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

loop_successors([[U,[F,H,G],Pere, Action] | Ls], Pf, Pu, Q, Newpf, Newpu) :-
	/*S est une nouvelle situation, on l'insere dans Pu et Pf*/
	write("fanifahofjaoeuhuiahiuae\n"),
	insert([U,[F,G,H], Pere, Action], Pu, Pu2),
	insert([[F,G,H], U], Pf, Pf2),
	loop_successors(Ls, Pf2, Pu2, Q, Newpf, Newpu).

	
expand(U, G, Successeurs):-
	findall( [S, [F, H, Ga], U, A], 
	(rule(A, Cpt, U, S), 
	heuristique2(S,H),
	Ga is G+Cpt,F is Ga+H), 
	Successeurs).


affiche_solution(_, nil) :- write("Finito\n").
affiche_solution(Q, U) :-
	belongs([U, _, Pere, _], Q), suppress([U, _, Pere, A], Q, NewQ),
	write(A), write(" ; "),
	write(U), write(\n),
	affiche_solution(NewQ, Pere).


aetoile(Pf, Pu, _) :-
	empty(Pf), empty(Pu),
	print("PAS de SOLUTION : L’ETAT FINAL N’EST PAS ATTEIGNABLE !").
aetoile(Pf, _, Q) :-
	suppress_min([[_, _, _], Fmin], Pf, _), final_state(Fmin), 
	affiche_solution(Q, Fmin).
aetoile(Pf, Pu, Q) :-
	suppress_min([[F,H,G], Umin], Pf, Pf2), 
	suppress([Umin, [F,H,G], Pere1, A], Pu, Pu2),
	expand(Umin, G, Succ), loop_successors(Succ, Pf2, Pu2, Q, Newpf, Newpu),
	insert([Umin,[F,H,G],Pere1,A], Q, NewQ),
	aetoile(Newpf, Newpu, NewQ).


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

tests() :-
	% initialisations S0, F0, H0, G0

	initial_state(S0),
	G0 is 0,
	heuristique2(S0,H0),
	F0 is G0 + H0,

	% initialisations Pf, Pu et Q 

	empty(Q),
	empty(Pu0),
	empty(Pf0),
	insert([[F0,H0,G0],S0], Pf0, Pf),
	insert([S0, [F0,H0,G0], nil, nil], Pu0, Pu),
	suppress_min([[F0,H0,G0],S0], Pf, Pf1),
	suppress([S0, [F0,H0,G0], nil, nil], Pu, Pu1),
	%aetoile(Pf1,Pu1,Q),
	
	expand(S0,G0,L),
	write(L),
	write("BBBBBBBBBBBB\n\n"),
	%Basic Test
	put_flat(Pu),
	write("\n\n"),
	put_flat(Pf),
	write("fauzgyfuyazghyfia\n\n"),
	loop_successors(L, Pf, Pu, Q, Pf1, Pu1),
	write("azeazeazeaaaaaaaaaaaaaaaa\n\n"),
	put_flat(Pu1),
	write("azafazfzafza\n\n"),
	put_flat(Pf1).
	%Test S in Q
	%insert([[[a,b,c],[g,h,d],[f,vide,e]],[4,3,1],[[a,b,c],[g,h,x],[vide,f,e]],right],Q,Q1),
	%loop_successors(L, Pu, Pf, Q1, Pu1, Pf1),
	%put_flat(Pu1),
	%write("\n\n"),
	%put_flat(Pf1).
	% Test with S in Pu wiht F0 > F
	%insert([[[a,b,c],[g,h,d],[f,vide,e]],[5,2,1],[[a,b,c],[g,h,d],[vide,f,e]],left],Pu,Pu2),
	%insert([[5,2,1],[[a,b,c],[g,h,d],[f,vide,e]]],Pf,Pf2),
	%loop_successors(L, Pu2, Pf2, Q, Pu1, Pf1),
	%put_flat(Pu1),
	%write("\n\n"),
	%put_flat(Pf1).
	/*
	% Test with S in Pu wiht F0 < F
	 insert([[[a,b,c],[g,h,d],[f,vide,e]],[3,2,1],[[a,b,c],[g,h,d],[vide,f,e]],left],Pu,Pu2),
	 insert([[3,2,1],[[a,b,c],[g,h,d],[f,vide,e]]],Pf,Pf2),
	 loop_successors(L, Pu2, Pf2, Q, Pu1, Pf1),
	 put_flat(Pu1),
	 write("\n\n"),
	 put_flat(Pf1).
	*/