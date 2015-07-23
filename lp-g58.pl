%	GRUPO 58
%	78974	Eduardo Jorge Beirao Janicas
%	79197	Maria Filipa Goncalves Matilde Oliveira

/* ------------------------------------------ REPRESENTACAO EXTERNA -------------------------------------------- */


%%% transformacao/2
% transformacao(C1, C2) em que C1 e C2 sao configuracoes representadas por listas
transformacao([A, B, C, D, E, F, G, H, I], [J, K, L, M, N, O, P, Q, R]) :-
	    write('Transformacao desejada:'), nl, 
	    escreve(A), escreve(B), escreve(C), write('    '), escreve(J), escreve(K), escreve(L), nl, 
	    escreve(D), escreve(E), escreve(F), write(' -> '), escreve(M), escreve(N), escreve(O), nl,
	    escreve(G), escreve(H), escreve(I), write('    '), escreve(P), escreve(Q), escreve(R), nl.

%%% escreve/1 e um predicado auxiliar de transformacao/2
% A primeira regra permite escrever uma configuracao
escreve([A, B, C, D, E, F, G, H, I]) :- nl,
		escreve(A), escreve(B), escreve(C), nl,
	    escreve(D), escreve(E), escreve(F), nl,
	    escreve(G), escreve(H), escreve(I), nl, nl.

escreve(S) :- S = 0, write('   '), !.
escreve(S) :- S < 10, write(' '), write(S), write(' ').

%%% escreve_solucao/1
% escreve_solucao(M) em que M e uma lista de movimentos, um movimento e um par (D, Peca) e D uma direccao 
escreve_solucao([(D, P) | []]) :- 
		write('mova a peca '), write(P), traduz(D, Mp), write(Mp), write('.'), nl.

escreve_solucao([(D, P) | R]) :- 
		write('mova a peca '), write(P), traduz(D, Mp), write(Mp), nl, escreve_solucao(R).

%%% traduz/2 e um predicado auxiliar de escreve_solucao/1
traduz(c, ' para cima').
traduz(b, ' para baixo').
traduz(e, ' para a esquerda').
traduz(d, ' para a direita').

%%% pede_movimento/1, D - direccao de um movimento (cima, baixo, esquerda ou direita)
pede_movimento(D) :- 
		write('Qual o seu movimento?'), nl,
		read(D).

/* --------------------------------------------------- FUNCOES BASE --------------------------------------------- */

%%% indice/3
% indice(L, E, I) em que L e uma lista, E um elemento e I um indice. Usa o predicado auxiliar indice/4
indice(L, E, I) :- indice(L, E, 0, I).
indice([E|_], E, I, I).
indice([_|R], E, Ac, I) :- 
		Ac_ind is Ac + 1, 
		indice(R, E, Ac_ind, I).

%%% troca/4
% troca(LI, E1, E2, LF) em que LI e a lista inicial e E1 e E2 elementos. Devolve LF como LI onde os elementos foram trocados  
troca([], _, _, []).
troca([E1 | R], E1, E2, [E2 | F]) :- troca(R, E1, E2, F).
troca([E2 | R], E1, E2, [E1 | F]) :- troca(R, E1, E2, F).
troca([P | R], E1, E2, [P | F]) :- 
		P \== E1, P \== E2,
		troca(R, E1, E2, F).

%%% membro/2
% membro(E,L) que verifica se o elemento E e membro da lista L
membro(E, [E | _]).
membro(E, [_ | R]) :- membro(E, R).

%%% remove/3
% remove(L,E,F) que devolve uma lista F onde o elemento E foi removido da lista L
remove([P | R], P, R).
remove([P | R], N, [P | S]) :- remove(R, N, S). 

%%% inverte/2
% inverte(L, I) em que L e I sao listas. Usa o predicado auxiliar inverte/3 para calcular a lista I (invertida)
inverte(L, I) :- inverte(L, [], I).
inverte([], I, I).
inverte([P | R], Ac, I) :- inverte(R, [P | Ac], I).

%%% mov_legal/4
/* mov_legal(C1, D, P, C2) em que C1 e C2 sao configuracoes representadas por listas, 
D a direccao de um movimento um movimento e P uma peca.
as regras associadas a possibilidade de movimento sao as seguintes:
  		c -> Index - 3 ; P entre 3 e 8 ; 0 entre 0 e 5
  		d -> Index + 1 ; P != 2, 5, 8  ; 0 != 0, 3, 6
  		e -> Index - 1 ; P != 0, 3, 6  ; 0 != 2, 5, 8
  		b -> Index + 3 ; P entre 0 e 5 ; 0 entre 3 e 8 */
mov_legal(C1, c, P, C2) :- 
		indice(C1, P, I), I > 2, I < 9,
		indice(C1, 0, I0), I0 >= 0, I0 < 6, 
		I - 3 =:= I0,
		troca(C1, P, 0, C2).
mov_legal(C1, b, P, C2) :- 
		indice(C1, P, I), I >= 0, I < 6,
		indice(C1, 0, I0), I0 > 2, I0 < 9, 
		I + 3 =:= I0,
		troca(C1, P, 0, C2).
mov_legal(C1, e, P, C2) :- 
		indice(C1, P, I), I =\= 0, I =\= 3, I =\= 6,
		indice(C1, 0, I0), I0 =\= 2, I0 =\= 5, I0 =\= 8, 
		I - 1 =:= I0,
		troca(C1, P, 0, C2).
mov_legal(C1, d, P, C2) :- 
		indice(C1, P, I), I =\= 2, I =\= 5, I =\= 8,
		indice(C1, 0, I0), I0 =\= 0, I0 =\= 3, I0 =\= 6,
		I + 1 =:= I0,
		troca(C1, P, 0, C2).


/* ---------------------------------------------- RESOLVE MANUAL ----------------------------------------------- */

%%%resolve_manual/2
/* resolve_manual(Conf_Ini, Conf_Desejada) em que as 2 configuracoes sao representadas por listas.
Usa o predicado auxiliar resolve_manual_ciclo/3 para entrar em ciclo durante o jogo */
resolve_manual(Conf_Ini, Conf_Desejada) :- 
		transformacao_possivel(Conf_Ini, Conf_Desejada),
		transformacao(Conf_Ini, Conf_Desejada),
		pede_movimento(D), 
		resolve_manual_ciclo(Conf_Ini, Conf_Desejada, D).

%%%resolve_manual_ciclo/3
% Caso de paragem
resolve_manual_ciclo(Conf_Ini, Conf_Desejada, D) :- 
		mov_legal(Conf_Ini, D, _, Conf_Desejada),
		escreve(Conf_Desejada),
		write('Parabens!'), nl.	
% Caso de movimento ilegal
resolve_manual_ciclo(Conf_Ini, Conf_Desejada, D) :- 
		\+(mov_legal(Conf_Ini, D, _, _)),
		write('Movimento ilegal'), nl,
		pede_movimento(D2),  
		resolve_manual_ciclo(Conf_Ini, Conf_Desejada, D2).					  
% Caso geral
resolve_manual_ciclo(Conf_Ini, Conf_Desejada, D) :-  
		mov_legal(Conf_Ini, D, _, C2),
		escreve(C2),
		pede_movimento(D2), 
		resolve_manual_ciclo(C2, Conf_Desejada, D2).


/* ---------------------------------------------- RESOLVE CEGO ------------------------------------------------- */

%%%resolve_cego/2
/* resolve_cego(Conf_Ini, Conf_Desejada) em que as 2 configuracoes sao representadas por listas.
Usa o predicado auxiliar resolve_cego_ciclo/3 para entrar em ciclo durante o jogo */
resolve_cego(Conf_Ini, Conf_Desejada) :- 
		transformacao_possivel(Conf_Ini, Conf_Desejada),
		transformacao(Conf_Ini, Conf_Desejada),
		resolve_cego_ciclo(Conf_Ini, Conf_Desejada, _, [Conf_Ini], []).

%%%resolve_cego_ciclo/3, LM - lista de movimentos efectuados
% Caso de paragem
resolve_cego_ciclo(Conf_Desejada, Conf_Desejada, _, _, LM) :- 
		inverte(LM, LM_final),
		escreve_solucao(LM_final), !.
% Caso geral
resolve_cego_ciclo(Conf_Ini, Conf_Desejada, D, L, LM) :-
		mov_legal(Conf_Ini, D, P, CF),
		\+(membro(CF, L)),					
		resolve_cego_ciclo(CF, Conf_Desejada, _, [CF | L], [(D, P) | LM]), !.

/* -------------------------------------------- RESOLVE INFORMADO ---------------------------------------------- */

cria_no(C, F, G, H, M, no(C, F, G, H, M)).

configuracao_de(no(C, _, _, _, _), C).
f_de(no(_, F, _, _, _), F).
g_de(no(_, _, G, _, _), G).
h_de(no(_, _, _, H, _), H).
movimentos_de(no(_, _, _, _, M), M).

/* C - Configuracao
   F - Valor funcao f
   G - Valor funcao g
   H - Valor funcao h
   M - Movimentos a partir da inicial */

%%% dist_Hamming/3
/* dist_Hamming(Conf_Atual, Conf_Desejada, H) em que as 2 configuracoes sao representadas por listas.
Usa o predicado auxiliar dist_Hamming/4 para calcular H (Distancia de Hamming) */
dist_Hamming(Conf_Atual, Conf_Desejada, H) :- dist_Hamming(Conf_Atual, Conf_Desejada, 0, H).
dist_Hamming([], [], H, H).
dist_Hamming([P | R_Atual], [P | R_Final], Acu_Hamming, H) :- 
		dist_Hamming(R_Atual, R_Final, Acu_Hamming, H), !.
dist_Hamming([P_Atual | R_Atual], [P_Final | R_Final], Acu_Hamming, H) :- 
		P_Atual \== P_Final,
		Acu_Hamming_aux is Acu_Hamming + 1,
		dist_Hamming(R_Atual, R_Final, Acu_Hamming_aux, H).


%%% sucessores_no/3 e um predicado auxiliar de expande_no/5
% sucessores_no(N, L, CF) em que N e um no e L uma lista. Usa o predicado auxiliar sucessores_no/4 para devolver uma lista CF de sucessores
sucessores_no(N, L, CF) :- sucessores_no(N, L, [], CF).
sucessores_no(N, L, L_Aux, CF) :- 
		configuracao_de(N, C),
		movimentos_de(N, M),
		g_de(N, G),
		mov_legal(C, D, P, C2),
		dist_Hamming(C2, CF, H),
		G_new is G + 1,
		F is G_new + H,
		cria_no(C2, F, G_new, H, [(D, P) | M], N_New),
		\+ membro(N_New, L_Aux),
		sucessores_no(N, L, [N_New | L_Aux], CF), !.
sucessores_no(_, L, L, _).

%%% adiciona_no_abertos/4 e um predicado auxiliar de expande_no/5
/* adiciona_no_abertos(L, Ab, Fec, Ab_fin) em que L e uma lista de nos, Ab a lista de abertos e Fec de fechados. 
Usa o predicado auxiliar adiciona_no_abertos/4 para devolver a lista Ab_fim de nos abertos, nao repitidos */
adiciona_no_abertos(L, Ab, Fec, Ab_fin) :- adiciona_no_abertos(L, Ab, Fec, Ab, Ab_fin).
adiciona_no_abertos([], _, _, Ab_fin, Ab_fin).
adiciona_no_abertos([P | R], Ab, Fec, Ab_aux, Ab_fin) :- 
		\+membro(P, Ab),
		\+membro(P, Fec),
		adiciona_no_abertos(R, Ab, Fec, [P | Ab_aux], Ab_fin).


%%% expande_no/5 e um predicado auxiliar de resolve_info_h/2
% expande_no(N, Ab, Fec, Ab_fin, CF) em que N e uma lista de nos, Ab de abertos e Fec a de fechados, e adiciona a Ab o menor dos sucessores
expande_no(N, Ab, Fec, Ab_fin, CF) :- 
		sucessores_no(N, L, CF),
		adiciona_no_abertos(L, Ab, Fec, Ab_fin).

%%% no_menor_f/2 e um predicado auxiliar de resolve_info_h/2
% no_menor_f(L, N) em que L e uma lista de nos e N um no usa o predicado auxiliar no_menor_f/3 para encontrar o no com menor valor de f
no_menor_f([P | R], N) :- no_menor_f(R, P, N).
no_menor_f([], N, N).
no_menor_f([P | R], Menor_prov, N) :- 
		f_de(Menor_prov, FA),
		f_de(P, FP),
		FA < FP,
		no_menor_f(R, Menor_prov, N).
no_menor_f([P | R], Menor_prov, N) :- 
		f_de(Menor_prov, FA),
		f_de(P, FP),
		FA >= FP,
		no_menor_f(R, P, N).


%%%resolve_info_h/2
/* resolve_info_h(Conf_Ini, Conf_Desejada) em que as 2 configuracoes sao representadas por listas.
Usa o predicado auxiliar resolve_info_h_ciclo/3 para entrar em ciclo durante o jogo */
resolve_info_h(Conf_Ini, Conf_Desejada) :- 
		transformacao_possivel(Conf_Ini, Conf_Desejada),
		transformacao(Conf_Ini, Conf_Desejada),
		dist_Hamming(Conf_Ini, Conf_Desejada, H),
		resolve_info_h_ciclo(Conf_Ini, Conf_Desejada, [no(Conf_Ini, H, 0, H, [])], []).

%%%resolve_cego_ciclo/3
% Caso de paragem
resolve_info_h_ciclo(_, CF, Ab, _) :- 
		no_menor_f(Ab, N),
		configuracao_de(N,CA),
		CA == CF,
		movimentos_de(N,M),
		inverte(M, M_final),
		escreve_solucao(M_final).
% Caso geral
resolve_info_h_ciclo(_, CF, Ab, Fec) :- 
		no_menor_f(Ab, N),
		configuracao_de(N,CA),
		CA \== CF,
		remove(Ab, N, Ab_aux),
		expande_no(N, Ab_aux, [N | Fec], Ab_fin, CF),
		resolve_info_h_ciclo(CA, CF, Ab_fin, [N | Fec]), !.


/* ------------------------------------------------ RESOLUBILIDADE --------------------------------------------- */

%%%transformacao_possivel/2
/* transformacao_possivel(Conf_Ini, Conf_Desejada) em que as 2 configuracoes sao representadas por listas.
Calcula o numero de inversoes na configuracao inicial vs final (com os 0 removidos). Caso o resultado seja impar, nao e resoluvel */
% Caso resoluvel
transformacao_possivel(Conf_Ini, Conf_Desejada) :- 
		remove(Conf_Ini, 0, Conf_Ini_aux),
		remove(Conf_Desejada, 0, Conf_Desejada_aux),
		num_inversoes(Conf_Ini_aux, Conf_Desejada_aux, Num),
		mod(Num, 2) =:= 0. 

%%%num_inversoes/3
/* num_inversies(Conf_Ini, Conf_desejada, Num) em que as 2 configuracoes sao representadas por listas sem 0.
Usa o predicado auxiliar num_inversoes/4 para calcular o num_inversoes.
Considera-se uma inversao quando um numero precede outro que, segundo a configuracao desejada, nao devia preceder. */
num_inversoes(Conf_Ini, Conf_Desejada, Num) :- num_inversoes(Conf_Ini, Conf_Desejada, 0, Num).
num_inversoes([], [], Num, Num).
num_inversoes([P_Ini | Res_Ini], Conf_Desejada, Ac, Num) :- 
		percorre_ate(P_Ini, Conf_Desejada, Conf_Desejada_Aux, Num_parcial),
		Ac_aux is Ac + Num_parcial,
		num_inversoes(Res_Ini, Conf_Desejada_Aux, Ac_aux, Num).

%%%percorre_ate/4
/* percorre_ate(P, Conf_Desejada, Conf_Desejada_final, Num_parcial) em que P e uma peca e as 2 configuracoes sao representadas por listas sem 0.
Num_parcial sera o numero de inversoes realitvas a P. Usa o predicado auxiliar percorre_ate/5 para calcular o numero de inversoes da peca.
Considera-se uma inversao quando um numero precede outro que, segundo a configuracao desejada, nao devia preceder. Apos tratar esse numero, 
elimina-se da lista de elementos que estamos a avaliar */
percorre_ate(P, Conf_Desejada, Conf_Desejada_final, Num_parcial) :- 
		percorre_ate(P, Conf_Desejada, Conf_Desejada, 0, Num_parcial),
		remove(Conf_Desejada, P, Conf_Desejada_final).
percorre_ate(P, [P|_], _, Num_parcial, Num_parcial) :- !.
percorre_ate(P, [P_Desejada | R_Desejada], Conf_Desejada_Aux, Ac, Num_parcial) :- 
		P_Desejada \== P,
		Ac_aux is Ac + 1,
		percorre_ate(P, R_Desejada, Conf_Desejada_Aux, Ac_aux, Num_parcial).




