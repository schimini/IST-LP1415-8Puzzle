%------------------------------------------------------------%
%                                                            %
%            Logica para a Programacao-2014/15               %
%               O puzzle de 8 - grupo 107                    %
%                                                            %
%                  81338 - Pedro Cerejo                      %
%                  81700 - Sara Azinhal	                     %
%------------------------------------------------------------%


%------------------------------------------------------------%
%		             FUNCOES PRINCIPAIS                          %
%------------------------------------------------------------%

% resolve_manual/2 - resolve_manual(C1,C2) recebe uma configuracao inicial (C1) 
% e dada uma serie de comandos introduzidos pelo utilizador,
% deve obter-se a configuracao final (C2) 

resolve_manual(C1,C2) :- objetivo_inicial(C1,C2), resolve_manual(C1,C2,_), writeln('Parabens!').
resolve_manual(C,C,_):-!.
resolve_manual(C1,C2,_) :- leitura_mov(C1,Res),nl,tabuleiro(Res),nl,resolve_manual(Res,C2,_).


% resolve_cego/2 - resolve_cego(C1,C2) recebe uma configuracao inicial (C1) 
% e dada uma serie de movimentacoes efetuadas pelo programa e que seguem um
% conjunto de regras, deve obter-se a configuracao final (C2) e a sequencia de movimentos usados.

resolve_cego(C1,C2):- objetivo_inicial(C1,C2),resolve_cego(C1,C2,[C1],[]),!.
resolve_cego(L,L,_,Jogadas) :- inverte(Jogadas,Jogadas1),imprime_jogadas(Jogadas1).
resolve_cego(L,C2,Configuracoes,Jogadas) :- mov_legal(L,M,P,Res),\+ membro(Res,Configuracoes),!,
							(resolve_cego(Res,C2,[Res|Configuracoes],[[M,P]|Jogadas]);
							resolve_cego(L,C2,[Res|Configuracoes],Jogadas)).


% resolve_info_h/2 - resolve_info_h(C1,C2) recebe uma configuracao inicial (C1) 
% e segundo o algoritmo A*, utilizando a heuristica de hamming,
% deve obter a configuracao final (C2) e a sequencia de movimentos usados. 

resolve_info_h(C1,C2) :- objetivo_inicial(C1,C2),hamming(C1,C2,F),resolve_info_h(C1,C2,[no(C1,F,0,F,[])],[]).
resolve_info_h(C1,C2,Abertos,Fechados) :- seleciona_no(Abertos,No),(tira_c(No,C),C \== C2,!,
							tira_no(Abertos,No,Abertos1),
							Fechados1=[No|Fechados],
							atualiza_abertos(No,C2,Abertos1,Fechados1,Abertos2),
							resolve_info_h(C1,C2,Abertos2,Fechados1)
							;tira_m(No,M),inverte(M,M1),imprime_jogadas(M1)).


%------------------------------------------------------------%
%		           CONFIGURACAO POSSIVEL                         %
%------------------------------------------------------------%

% configuracao_possivel/2 - configuracao possivel(C1,C2) devolve verdadeiro
% apenas se for possivel transformar a configuracao C1 na configuracao C2

configuracao_possivel(C1,C2):-configuracao_possivel([1,2,3,4,5,6,7,8],C1,C2,Res),Res mod 2 =:= 0.
configuracao_possivel(L,C1,C2,Res):-configuracao_possivel(L,C1,C2,Res,0).
configuracao_possivel([],_,_,_,Res,Res):-!.
configuracao_possivel([X|R],C1,C2,Res,Ac):-verifica_numero(X,R,C1,C2,N),Ac1 is Ac + N,configuracao_possivel(R,C1,C2,Res,Ac1).


%------------------------------------------------------------%
%          PREDICADOS ACESSORIOS AO RESOLVE MANUAL           %
%------------------------------------------------------------%

% leitura_mov/2 - leitura_mov(L,C) recebe uma configuracao (L) e atraves do comando introduzido
% pelo utilizador, se este for legal, devolve a configuracao (C) obtida desse movimento 

leitura_mov(L,C) :- writeln('Qual o seu movimento?'), read(J), mov_legal(L,J,_,C),!; 
							(writeln('Movimento ilegal'), leitura_mov(L,C) ).

							
%------------------------------------------------------------%
%         PREDICADOS ACESSORIOS AO RESOLVE INFORMADA         %
%------------------------------------------------------------%



% hamming/3 - hamming(C1,C2,H) recebe uma configuracao (C1) e a configuracao final (C2) 
% e utilizando a heuristica de hamming, numero de quadrados fora da posicao,
% devolve o valor da funcao h (H), distancia de hamming

hamming(C1,C2,H) :- hamming(C1,C2,0,H).
hamming([],[],H,H).
hamming([E1|R1],[E2|R2],H,Res) :- E1 =\= E2,E1=\=0,!, H1 is H + 1,hamming(R1,R2,H1,Res);hamming(R1,R2,H,Res).


% seleciona_no/2 - seleciona_no(L,No) recebe uma lista de nos (L)
% e devolve o no (No) com menor valor da funcao f

seleciona_no([E|R],No) :- tira_f(E,F),seleciona_no(R,F,E,No).
seleciona_no([],_,No,No).
seleciona_no([E|R],Menor,Ac,No) :- tira_f(E,F), Menor>F,!, seleciona_no(R,F,E,No);
							seleciona_no(R,Menor,Ac,No).


% atualiza_abertos/5 - atualiza_abertos(No,C2,Abertos,Fechados,Abertos1)
% recebe um no (No), coloca na lista de nos abertos (Abertos) todos os seus sucessores
% que nao se encontrem nem na lista de abertos nem na lista de fechados (Fechados)
% e devolve a lista de abertos atualizada (Abertos1)

atualiza_abertos(No,C2,Abertos,Fechados,Abertos1) :- expande(No,C2,Lista_nos), 
							mete_nos(Abertos,Lista_nos,Fechados,Abertos1).


% tira_no/3 - tira_no(Aberta,No,Nova_aberta) recebe uma lista de nos (Aberta)
% e um no (No), remove esse no da lista e devolve a lista resultante (Nova_aberta)

tira_no(Aberta,No,Nova_aberta):- tira_no(Aberta,No,Nova_aberta,[]).
tira_no([],_,Nova_aberta,Nova_aberta):-!.
tira_no([E|R],No,Nova_aberta,Ac):- E\==No,!,tira_no(R,No,Nova_aberta,[E|Ac]);tira_no(R,No,Nova_aberta,Ac).													   


% expande/3 - expande(No,C2,Lista_nos) recebe um no (No), expande esse no
% e devolve uma lista das expansoes desse no (Lista_nos)

expande(No,C2,Lista_nos):-expande(No,C2,Lista_nos,[],[c,b,d,e]).
expande(_,_,Lista_nos,Lista_nos,[]):-!.
expande(No,C2,Lista_nos,Ac,[E|R]) :- tira_c(No,C),tira_g(No,G),mov_legal(C,E,P,Res),G1 is G+1,tira_m(No,M),
							hamming(Res,C2,H),F is H+G1,No_res = no(Res,F,G1,H,[[E,P]|M]),!,
							expande(No,C2,Lista_nos,[No_res|Ac],R);
							expande(No,C2,Lista_nos,Ac,R).	


% mete_nos/4 - mete_nos(Aberta,Nos,Fechada,Nova_aberta) recebe 
% uma lista de abertos (Aberta), uma lista de nos (Nos) e uma lista de fechados (Fechada), 
% coloca os nos da lista Nos, que nao se encontrem nem na lista de abertos nem na lista de
% fechados, na lista de abertos e devolve a lista resultante (Nova_Aberta) 

mete_nos(Aberta,Nos,Fechada,Nova_aberta):- mete_nos(Aberta,Nos,Fechada,Nova_aberta,Aberta).
mete_nos(_,[],_,Nova_aberta,Nova_aberta):-!.
mete_nos(Aberta,[E|R],Fechada,Nova_aberta,Ac) :- \+ membro(E,Aberta),\+ membro(E,Fechada),!,Ac1=[E|Ac],
							mete_nos(Aberta,R,Fechada,Nova_aberta,Ac1); 
							mete_nos(Aberta,R,Fechada,Nova_aberta,Ac).	



% no_<prop>/2 - no_<prop>(No,X) afirma que X e a propriedade <prop> de No.

tira_c(no(C,_,_,_,_),C).
tira_f(no(_,F,_,_,_),F).
tira_g(no(_,_,G,_,_),G).
tira_m(no(_,_,_,_,M),M).


%------------------------------------------------------------%
%       PREDICADOS ACESSORIOS AO CONFIGURACAO POSSIVEL       %
%------------------------------------------------------------%

% verifica_numero/5 - verifica_numero(X,L,C1,C2,N) devolve todas as inversoes do elemento X 
% face aos elementos da lista L a partir dos indices de X e Y nas configuracoes C1 e C2

verifica_numero(X,L,C1,C2,N):-verifica_numero(X,L,C1,C2,N,0).
verifica_numero(_,[],_,_,N,N):-!.
verifica_numero(X,[Y|R],C1,C2,N,Z):- elem(C1,X,A1),elem(C1,Y,A2),elem(C2,X,B1),elem(C2,Y,B2),
							(A1>A2,B1<B2;A1<A2,B1>B2),!,Z1 is Z+1,
							verifica_numero(X,R,C1,C2,N,Z1);
							verifica_numero(X,R,C1,C2,N,Z).


%------------------------------------------------------------%
%                   PREDICADOS DE ESCRITA                    %
%------------------------------------------------------------%

% objetivo_inicial/2 - objetivo_inicial(I,F) escreve a transformacao desejada do puzzle,
% ou seja, a representacao da configuracao inicial (I) e da configuracao final (F)

objetivo_inicial(I,F) :- writeln('Transformacao desejada:'), objetivo_inicial(I,F,0).
objetivo_inicial([],[],_).
objetivo_inicial([E1,E2,E3|R1],[F1,F2,F3|R2],A) :- escreve_linha([E1,E2,E3]),
						(A =:= 1,!,
						write(' -> ');
						write('    ')),
							escreve_linha([F1,F2,F3]),nl,
							A1 is A+1,
							objetivo_inicial(R1,R2,A1).


% tabuleiro/1 - tabuleiro(C) recebe uma configuracao (C) e escreve a sua representacao 

tabuleiro([]).
tabuleiro([E1,E2,E3|R]) :- escreve_linha([E1,E2,E3]), nl, tabuleiro(R).


% escreve_linha/1 - escreve_linha(L) recebe uma lista L e escreve-a, de acordo com
% a representacao do puzzle, ou seja, o zero e o equivalente a um espaco em branco

escreve_linha([]).
escreve_linha([E|R]) :- write(' '),
			(E =:= 0,!,
			write(' ');
			write(E)),
				write(' '),escreve_linha(R).


% imprime_jogadas/1 - imprime_jogadas(L) recebe uma lista de jogadas (L) e escreve-las,
% lista que dividida aos pares contem os movimentos para as repetivas pecas

imprime_jogadas([[M,P]|R]):- write('mova a peca '),write(P),write(' para '),
							atribui_letra(M,Texto),write(Texto),(R==[],!,
							writeln('.');nl,imprime_jogadas(R)). 


% atribui_letra/2 - atribui_letra(M,Texto) recebe uma inicial (M)
% e devolve o movimento correspondente (Texto)

atribui_letra(M,Texto) :-((M==c,Texto = cima);
			(M==b,Texto = baixo);
			(M==d,Texto = 'a direita');
			(M==e,Texto = 'a esquerda')),!.


%------------------------------------------------------------%
%                 VERIFICACAO DE MOVIMENTOS                  %
%------------------------------------------------------------%

% mov_legal/4 - mov_legal(L,M,P,Res) afirma que a configuracao (Res) e obtida
% da configuracao (C1), fazendo o movimento (M), com a peca (P) 

mov_legal(L,M,P,Res):- elem(L,0,I),((I//3) =\= 2, M = c, Pos is I+3;
				(I//3) =\= 0, M = b, Pos is I-3;
				(I mod 3) =\= 2, M = e, Pos is I+1;
				(I mod 3) =\= 0, M = d, Pos is I-1),
					elem(L,P,Pos),troca_el(L,P,Res).


%------------------------------------------------------------%
%                          AUXILIARES                        %
%------------------------------------------------------------%

% membro/2 - membro(E,L) devolve verdadeiro se o elemento (E) esta contido na lista (L) 

membro(E,[E|_]):-!.
membro(E,[_|R]):- membro(E,R).


% elem/3 - elem(L,P,I) recebe uma lista (L) e um elemento/peca (P), associa um indice
% a posicao do elemento na lista e devolve esse indice (I)

elem(L,P,I):-elem(L,P,I,0).
elem([P|_],P,I,I):-!.
elem([_|R],P,I,Ac):- Ac1 is Ac + 1, elem(R,P,I,Ac1).


% troca_el/3 - troca_el(L1,P,L2) recebe uma lista (L1) e um elemento/peca (P)
% e devolve a lista resultante da troca do zero da lista pelo elemento e vice-versa (L2)

troca_el(L1,P,L2) :- troca_el(L1,P,L2,[]).
troca_el([],_,Res,Inv) :- inverte(Inv,Res).
troca_el([E|R],P,Res,Aux) :- E =:= P,!, troca_el(R,P,Res,[0|Aux]).
troca_el([E|R],P,Res,Aux) :- E =:= 0,!, troca_el(R,P,Res,[P|Aux]).
troca_el([E|R],P,Res,Aux) :- troca_el(R,P,Res,[E|Aux]).


% inverte/2 - inverte(L1,L2) recebe uma lista (L1) e devolve essa lista invertida (L2)

inverte(L1,L2) :- inverte(L1,L2,[]).
inverte([],Res,Res).
inverte([E|R],L2,Res) :- inverte(R,L2,[E|Res]).
