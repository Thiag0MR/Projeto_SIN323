:- use_module(library(lists)).

% Define os fatos que são dinâmicos, ou seja, podem ser removidos e adicionados durante a execução do programa
:- dynamic([
    posicao_agente/1,
    posicao_coracoes/1,
    posicao_garrafas/1,
    posicao_elevadores/1,
    posicao_brutus/1,
    posicao_espinafre/1,
    estados_visitados/1,
    pontuacao/1,
    objetivo/1,
    caminho_percorrido/1
    ]).

% Função inicial, usada para iniciar o jogo. Os parâmetros são as posições dos elementos no tabuleiro.
% Passar todas as posições dentro de listas (Ex: [[0,0], [1,1]]). Todas conjuntos de posições devem possuir como interseção
% o conjunto vazio. Não foi feito verificações quanto as posições passadas.

% Exemplo de ambiente inválido:
% iniciar([[4,0]],[[0,2],[1,0],[2,4],[3,9]],[[1,3],[2,5],[3,2],[4,2]],[[0,1],[0,5],[1,8],[2,1],[4,4],[4,5]],[[2,7]],[[4,1]]).

% Exemplo de ambiente válido:
% iniciar([[4,0]],[[0,2],[1,0],[2,4],[3,9]],[[1,3],[2,5],[3,2],[4,1]],[[0,1],[0,5],[1,8],[2,1],[4,4],[4,5]],[[2,7]],[[0,9]]).

iniciar([Agente], Coracoes, Elevadores, Garrafas, Espinafre, Brutus) :-
    adicinar_fatos(Agente, Coracoes, Elevadores, Garrafas, Espinafre, Brutus),
    imprimir_tabuleiro,!, % Se a busca falhar ele não tenta refazer a meta anterior
    iniciar_busca.

iniciar_busca :- 
    posicao_coracoes(Coracoes),
    definir_objetivo(Coracoes),
    coletar_coracoes,
    posicao_espinafre(Espinafre),
    definir_objetivo(Espinafre),!,
    coletar_espinafre,
    posicao_brutus(Brutus),
    definir_objetivo(Brutus),!,
    derrotar_brutus,
    imprimir_informacoes_finais.
    
coletar_coracoes :- 
    objetivo([]),
    format("Todos os corações foram coletados.~n"),
    % Conseguiu coletar todos os corações, logo não existe mais corações para imprimir
    retract(posicao_coracoes(_)), 
    asserta(posicao_coracoes([])).

coletar_coracoes :- 
    posicao_agente(Agente),
    (busca_em_largura(Agente,[EstadoObjetivo|Caminho]) ->
        reverse([EstadoObjetivo|Caminho],Solucao),
        imprimir_caminho(Solucao),
        atualizar_posicao_coracoes(EstadoObjetivo,NovoConjuntoCoracoes),
        atualizar_pontuacao(EstadoObjetivo),
        definir_objetivo(NovoConjuntoCoracoes),
        format("Coração na posição ~w coletado.~n",[EstadoObjetivo]),
        coletar_coracoes();
        objetivo(Obj),
        format("Falha ao buscar os corações nas seguintes posições: ~w~n",[Obj]),
        imprimir_informacoes_finais,
        fail).

coletar_espinafre :-
    posicao_agente(Agente),
    (busca_em_largura(Agente,[EstadoObjetivo|Caminho]) ->
        reverse([EstadoObjetivo|Caminho],Solucao),
        imprimir_caminho(Solucao),
        retract(posicao_espinafre(_)),
        asserta(posicao_espinafre([])),
        format("Espinafre na posição ~w coletado~n",[EstadoObjetivo]);
        objetivo(Obj),
        format("Falha ao buscar o espinafre na seguinte posição: ~w~n",[Obj]),
        imprimir_informacoes_finais,
        fail).

derrotar_brutus :- 
    posicao_agente(Agente),
    (busca_em_largura(Agente,[EstadoObjetivo|Caminho]) ->
        reverse([EstadoObjetivo|Caminho],Solucao),
        imprimir_caminho(Solucao),
        format("Brutus na posição ~w foi derrotado~n",[EstadoObjetivo]);
        objetivo(Obj),
        format("Falha ao buscar o brutus na seguinte posição: ~w~n",[Obj]),
        imprimir_informacoes_finais,
        fail).


%****************************
%*      Busca em largura    *
%****************************

busca_em_largura(Inicial,CaminhoEncontrado) :- bl([[Inicial]],[],CaminhoEncontrado).

bl([[Estado|Caminho]|_],ListaExplorados,[Estado|Caminho]) :- 
    objetivo(Objetivo),
    meta(Estado,Objetivo), 
    % Cria o caminho percorrido
    retract(caminho_percorrido(C)),
    append(Caminho,C,C1),
    asserta(caminho_percorrido(C1)).

bl([[Estado|Caminho]|Outros],ListaExplorados,CaminhoEncontrado) :- 
    sucessores(Estado,ListaSucessores),
    findall(No,(member(No,ListaSucessores),
        not(member(No,ListaExplorados)),
        not(pertence_fronteira(No,Outros))),Nos),
    estender_caminho(Nos,[Estado|Caminho],Caminhos),
    append(Outros,Caminhos,NovaFronteira),
    bl(NovaFronteira,[Estado|ListaExplorados],CaminhoEncontrado).


% Quando o prolog não conseguir unificar com as duas regras acima (quando o lista de fronteira estiver vazia, ou seja, 
% quando nenhum estado for o objetivo), ele vai unificar com essa regra, parando a recursão e retornando false.

bl([],_,_) :- fail.
    
sucessores(Estado,ListaSucessores) :-
    findall(Sucessor,sucessor(Estado,Sucessor),ListaSucessores).

estender_caminho([],_,[]).
estender_caminho([Node|Nodes],Path,[[Node|Path]|Extended]) :-
    estender_caminho(Nodes,Path,Extended).

meta(Estado,Objetivo) :- member(Estado,Objetivo).

pertence_fronteira(Elem,[[Elem|_]|_]).
pertence_fronteira(Elem,[_|Cauda]) :- pertence_fronteira(Elem,Cauda).


%****************************
%*        Condições         *
%****************************

% Tamanho tabuleiro
numeroLinhas(5).
numeroColunas(10).

% Verifica se [Linha,Coluna] está dentro do limite do tabuleiro, lembrando que os índices do tabuleiro começam em zero
dentro_tabuleiro([Linha,Coluna]) :- numeroLinhas(L) , numeroColunas(C),
    Linha >= 0 , Linha < L , Coluna >= 0 , Coluna < C.

% Verifica se a posição [Linha,Coluna] tem um elevador
tem_elevador([Linha,Coluna]) :-posicao_elevadores(X),member([Linha,Coluna],X).

% Verifica se a posição [Linha,Coluna] tem uma garrafa
tem_garrafa([Linha,Coluna]) :- posicao_garrafas(G),member([Linha,Coluna],G).

% Verifica se a posição [Linha,Coluna] tem o brutus 
tem_brutus([Linha,Coluna]) :- posicao_brutus(B),member([Linha,Coluna],B).

% Verifica se a posição [Linha,Coluna] tem um espinafre
tem_espinafre([Linha,Coluna]) :- posicao_espinafre(F),member([Linha,Coluna],F).

% Verifica se a posição [Linha,Coluna] é um obstáculo, lembrando que em determinado momento do jogo, o brutus passa a ser 
% um objetivo e não um obstáculo, por isso verifica-se se a posição do brutus não pertence ao objetivo 
tem_obstaculo([Linha,Coluna]) :-
    objetivo(Objetivo),
    (tem_garrafa([Linha,Coluna]);
    (tem_brutus([Linha,Coluna]),not(meta([Linha,Coluna],Objetivo)))).

% Verifica se agente pode subir ou descer e se a posição destino não é um obstáculo
posicao_diagonal_livre(ElevadorLinha,ElevadorColuna,DiagonalLinha,DiagonalColuna) :-
    tem_elevador([ElevadorLinha,ElevadorColuna]), 
    dentro_tabuleiro([DiagonalLinha,DiagonalColuna]),
    not(tem_obstaculo([DiagonalLinha,DiagonalColuna])),!.

% Retorna a posição adjacente livre [Linha,C]
% O FatorMultiplicador é 1 ou -1, usado para inverter o sinal, aumentando as coordenadas (indo para a direita) ou 
% diminuindo as coordenadas (indo para a esquerda)
obter_posicao_adjacente_livre([Linha,Coluna],FatorMultiplicador,[Linha,C]) :-
    C1 is (Coluna + (1 * FatorMultiplicador)),
    C2 is (Coluna + (2 * FatorMultiplicador)),
    ((not(tem_obstaculo([Linha,C1])), C is C1); % verifica se a posição adjacente é uma obstáculo, se não for, logo C = C1. Como
    % é um OU ele não tenta a próxima 
    (not(tem_brutus([Linha,C1])),not(tem_obstaculo([Linha,C2])), C is C2)), % se a primeira regra do OU falha é porque a posição é um obstáculo, logo 
    % verificamos se a posição adjacente ao obstáculo é outro obstáculo, caso seja não é possível pular
    dentro_tabuleiro([Linha,C]),!. % O corte faz o prolog executar a regra obter_posicao_adjacente_livre somente uma vez. Se o
    % primeiro OU é verdade, ele irá executar o obter_posicao_adjacente_livre de novo tentando o segundo OU, e não queremos isso.
    

% Direita
sucessor([Linha,Coluna],[Linha,C]) :- 
    obter_posicao_adjacente_livre([Linha,Coluna],1,[Linha,C]).

% Esquerda
sucessor([Linha,Coluna],[Linha,C]) :- 
    obter_posicao_adjacente_livre([Linha,Coluna],-1,[Linha,C]).

% Para cima direita
sucessor([Linha,Coluna],[LinhaMenos1,ColunaMais1]) :-
    ColunaMais1 is Coluna+1,
    LinhaMenos1 is Linha-1,
    posicao_diagonal_livre(Linha,ColunaMais1,LinhaMenos1,ColunaMais1).

% Para cima esquerda
sucessor([Linha,Coluna],[LinhaMenos1,ColunaMenos1]) :-
    LinhaMenos1 is Linha-1,
    ColunaMenos1 is Coluna-1,
    posicao_diagonal_livre(Linha,ColunaMenos1,LinhaMenos1,ColunaMenos1).

% Para baixo direita
sucessor([Linha,Coluna],[LinhaMais1,ColunaMais1]) :-
    LinhaMais1 is Linha+1,
    ColunaMais1 is Coluna+1,
    posicao_diagonal_livre(LinhaMais1,Coluna,LinhaMais1,ColunaMais1).

% Para baixo esquerda
sucessor([Linha,Coluna],[LinhaMais1,ColunaMenos1]) :-
    LinhaMais1 is Linha+1,
    ColunaMenos1 is Coluna-1,
    posicao_diagonal_livre(LinhaMais1,Coluna,LinhaMais1,ColunaMenos1).

%****************************************************
%*      Relacionado a adição e remoção de fatos     *
%****************************************************

definir_objetivo(Objetivo) :-
    retract(objetivo(_)),
    asserta(objetivo(Objetivo)).

atualizar_pontuacao([Linha,_]) :-
    retract(pontuacao(PontuacaoAtual)),
    NovaPontuacao is (PontuacaoAtual + (500 - (Linha * 100 ))),
    asserta(pontuacao(NovaPontuacao)).

atualizar_posicao_coracoes(Objetivo,NovoConjuntoCoracoes) :-
    posicao_coracoes(Coracoes),
    delete(Coracoes,Objetivo,NovoConjuntoCoracoes),
    retract(posicao_coracoes(_)),
    asserta(posicao_coracoes(NovoConjuntoCoracoes)).

add_estado_vizitado(Estado) :- 
    retract(estados_visitados(Visitados)),
    asserta(estados_visitados([Estado|Visitados])).

mover_agente(Posicao) :-
    retract(posicao_agente(_)),
    asserta(posicao_agente(Posicao)).

adicinar_fatos(Agente, Coracoes, Elevadores, Garrafas, Espinafre, Brutus) :-
    retract_list(
        [posicao_agente(_),
        posicao_coracoes(_),
        posicao_elevadores(_),
        posicao_garrafas(_),
        posicao_espinafre(_),
        posicao_brutus(_),
        estados_visitados(_),
        pontuacao(_),
        objetivo(_),
        caminho_percorrido(_)]),
    assert_list(
        [posicao_agente(Agente),
        posicao_coracoes(Coracoes),
        posicao_elevadores(Elevadores),
        posicao_garrafas(Garrafas),
        posicao_espinafre(Espinafre),
        posicao_brutus(Brutus),
        estados_visitados([]),
        pontuacao(0),
        objetivo([]),
        caminho_percorrido([])]).

assert_list([]).
assert_list([Fato|Fatos]) :-
  asserta(Fato),
  assert_list(Fatos).

retract_list([]).
retract_list([Fato|Fatos]) :-
    retractall(Fato),
    retract_list(Fatos).

%****************************
%*          Imprimir        *
%****************************

imprimir_informacoes_finais :-
    pontuacao(Pontuacao),
    format("Pontuação final: ~w~n",[Pontuacao]),
    posicao_agente(Agente),
    caminho_percorrido(C1),
    append([Agente],C1,C2),
    reverse(C2,C),
    format("Caminho percorrido: ~w~n",[C]).

imprimir_caminho([H]) :- mover_agente(H).
imprimir_caminho([H1|[H2|T]]) :- 
    add_estado_vizitado(H1),
    mover_agente(H2),
    imprimir_tabuleiro,
    imprimir_caminho([H2|T]).


imprimir_tabuleiro :-
    numeroLinhas(QtdLinhas),
    numeroColunas(QtdColunas),
    nl,format(" "),imprimir_numeros_coluna(0,QtdColunas),
    imprimir_linhas(0,QtdLinhas,QtdColunas).

imprimir_numeros_coluna(QtdColunas,QtdColunas) :- format("~n").
imprimir_numeros_coluna(ColunaAtual,QtdColunas) :- 
    format("   ~w",[ColunaAtual]),
    ColunaAtualMais1 is ColunaAtual+1,
    imprimir_numeros_coluna(ColunaAtualMais1,QtdColunas).

imprimir_linhas(QtdLinhas,QtdLinhas,QtdColunas) :- format("  "),imprimir_separacao(0,QtdColunas).
imprimir_linhas(LinhaAtual,QtdLinhas,QtdColunas) :-
    format("  "),imprimir_separacao(0,QtdColunas),
    format("~w ",[LinhaAtual]),imprimir_linha(0,LinhaAtual,QtdColunas), 
    LinhaAtualMais1 is LinhaAtual+1,
    imprimir_linhas(LinhaAtualMais1,QtdLinhas,QtdColunas).

imprimir_separacao(QtdColunas,QtdColunas) :- format("+~n").
imprimir_separacao(ColunaAtual,QtdColunas) :-
    format("+---"),
    ColunaAtualMais1 is ColunaAtual+1,
    imprimir_separacao(ColunaAtualMais1,QtdColunas).

imprimir_linha(QtdColunas,LinhaAtual,QtdColunas) :- format("|~n").
imprimir_linha(ColunaAtual,LinhaAtual,QtdColunas) :-
    format('|'),
    imprimir_informacao(LinhaAtual,ColunaAtual),
    ColunaAtualMais1 is ColunaAtual+1,
    imprimir_linha(ColunaAtualMais1,LinhaAtual,QtdColunas).

% Descrição - Código ASCII
% Espaço - 32 | Estado visitado - 42 | Agente - 65 | Brutus - 66 | Coração - 67 | Elevador - 69 | espinaFre - 70 | Garrafa - 71
imprimir_informacao(LinhaAtual,ColunaAtual) :-
    ((posicao_agente([LinhaAtual,ColunaAtual]),Caracter is 65);
    (posicao_coracoes(X),member([LinhaAtual,ColunaAtual],X),Caracter is 67);
    (posicao_elevadores(X),member([LinhaAtual,ColunaAtual],X),Caracter is 69);
    (posicao_garrafas(X),member([LinhaAtual,ColunaAtual],X),Caracter is 71);
    (posicao_espinafre([[LinhaAtual,ColunaAtual]]),Caracter is 70);
    (posicao_brutus([[LinhaAtual,ColunaAtual]]),Caracter is 66);
    (estados_visitados(X),member([LinhaAtual,ColunaAtual],X),Caracter is 42)),
    format(' ~c ',[Caracter]).

imprimir_informacao(LinhaAtual,ColunaAtual) :- format(' ~c ',[32]).



