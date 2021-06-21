# Projeto Final SIN323 - Inteligência Artificial

Esse projeto consiste em desenvolver uma solução de busca para o ambiente de jogo Popeye, de forma que o agente (Popeye) possa derrotar o seu inimigo (Brutus) após coletar todos os corações e o espinafre. 

**Regras:**
- O ambiente é uma matriz 5 x 10.
- O agente se movimenta em um bloco de cada vez, podendo se movimentar pela esquerda e direita, subir e descer escadas (pela diagonal) e pular obstáculos (garrafas).

**Restrições:**
- O agente não pode pular uma garrafa onde a posição adjacente a esta garrafa é outra garrafa ou o brutus.
- Escadas com obstáculos (garrafa ou brutus) no início ou no final não podem ser utilizadas.

**Pontuação:**
- Definida ao coletar um coração, cada coração vale (500 - (Linha * 100 )).
	
**Objetivos do agente:**
1. Coletar todos os corações
2. Coletar o espinafre
3. Derrotar o brutus

O ambiente é impresso na tela a cada movimento do agente, no final é impresso a pontuação final e o caminho percorrido. 

**Como iniciar o jogo:** Passar as posições dos elementos para o predicado iniciar na seguinte ordem: Agente, Coracoes, Elevadores, Garrafas, Espinafre, Brutu. Por exemplo: 
```prolog 
iniciar([[4,0]],[[0,2],[1,0],[2,4],[3,9]],[[1,3],[2,5],[3,2],[4,1]],[[0,1],[0,5],[1,8],[2,1],[4,4],[4,5]],[[2,7]],[[0,9]]).
``` 

Exemplo de ambiente inicial:

![image](https://user-images.githubusercontent.com/51718141/122809777-ec8c0180-d2a4-11eb-9850-0a28703965cd.png)

A - Agente,
E - Escada,
G - Garrafa,
C - Coração,
F - Espinafre,
B - Brutus
