<img src="./banner.jpg" alt="Banner" width="100%" height="300" style="display: block; object-fit: cover;">

# Testes Adaptativos Computadorizados na TRI
Uma comparação entre métodos utilizados em **Testes Adaptativos Computadorizados** (CAT, na sigla em inglês) no contexto da Teoria de Resposta ao Item (TRI): **informação de Fisher versus entropia a posteriori esperada**.

A proposta é utilizar todas as informações contidas na distribuição a posteriori do parâmetro estimado (parâmetro de proficiência) para selecionar o próximo item a ser apresentado ao participante, em vez de depender apenas da informação local captada pelo método baseado na informação de Fisher.

Espera-se que o método proposto seja capaz de **reduzir o número de perguntas necessárias** para se obter uma estimativa razoáveç para o traço latente do candidato.

# Fonte

Os dados foram obtidos por meio deste [link](https://download.inep.gov.br/microdados/microdados_enem_2023.zip).

# Metodologia

Para este projeto, **consideramos apenas o caderno de matemática as provas Azul, Amarela, Rosa e Cinza** (cujos respectivos códigos são: 1211, 1212, 1213, 1214). Removemos da análise os candidatos com valores desconhecidos para alguma das seguintes colunas (as colunas de interesse para o projeto):

- *NU_NOTA_MT*
- *TX_RESPOSTAS_MT*
- *TX_GABARITO_MT*
- *CO_PROVA_MT*

Nossa base de dados foi, então, de aproximadamente 4 milhões de observações para 2.7 milhões (uma redução em torno de 32.5%).


O órgão estima um traço latente $\theta$ para cada área, isto é, um para Ciências da natureza, um para matemática, e assim por diante. Nesta análise, tomaremos apenas as questões do caderno de matemática. Com relação aos parâmetros de cada questão para o modelo TRI-3PL, o INEP os calibra previamente utilizando algoritmos de **máxima verossimilhança marginal**. Então, ao aplicar a prova, o único parâmetro a ser estimado é o traço latente de habilidade do aluno. 

Para estimar o traço latente, o órgão adota uma abordagem bayesiana para evitar problemas de estimação decorrentes de casos em que um aluno acertasse todas as questões (ou errasse todas as questões). Assume-se que a dsitribuição a priori do parâmetro de habilidade (traço latente) de cada candidato é uma normal padrão. Então, na medida em que se verifica os acertos do candidato com respostas às questões e seus respectivos parâmetros, atualiza-se a distribuição destes parâmetros. Ao final da avaliação de todas as questões, a estimativa pontual é obitda via **EAP - Expected A Posteriori**. Então, soma-se multiplica-se por 100 e soma-se 500 à esta estimativa (para reescalar para uma normal com média 500 e desvio padrão 100, que escala utilizada no ENEM).
