# CAT com parâmetros de itens do Enem 2023

## Arquivos

Baixar microdados [aqui](https://download.inep.gov.br/microdados/microdados_enem_2023.zip).

- `prepare_ENEM.R`: prepara respostas, mapas, auditorias e bancos por área.
- `analyze_cat.R`: simulações Monte Carlo por área e língua em LC.
- `cat.R`: 3PL, seleção Fisher/EPE e restrições de composição.
- `interpretation.qmd`: relatório acadêmico em português.
- `Dicionário_Microdados_Enem_2023.xlsx`: definições/códigos oficiais.
- `enem_2023_dictionary_mapping.R`, `Dicionario_revisao.md`: mapeamento e interpretação do dicionário.
- `validate_ENEM_LC_source_updated.R`: pré-validação dos CSVs fonte para LC.
- `test_lc_structure_updated.R`: teste sintético da restrição no CAT.

Os arquivos `_updated` são substituições propostas; os arquivos originais não puderam ser sobrescritos neste ambiente. Faça backup e substitua os arquivos canônicos pelas versões correspondentes antes de executar.

## Formulários padrão

Códigos do dicionário: LC `CO_PROVA_LC` 1201–1204; CH `CO_PROVA_CH` 1191–1194; CN `CO_PROVA_CN` 1221–1224; MT `CO_PROVA_MT` 1211–1214. Reaplicações e formas ampliadas/adaptadas usam outros códigos e não entram automaticamente no banco padrão.

## Linguagens (LC)

O arquivo de itens tem 50 registros por forma, representando 45 posições: 40 questões comuns e duas alternativas de cinco questões (inglês/espanhol). `TP_LINGUA` identifica 0 = inglês e 1 = espanhol. O dicionário especifica 45 caracteres em `TX_RESPOSTAS_LC` e 50 em `TX_GABARITO_LC`: as cinco respostas iniciais usam a chave da língua escolhida, e as 40 seguintes correspondem às posições 11–50 do gabarito.

A preparação preserva os 50 registros, valida a classificação por idioma e cria pools elegíveis inglês/espanhol. Cada trajetória LC usa exatamente 40 itens comuns e cinco itens de uma só língua. Nunca administra as duas alternativas. Se exclusões anuladas ou parâmetros ineligíveis deixarem menos de 40 comuns ou cinco da língua, a simulação falha sem substituições. Como LC deve ter 45 itens, DP posterior ≤0,6 é avaliado ao final da prova, não usado para interromper antecipadamente. Em CH/CN/MT, permanece a parada por DP≤0,6 ou esgotamento do banco.

## Exclusões e metodologia

`IN_ITEM_ABAN == 1` e `TX_GABARITO == "X"` indicam anulação conforme o dicionário. Parâmetros A/B/C ausentes ou inválidos e inconsistência entre aparições de `CO_ITEM` impedem elegibilidade; nada é imputado. Status e motivos são auditados. Não há no dicionário variável específica de falha de convergência; ausência de parâmetro não é classificada automaticamente como convergência.

Fisher e EPE usam 3PL, priori normal padrão e posterior em grade de 301 pontos entre −4 e 4. As habilidades/respostas são simuladas; RMSE, MAE e viés não são erros observados de candidatos. Nas comparações LC, os dois métodos usam o mesmo banco por idioma e as mesmas quotas.

## Execução

Requisitos: R, `data.table`, `arrow` e Quarto. Com os microdados originais em `DADOS`:

```sh
Rscript validate_ENEM_LC_source_updated.R --data-dir=/caminho/microdados_enem_2023/DADOS
Rscript prepare_ENEM.R --data-dir=/caminho/microdados_enem_2023/DADOS
Rscript test_lc_structure_updated.R
Rscript analyze_cat.R --replications=100 --n=50 --seed=2025
quarto render interpretation.qmd
```

A pré-validação confere os registros/posições LC, grupos `TP_LINGUA` e comprimentos dos vetores. A preparação repete as verificações estruturais e compara chaves. Se qualquer critério falhar, investigue a fonte em vez de inferir ou remover itens. Os arquivos de origem não estavam disponíveis nesta revisão; execução, reconciliação das contagens, escala dos parâmetros e renderização ainda precisam ser confirmadas. Nenhum resultado empírico foi produzido aqui.
