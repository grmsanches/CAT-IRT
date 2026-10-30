library(data.table)

# 1. Leitura (Latin-1, como definido pelo INEP)
itens_2023 <- fread(
  input     = "ITENS_PROVA_2023.csv",
  integer64 = "character",
  encoding  = "Latin-1"
)

# 2. Fatores
itens_2023[, SG_AREA := factor(SG_AREA,
                               levels = c("CH", "CN", "LC", "MT"),
                               labels = c("Ciências Humanas", "Ciências da Natureza",
                                          "Linguagens e Códigos", "Matemática"))]

itens_2023[, TP_LINGUA := factor(TP_LINGUA,
                                 levels = c(0, 1), labels = c("Inglês", "Espanhol"))]

itens_2023[, IN_ITEM_ADAPTADO := factor(IN_ITEM_ADAPTADO,
                                        levels = c(1, 0), labels = c("Sim", "Não"))]

itens_2023[, IN_ITEM_ABAN := factor(IN_ITEM_ABAN,
                                    levels = c(1, 0), labels = c("Sim", "Não"))]

# 3. Diagnóstico: o que existe de fato em Matemática?
itens_2023[SG_AREA == "Matemática",
           .(provas = uniqueN(CO_PROVA), itens = uniqueN(CO_ITEM), linhas = .N),
           by = .(TX_COR, IN_ITEM_ADAPTADO)]

# 4. Seleção completa (σ) + eliminação de duplicatas (π)
mat_2023 <- itens_2023[
  SG_AREA == "Matemática" &
    TX_COR == "VERDE" &
    IN_ITEM_ADAPTADO == "Não"
]
setorder(mat_2023, CO_POSICAO)
mat_2023 <- unique(mat_2023, by = "CO_ITEM")

### Agora preciso formatar a tabela e certificar que a ordem das questões é a mesma
### que está suposta no outro arquivo .parquet
