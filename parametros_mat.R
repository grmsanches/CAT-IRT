pacman::p_load(data.table, arrow, dplyr)

# 1. Leitura (Latin-1, como definido pelo INEP)
itens_2023 <- fread(
  input     = "ITENS_PROVA_2023.csv",
  integer64 = "character",
  encoding  = "Latin-1"
)

mat_2023 <- dplyr::filter(itens_2023, SG_AREA == "MT")

codigos_catalogados = c("1211", "1212", "1213", "1214")

mat_2023 <- dplyr::filter(mat_2023, CO_PROVA %in% codigos_catalogados)

mat_2023 <- dplyr::select(mat_2023, -SG_AREA, -TP_LINGUA,
                          -TX_COR, -TX_MOTIVO_ABAN, -CO_ITEM, -CO_HABILIDADE) 
mat_2023 <- mat_2023 |> dplyr::arrange(CO_PROVA, CO_POSICAO)

write_parquet(mat_2023, "mat_2023.parquet")

### Agora preciso formatar a tabela e certificar que a ordem das questões é a mesma
### que está suposta no outro arquivo .parquet
