# Mapeamentos transcritos do Dicionário_Microdados_Enem_2023.xlsx.
# Revise a compatibilidade com os arquivos CSV antes da análise.
enem_standard_forms <- list(
  LC = as.character(1201:1204),
  CH = as.character(1191:1194),
  CN = as.character(1221:1224),
  MT = as.character(1211:1214)
)
enem_reapplication_forms <- list(
  LC = as.character(1281:1284),
  CH = as.character(1271:1274),
  CN = as.character(1301:1304),
  MT = as.character(1291:1294)
)

# Mapeia posição do vetor TX_RESPOSTAS_LC (1:45) à posição no vetor
# TX_GABARITO_LC (1:50), condicionado a TP_LINGUA: 0 inglês, 1 espanhol.
lc_answer_key_positions <- function(tp_lingua) {
  if (length(tp_lingua) != 1L || is.na(tp_lingua) || !tp_lingua %in% c(0, 1))
    stop("TP_LINGUA deve ser 0 (inglês) ou 1 (espanhol).")
  response_pos <- seq_len(45L)
  key_pos <- response_pos + 5L
  key_pos[1:5] <- if (tp_lingua == 0) 1:5 else 6:10
  key_pos
}

# Seleção explícita das formas padrão documentadas; não inclui reaplicações nem
# provas ampliadas/adaptadas automaticamente.
select_enem_standard_forms <- function(area, co_prova) {
  if (!area %in% names(enem_standard_forms)) stop("Área desconhecida: ", area)
  intersect(as.character(co_prova), enem_standard_forms[[area]])
}
