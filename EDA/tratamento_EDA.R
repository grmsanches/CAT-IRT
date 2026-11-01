# Defina como diretório a pasta DADOS obtida no link fonte
setwd("~/R/microdados_enem_2023/DADOS")

pacman::p_load(data.table, arrow, dplyr)

# Colunas relevantes para EDA
colunas_EDA <- c(
  "NU_INSCRICAO",
  # Covariávies
  "TP_FAIXA_ETARIA",
  "TP_SEXO",
  "TP_COR_RACA",
  "TP_ESCOLA",
  "SG_UF_ESC"
)

# Dados para a EDA
dados_eda <- data.table::fread(
  input = "MICRODADOS_ENEM_2023.csv",
  select = colunas_EDA,
  na.strings = "",
  showProgress = TRUE
)

# Trasncrevendo para .parquet
write_parquet(dados_eda, "dados_eda.parquet")
