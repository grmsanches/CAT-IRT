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

# ------------------------------------------------------------------------------

# Aqui a análise considera apenas o caderno de matemática. Para realizar
# a mesma análise com outros cadernos, troque "MT" por "CN", "CH" ou "LC"

colunas_interesse <- c(
  "NU_NOTA_MT",
  # Vetores de respostas do candidato para o caderno de matemática
  "TX_RESPOSTAS_MT",
  # Vetores de gabaritos oficiais do caderno de matemática
  "TX_GABARITO_MT"
)

# Carregando os dados de forma otimizada usando a biblioteca data.table
ENEM_2023 <- data.table::fread(
  input = "MICRODADOS_ENEM_2023.csv",
  select = colunas_interesse,
  na.strings = "",
  showProgress = TRUE
)

# Remoção de registros com NA nas colunas

# 1. Converte o vetor de respostas e de gabaritos em matrizes (N x 45)
resp_mat <- do.call(rbind, strsplit(ENEM_2023$TX_RESPOSTAS_MT, ""))
gab_mat  <- do.call(rbind, strsplit(ENEM_2023$TX_GABARITO_MT, ""))

# 2. Compara as matrizes elemento a elemento e converte TRUE/FALSE para 1/0
bin_mat <- (resp_mat == gab_mat) * 1

# 3. Nomeia as 45 colunas de questões
colnames(bin_mat) <- sprintf("Q%02d", 1:45)

# 4. Acopla as novas colunas e remove os textos originais
ENEM_2023 <- ENEM_2023 |>
  cbind(bin_mat) |>
  select(-TX_RESPOSTAS_MT, -TX_GABARITO_MT)

# Escrevendo os dados em formato Parquet (é mais leve)
write_parquet(ENEM_2023, "enem_mt_2023.parquet")



