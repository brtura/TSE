# preparar_tse_2026.R
# Baixa e prepara os dados do TSE - Eleições 2026 (eleitorado, votos e abstenção)

library(data.table)
library(curl)

ANO   <- 2026
TURNO <- 1
BASE  <- "/media/tura/scripts/tse"
CSV   <- file.path(BASE, "csv")
ZIPS  <- file.path(BASE, "zips")

dir.create(CSV,  recursive = TRUE, showWarnings = FALSE)
dir.create(ZIPS, recursive = TRUE, showWarnings = FALSE)

CHAVE <- c("SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")

# DOWNLOAD (grava em .parcial e só renomeia se terminar; apaga o zip para atualizar)

baixar <- function(url, destino, tentativas = 3) {
  if (file.exists(destino)) return(invisible(destino))
  parcial <- paste0(destino, ".parcial")
  for (i in seq_len(tentativas)) {
    ok <- tryCatch({ curl_download(url, parcial); TRUE }, error = function(e) FALSE)
    if (ok) {
      file.rename(parcial, destino)
      return(invisible(destino))
    }
  }
  stop("Não foi possível baixar: ", url)
}

URL <- "https://cdn.tse.jus.br/estatistica/sead/odsele/"

zip_locais <- file.path(ZIPS, paste0("eleitorado_local_votacao_", ANO, ".zip"))
zip_votos  <- file.path(ZIPS, paste0("votacao_secao_", ANO, "_BR.zip"))

baixar(paste0(URL, "eleitorado_locais_votacao/eleitorado_local_votacao_", ANO, ".zip"), zip_locais)
baixar(paste0(URL, "votacao_secao/votacao_secao_", ANO, "_BR.zip"), zip_votos)

# LEITURA (select falha se faltar coluna, então não precisa checar à parte)

ler <- function(zip, arquivo, colunas, ...) {
  fread(
    cmd = paste("unzip -p", shQuote(zip), shQuote(arquivo)),
    sep = ";", encoding = "Latin-1", select = colunas, ...
  )
}

# ELEITORADO

eleitorado <- ler(
  zip_locais, paste0("eleitorado_local_votacao_", ANO, "_BRASIL.csv"),
  c("NR_TURNO", CHAVE, "NM_MUNICIPIO", "DS_TIPO_SECAO_AGREGADA",
    "NR_SECAO_PRINCIPAL", "NR_LOCAL_VOTACAO", "NM_LOCAL_VOTACAO",
    "DS_ENDERECO", "NM_BAIRRO", "NR_LATITUDE", "NR_LONGITUDE",
    "QT_ELEITOR_ELEICAO_FEDERAL"),
  dec = ","
)

eleitorado <- unique(eleitorado[NR_TURNO == TURNO, !"NR_TURNO"], by = CHAVE)

fwrite(eleitorado, file.path(CSV, paste0("eleitorado_", ANO, ".csv")), sep = ";", bom = TRUE)

# VOTAÇÃO (Presidente = CD_CARGO 1)

votos <- ler(
  zip_votos, paste0("votacao_secao_", ANO, "_BR.csv"),
  c("NR_TURNO", "CD_CARGO", CHAVE, "NM_MUNICIPIO", "NR_LOCAL_VOTACAO",
    "NM_LOCAL_VOTACAO", "NM_VOTAVEL", "QT_VOTOS")
)

votos <- votos[NR_TURNO == TURNO & CD_CARGO == 1, !c("NR_TURNO", "CD_CARGO")]

fwrite(votos, file.path(CSV, paste0("votos_", ANO, ".csv")), sep = ";", bom = TRUE)

# ABSTENÇÃO

abstencao <- merge(
  eleitorado,
  votos[, .(COMPARECIMENTO = sum(QT_VOTOS)), by = CHAVE],
  by = CHAVE, all.x = TRUE
)

setnafill(abstencao, fill = 0, cols = "COMPARECIMENTO")

abstencao[, ABSTENCAO := QT_ELEITOR_ELEICAO_FEDERAL - COMPARECIMENTO]
abstencao[, PCT_ABSTENCAO := 100 * ABSTENCAO / QT_ELEITOR_ELEICAO_FEDERAL]

fwrite(abstencao, file.path(CSV, paste0("abstencao_", ANO, ".csv")), sep = ";", bom = TRUE)
