library(jsonlite); library(purrr); library(dplyr); library(readr)

# --- Parâmetros ---
USAR_SIMULADO <- FALSE
INTERVALO_S   <- 60
ARQ_CSV       <- "presidente_evolucao.csv"
TZ            <- "America/Sao_Paulo"

if (USAR_SIMULADO) {
  base <- "https://resultados-sim.tse.jus.br/simulado/simulado2026"; eleicao <- 21270
} else {
  base <- "https://resultados.tse.jus.br/oficial"; eleicao <- 6257
}
ciclo <- "ele2026"

ufs <- c("br","zz","ac","al","am","ap","ba","ce","df","es","go","ma","mg","ms","mt",
         "pa","pb","pe","pi","pr","rj","rn","ro","rr","rs","sc","se","sp","to")

# --- Valida códigos no arquivo de configuração (EA11) ---
cfg <- tryCatch(paste(readLines(paste0(base, "/comum/config/ele-c.json"), warn = FALSE),
                      collapse = ""), error = function(e) "")
if (!grepl(as.character(eleicao), cfg) || !grepl(ciclo, cfg))
  warning("Código da eleição ou ciclo não encontrado em ele-c.json: confira os parâmetros.")

# --- Aguarda a liberação (Presidente: 17h de Brasília, 04/10/2026) ---
liberacao <- as.POSIXct("2026-10-04 17:00:00", tz = TZ)
if (!USAR_SIMULADO && Sys.time() < liberacao) {
  message("Aguardando liberação até ", format(liberacao, "%d/%m %H:%M"))
  Sys.sleep(as.numeric(difftime(liberacao, Sys.time(), units = "secs")))
}

url_uf <- function(uf) sprintf("%s/%s/%d/dados/%s/%s-c0001-e%06d-u.json",
                               base, ciclo, eleicao, uf, uf, eleicao)

acha_cand <- function(x) {
  if (!is.list(x)) return(list())
  if (all(c("nm", "vap") %in% names(x))) return(list(x))
  unlist(lapply(x, acha_cand), recursive = FALSE)
}

baixa_uf <- function(uf, ult_idg) {
  Sys.sleep(0.1)  # limite: 100 req/s por IP
  j <- tryCatch(fromJSON(url_uf(uf), simplifyVector = FALSE),
                error = function(e) { message(uf, ": ", conditionMessage(e)); NULL })
  if (is.null(j)) return(NULL)
  idg <- as.character(j$idg %||% NA)
  if (isTRUE(ult_idg[toupper(uf)] == idg)) return(NULL)   # arquivo sem nova geração
  map_dfr(acha_cand(j), ~ as_tibble(map(.x[c("sqcand","n","nm","vap","pvap")], ~ .x %||% NA))) |>
    mutate(uf = toupper(uf), idg = idg,
           andamento = as.character(j$and %||% NA),
           hora_tse  = as.character(j$hg %||% NA), .before = 1)
}

ciclo_coleta <- function() {
  agora <- Sys.time()
  ult_idg <- character(0)
  if (file.exists(ARQ_CSV))
    ult_idg <- read_csv(ARQ_CSV, col_types = cols(.default = "c"), show_col_types = FALSE) |>
      group_by(uf) |> slice_tail(n = 1) |> ungroup() |> with(setNames(idg, uf))

  novo <- map_dfr(ufs, baixa_uf, ult_idg = ult_idg)
  if (nrow(novo) == 0) { message(format(agora, "%H:%M", tz = TZ), " - sem alterações"); return(FALSE) }

  novo <- novo |> mutate(vap = as.numeric(vap),
                         pvap = as.numeric(sub(",", ".", pvap)),
                         sqcand = as.character(sqcand),
                         data_coleta = format(agora, "%Y-%m-%d", tz = TZ),
                         hora_atualizacao = format(agora, "%H:%M", tz = TZ))
  write_csv(novo, ARQ_CSV, append = file.exists(ARQ_CSV))
  message(format(agora, "%H:%M", tz = TZ), " - ", nrow(novo), " linhas novas")
  any(novo$uf == "BR" & novo$andamento == "f", na.rm = TRUE)   # TRUE = totalização final
}

repeat { if (isTRUE(ciclo_coleta())) break; Sys.sleep(INTERVALO_S) }
message("Totalização final do Brasil registrada.")



