library(arrow)
library(data.table)
library(purrr)

source("fun/fn_Leitura_XML_Validador.r")

files <- list.files("data-raw/", full.names = TRUE, pattern = ".xml", recursive = T)

# file <- files[1]

save_parquet <- function(file) {
  
  df <- xml2df(f = file)
  write_parquet(sprintf("data-raw/bilhetagem-parquet/%s.parquet",
                        tools::file_path_sans_ext(basename(file))))
  
}

walk(.x = files, .f = xml2df)
