# Pacotes a carregar ------------------------------------------------------
.packages <- c(
 "tidyverse",
 "lubridate",
 "XML"
 )
.inst <- .packages %in% installed.packages()
if (length(.packages[!.inst]) > 0)
 install.packages(.packages[!.inst])
lapply(.packages, require, character.only = TRUE)
rm(.inst)
rm(.packages)

# Funções -----------------------------------------------------------------
xml2df <- function(f){ #f: caminho completo do arquivo a processar. XML ou ZIP.

  extensao <- tools::file_ext(f)
  eh_zip <- extensao == "zip"
  if (eh_zip) {f <- unzip(f,overwrite = T, exdir = tempdir())}
  arqXML <- xmlParse(f)
  if(eh_zip){file.remove(f)}
  
  
  df_viagem <-
    bind_rows(xpathApply(arqXML, "//Viagem", function(x) {
        parent <- data.frame(as.list(xmlAttrs(x)), stringsAsFactors = FALSE)
        kids <- bind_rows(lapply(xmlChildren(x), function(x) as.list(xmlAttrs(x))))
          cbind.data.frame(parent, kids, stringsAsFactors = FALSE)
    })) %>%
    mutate(big_key = paste0(data_hora_abertura,
                            data_hora_fechamento,
                            catraca_inicio,
                            catraca_final,
                            sentido,
                            ponto_abertura,
                            ponto_fechamento)) %>% dplyr::select(big_key, everything())
  
  df_linha <- 
  
    bind_rows(xpathApply(arqXML, "//Linha", function(x) {
      parent <- data.frame(as.list(xmlAttrs(x)), stringsAsFactors = FALSE)
      kids <- bind_rows(lapply(xmlChildren(x), function(x) as.list(xmlAttrs(x))))
      cbind.data.frame(parent, kids, stringsAsFactors = FALSE)
    })) %>%
    rename(numero_linha = Numero) %>%
    mutate(big_key = paste0(data_hora_abertura,
                            data_hora_fechamento,
                            catraca_inicio,
                            catraca_final,
                            sentido,
                            ponto_abertura,
                            ponto_fechamento))
  
  df_parcial <- df_linha %>%
    dplyr::select(1:6, big_key) %>% 
    left_join(df_viagem, by = "big_key") %>%
    dplyr::select(-big_key) %>%
    mutate(big_key = paste0(numero_linha,
                            jornada,
                            num_operador,
                            tabela,
                            hora_abertura,
                            hora_fechamento))
  
  #Leitura do veículo
  
  df_veiculo <-
    bind_rows(xpathApply(arqXML, "//Veiculo", function(x) {
      parent <- data.frame(as.list(xmlAttrs(x)), stringsAsFactors = FALSE)
      kids <- bind_rows(lapply(xmlChildren(x), function(x) as.list(xmlAttrs(x))))
      cbind.data.frame(parent, kids, stringsAsFactors = FALSE)
    })) %>%
    rename(numero_carro = Numero...1,
           numero_linha = Numero...3) %>% mutate(big_key = paste0(numero_linha,
                                                      jornada,
                                                      num_operador,
                                                      tabela,
                                                      hora_abertura,
                                                      hora_fechamento))
  
  df_parcial2 <- df_veiculo %>%
    dplyr::select(1:2, big_key) %>%
    left_join(df_parcial, by = "big_key") %>% dplyr::select(-big_key) %>% 
    mutate(big_key = paste0(numero_carro,
                            validador))
  
  
  df_empresa <-
    bind_rows(xpathApply(arqXML, "//Empresa", function(x) {
      parent <- data.frame(as.list(xmlAttrs(x)), stringsAsFactors = FALSE)
      kids <- bind_rows(lapply(xmlChildren(x), function(x) as.list(xmlAttrs(x))))
      cbind.data.frame(parent, kids, stringsAsFactors = FALSE)
    })) %>%
    mutate(big_key = paste0(Numero,
                            validador))
  
  df_parcial3 <- df_empresa %>%
    dplyr::select(1:2, big_key) %>%
    left_join(df_parcial2, by = "big_key") %>% dplyr::select(-big_key) %>% 
    mutate(big_key = paste0(Codigo,
                            modalidade))
    
    df_categoria <-
      bind_rows(xpathApply(arqXML, "//Categoria", function(x) {
        parent <- data.frame(as.list(xmlAttrs(x)), stringsAsFactors = FALSE)
        kids <- bind_rows(lapply(xmlChildren(x), function(x) as.list(xmlAttrs(x))))
        cbind.data.frame(parent, kids, stringsAsFactors = FALSE)
      })) %>%
      mutate(big_key = paste0(Codigo,
                              modalidade))
    
    df_parcial4 <- df_categoria %>%
      dplyr::select(1, big_key) %>%
      left_join(df_parcial3, by = "big_key") %>% dplyr::select(-big_key)
    
  df <- df_parcial4 %>% mutate(across(starts_with("data_hora"),ymd_hms))
  rm(df_parcial, df_parcial2, df_parcial3, df_parcial4, df_viagem, df_linha, df_empresa, df_categoria)
  df$data <- date(df$data_hora)
  df$dia <- weekdays(df$data, abbreviate = T)
  df$hora_int <- hour(df$data_hora)
  df <- df %>% mutate(across(starts_with(c("valor","catraca")),as.numeric))

  rm("arqXML")
  gc()
  return(df)
}


