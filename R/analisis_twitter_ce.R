#' @title Análisis del Twitter cuentas.
#'
#' @description Analiza el Twitter en base a un vector (datos) de cuentas/usuarios de Twitter especificados.
#'
#' @param centros_empresa
#'
#' @return json
#'
#' @import twitteR
#' dplyr
#' rtweet
#' magrittr
#' stringr
#' jsonlite
#' anytime
#' httr
#'
#' @export

analisis_twitter_ce <- function(centros_empresa){

  #Parametros de configuracion
  #===========================

  config_geocoords       <- "39.498409,2.500940,10mi"
  #config_strings         <- c("ermua",
  #                           "izarra centre",
  #                           "ayuntamiento de ermua")

  twiters <- str_trim(str_split(tolower(centros_empresa),",")[[1]])

  #Autenticacion
  #=============

  twitter_token <- create_token(app="rtweet_gonzalo",
                                consumer_key="msWgMHrqNc1qw8OrJI7CLbEkA",
                                consumer_secret="ZNR2LgkQlPXN1XZkjRcZR1cSWMlRkeG11RM9QEGKECHKWWZqXJ",
                                access_token="956801297180741632-wK2QJ3LHTzKn1f2uKppUeaXpQWpnGnv",
                                access_secret="2vyiRx49KGnuaFHPUYRkV8czBbNi8eWwqNfiMqzUOMr6N")

  tweets_cuentas <- NULL
  for(i in 1:length(twiters)){
    Sys.sleep(6)

    tweets_cuentasq <- get_timeline(twiters[i], n = 30)
    tweets_cuentasq <- tweets_cuentasq[(format((as.numeric(anytime(tweets_cuentasq$created_at))*1000),scientific = F) >= format((as.numeric(anytime(Sys.Date()-1))*1000),scientific = F)), ]
    tweets_cuentasq <- flatten(tweets_cuentasq)
    tweets_cuentasq <- mutate(tweets_cuentasq,`Twitter objetivo`=twiters[i])

    tweets_cuentas <- bind_rows(tweets_cuentas,tweets_cuentasq)
  }

  tweets_cuentas <- rtweet::flatten(tweets_cuentas)

  tweets_cuentas <- dplyr::distinct(tweets_cuentas)

  data_frame_resultado <- tweets_cuentas


  #===============================================================
  # CREACIÓN JSON Y ENVÍO A PLATAFORMA SMART CITY
  #===============================================================

  #Variables envío JSON a plataforma
  TB_token <- "9vIrujfnOkivmp139pzC"
  TB_url   <- paste("http://78.47.39.122:8080/api/v1/",TB_token,"/telemetry",sep="")

  json_twitter_return <- toJSON(data_frame_resultado,pretty=T)

  #Extracción timestamp en formato unix
  tsi <- format(as.numeric(anytime(as.Date(anytime(tweets_cuentasq$created_at[1]))))*1000,scientific = F)

  #tsi <- sub("\\..*", "",tsi)
  for(i in 1:nrow(data_frame_resultado)){
    ts <- as.numeric(tsi) + i  #Añade i ms al timestamp para poder verse sin solapamiento en el widget de la plataforma smart city.

    #Creación de JSON noticias y eliminación de ][ para cumplir con el formato json con modificación de timestamp de thingsboard.
    json_twitter <- toJSON(data_frame_resultado[i,],pretty=T)
    json_twitter <- sub("[[]","",json_twitter)
    json_twitter <- sub("[]]","",json_twitter)

    #Formato json con modificación de timestamp de thingsboard.
    json_envio_plataforma <- paste('{"ts":',ts,', "values":',json_twitter,"}",sep="")

    #Envío JSON a plataforma
    POST(url=TB_url,body=json_envio_plataforma)
  }

  return(json_twitter_return)
}
