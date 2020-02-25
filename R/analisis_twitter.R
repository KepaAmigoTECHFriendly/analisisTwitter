#' @title Análisis del Twitter en función de palabras clave.
#'
#' @description Analiza el Twitter en base a un vector de palabras clave.
#'
#' @param palabras_clave
#'
#' @return json
#'
#' @examples analisis_twitter("ermua,izarra centre,ayuntamiento de ermua")
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

analisis_twitter <- function(palabras_clave){

  #Parametros de configuracion
  #===========================

  config_geocoords       <- "39.498409,2.500940,10mi"
  #config_strings         <- c("ermua",
  #                           "izarra centre",
  #                           "ayuntamiento de ermua")

  config_strings <- str_trim(str_split(tolower(palabras_clave),",")[[1]])

  #Autenticacion
  #=============

  twitter_token <- create_token(app="rtweet_gonzalo",
                                consumer_key="msWgMHrqNc1qw8OrJI7CLbEkA",
                                consumer_secret="ZNR2LgkQlPXN1XZkjRcZR1cSWMlRkeG11RM9QEGKECHKWWZqXJ",
                                access_token="956801297180741632-wK2QJ3LHTzKn1f2uKppUeaXpQWpnGnv",
                                access_secret="2vyiRx49KGnuaFHPUYRkV8czBbNi8eWwqNfiMqzUOMr6N")


  #Descarga de tuits con cadena de busqueda
  #========================================

  tuits <- NULL
  fecha <- Sys.Date()

  num_strings <- length(config_strings)

  for(i in 1:num_strings)
  {
    Sys.sleep(6)

    tuits_q <- rtweet::search_tweets(q=config_strings[i],n=500,include_rts=TRUE,retryonratelimit = TRUE)

    # Comprobación de falta de datos
    if(nrow(tuits_q) == 0){
      next
    }

    tuits_q$created_at <- tuits_q$created_at %>% format("%Y-%m-%d")
    tuits_q <- subset(tuits_q,tuits_q$created_at==fecha)
    tuits_q <- flatten(tuits_q)
    tuits_q <- mutate(tuits_q,cadena_de_busqueda=config_strings[i])

    tuits <- bind_rows(tuits,tuits_q)

    print(config_strings[i])

  }

  tuits <- rtweet::flatten(tuits)

  tuits <- dplyr::distinct(tuits)

  tuits$text

  usuarios <- tuits %>% select(user_id,screen_name,name,location,description,account_created_at,account_lang,profile_expanded_url,profile_image_url,cadena_de_busqueda) %>% distinct()

  usuarios <- mutate(usuarios,usuario_gps_latitud=" ",usuario_gps_longitud=" ")

  num_usuarios <- nrow(usuarios)

  for(i in 1:num_usuarios)
  {
    geocoder    <- "https://geocoder.api.here.com/6.2/geocode.json?app_id=HRwFz9rfbtRq63qGH4ZQ&app_code=aMRd84WGRs4h1591F-g82w&searchtext="
    direccion   <- usuarios$location[i]
    direccion   <- gsub(",","",direccion)
    direccion   <- gsub(" ","+",direccion)
    direccion   <- gsub("N/D","N_D",direccion)
    direccion   <- iconv(direccion,from="UTF-8",to="ASCII//TRANSLIT")

    error <- NULL
    error <- tryCatch(coordenadas <- fromJSON(paste(geocoder,direccion,sep="")),error=function(e){"error"})

    if(error=="error"){}else
    {
      coordenadas <-  coordenadas$Response$View$Result %>% as.data.frame()

      latitud                          <- coordenadas$Location$DisplayPosition$Latitude[1]
      longitud                         <- coordenadas$Location$DisplayPosition$Longitude[1]

      if(!identical(latitud,NULL)){usuarios$usuario_gps_latitud[i]  <- latitud}      #VARIABLE: posición GPS latitud del origen del usuario
      if(!identical(latitud,NULL)){usuarios$usuario_gps_longitud[i] <- longitud}     #VARIABLE: posición GPS longitud del origen del usuario
    }

  }

  # Merge dataframe tuits con usuarios
  data_frame_resultado <- unique(merge(usuarios, tuits, by.x="user_id", by.y="user_id"))

  #Liberar memoria
  rm(tuits)
  rm(usuarios)

  data_frame_resultado <- data.frame(lapply(data_frame_resultado, as.character), stringsAsFactors=FALSE)
  data_frame_resultado[data_frame_resultado==""] <- "-"
  data_frame_resultado[data_frame_resultado==" "] <- "-"
  data_frame_resultado[is.na(data_frame_resultado)] <- "-"



  #===============================================================
  # CREACIÓN JSON Y ENVÍO A PLATAFORMA SMART CITY
  #===============================================================

  #Variables envío JSON a plataforma
  TB_token <- "token_twitter_tech"
  TB_url   <- paste("http://88.99.184.239:8080/api/v1/",TB_token,"/telemetry",sep="")

  json_twitter_return <- toJSON(data_frame_resultado,pretty=T)

  #Extracción timestamp en formato unix
  tsi <- format(as.numeric(anytime(Sys.Date()))*1000,scientific = F)
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

  #Liberar memoria
  rm(data_frame_resultado)

  return(json_twitter_return)
}

