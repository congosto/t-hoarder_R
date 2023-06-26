# shared functions for the standard API

parser_tweets_API_standard <- function (tweets){             
  # Si no existe el dato "source, se lo añadimos con valor nulo"
  if (is.null (tweets$source)){ 
    tweets <-  tweets %>%
      mutate (source = NA)
  }
  #normalizamos los datos al formato t-hoarder
  # Extraemos los datos de los tweets
  base_df <- tweets %>%
    # Re-nombramos columnas para que sean igual que en t-hoarder 
    rename (
      "id_tweet" = "id_str",
      "date" = "created_at",
      "app" = "source",
      "replied_id" ="in_reply_to_status_id_str",
      "user_replied" = "in_reply_to_screen_name"
    ) %>%  
    # Establecemos el tipo de relación
    mutate (text = full_text) %>%
    select (
      id_tweet,date,text,app,retweet_count,favorite_count,quote_count,reply_count,lang,
      replied_id,user_replied,is_quote_status,retweeted
    )
  # Extraemos El primer hashtag, las urls expandidas y la multimedia si exusten
  col_NA <- rep(NA, nrow(tweets))
  if ("hashtags" %in% names(tweets$entities)){
    first_HT <- lapply(tweets$entities, `[[`, "hashtags") %>%
      lapply(`[[`, "tag") %>%
      lapply(function(x) {ifelse (is.null (x), NA,x)}) %>%
      unlist (recursive = FALSE)
  } else{first_HT = col_NA}
  if ("urls" %in% names(tweets$entities)){
    urls <- lapply(tweets$entities, `[[`, "urls") %>%
      lapply(`[[`, "expanded_url") %>%
      lapply(function(x) {ifelse (is.null (x), NA,x)}) %>%
      unlist (recursive = FALSE)
  } else{urls = col_NA}
  if ("media" %in% names(tweets$entities)){
    url_media <- lapply(tweets$entities$media, `[[`, "media") %>%
      lapply(`[[`, "media_url") %>%
      lapply(function(x) {ifelse (is.null (x), NA,x)}) %>%
      unlist (recursive = FALSE)
    type_media <- lapply(tweets$entities, `[[`, "media") %>%
      lapply( `[[`, "type") %>%
      lapply(function(x) {ifelse ( is.null (x), NA,x)}) %>%
      unlist (recursive = FALSE)
  } else{url_media = col_NA
  type_media = col_NA}
  entities_df <- data.frame(urls = urls, first_HT = first_HT, url_media = url_media, type_media = type_media)
  #coordinates <- tweets$coordinates
  long <- lapply(tweets$coordinates, `[[`, "long") %>%
    unlist (recursive = FALSE)
  lat <- lapply(tweets$coordinates, `[[`, "lat") %>%
    unlist (recursive = FALSE)
  geolocation_df <- data.frame(long =long, lat = lat)
  # Extraemos datos de los usuarios
  user_df <- users_data(tweets) %>%
    # Re-nombramos columnas para que sean igual que en t-hoarder 
    rename (
      "id_user" = "id_str",
      "author" = "screen_name",
      "followers" = "followers_count",
      "following" = "friends_count",
      "statuses" = "statuses_count",
      "avatar" = "profile_image_url_https"
    ) %>%
    select(
      id_user,author,followers,following,statuses,name,location,description,
      created_at,verified,avatar
    )
  # Si es una cita, extraemos los datos
  if ("full_text" %in% names(tweets$quoted_status)) {                                            
    quote <- lapply(tweets$quoted_status, `[[`, "full_text") %>%
      unlist (recursive = FALSE)
    quoted_id <- lapply(tweets$quoted_status, `[[`, "id_str") %>%
      unlist (recursive = FALSE)
    user_quoted <- lapply(tweets$quoted_status, `[[`, "user") %>%
      lapply(`[[`, "screen_name") %>%
      unlist (recursive = FALSE)
  } else {
    quote = col_NA
    quoted_id = col_NA
    user_quoted = col_NA
  }
  quoted_df <- data.frame(quote = quote, quoted_id = quoted_id, user_quoted = user_quoted)
  # Si RT, extraemos los datos 
  if ("id_str" %in% names(tweets$retweeted_status)){     
    retweeted_id <- lapply(tweets$retweeted_status, `[[`, "id_str") %>%
      unlist (recursive = FALSE)
    user_retweeted <- lapply(tweets$retweeted_status, `[[`, "user") %>%
      lapply(`[[`, "screen_name") %>%
      unlist (recursive = FALSE)
  } else {
    retweeted_id = col_NA
    user_retweeted = col_NA
  }
  RT_df <- data.frame( retweeted_id  = retweeted_id ,user_retweeted = user_retweeted)
  # Unimos los datasets
  tweets_df <- cbind (base_df,entities_df,user_df,quoted_df,RT_df,geolocation_df)
  # Ajsustamos los datos
  tweets_nor_df <- tweets_df %>%
    # Añadimos el link al tweet
    mutate (link = paste0("https://twitter.com/",author,"/status/",id_tweet)) %>%
    # Formato de fechas
    mutate (date = format(date, "%Y-%m-%d %H:%M:%S")) %>%
    mutate (created_at = as.POSIXlt(created_at, tz="GMT",format ("%a %b %d %H:%M:%S %z %Y"))) %>%
    # Parse app
    mutate (app = str_extract (app,">[\\w ]+")) %>%
    mutate (app = str_extract (app,"[\\w ]+")) %>%
    # añadimos geo-localización
    mutate (geolocation = ifelse (!is.na(lat), paste0(lat,",",long),NA )) %>%
    mutate (relation = ifelse ( !is.na(user_replied), 'reply',  
                              ifelse (!is.na(user_quoted), 'quoted', 
                                      ifelse (!is.na(user_retweeted), 'RT',NA)))
    ) %>%
    # Añadimos @  los screen_name
    mutate (author = paste0 ('@',author)) %>%
    mutate (user_retweeted =  ifelse (relation == 'RT', paste0 ('@',user_retweeted),NA)) %>%
    mutate (user_replied = ifelse (relation == 'reply',paste0('@',user_replied),NA )) %>%
    mutate (user_quoted = ifelse (relation == 'quoted',paste0('@',user_quoted),NA )) %>%
    # sustituimos los saltos de línea por espacios en text, location, description y quote para que ç
    # se pueda exportar más fácil el csv
    mutate (text = str_replace_all (text, '[\n\r]+',' ')) %>%
    mutate (name = str_replace_all (name, '[\n\r]+',' ')) %>%
    mutate (location = str_replace_all (location, '[\n\r]+',' ')) %>%
    mutate (description = str_replace_all (description, '[\n\r]+',' ')) %>%
    mutate (quote = str_replace_all (quote, '[\n\r]+',' ')) %>%
    select (
      id_tweet,date,author,text,app,id_user,followers,following,statuses,
      location,urls,geolocation,name,description,url_media,type_media,quote,
      relation,replied_id,user_replied,retweeted_id,user_retweeted,quoted_id,user_quoted,
      first_HT,lang,created_at,verified,avatar, link,
      retweet_count,reply_count, quote_count, favorite_count
    )
  return (tweets_nor_df)
}
