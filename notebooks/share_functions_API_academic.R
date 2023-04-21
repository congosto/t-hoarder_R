# Get user fron url quoted
get_user_quoted <- function (url) {
  if (!is.na(url)) {
    part <- str_split_1 (url,"/")
    return (paste0("@",part[4]))
  }
  return (NA)
}
get_user_quoted_v <- Vectorize(get_user_quoted)

# shared functions for the standard API
parser_tweets_API_academic <- function(tweets){  
  # Si no existe el dato "source, se lo añadimos con valor nulo"
  # en la API V2, no es accesible este dato desde el 21-12-2022
  if(is.null(tweets$source)){ 
    tweets <-  tweets %>%
      mutate(source = NA)
  }
  # Comprobamos que hay tweets relacionados(RTs, replies o quotes)       
  if(!is.null(tweets$referenced_tweets)){ 
    base_df <-  tweets %>%
      # Re-nombramos para que no haya conflicto de nombres al expandir referenced_tweets
      rename("tweet_id" = "id") %>% 
      unnest(referenced_tweets,keep_empty = TRUE) %>%
      rename(
        "relation" = "type",
        "id_relation" = "id"
      ) 
  } else{
    base_df <- tweets %>%
      rename(
        "tweet_id" = "id") %>% 
      mutate(
        relation = NA,
        id_relation = NA
      )
  }
  base_df <-  base_df  %>%
    # Expandimos 
    unnest(public_metrics,keep_empty = TRUE) %>%
    # Normalizamos las relaciones 
    mutate(relation = ifelse(relation  == 'retweeted','RT',relation)) %>%
    mutate(relation = ifelse(relation  == 'replied_to','reply',relation)) %>%
    # Creamos una columna para cada tipo de usuario relacionado
    mutate(retweeted_id =  ifelse(relation == 'RT',id_relation,NA )) %>%
    mutate(replied_id =  ifelse(relation == 'reply',id_relation,NA )) %>%
    mutate(quoted_id =  ifelse(relation == 'quoted',id_relation,NA )) %>%
    mutate(user_retweeted = ifelse(relation == 'RT',str_extract(text, '@\\w+'),NA )) %>%
    mutate(user_replied = ifelse(relation == 'reply',str_extract(text, '@\\w+'),NA ))
    
  # Extraemos los hashtags, urls expandidas y la multimedia
  col_NA <- rep(NA, nrow(base_df))
  if(!is.null(base_df$entities$hashtags)){
    first_HT <- lapply(base_df$entities$hashtags, `[[`, "tag") %>%
      lapply(function(x) {ifelse(is.null(x), NA,x)}) %>%
      unlist(recursive = FALSE)
  } else{first_HT = col_NA}
  if(!is.null(base_df$entities$urls)){
    urls <- lapply(base_df$entities$urls, `[[`, "expanded_url") %>%
      lapply(function(x) {ifelse(is.null(x), NA,x)}) %>%
      unlist(recursive = FALSE)
  } else{urls = col_NA}
  if(!is.null(base_df$entities$media)){
    url_media <- lapply(base_df$entities$media, `[[`, "media_url") %>%
      lapply(function(x) {ifelse(is.null(x), NA,x)}) %>%
      unlist(recursive = FALSE)
    type_media <- lapply(base_df$entities$media, `[[`, "type") %>%
      lapply(function(x) {ifelse( is.null(x), NA,x)}) %>%
      unlist(recursive = FALSE)
  } else{
      url_media = col_NA
      type_media = col_NA
    }
  entities_df <- data.frame(first_HT = first_HT, urls = urls, url_media = url_media, type_media = type_media)
  cols_num_df <- data.frame( geolocation = col_NA, quote =col_NA)
  # Añadimos entidades y columnas nulas
  base_df <- cbind(base_df,entities_df,cols_num_df)
  # Reparamos entidades de los retweets(La API lo da truncado)
  original_entities <- base_df %>%
    filter(relation  != 'RT' | is.na(relation)) %>%
    rename(
      "tweet_id_original" = "tweet_id",
      "first_HT_original" = "first_HT",
      "urls_original" = "urls",
      "url_media_original" = "url_media",
      "type_media_original" = "type_media"
    ) %>%
    select(tweet_id_original, first_HT_original,urls_original, url_media_original, type_media_original )
  # Si no existe el la entidad en en tweet se toma del original
  base_df <- left_join(base_df,original_entities,  by = c( "retweeted_id" = "tweet_id_original" )) %>%
    mutate(first_HT = ifelse(is.na(first_HT) & relation == "RT",first_HT_original,first_HT )) %>%
    mutate(urls = ifelse(is.na(urls) & relation == "RT",urls_original,urls )) %>%
    mutate(url_media = ifelse(is.na(url_media) & relation == "RT",urls_original,url_media )) %>%
    mutate(type_media = ifelse(is.na(type_media) & relation == "RT",type_media_original,type_media ))
  # Obtenemos los datos de  los usuarios que están en json
  users <- bind_tweets(temporal_file, user = TRUE, verbose = TRUE, output_format = NA)
  users_df <- users %>%
    # Eliminamos datos que no vamos a necesitar
    select(!c(entities)) %>% 
    # Expandimos 
    unnest(public_metrics) %>%
    # Re-nombramos para evitar conflicto de nombres con el join
    rename("account_created_at" = "created_at") %>%
    # Quitamos repetidos
    group_by(id) %>% slice(1)
  # Hacemos  un join de los tweets y los datos de los usuarios 
  tweets_users_df <- left_join(base_df,users_df,  by = c("author_id" = "id"))
  tweet_users_nor_df <-  tweets_users_df %>%
    # Re-nombramos columnas para que sean igual que en t-hoarder 
    rename(
      "id_tweet" = "tweet_id",
      "date" = "created_at",
      "author" = "username",
      "app" = "source",
      "id_user" = "author_id",
      "followers" = "followers_count",
      "following" = "following_count",
      "statuses" = "tweet_count",
      "favorite_count" = "like_count",
      "created_at" = "account_created_at",
      "avatar" = "profile_image_url"
    ) %>% 
    # añadimos el handle del usuario citado
    mutate(user_quoted = ifelse(relation == 'quoted',get_user_quoted_v(urls),NA )) %>%
    # Añadimos el link al tweet
    mutate(link = paste0("https://twitter.com/",author,"/status/",id_tweet)) %>%
    # Seleccionamos solo las columnas que nos interesan
    select(
      id_tweet,date,author,text,app,id_user,followers,following,statuses,
      location,urls,geolocation,name,description,url_media,type_media,quote,
      relation,replied_id,user_replied,retweeted_id,user_retweeted,quoted_id,user_quoted,
      first_HT,lang,created_at,verified,avatar, link,
      retweet_count,reply_count, quote_count, favorite_count,impression_count
    )  %>%
    mutate(link = paste0("https://twitter.com/",author,"/status/",id_tweet)) %>%
    # Le añadimos @ al author
    mutate(author = paste0('@',author)) %>% 
    # sustituimos los saltos de línea por espacios en text, name, location y description 
    mutate(text = str_replace_all(text, '[\n\r]+',' ')) %>%
    mutate(name = str_replace_all(name, '[\n\r]+',' ')) %>%
    mutate(location = str_replace_all(location, '[\n\r]+',' ')) %>%
    mutate(description = str_replace_all(description, '[\n\r]+',' ')) %>%
    # Quitamos filas repetidas por el join
    group_by(id_tweet) %>% slice(1) %>%
    # Ordenamos de más recientes a más antiguos
    arrange( desc(date))
  return(tweet_users_nor_df)
}
# shared functions for the standard API
parser_tweets_basic_API_academic <- function(tweets){  
  # Si no existe el dato "source, se lo añadimos con valor nulo"
  # en la API V2, no es accesible este dato desde el 21-12-2022
  if(is.null(tweets$source)){ 
    tweets <-  tweets %>%
      mutate(source = NA)
  }
  tweets_basic_nor_df <-  tweets %>%
    # Expandimos 
    unnest(public_metrics,keep_empty = TRUE)  %>%
    # Re-nombramos columnas para que sean igual que en t-hoarder 
    rename(
      "id_tweet" = "id",
      "date" = "created_at",
      "app" = "source",
      "id_user" = "author_id",
      "favorite_count" = "like_count"
    ) %>% 
    mutate(text = str_replace_all(text, '[\n\r]+',' ')) %>%
    select(
      id_tweet,date,text,app,id_user,lang, retweet_count,reply_count, quote_count, favorite_count,impression_count
    )  %>%
    arrange( desc(date))
  return(tweets_basic_nor_df)
}
parser_users_API_academic <- function(users, relation){  
users_nor_df <- users %>%
  unnest(public_metrics,keep_empty = TRUE) %>%
  rename (
    "id_user" = "id",
    "screen_name" = "username",
    "followers" = "followers_count",
    "following" = "following_count",
    "statuses" = "tweet_count",
    "lists" = "listed_count",
    "since" = "created_at",
    "bio" ="description"
  ) %>%
  mutate (
    relation = relation,
    time_zone = NA,
    web = NA,
    avatar = NA, 
    favorites_count = NA,
    lang = NA,
    timestamp = Sys.time ()
  ) %>%
  mutate(name = str_replace_all(name, '[\n\r]+',' ')) %>%
  mutate(location = str_replace_all(location, '[\n\r]+',' ')) %>%
  mutate(bio = str_replace_all(bio, '[\n\r]+',' ')) %>%
  select (
    id_user, screen_name, net, relation, followers,	following,statuses,
    lists, since,	name,	time_zone, location, web,	avatar,	bio,
    favorites_count, lang,	verified,	protected, timestamp
  )
  return (users_nor_df)
}