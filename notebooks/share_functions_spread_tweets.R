# Functions shared by spread_tweet.Rmd
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# Summary_tweets
#
# Bar chart de los tipos de tweets
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
Summary_tweets <- function(df) {
  resumen <- df %>%
    mutate(relation = ifelse(is.na(relation), "original",relation)) 
  p <- ggplot(
    data = resumen,
    aes(x = relation)) + 
    geom_bar(
      aes(y=(..count..)/sum(..count..), fill = relation),
      stat="count",
      alpha = 0.7
    ) +
    geom_text(
      aes(
        label = paste0(round(..count../sum(..count..)*100,1),"%"),
        y= ..count../sum(..count..)), 
      size =4.5,
      stat="count",vjust = -0.5
    ) + 
    scale_y_continuous(
      labels=scales::percent_format(scale = 100, accuracy = 1),
      expand= c(0,0,0.2,0)
    ) +
    # Ponemos los títulos
    labs(
      title = paste(base_title, ": summary"),
      x = "",
      y = "% de tweets"
    ) +
    my_theme() 
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# tweets_vs_reach
#
# Chart line de doble escala del total tweets vs alcance de los mismos
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
tweets_vs_reach <- function(df, periodo,  ini_date, end_date) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  # Agrupamos los tweets por hora y calculamos el alcance
  tweets_vs_reach_df <- df %>% 
    group_by(slot_time) %>%
    summarise(
      num_tweets = n(), 
      reach = sum(followers,na.rm=T),
      .groups = 'drop'
    ) %>% 
    ungroup() 
  # Buscamos los influencers de cada una de las horas
  tweets_vs_influencer_df <- df %>% 
    group_by(slot_time) %>%
    summarise(
      reach = sum(followers,na.rm=T),
      influencer = ifelse(followers >= min_followers_influencers, author, NA),
      .groups = 'drop'
    ) %>%
    ungroup() %>%
    distinct(slot_time, reach, influencer)
  # Calculamos las dos escalas
  max_tweets <- max(tweets_vs_reach_df$num_tweets,na.rm = TRUE)
  max_reach <- max(tweets_vs_reach_df$reach,na.rm = TRUE)
  ajuste_escala <- max_reach/max_tweets
  limit_y = max_tweets
  #definimos la paleta de color
  my_color = c("Num. Tweets"= "#33E9FF", "Reach" = "red4")
  p <- ggplot(data = tweets_vs_reach_df) + 
    # Pintamos la evolución de los tweets
    geom_line(
      aes( x = slot_time, y = num_tweets,  color ="Num. Tweets"),
      size = 1.3
    ) +
    geom_area(
      aes( x = slot_time, y= num_tweets),
      fill ="#33E9FF",
      alpha = 0.4) +
    # Pintamos los la evolución del alcance  
    geom_line(
      aes( x=slot_time,  y= reach/ajuste_escala, color="Reach"),
      alpha = 1,
      size =1.2) +
    # Pintamos los influencers
    geom_text_repel(
      data =tweets_vs_influencer_df,
      aes( x = slot_time,  y = reach/ajuste_escala,  label = influencer), 
      ylim = c(0, limit_y*1.5),
      force = 10,
      max.overlaps = 30,
      max.time = 10,
      color = "grey50",
      size = 3.5,
      vjust = .5,
      segment.colour = "grey80",
      show.legend = FALSE
    ) +
    # Ajustamos la escala de tiempo
    scale_x_datetime(
      date_labels = format_time(ini_date, end_date),
      date_breaks = time_scale(ini_date, end_date)
    ) +
    #Ajustamos la doble escala
    scale_y_continuous(
      name = paste("Num. Tweets per",slot_time), 
      labels = label_number_si(),
      limits= c(0,limit_y*1.6 ),
      expand= c(0,0),
      sec.axis = sec_axis(
        trans=(~ . * ajuste_escala), 
        name = paste("Reach per", slot_time),
        labels = label_number_si() 
      )
    ) +
    # Aplicamos color
    scale_color_manual(values = my_color) +
    # Ponemos los títulos
    labs(
      title = paste(base_title, ": Tweets per",slot_time, "vs. Reach"),
      subtitle = paste(
        "Tagged profiles with more than",
        format(min_followers_influencers,
                big.mark=".", scientific=FALSE),
        "followers"
      ),
      x = "", 
      color=""
    ) +
    # Aplicamos template
    my_theme() +
    theme( legend.position="top",
           axis.title.y = element_text(color = "steelblue4", size = 14),
           axis.title.y.right = element_text(color = "red4", size = 14),
           axis.text.y = element_text(color = "steelblue4"),
           axis.text.y.right = element_text(color = "red4")
    )
  
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# tweets_vs_RTs
#
# Chart line de doble escala del total tweets vs RTs recibidos
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
tweets_vs_RTs <- function(df, periodo,  ini_date, end_date) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  # Agrupamos los tweets por hora y calculamos los RT/hora
  tweets_RT_df <- df %>% 
    filter(relation == "RT") %>%
    group_by(slot_time) %>%
    summarise(
      num_tweets = n(),
      .groups = 'drop'
    ) %>% 
    ungroup() 
  # Agrupamos los tweets por hora y calculamos los mensajes originales/hora
  tweets_original_df <- df %>% 
    filter(relation != "RT" | is.na(relation)) %>%
    group_by(slot_time) %>%
    summarise(
      num_tweets = n(),
      .groups = 'drop'
    ) %>% 
    ungroup()  
  # Calculamos las dos escalas
  max_tweets <- max(tweets_original_df$num_tweets,na.rm = TRUE)
  max_RT <- max(tweets_RT_df$num_tweets,na.rm = TRUE)
  ajuste_escala <- max_RT/max_tweets
  #definimos la paleta de color
  my_color = c("Num. original tweets"= "#33E9FF", "RTs" = "red4")
  p <- ggplot() + 
    # Pintamos la evolución de los tweets originales/hora
    geom_line(
      data = tweets_original_df,
      aes(x = slot_time, y = num_tweets, color = "Num. original tweets"),
      size =1.3
    ) +
    geom_area(
      data = tweets_original_df,
      aes( x=slot_time, y= num_tweets), fill = "#33E9FF", 
      alpha=0.4
    ) +
    # Pintamos los la evolución de los RTs/hora
    geom_line(
      data = tweets_RT_df,
      aes( x = slot_time, y = num_tweets/ajuste_escala, color="RTs"),
      size =1.3)+
    # Anotamos el máximo de tweets originales/hora
    geom_text_repel(
      data = tweets_original_df %>% top_n(1, num_tweets),
      aes(
        x = slot_time, y = num_tweets * 1.2, 
        label = paste0(
          slot_time,
          "\n",
          "Max. original tweets = ",scales::comma(num_tweets)
        )
      ),
      force = 10,
      max.time = 10,
      color = "grey50",
      size = 3.5,
      vjust = .5,
      segment.colour = "grey80",
      show.legend = FALSE
    ) +
    # Anotamos el máximo de RTs/hora
    geom_text_repel(
      data = tweets_RT_df %>%  top_n(1, num_tweets),
      aes(
        x = slot_time,  y = num_tweets/ajuste_escala *1.2, 
        label = paste0(
          slot_time,
          "\n",
          "Max.RTs = ",scales::comma(num_tweets)
        )
      ),
      force = 10,
      max.time = 10,
      color = "grey50",
      size = 3.5,
      vjust = .5,
      segment.colour = "grey80",
      show.legend = FALSE
    ) +
    # Ajustamos la escala de tiempo
    scale_x_datetime(
      date_labels = format_time(ini_date, end_date),
      date_breaks = time_scale(ini_date, end_date)
    ) +
    # Ajustamos la doble escala
    scale_y_continuous(
      name = paste("Num. Original tweets per",slot_time), 
      labels = label_number_si(),
      limits= c(0,max_tweets*1.3),
      expand= c(0,0),
      sec.axis = sec_axis(
        trans=(~ . * ajuste_escala), 
        name = paste("RTs per",slot_time),
        labels = label_number_si() )
    ) +
    # Aplicamos color
    scale_color_manual(values = my_color) +
    # Ponemos los títulos
    labs(
      title =  paste(base_title, ": Tweets per",slot_time,"vs. RTs"),
      x = "", 
      color=""
    ) +
    # Aplicamos template
    my_theme() +
    theme(
      legend.position="top",
      axis.title.y = element_text(color = "steelblue4", size = 14),
      axis.title.y.right = element_text(color = "red4", size = 14),
      axis.text.y = element_text(color = "steelblue4"),
      axis.text.y.right = element_text(color = "red4")
    )
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# tweets_by_lang
#
# Chart line desglosado por idiomas
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
tweets_by_lang <- function(df, periodo,  ini_date, end_date) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  # Definimos los colores de los lenguajes por defecto
  color_lang <- c(
    "es" = "#FF0000",
    "ca" = "#FFFF00",
    "eu" = "#5EFF2E",
    "en" = "#19C3FF",
    "fr" = "#0070C0",
    "de" = "#005426",
    "pt" = "#FF9933",
    "it" = "#00B050",
    "und" = "#808080",
    "qme" = "#808080",
    "zxx" = "#808080",
    "qht" = "#808080"
  )
  # Calculamos el top de lenguajes
  top_lang_df <- df %>%
    # Agrupamos por lenguaje y contamos cuantos tweets hay en cada uno de ellos
    group_by(lang) %>%
    summarise(
      num_tweets = n(),
      .groups ="drop") %>%
    ungroup() %>%
    arrange(desc( num_tweets))
  tweets_lang_df <- df %>%
    # Agrupamos por lenguaje y contamos cuantos tweets hay en cada slot de tiempo
    group_by(slot_time,lang) %>%
    summarise(
      num_tweets = n(),
      .groups ="drop") %>%
    ungroup() %>%
    arrange(desc( num_tweets))
  #ordenamos los lenguajes de más a menor frecuencia
  order_langs <- unique(top_lang_df$lang %>% head(max_langs))
  tweets_lang_df$lang <- factor(tweets_lang_df$lang, levels = order_langs)
  # Generamos la gráfica de tweets por lenguaje
  p <- ggplot(tweets_lang_df) + 
    # Pintamos la evolución por lenguajes
    geom_line(
      aes(x = slot_time, y = num_tweets, color = lang), 
      size = 1.2,
      alpha = 1
    ) +
    # Ponemos los títulos
    labs(
      title = paste(base_title, ": Tweets by language"),
      x = "",
      y = paste("Num tweets per",slot_time),
      color = "Lang"
    ) +
    # Ajustamos la escala de tiempo
    scale_x_datetime(
      date_labels = format_time(ini_date, end_date),
      date_breaks = time_scale(ini_date, end_date)
    ) +
    # Ajustamos el eje y
    scale_y_continuous(
      limits= c(0, tweet_peak * 1.1),
      expand= c(0,0),
      labels = label_number_si()
    ) +
    #coloreamos según el fichero
    scale_color_manual(values = color_lang) +
    # Aplicamos template
    my_theme() +
    theme(
      legend.position="right",
      legend.key.size = unit(0.5, 'cm'),
      axis.title.y = element_text(vjust = +4),
      panel.grid.major.x = element_blank(),
      axis.ticks.x = element_line(),
    )
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# tweets_by_HT
#
# Chart line desglosado por hashtags
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
tweets_by_HT <- function(df, periodo,  ini_date, end_date) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  # Definimos los colores de los hashtags
  hashtags_color <- c(
    "#CC66FF",
    "#92D050",
    "#00B0F0",
    "#404040",
    "#FF9900",
    "#FF5050",
    "#00D67F",
    "#F8CBAD",
    "#8A2E00",
    "#993366",
    "#0033CC",
    "#008E55",
    "#45682D",
    "#702500"
  )
  # Calculamos el top de hashtags
  top_hashtags <- df %>% 
    # Eliminamos tweets sin Hashtags
    filter(!is.na(first_HT)) %>%
    # Agrupamos por lenguaje y contamos cuantos tweets hay en cada una de ellos
    group_by(first_HT) %>%
    summarise(
      num_tweets = n(),
      .groups ="drop"
    ) %>%
    ungroup() %>%
    arrange(desc(num_tweets)) %>%
    head(max_hashtags) %>%
    select(first_HT) 
  # Preparamos los datos para la gráfica
  tweets_hashtags_df <- df %>% 
    # Eliminamos tweets sin Hashtags
    filter(first_HT %in% top_hashtags$first_HT) %>%
    # Agrupamos por lenguaje y contamos cuantos tweets hay en cada una de ellos en cada slot time
    group_by(slot_time,first_HT) %>%
    summarise(
      num_tweets = n(),
      .groups ="drop"
    ) %>%
    ungroup()
  #ordenamos los lenguajes de más a menor frecuencia
  tweets_hashtags_df$first_HT <- factor(tweets_hashtags_df$first_HT, levels = top_hashtags$first_HT)
  # Generamos la gráfica de evolución de los hashtags
  p <- ggplot(data = tweets_hashtags_df) + 
    # Pintamos la evolución por lenguajes
    geom_col(
      aes(x = slot_time, y = num_tweets, fill = first_HT), 
      alpha = 1
    )+
    # Ponemos los títulos
    labs(
      title = paste(base_title, ": Tweets by hashtag"),
      x = "",
      y = paste("Num tweets per",slot_time),
      fill = "hashtags"
    ) +
    # Ajustamos la escala de tiempo
    scale_x_datetime(
      date_labels = format_time(ini_date, end_date),
      date_breaks = time_scale(ini_date, end_date)
    ) +
    # Ajustamos el eje y
    scale_y_continuous(
      limits= c(0,tweet_peak*1.1),
      expand= c(0,0),
      labels = label_number_si()
    ) +
    #coloreamos 
    scale_fill_manual(values = hashtags_color) +
    # Aplicamos template
    my_theme() +
    theme(
      legend.position="right",
      legend.key.size = unit(0.5, 'cm'),
      axis.title.y = element_text(vjust = +4),
      panel.grid.major.x = element_blank(),
      axis.ticks.x = element_line()
    )
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# tweets_by_community 
#
# Bar line desglosado por grupos o comunidades
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
tweets_by_community <-  function(df, periodo,  ini_date, end_date, communities) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  top_community <- communities$community
  top_community_names <- communities$name_community
  top_community_color <- communities$color
  # Seleccionamos las comunidades del top
  tweets_communities_df <- df %>% 
    filter(community %in%  top_community) %>%
    #agrupamos por comunidad y contamos cuantos tweets hay en cada una de ellas en cada hora
    group_by(slot_time,community) %>%
    summarise(
      num_tweets = n(),
      .groups ="drop" )%>%
    ungroup()
  # Ordenamos las comunidades como aparecen en el fichero
  tweets_communities_df$community <- factor(tweets_communities_df$community, levels = top_community)
  # Generamos la gráfica por comunidades
  p <- ggplot(tweets_communities_df) + 
    # Pintamos la evolución por comunidades
    geom_col(
      aes(x = slot_time, y = num_tweets, fill = community),
      alpha = 0.7
    )+
    # Ponemos los títulos
    labs(
      title =paste(base_title, ": Tweets by group"),
      x = "",
      y = paste("Num tweets per",slot_time),
      fill = "Groups"
    ) +
    # Ajustamos la escala de tiempo
    scale_x_datetime(
      date_labels = format_time(ini_date, end_date),
      date_breaks = time_scale(ini_date, end_date)
    ) +
    # Ajustamos el eje y
    scale_y_continuous(
      limits= c(0,tweet_peak*1.1),
      expand= c(0,0),
      labels = label_number_si()
    ) +
    #coloreamos según el fichero
    scale_fill_manual(
      values = top_community_color,
      labels = top_community_names
    ) +
    # Aplicamos template
    my_theme() +
    theme(
      legend.position="right",
      legend.key.size = unit(0.4, 'cm'),
      axis.title.y = element_text(vjust = +4),
      panel.grid.major.x = element_blank(),
      axis.ticks.x = element_line(),
      legend.text = element_text(size=11)
    ) 
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# accumulated_sites
#
# Char line acumulado de los sitios referenciados en los tweets
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
accumulated_sites <-  function(df, ini_date, end_date) {
  df <- df %>%
  filter(relation != "RT" | is.na(relation)) %>%
  mutate(site = str_extract(urls,"https://[www]*[.\\w-]*"))  %>%
  filter(
    site != "https://twitter.com" &
      site != "https://ift.tt" &
      site != "https://t.co" &
      site != "https://bit.ly") %>% 
  group_by(site,slot_time) %>% 
  summarise(
    tweets_count = n()+ sum(retweet_count)
  ) %>%
  mutate(cumulative_sum = cumsum(tweets_count)) 
visible_dates <- as.POSIXct(seq(min(tweets_df$slot_time),max(tweets_df$slot_time), by = time_scale(ini_date, end_date)) )
limit_x = as.POSIXct(c(min(df$slot_time), max(df$slot_time)+( expand_time(ini_date, end_date, 40))))
limit_y =  max(df$cumulative_sum) 
offset <- 2
max_date <- max(df$slot_time)
p <- ggplot() + 
  geom_path(
    data = df,  
    aes(x = slot_time, y = cumulative_sum, color = site),
    size =1.3,
    show.legend =FALSE
  ) +
  geom_text_repel(
    data = df %>% 
      top_n(1, cumulative_sum) %>%
      filter(cumulative_sum > 100),
    aes(
      x = slot_time, y = cumulative_sum, color = site,
      label = paste0(
        site,
        "(",
        format(cumulative_sum, big.mark=".",decimal.mark=","),
        " ref.)")
    ), 
    vjust = 1,
    size = 4,
    nudge_x = 40*24*60*60, # Ajuste eje x
    nudge_y = 0.005,  # Ajuste eje y
    direction = "y",
    max.overlaps = 30,
    segment.size = 0.5,
    segment.linetype = 2,
    show.legend =FALSE
  )+
  scale_x_datetime(
    limits=limit_x,
    breaks = visible_dates,
    date_labels = format_time(ini_date, end_date)
  ) +
  scale_y_continuous(
    labels = label_number_si(),
    limits= c(0,limit_y*1.3 ),
    expand= c(0,0)
  ) +
  labs(
    title = paste0(base_title, ": cumulative site mentions per ",slot_time),
    x = "", 
    y = "cumulative referrals",
    color=""
  ) +
  guides(color=guide_legend(ncol=2)) +
  my_theme() +
  theme( legend.position="top")
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# location_users_by_community
#
# Word cloud de las localizaciones de los autores en una rejilla por grupo
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
location_users_by_community <- function(df, communities ) {
  # Ordenamos por comunidad de más a menos miembros
  top_community <- communities$community
  top_community_color <- communities$color
  communities$name_community <- factor(communities$name_community,levels = communities$name_community )
  # Obtenemos las stop words en Inglés, español y catalán
  custom_stop_words <- bind_rows(
    stop_words,
    data_frame(word = tm::stopwords("spanish"),lexicon = "custom"),
    data_frame(word = tm::stopwords("catalan"),lexicon = "custom")
  )
  # Extraemos el corpus de texto de la columna "location" para generar el wordcloud 
  corpus_text <- tweets_df %>%
    # Le añadimos el nombre de la comunidad
    left_join(communities, by  = "community") %>%
    # Dejamos solo las comunidades con más miembros
    filter(community %in%  top_community) %>%
    # Seleccionamos las columnas location y name_community
    select(location, name_community) %>%
    unnest_tokens(word, location) %>% # Convertimos las frases en un conjunto de palabras
    anti_join(custom_stop_words) %>% # Eliminamos las stop words
    filter(!is.na(word)) %>% #eliminamos lo valores nulos
    group_by(word, name_community) %>% # Agrupamos por palabras   
    summarise(
      freq = n(),   # Obtenemos la frecuencia de cada palabra
      .groups = "drop"
    ) %>% 
    ungroup() %>%
    arrange(desc(freq)) %>% # Ordenamos de mayor a menor frecuencia de aparición
    group_by(name_community) %>% # Agrupamos por name_community
    top_n(n = 15, freq)  # Obtenemos las 15 localizaciones más frecuentes de cada comunidad
  # Generamos los colores para para que las facetas tengan el color de la comunidad
  strip <- strip_themed(
    background_x = elem_list_rect(
      fill = top_community_color,
      color  = top_community_color
    )
  )
  # Generamos los colores para las palabras según frecuencia
  paleta <- brewer.pal(8, "Dark2")
  # Pintamos la nube de palabras
  p <- ggplot() +
    # Dibujamos la nube de palabras
    geom_text_wordcloud_area(
      data = corpus_text,
      aes(label = word, group = name_community, size = freq, color = freq),
      angle = 0.35
    ) +
    # Definimos la proporción del tamaño de las letras según frecuencia
    geom_text_wordcloud_area(rm_outside = TRUE) +
    geom_text_wordcloud_area(area_corr_power = 1) +
    scale_radius(range = c(4, 20), limits = c(0, NA)) +
    # Aplicamos una paleta de color
    scale_color_gradientn(colors = paleta) +
    # Definimos el título principal y el de los ejes
    labs(
      title = paste(base_title,": Locations by group"),
      x = "", y = "",
      color=""
    ) +
    # Desdoblamos la gráfica en facetas según la comunidad
    facet_wrap2(~name_community, ncol = 2, strip = strip ) +
    # Aplicamos un template minimalista
    my_theme() +
    # Ponemos la letras del título de las facetas de color blanco
    theme(strip.text = element_text(color  = "white"))
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# words_frequency_by_community
#
# Word cloud de las localizaciones de los autores en una rejilla por grupo
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
words_frequency_by_community <- function(df, communities ) {
  # Ordenamos por comunidad de más a menos miembros
  top_community <- communities$community
  top_community_color <- communities$color
  communities$name_community <- factor(communities$name_community,levels = communities$name_community )
  #Obtenemos las stop words en Inglés, español y catalán
  custom_stop_words <- bind_rows(
    stop_words,
    data_frame(word = tm::stopwords("spanish"),lexicon = "custom"),
    data_frame(word = tm::stopwords("catalan"),lexicon = "custom")
  )
  # Extraemos el corpus de texto de la columna "text" para generar el wordcloud 
  corpus_text <- tweets_df %>%
    # Le añadimos el nombre de la comunidad
    left_join(communities, by  = "community") %>%
    # Dejamos solo las comunidades con más miembros
    filter(community %in%  top_community) %>%
    # Extraemos las URL y los handles  de perfiles del texto del tweet
    mutate(text_plain = gsub('http\\S+\\s*',"",text)) %>% # Quitamos las URLs
    mutate(text_plain = gsub("RT @\\w+:","",text_plain)) %>% # Quitamos los RTs
    mutate(text_plain = gsub("&amp;","&",text_plain)) %>% # Rectificamos el &
    mutate(text_plain = gsub("@\\w+","",text_plain)) %>% # Quitamos las menciones
    select(text_plain,name_community) %>%
    unnest_tokens(word, text_plain) %>% # Convertimos las frases en un conjunto de palabras
    anti_join(custom_stop_words) %>% # Eliminamos las stop words
    group_by(word,name_community) %>%  # Agrupamos por palabras   
    summarise(
      freq = n(), # Calculamos la frecuencia de cada palabra
     .group = "drop"
    ) %>%  
    ungroup() %>%
    arrange(name_community,desc(freq)) %>% # Ordenamos de mayor a menor frecuencia de aparición
    group_by(name_community) %>% # Agrupamos por name_community
    top_n(n = 15, freq)  # Obtenemos las 15 palabras más frecuentes de cada comunidad
  # Generamos los colores para para que las facetas tengan el color de la comunidad
  strip <- strip_themed(
    background_x = elem_list_rect(
      fill = top_community_color,
      color  = top_community_color
    )
  )
  # Generamos los colores para las palabras según frecuencia
  paleta <- brewer.pal(8, "Dark2")
  # Pintamos la nube de palabras
  p <- ggplot() +
    # Dibujamos la nube de palabras
    geom_text_wordcloud_area(
      data = corpus_text,
      aes(label = word, group = name_community, size = freq, color = freq),
      angle = 0.35
    ) +
    # Definimos la proporción del tamaño de las letras según frecuencia
    geom_text_wordcloud_area(rm_outside = TRUE) +
    geom_text_wordcloud_area(area_corr_power = 1) +
    scale_radius(range = c(4, 20), limits = c(0, NA)) +
    # Aplicamos una paleta de color
    scale_color_gradientn(colors = paleta) +
    # Definimos el título principal y el de los ejes
    labs(
      title = paste(base_title,": Most frequent words by group"),
      x = "", y = "",
      color=""
    ) +
    # Desdoblamos la gráfica según el periodo previo o posterior de la compra
    facet_wrap2(~name_community, ncol = 2, strip = strip ) +
    # Aplicamos un template minimalista
    my_theme() +
    theme(strip.text = element_text(color  = "white"))
  return(p)
}