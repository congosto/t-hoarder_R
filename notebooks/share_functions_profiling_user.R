# Functions shared by spread_tweet.Rmd
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# daily_routine
#
# Scatterplot chart de la rutina de publicación
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
daily_routine <- function(df, date_ini, date_end, time_zone) {
tweets_horario_df <- df %>% 
  filter(date >= date_ini & date <= date_end) %>%
  group_by(slot_time,hour_tweet,relation) %>%
  summarise(
    num_tweets = n(),
    .group = "drop"
  ) %>%
  ungroup()
p <- ggplot() + 
  geom_point(
    data = tweets_horario_df,
    aes(
      x = hour_tweet,
      y= slot_time,
      size = num_tweets,
      color = relation
    ),
    alpha =0.7) +
  scale_y_datetime(
    date_labels = format_time_plain(date_ini, date_end),
    date_breaks = time_scale(date_ini, date_end)
  ) +
  scale_x_continuous(
    breaks = seq(0,23,1),
    sec.axis = dup_axis()
  ) +
  guides(color = guide_legend(nrow=2,override.aes = list(size = 4) ) ) +
  labs(
    title = paste0(base_title,": dayly routine"),
    subtitle = paste0("Time zone:", time_zone),
    x = "",
    y = "",
    color = "",
    size = "N. tweets") +
  my_theme() +
  theme(
    panel.grid.major.x = element_line(),
    legend.position="top"
  )
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# daily_activity
#
# line chart desglosado por tipo de pubicación
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
daily_activity <- function(df, date_ini, date_end){
  tweets_tipo_df <- tweets_df %>% 
    filter(date >= date_ini & date <= date_end) %>%
    group_by(slot_time,relation) %>%
    summarise(
      num_tweets = n(),
      .groups = "drop"
    ) %>%
    ungroup()
  tweets_tipo_tot_df <- tweets_df %>% 
    filter(date >= date_ini & date <= date_end) %>%
    group_by(slot_time) %>%
    summarise(
      num_tweets = n(),
      .groups = "drop"
    ) %>%
    ungroup()
  max_tweets <- max(tweets_tipo_tot_df$num_tweets,na.rm = TRUE)
  p <- ggplot() + 
    geom_col(
      data = tweets_tipo_df, 
      aes(
        x = slot_time,
        y = num_tweets,
        fill = relation
      ),
      size =1, alpha  = 0.7)+
    geom_text_repel(
      data = tweets_tipo_tot_df %>%   
        top_n(2, num_tweets),
      aes(
        x = slot_time,
        y = num_tweets * 1.1, 
        label = paste(slot_time,"\n(",num_tweets," tweets)")
      ),
      force = 10,
      max.time = 10,
      color = "grey50",
      size = 3.5,
      vjust = .5,
      segment.colour = "grey80",
      show.legend = FALSE
    ) +
    scale_x_datetime(
      date_labels = format_time(date_ini, date_end),
      date_breaks = time_scale(date_ini, date_end)
    ) +
    scale_y_continuous(
      labels = label_number_si(),
      limits= c(0,max_tweets*1.5),
      expan =c(0,0)
    ) +
    labs(
      title = paste0(base_title,": Tipo de tweets"),
      x = "",
      y = "Num. Tweets per day",
      fill=""
     ) +
    my_theme() +
    theme(legend.position="top")
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# impact_tweets
#
# line chart de doble escala con los tweet originales vs impacto
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
impact_tweets <- function(df, date_ini, date_end, indicator, my_color){
  tweets_impacto_df <- df %>% 
    filter(date >= date_ini & date <= date_end) %>%
    filter(relation != "RT") %>%
    group_by(slot_time) %>%
    summarise(
      num_tweets = n(),
      impact = 
        ifelse(indicator == "Fav", sum(favorite_count),
          ifelse(indicator == "RTs", sum(retweet_count),
            ifelse(indicator == "Replies", sum(reply_count),
              ifelse(indicator == "Quotes", sum(quote_count),
                ifelse(indicator == "Impresions",sum(impression_count), NA))))
      ),
      .groups = "drop"
    ) %>% 
    ungroup()
  max_tweets <- max(tweets_impacto_df$num_tweets,na.rm = TRUE)
  max_impact <- max(tweets_impacto_df$impact,na.rm = TRUE)
  ajuste_escala <- max_impact/max_tweets
  p <- ggplot(data = tweets_impacto_df) + 
    geom_col(
      aes(
        x=slot_time,
        y= num_tweets,
      ),
      fill = "#33E9FF", 
      alpha=0.4)+
      geom_line(
        aes(
          x=slot_time,
          y= num_tweets,
          color = "Num. original tweets"
      ),
      alpha=0.6
    ) +
    geom_line(
      aes(
        x=slot_time,
        y= impact/ajuste_escala,
        color = indicator
      ),
      size = 1.2
    )+
    geom_text_repel(
      data = tweets_impacto_df %>%   
        top_n(2, impact),
      aes(
        x = slot_time,
        y =(impact/ajuste_escala)*1.1, 
        label = paste(
          slot_time,
          "\n(",
          scales::comma(impact),
          indicator,
          ")"
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
    scale_x_datetime(
      date_labels = format_time(date_ini, date_end),
      date_breaks = time_scale(date_ini, date_end)
      ) +
    scale_y_continuous(
        labels = label_number_si(),
        limits= c(0,max_tweets*1.3),
        expand= c(0,0),
        sec.axis = sec_axis(trans=(~ . * ajuste_escala), name = paste(indicator, "per day"),
        labels = label_number_si() )) +
    scale_color_manual(values = my_color) +
    labs(
      title = paste0(base_title,": Tweets per day"," vs.",indicator),
      x = "",
      y = "Num. tweets per day",
      color=""
    ) +
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
# endgadgement_tweets
#
# line chart de doble escala con los tweet originales vs impacto
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
endgadgement_tweets <- function(df, date_ini, date_end, indicator, my_color){
  tweets_endgadgement_df <- df %>% 
    filter(date >= date_ini & date <= date_end) %>%
    filter(relation != "RT") %>%
    group_by(slot_time) %>%
    summarise(
      endgadgement = ifelse(
        sum(impression_count) > 0,
        sum(retweet_count)*100/sum(impression_count),
        0
      ),
      .groups = "drop"
    ) %>% 
    ungroup()
  max_endgadgement <- max(tweets_endgadgement_df$endgadgement,na.rm = TRUE)
  p <- ggplot(data = tweets_endgadgement_df) + 
    geom_line(
      aes(
        x=slot_time,
        y= endgadgement,
        color = "Endgadgement"
      ),
      size =1.2,
      alpha=0.8)+
    geom_text(
      data = tweets_endgadgement_df %>%   
        top_n(1, endgadgement),
      aes(
        x = slot_time,
        y = endgadgement * 1.1, 
        label = paste(
          slot_time,
          "\n(",
          scales::comma(endgadgement,accuracy = 0.01),
          "Endgadgement)"
        )
      ),
      nudge_x = 800, 
      nudge_y = 0.0001,
      size = 3.5,
    ) +
    scale_x_datetime(
      date_labels = format_time(date_ini, date_end),
      date_breaks = time_scale(date_ini, date_end)
    ) +
    scale_y_continuous(
      labels = label_number_si(),
      limits= c(0,max_endgadgement*1.3),
      expand= c(0,0)
    ) +
    labs(
      title = paste0(
        base_title,
        ": Endgadgement per day"
      ),
      subtitle = "endgadgement = (Sum(RTs) * 100) / impresions", 
      x = "",
      y = "Endgadgement per day",
      color=""
    ) +
    my_theme() +
    theme(
      legend.position="top"
    )
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# words_frequency
#
# Wordcloud con los términos más frecuentes
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
words_frequency <- function(df, date_ini, date_end){
  #Obtenemos las stop words en Inglés, español y catalán
  custom_stop_words <- bind_rows(
    stop_words,
    data_frame(word = tm::stopwords("spanish"),lexicon = "custom"),
    data_frame(word = tm::stopwords("catalan"),lexicon = "custom")
  )
  corpus_text <- df %>%
    filter(date >= date_ini & date <= date_end) %>%
    mutate(text_plain = gsub('http\\S+\\s*',"",text)) %>% # Quitamos las URLs
    mutate(text_plain = gsub("RT @\\w+:","",text_plain)) %>% # Quitamos los RTs
    mutate(text_plain = gsub("&amp;","&",text_plain)) %>% # Rectificamos el &
    mutate(text_plain = gsub("@\\w+","",text_plain)) %>% # Quitamos las menciones
    select(text_plain) %>%
    unnest_tokens(word, text_plain) %>% # Convertimos las frases en un conjunto de palabras
    anti_join(custom_stop_words) %>% # Eliminamos las stop words
    group_by(word) %>%     # Agrupamos por palabras   
    summarise(
      freq = n(),
      .groups = "drop"
     ) %>%  # Calculamos la frecuencia de cada palabra
    ungroup() %>%
    arrange(desc(freq)) %>% # Ordenamos de mayor a menor frecuencia de aparición
    head(50)
  paleta <- brewer.pal(8, "Dark2")
  p <- ggplot() +
    # Dibujamos la nube de palabras
    geom_text_wordcloud_area(
      data = corpus_text,
      aes(
        label = word,
        size = freq,
        color = freq
      ),
      angle = 0.35) +
    # Definimos la proporción del tamaño de las letras según frecuencia
    geom_text_wordcloud_area(rm_outside = TRUE) +
    geom_text_wordcloud_area(area_corr_power = 1) +
    scale_radius(range = c(4, 20), limits = c(0, NA)) +
    # Aplicamos una paleta de color
    scale_color_gradientn(colors = paleta) +
    # Definimos el título principal y el de los ejes
    labs(
      title = paste0(base_title,": Most frequent words"),
      subtitle = paste0(
        "Period: ",
        as.Date(date_ini,"%Y-%m-%d"),
        " - ",
        as.Date(date_end,"%Y-%m-%d")
      ),
      x = "",
      y = "",
      color=""
    ) +
  # Aplicamos un template minimalista
    my_theme() +
    theme ( panel.border = element_blank())
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# retweets_frequency
#
# Wordcloud con los términos más frecuentes
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
retweets_frequency <- function(df, date_ini, date_end){
  corpus_menciones <- df %>%
    filter(date >= date_ini & date <= date_end) %>%
    # Extraemos los handles de los comentarios con una expresión regular "@\\w+"
    mutate(mentions = ifelse(relation == "RT",str_extract(text,"@\\w+"), NA)) %>%
    # Eliminamos las filas que están vacías 
    filter(!is.na(mentions)) %>%
    select(mentions) %>%  # Seleccionamos las menciones y periodo
    group_by(mentions) %>%
    summarise(
      freq = n(),
      .groups = "drop"
    ) %>% # Calculamos la frecuencia de cada palabra
    ungroup() %>%
    arrange(desc(freq)) %>% # Ordenamos de mayor a menor frecuencia de aparición
    head(30)
  paleta <- brewer.pal(8, "Dark2")
  p <- ggplot() +
    # Dibujamos la nube de palabras
    geom_text_wordcloud_area(
      data = corpus_menciones,
      aes(
        label = mentions,
        size = freq,
        color = freq
      ),
      angle = 0.35) +
    # Definimos la proporción del tamaño de las letras según frecuencia
    geom_text_wordcloud_area(rm_outside = TRUE) +
    geom_text_wordcloud_area(area_corr_power = 1) +
    scale_radius(range = c(4, 20), limits = c(0, NA)) +
    # Aplicamos una paleta de color
    scale_color_gradientn(colors = paleta) +
    # Definimos el título principal y el de los ejes
    labs(
      title = paste0(base_title,": Most frequent retweets"),
      subtitle = paste0(
        "Period: ",
        as.Date(date_ini,"%Y-%m-%d"),
        " - ",
        as.Date(date_end,"%Y-%m-%d")
      ),
      x = "",
      y = "",
      color=""
    ) +
    # Aplicamos un template minimalista
    my_theme() +
    theme ( panel.border = element_blank())
  return(p)
}
