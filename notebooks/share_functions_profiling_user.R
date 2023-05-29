# Functions shared by spread_tweet.Rmd
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# Summary_tweets
#
# Bar chart de los tipos de tweets
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
Summary_tweets <- function(df) {
  p <- ggplot(
    data = df,
    aes(x = relation_ext)) + 
    geom_bar(
      aes(y=(..count..)/sum(..count..), fill = relation_ext),
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
    # Aplicamos color
    scale_fill_manual(values = color_relation) +
    # Ponemos los títulos
    labs(
      title = paste(base_title, ": summary"),
      x = "",
      y = "% de tweets",
      fill = "Tweet type"
    ) +
    my_theme() 
  return(p)
}
# Functions shared by spread_tweet.Rmd
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# daily_routine_app
#
# Scatterplot chart de la rutina de publicación by app
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
daily_routine_app <- function(df, date_ini, date_end, time_zone) {
  tweets_horario_df <- df %>% 
    filter(date >= date_ini & date <= date_end) %>%
    group_by(slot_time,hour_tweet,app) %>% 
    summarise(
      num_tweets = n(),
      .group = "drop"
    ) %>%
    ungroup()
  summary_tweets <- summary (df)
  p <- ggplot() + 
    geom_point(
      data = tweets_horario_df,
      aes(
        x = hour_tweet,
        y= slot_time,
        size = num_tweets,
        color = app 
      ),
      alpha =0.5) +
    scale_y_datetime(
      date_labels = format_time_plain(date_ini, date_end),
      date_breaks = time_scale(date_ini, date_end)
    ) +
    scale_x_continuous(
      breaks = seq(0,23,1),
      limits= c(0,23),
      sec.axis = dup_axis()
    ) +
    guides(color = guide_legend(nrow=2,override.aes = list(size = 4) ) ) +
    labs(
      title = paste0(base_title,": daily routine by app"),
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
# Functions shared by spread_tweet.Rmd
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# daily_routine
#
# Scatterplot chart de la rutina de publicación by relation
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
daily_routine <- function(df, date_ini, date_end, time_zone) {
  df <- df %>%
    filter(date >= date_ini & date <= date_end)
  tweets_horario_df <- df %>% 
    group_by(slot_time,hour_tweet,relation_ext) %>% 
    summarise(
      num_tweets = n(),
      .group = "drop"
    ) %>%
    ungroup()
  summary_tweets <- summary (df)
  p <- ggplot() + 
    geom_point(
      data = tweets_horario_df,
      aes(
        x = hour_tweet,
        y= slot_time,
        size = num_tweets,
        color = relation_ext
      ),
      alpha =0.5) +
    scale_y_datetime(
      date_labels = format_time_plain(date_ini, date_end),
      date_breaks = time_scale(date_ini, date_end)
    ) +
    scale_x_continuous(
      breaks = seq(0,23,1),
      limits= c(0,23),
      sec.axis = dup_axis()
    ) +
    guides(color = guide_legend(nrow=2,override.aes = list(size = 4) ) ) +
    # Aplicamos color
    scale_color_manual(
      values = color_relation,
      labels = paste(
        "<span style='color:",
        rev(color_relation),
        "'>",
        order_relation,
        "(",
        summary_tweets$percent,
        "%)",
        "</span>"), 
      drop = FALSE
    ) +
    labs(
      title = paste0(base_title,": daily routine"),
      subtitle = paste0("Time zone:", time_zone),
      x = "",
      y = "",
      color = "",
      size = "N. tweets") +
    my_theme() +
    theme(
      panel.grid.major.x = element_line(),
      legend.position="top",
      legend.text=element_markdown(size=12)
    )
  return(p)
}
# Functions shared by spread_tweet.Rmd
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# rhythm_week 
#
# heatmap de la rutina de publicación por dos unidades de tiempos
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
rhythm_week <- function(df, date_ini, date_end, time_zone) {
  df <- df %>%
    filter(date >= date_ini & date <= date_end)
  tweets_horario_df <- df %>% 
    mutate( slot_time_Y = lubridate::wday (date, label  = TRUE)) %>%
    mutate( slot_time_X = as.character (lubridate::hour (date))) %>%
    group_by(slot_time_Y,slot_time_X) %>% 
    summarise(
      num_tweets = n(),
      .group = "drop"
    ) %>%
    ungroup()
  tweets_horario_df$slot_time_Y <- factor (
    tweets_horario_df$slot_time_Y,
    levels = c("Mon","Tue","Wed","Thu","Fri","Sat", "Sun"))
  tweets_horario_df$slot_time_Y <- fct_rev(tweets_horario_df$slot_time_Y)
  tweets_horario_df$slot_time_X <- factor (
    tweets_horario_df$slot_time_X,
    levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12",
               "13","14","15","16","17","18","19","20","21","22","23"))
  p <- ggplot() + 
    geom_tile(
      data = tweets_horario_df,
      aes(x = slot_time_X, y = slot_time_Y, fill=num_tweets),
      color = "white")+
    scale_fill_gradient(low = "#DDEAFA", high = "#036DFA")+
    scale_x_discrete(
      expand =  c(0,0)
    ) +
    scale_y_discrete(
      expand =  c(0,0)
    ) +
    guides(color = guide_legend(nrow=2,override.aes = list(size = 4) ) ) +
    labs(
      title = paste0(base_title,": weekly rhythm"),
      subtitle = paste0("Time zone: ", time_zone),
      x = "Hour",
      y = "",
      fill = "N. tweets") +
    coord_fixed() +
    my_theme() +
    theme(
      legend.position="right",
      axis.title.y=element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    )
  return(p)
}
# Functions shared by spread_tweet.Rmd
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# rhythm_month 
#
# heatmap de la rutina de publicación por dos unidades de tiempos
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
rhythm_month <- function(df, date_ini, date_end, time_zone) {
  df <- df %>%
    filter(date >= date_ini & date <= date_end)
  tweets_horario_df <- df %>% 
    mutate( slot_time_Y = lubridate::month (date, label  = TRUE)) %>%
    mutate( slot_time_X = as.character (format (date,format ="%y"))) %>%
    group_by(slot_time_Y,slot_time_X) %>% 
    summarise(
      num_tweets = n(),
      .group = "drop"
    ) %>%
    ungroup()
  tweets_horario_df$slot_time_Y <- factor (
    tweets_horario_df$slot_time_Y,
    levels = c("Jan","Feb","Mar","Apr","May","Jun", "Jul","Aug","Sep","Oct","Nov","Dec"))
  tweets_horario_df$slot_time_Y <- fct_rev(tweets_horario_df$slot_time_Y)
  p <- ggplot() + 
    geom_tile(
      data = tweets_horario_df,
      aes(x = slot_time_X, y = slot_time_Y, fill=num_tweets),
      color = "white")+
    scale_fill_gradient(low = "#DDEAFA", high = "#036DFA")+
    scale_x_discrete(
      expand =  c(0,0)
    ) +
    scale_y_discrete(
      expand =  c(0,0)
    ) +
    coord_fixed() +
    guides(color = guide_legend(nrow=2,override.aes = list(size = 4) ) ) +
    labs(
      title = paste0(base_title,": monthly rhythm"),
      subtitle = paste0("Time zone: ", time_zone),
      x = "Year",
      y = "",
      fill = "N. tweets") +
    my_theme() +
    theme(
      legend.position="right",
      axis.title.y=element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
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
  df <- df %>%
    filter(date >= date_ini & date <= date_end)
  tweets_tipo_df <- df %>% 
    group_by(slot_time,relation_ext) %>%
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
  summary_tweets <- summary (df)
  p <- ggplot() + 
    geom_col(
      data = tweets_tipo_df, 
      aes(
        x = slot_time,
        y = num_tweets,
        fill = relation_ext
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
    scale_fill_manual(
      values = color_relation,
      labels = paste(
        "<span style='color:",
        rev(color_relation),
        "'>",
        order_relation,
        "(",
        summary_tweets$percent,
        "%)",
        "</span>"), 
      drop = FALSE
    ) +
    labs(
      title = paste0(base_title,": Tipo de tweets"),
      x = "",
      y = "Num. Tweets per day",
      fill=""
     ) +
    my_theme() +
    theme(
      legend.position="top",
      legend.text=element_markdown(size=12)
    )
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# daily_activity_lang
#
# line chart desglosado por tipo de pubicación
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
daily_activity_lang <- function(df, date_ini, date_end){
  max_langs <- 4
  # Limitamos el tiempo
  df <- df %>%
    filter(date >= date_ini & date <= date_end)
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
    group_by(slot_time,lang) %>%
    summarise(
      num_tweets = n(),
      .groups = "drop"
    ) %>%
    ungroup()
  #ordenamos los lenguajes de más a menor frecuencia
  order_langs <- unique(top_lang_df$lang %>% head(max_langs))
  tweets_lang_df$lang <- factor(tweets_lang_df$lang, levels = order_langs)
  tweets_tipo_tot_df <- tweets_df %>% 
    filter(date >= date_ini & date <= date_end) %>%
    group_by(slot_time) %>%
    summarise(
      num_tweets = n(),
      .groups = "drop"
    ) %>%
    ungroup()
  max_tweets <- max(tweets_tipo_tot_df$num_tweets,na.rm = TRUE)
  summary_tweets <- summary_lang (df, max_langs)
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
  p <- ggplot() + 
    geom_col(
      data = tweets_lang_df, 
      aes(
        x = slot_time,
        y = num_tweets,
        fill = lang
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
    scale_fill_manual(
      values = color_lang,
      labels = paste(
        "<span style='color:",
        rev(color_lang),
        "'>",
        order_langs,
        "(",
        summary_tweets$percent,
        "%)",
        "</span>"), 
      drop = FALSE
    ) +
    labs(
      title = paste0(base_title,": Tipo de tweets"),
      x = "",
      y = "Num. Tweets per day",
      fill=""
    ) +
    my_theme() +
    theme(
      legend.position="top",
      legend.text=element_markdown(size=12)
    )
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# topics_tweets
#
# line chart de doble escala con los tweet originales vs impacto
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

topics_tweets <- function(df, date_ini, date_end, topics){

  custom_stop_words <- bind_rows(
    stop_words,
    data_frame(word = tm::stopwords("spanish"),lexicon = "custom"),
    data_frame(word = tm::stopwords("catalan"),lexicon = "custom")
  )
  # extraemos los topics
  topics_df <- tweets_df %>%
    filter(date >= date_ini & date <= date_end) %>%
    mutate(text_plain = gsub('http\\S+\\s*',"",text)) %>% # Quitamos las URLs
    mutate(text_plain = gsub("RT @\\w+:","",text_plain)) %>% # Quitamos los RTs
    mutate(text_plain = gsub("&amp;","&",text_plain)) %>% # Rectificamos el &
    mutate(text_plain = gsub("@\\w+","",text_plain)) %>% # Quitamos las menciones
    select(slot_time, text_plain) %>%
    unnest_tokens(word, text_plain) %>% # Convertimos las frases en un conjunto de palabras
    anti_join(custom_stop_words) %>% # Eliminamos las stop words
    filter (word %in% topics) %>%
    group_by(word, slot_time) %>%
    summarise(nun_topics = n()) %>%
    mutate(cumulative_sum = cumsum(nun_topics))
  min_date <- min(topics_df$slot_time)
  max_date <- max(topics_df$slot_time)
  visible_dates <- as.POSIXct(seq(min_date,max_date, by = time_scale(date_ini, date_end)) )
  limit_x = as.POSIXct(c(min_date, max_date+( expand_time(date_ini, date_end, 50))))
  limit_y =  max(topics_df$cumulative_sum) 
  p <- ggplot() + 
    geom_line(
      data = topics_df, 
      aes(
        x = slot_time,
        y = cumulative_sum,
        color = word
      ),
      show.legend =FALSE,
      size =1, alpha  = 0.7)+
    geom_text_repel(
      data = topics_df %>% 
        top_n(1, cumulative_sum),
      aes(
        x = slot_time, y = cumulative_sum, color = word,
        label = paste0(
          word,
          "(",
          format(cumulative_sum, big.mark=".",decimal.mark=","),
          " ref.)")
      ), 
      vjust = 1,
      size = 4,
      nudge_x =  expand_time(min_date,max_date, 10), # Ajuste eje x
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
      date_labels = format_time(date_ini, date_end)
    ) +
    scale_y_continuous(
      labels = label_number_si(),
      limits= c(0,limit_y*1.3 ),
      expand= c(0,0)
    ) +
    labs(
      title = paste0(base_title,": Topics"),
      x = "",
      y = "Num. accumulated topics per day",
      color = ""
    ) +
    my_theme() +
    theme(
      legend.position="top"
    )
  return (p)
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
    filter(relation_ext != "RT") %>%
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
# engagement_tweets
#
# line chart de doble escala con los tweet originales vs impacto
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
engagement_tweets <- function(df, date_ini, date_end, indicator, my_color){
  tweets_engagement_df <- df %>% 
    filter(date >= date_ini & date <= date_end) %>%
    filter(relation_ext != "RT") %>%
    group_by(slot_time) %>%
    summarise(
      engagement = ifelse(
        sum(impression_count) > 0,
        sum(retweet_count)*100/sum(impression_count),
        0
      ),
      .groups = "drop"
    ) %>% 
    ungroup()
  max_engagement <- max(tweets_engagement_df$engagement,na.rm = TRUE)
  p <- ggplot(data = tweets_engagement_df) + 
    geom_line(
      aes(
        x=slot_time,
        y= engagement,
        color = "engagement"
      ),
      size =1.2,
      alpha=0.8)+
    geom_text(
      data = tweets_engagement_df %>%   
        top_n(1, engagement),
      aes(
        x = slot_time,
        y = engagement * 1.1, 
        label = paste(
          slot_time,
          "\n(",
          scales::comma(engagement,accuracy = 0.01),
          "engagement)"
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
      limits= c(0,max_engagement*1.3),
      expand= c(0,0)
    ) +
    labs(
      title = paste0(
        base_title,
        ": engagement per day"
      ),
      subtitle = "engagement = (Sum(RTs) * 100) / impresions", 
      x = "",
      y = "engagement per day",
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
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# reply_frequency
#
# Wordcloud con los términos más frecuentes
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
reply_frequency <- function(df, date_ini, date_end){
  corpus_menciones <- df %>%
    filter(date >= date_ini & date <= date_end) %>%
    # Extraemos los handles de los comentarios con una expresión regular "@\\w+"
    mutate(mentions = ifelse(relation == "reply",str_extract(text,"@\\w+"), NA)) %>%
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
      title = paste0(base_title,": Most frequent replies"),
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
