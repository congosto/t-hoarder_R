# Functions shared by visualization notebooks
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# my_theme
#
# Plantilla para las gráficas
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
my_theme <- function(base_size = 11){
  # Establecemos la proporción del tamaño de la letra de cada uno de los elementos
  template <- 
    theme_bw() +  # tema minimalista de fondo blanco
    theme(
      legend.position="right",
      plot.title=element_text(size=base_size + 4, vjust = 1.25, hjust = 0.5),
      plot.subtitle=element_text(size=base_size + 1, hjust = 0.5),
      plot.caption = element_text(hjust = 1),
      plot.margin = margin(
        t = 0.5,  # Margen superior
        r = 6,  # Margen derecho
        b = 0.5,  # Margen inferior
        l = 6
      ),
      text = element_text(size=base_size + 2),
      axis.text.x=element_text(size=base_size + 1),
      axis.text.y=element_text(size=base_size + 1),
      axis.title.x=element_text(
        size=base_size + 2,
        margin = unit(c(3, 0, 0, 0), "mm")
      ),
      axis.title.y=element_text(
        size=base_size + 2,
        margin = unit(c(0, 3, 0, 0), "mm")
      ),
      axis.title.y.right=element_text(
        size=base_size + 2,
        margin = unit(c(0, 0, 0, 3), "mm")
      ),
      strip.text = element_text(size=base_size + 2),
      strip.text.x = element_text(size=base_size + 2),
      strip.text.y = element_text(size=base_size + 2),
      strip.background =  element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
    )
  return(template)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# time_scale
#
# Escala de tiempo para las fechas del eje x
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
time_scale <- function(ini_date,end_date) {
  num_days <- as.numeric(difftime(end_date ,ini_date , units = c("days")))
  num_years <- num_days / 365
  num_months <- num_days / 30
  num_weeks <- num_days / 7
  num_hours <- num_days * 24
  spaced_dates <- case_when(
    num_years >= 10 ~ "years",
    num_months >= 12 ~ paste(as.integer((num_months+12)/12), "months"),
    num_weeks >= 25 ~ paste(as.integer((num_weeks+7)/7), "weeks"),
    num_days >= 4 ~ paste(as.integer((num_days+10)/10), "days"),
    num_hours >= 0 ~ paste(as.integer((num_hours+6)/6), "hours") 
) 
  return(spaced_dates)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# format_time
#
# Formato para las fechas del eje x
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
format_time <- function(ini_date,end_date) {
  num_days <- as.numeric(difftime(end_date ,ini_date , units = c("days")))
  num_years <- num_days / 365
  num_months <- num_days / 30
  num_weeks <- num_days / 7
  num_hours <- num_days * 24
  format_dates <- case_when(
    num_years >= 10 ~ "%Y",
    num_months >= 12 ~ "%b\n%Y",
    num_weeks >= 25 ~ "%d-%b\n%Y",
    num_days >= 4 ~ "%d-%b\n%Y",
    num_hours >= 0 ~ "%H:00\n%d-%b" 
  )
  return(format_dates)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# format_time_2
#
# Formato para las fechas del eje x
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
format_time_plain <- function(ini_date,end_date) {
  num_days <- as.numeric(difftime(end_date ,ini_date , units = c("days")))
  num_years <- num_days / 365
  num_months <- num_days / 30
  num_weeks <- num_days / 7
  num_hours <- num_days * 24
  format_dates <- case_when(
    num_years >= 10 ~ "%Y",
    num_months >= 12 ~ "%b-%Y",
    num_weeks >= 25 ~ "%d-%b-%Y",
    num_days >= 4 ~ "%d-%b-%Y",
    num_hours >= 0 ~ "%H:00-%d-%b" 
  )
  return(format_dates)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# expand_time
#
# Aumentar el tamaño del eje X
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
expand_time <- function(ini_date,end_date, percentage) {
  num_days <- as.numeric(difftime(end_date ,ini_date , units = c("days")))
  num_seconds <- num_days * 24 * 60 * 60
  expand <- (num_seconds * percentage)/100
  return(expand)
}
