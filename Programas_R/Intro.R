
# ---------------------------------------------------------------------#
# Inicio de programa ----
# ---------------------------------------------------------------------#

# llamado de librerias ----

#install.packages("pacman")
library(pacman)
p_load(xts, lubridate, lmtest,readxl,sandwich, ggridges, ggplot2, forecast, GGally)

# Lectura de infomracion ----

Datos_ent <- read_xlsx(file.choose(),sheet = "Base_LR",range = "a3:j200"
                       ,col_names = T)

tail(Datos_ent,10)
head(Datos_ent,10)

# Conversion objeto serie de tiempo ----

Datos_ent_ts1 <- ts(Datos_ent[,-1],start = c(2006,1),frequency = 12)  
tail(Datos_ent_ts1)
class(Datos_ent_ts1)

Datos_ent_ts2 <- xts(Datos_ent[,-1],order.by = as.Date(Datos_ent$Fecha))
class(Datos_ent_ts2)


# Calculos de los retornos ----

ret_log_YoY <- diff(log(Datos_ent_ts1),lag = 12,differences = 1)
View(ret_log_YoY)

# variacion anual

var_YoY <- round((Datos_ent[13:nrow(Datos_ent),-1]/Datos_ent[1:(nrow(Datos_ent)-12),-1]-1)*100,2)
tail(var_YoY)


# Algunos graficos interesantes ----

windows()
ggseasonplot(ret_log_YoY[,1], year.labels = T,year.labels.left = T) + ylab("ret. anuales")

windows()
ggsubseriesplot(ret_log_YoY[,1], year.labels = T,year.labels.left = T) + ylab("ret. anuales")


windows()
GGally::ggpairs(var_YoY)