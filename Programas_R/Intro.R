
# -----------------------------------------------------------------------------#
# Inicio de programa ----
# -----------------------------------------------------------------------------#

# llamado de librerias ---------------------------------------------------------

#install.packages("pacman")
library(pacman)
p_load(xts, lubridate, lmtest,readxl,sandwich, ggridges, ggplot2, forecast, GGally
       , TSstudio, dplyr, tidyr)

# Lectura de infomracion -------------------------------------------------------

Datos_ent <- read_xlsx(file.choose(),sheet = "Base_LR",range = "a3:j200"
                       ,col_names = T)

tail(Datos_ent,10)
head(Datos_ent,10)

# Conversion objeto serie de tiempo --------------------------------------------

Datos_ent_ts1 <- ts(Datos_ent[,-1],start = c(2006,1),frequency = 12)  
tail(Datos_ent_ts1)
class(Datos_ent_ts1)

Datos_ent_ts2 <- xts(Datos_ent[,-1],order.by = as.Date(Datos_ent$Fecha))
class(Datos_ent_ts2)


# Calculos de los retornos -----------------------------------------------------

ret_log_YoY <- diff(log(Datos_ent_ts1),lag = 12,differences = 1)
View(ret_log_YoY)

ret_log_YoY2 <- diff(log(Datos_ent_ts2),lag = 12,differences = 1)
View(ret_log_YoY2)


# variacion anual

var_YoY <- round((Datos_ent[13:nrow(Datos_ent),-1]/Datos_ent[1:(nrow(Datos_ent)-12),-1]-1)*100,2)
var_YoY <- cbind(Datos_ent[13:nrow(Datos_ent),1],var_YoY)
tail(var_YoY)


# Algunos graficos interesantes ------------------------------------------------

windows()
ggseasonplot(ret_log_YoY[,1], year.labels = T,year.labels.left = T) + ylab("ret. anuales")

windows()
ggsubseriesplot(ret_log_YoY[,1], year.labels = T,year.labels.left = T) + ylab("ret. anuales")


windows()
GGally::ggpairs(var_YoY)


ts_plot(ret_log_YoY2[,1:3]
        ,title = "Retornos log. anuales"
        ,Xtitle = "Fecha"
        ,Ytitle = "Porcentaje")

ts_plot(ret_log_YoY2[,1:3]
        ,title = "Retornos log. anuales"
        ,Xtitle = "Fecha"
        ,Ytitle = "Porcentaje"
        ,type = "multiple")


# Distribuciones de los crecimientos -------------------------------------------

# Separacion por columnas
Datos_ent1 <- var_YoY %>% separate(Fecha,sep = "-",into = c("anho","mes","dia"))
tail(Datos_ent1)

# Grafico de distribucion
windows()
ggplot(Datos_ent1, aes(x = IPC, y = as.factor(anho) , fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.01, 0.99)
  ) +
    scale_fill_manual(
    name = "Probabilidad", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
    labels = c("(0, 1%]", "(1%, 99%]", "(99%, 100%]")
  ) +
  labs(title = 'Distribucion inflacion anual por anhos')


# Correlacion dinamicas --------------------------------------------------------

Correlacion_dinamica <- function(a,b,var_ref,var_comp)
{
  d <- ccf(a, b, plot = TRUE, lag.max = 12, type = "correlation", main=paste(var_ref,"&",var_comp, sep = " "))
  cor = d$acf[,,1]
  lag = d$lag[,,1]
  res = data.frame(cor,lag)
  res_max = res[which.max(res$cor),]
  return(res)
  print(res_max)
} 

names(Datos_ent1)
windows()
Correlacion_dinamica(Datos_ent1$BCOM,Datos_ent1$IPC,"BCOM","IPC")


# Regresion lineal -------------------------------------------------------------
Datos_modelo <- na.omit(var_YoY)
tail(Datos_modelo)

mod1 <- lm(ISE_PM3~. ,data = Datos_modelo)
summary(mod1)

windows()
checkresiduals(mod1)

windows()
plot(as.ts(Datos_modelo$ISE_PM3),col="b")
lines(as.ts(mod1$fitted.values))


mod2 <- lm(ISE_PM3~I(Cartera_Comercial-IPC)+I(Cartera_consumo-IPC)+I(M1/IPC)
           ,data = Datos_modelo)
summary(mod2)

mod3 <- lm(ISE_PM3~.,data = log(Datos_ent[,-1]))
summary(mod3)

windows()
checkresiduals(mod3)

windows()
ts_plot(as.ts(cbind(Datos_modelo$ISE_PM3,mod3$fitted.values)))

windows()
plot(as.ts(Datos_modelo$ISE_PM3))
lines(as.ts(mod3$fitted.values))



# Uso del modelo -----

predic_mod1 <- predict(mod1,newdata = tail(var_YoY[,-2],2))
predic_mod1
