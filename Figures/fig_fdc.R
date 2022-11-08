rm(list=ls())
cat("\014")  

library("plotly")
library("RColorBrewer")
setwd("/home/rooda/Dropbox/Projects/Aysen_Hydrology/")

data   <- read.csv("Data/Q_daily.csv")
areas  <- read.csv("Data/hectareas cuencas.csv")
data$d <- as.Date(data$d)
data$doy <- as.factor(strftime(data$d, format = "%j"))

rou = 4
data$trapa_ref     <- round((as.numeric(data$trapa_ref) * 86400 ) / (areas$TrapRef    * 1e4), rou)
data$trapa_imp1    <- round((as.numeric(data$trapa_imp1)* 86400 ) / (areas$TrapImp.1  * 1e4), rou)
data$trapa_imp2    <- round((as.numeric(data$trapa_imp2)* 86400 ) / (areas$TrapImp.2  * 1e4), rou)
data$port_ref      <- round((as.numeric(data$port_ref)  * 86400 ) / (areas$PortRef    * 1e4), rou)
data$port_imp1     <- round((as.numeric(data$port_imp1) * 86400 ) / (areas$PortImp.1  * 1e4), rou)
data$port_imp2     <- round((as.numeric(data$port_imp2) * 86400 ) / (areas$PortImp.2  * 1e4), rou)
data$carr_ref      <- round((as.numeric(data$carr_ref)  * 86400 ) / (areas$CarrRef    * 1e4), rou) 
data$carr_imp1     <- round((as.numeric(data$carr_imp1) * 86400 ) / (areas$CarrImp.1  * 1e4), rou)
data$carr_imp2     <- round((as.numeric(data$carr_imp2) * 86400 ) / (areas$CarrImp.2  * 1e4), rou)
data$coyalto_ref   <- round((as.numeric(data$coyalto_ref)  * 86400 )  / (areas$CoyRef    * 1e4), rou)
data$coyalto_imp1  <- round((as.numeric(data$coyalto_imp1) * 86400 )  / (areas$CoyImp.1  * 1e4), rou)
data$coyalto_imp2  <- round((as.numeric(data$coyalto_imp2) * 86400 )  / (areas$CoyImp.2  * 1e4), rou)

marker_1 = list(color = brewer.pal(8, 'Set1')[2], width = 2)
marker_2 = list(color = brewer.pal(8, 'Set1')[3], width = 2)
marker_3 = list(color = brewer.pal(8, 'Set1')[1], width = 2)
marker_4 = list(color = brewer.pal(8, 'Set1')[5], width = 2)

f <- list(family = "Times New Roman", size = 22)
f2 <- list(family = "Times New Roman", size = 18)

x1   <- list(title = "Percentage of time flow is equaled or less than (%)", titlefont = f, tickfont = f2, ticks = "outside")
y1   <- list(title = "log Q (mm day<sup>-1 </sup>)", titlefont = f,  tickfont = f2,  ticks = "outside", zeroline = FALSE, type = "log", range = c(-2,2))

# Trapananda
flow <- sort(na.omit(data$trapa_ref), decreasing=T )
df1  <- data.frame(x=100/length(flow)*1:length(flow), y=flow)
fig1 <- plot_ly(x = df1$x, y =  df1$y, type = 'scatter', mode = 'lines', line = marker_1, name = "Trapananda", legendgroup = 'group1')

flow <- sort(na.omit(data$trapa_imp1), decreasing=T )
df2  <- data.frame(x=100/length(flow)*1:length(flow), y=flow)
fig1 <- fig1 %>% add_trace(x = df2$x, y = df2$y, type = 'scatter', mode = 'lines', line = c(marker_1, dash = 'dot'),  showlegend = FALSE, legendgroup = 'group1')

# Portales
flow <- sort(na.omit(data$port_ref), decreasing=T )
df2  <- data.frame(x=100/length(flow)*1:length(flow), y=flow)
fig1 <- fig1 %>% add_trace(x = df2$x, y = df2$y, type = 'scatter', mode = 'lines', line = marker_2, name = "Portales", legendgroup = 'group2')

flow <- sort(na.omit(data$port_imp1), decreasing=T )
df2  <- data.frame(x=100/length(flow)*1:length(flow), y=flow)
fig1 <- fig1 %>% add_trace(x = df2$x, y = df2$y, type = 'scatter', mode = 'lines', line = c(marker_2, dash = 'dot'),  showlegend = FALSE, legendgroup = 'group2')

# General Carrera
flow <- sort(na.omit(data$carr_ref), decreasing=T )
df2  <- data.frame(x=100/length(flow)*1:length(flow), y=flow)
fig1 <- fig1 %>% add_trace(x = df2$x, y = df2$y, type = 'scatter', mode = 'lines', line = marker_3, name = "General Carrera", legendgroup = 'group3')

flow <- sort(na.omit(data$carr_imp1), decreasing=T )
df2  <- data.frame(x=100/length(flow)*1:length(flow), y=flow)
fig1 <- fig1 %>% add_trace(x = df2$x, y = df2$y, type = 'scatter', mode = 'lines', line = c(marker_3, dash = 'dot'),  showlegend = FALSE, legendgroup = 'group3')

# Coyhaique Alto
flow <- sort(na.omit(data$coyalto_ref), decreasing=T )
df2  <- data.frame(x=100/length(flow)*1:length(flow), y=flow)
fig1 <- fig1 %>% add_trace(x = df2$x, y = df2$y, type = 'scatter', mode = 'lines', line = marker_4, name = "Coyhaique Alto", legendgroup = 'group4')

flow <- sort(na.omit(data$coyalto_imp1), decreasing=T )
df2  <- data.frame(x=100/length(flow)*1:length(flow), y=flow)
fig1 <- fig1 %>% add_trace(x = df2$x, y = df2$y, type = 'scatter', mode = 'lines', line = c(marker_4, dash = 'dot'),  showlegend = FALSE, legendgroup = 'group4')

fig1 <- fig1 %>% layout(xaxis = x1, yaxis = y1, showlegend = TRUE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)", barmode = 'stack')
fig1 <- fig1 %>% layout(legend = list(x = 0.72, y = 0.97), font = f2)
fig1

reticulate::use_miniconda('r-reticulate')
save_image(fig1, file = "/home/rooda/Dropbox/Projects/Aysen_Hydrology/Figure_fdc.png", width = 800, height = 800, scale = 4)
