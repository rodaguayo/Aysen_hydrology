  rm(list=ls())
  cat("\014")  
  
  library("plotly")
  library("RColorBrewer")
  setwd("/home/rooda/Dropbox/Projects/Aysen_Hydrology/")
  
  data   <- read.csv("Data/Q_daily.csv")
  areas  <- read.csv("hectareas_cuencas.csv")
  data$d <- as.Date(data$d)
  data$doy <- as.factor(strftime(data$d, format = "%j"))
  
  data$trapa_ref     <- (as.numeric(data$trapa_ref) * 86400 ) / (areas$TrapRef    * 1e4)
  data$trapa_imp1    <- (as.numeric(data$trapa_imp1)* 86400 ) / (areas$TrapImp.1  * 1e4)
  data$trapa_imp2    <- (as.numeric(data$trapa_imp2)* 86400 ) / (areas$TrapImp.2  * 1e4)
  data$port_ref      <- (as.numeric(data$port_ref)  * 86400 ) / (areas$PortRef    * 1e4)
  data$port_imp1     <- (as.numeric(data$port_imp1) * 86400 ) / (areas$PortImp.1  * 1e4)
  data$port_imp2     <- (as.numeric(data$port_imp2) * 86400 ) / (areas$PortImp.2  * 1e4)
  data$carr_ref      <- (as.numeric(data$carr_ref)  * 86400 ) / (areas$CarrRef    * 1e4)
  data$carr_imp1     <- (as.numeric(data$carr_imp1) * 86400 ) / (areas$CarrImp.1  * 1e4)
  data$carr_imp2     <- (as.numeric(data$carr_imp2) * 86400 ) / (areas$CarrImp.2  * 1e4)
  data$coyalto_ref   <- (as.numeric(data$coyalto_ref)  * 86400 )  / (areas$CoyRef    * 1e4)
  data$coyalto_imp1  <- (as.numeric(data$coyalto_imp1) * 86400 )  / (areas$CoyImp.1  * 1e4)
  data$coyalto_imp2  <- (as.numeric(data$coyalto_imp2) * 86400 )  / (areas$CoyImp.2  * 1e4)
  
  table_doy <- function(data, basin) {
    Qdoy <- as.data.frame(array(data=NA, c(max(as.numeric(data$doy)),6)))
    Qdoy[,1]<-tapply(data[[basin]], data$doy, max, na.rm=TRUE)
    Qdoy[,2]<-tapply(data[[basin]] , data$doy, min, na.rm=TRUE)
    Qdoy[,3]<-tapply(data[[basin]] , data$doy, mean, na.rm=TRUE)
    Qdoy[,4]<-tapply(data[[basin]] , data$doy, stats::quantile, 0.85, na.rm=TRUE)
    Qdoy[,5]<-tapply(data[[basin]] , data$doy, stats::quantile, 0.15, na.rm=TRUE)
    Qdoy[,6]<-tapply(data[[basin]] , data$doy, stats::median, na.rm=TRUE)
    colnames(Qdoy)<- c("max", "min", "mean", "Qalto", "Qbajo", "median")
    return(Qdoy)
  }
  
  marker_1 = list(color = brewer.pal(8, 'Set1')[2], width = 1)
  marker_2 = list(color = brewer.pal(8, 'Set1')[3], width = 1)
  marker_3 = list(color = brewer.pal(8, 'Set1')[1], width = 1)
  marker_4 = list(color = brewer.pal(8, 'Set1')[5], width = 1)
  
  f <- list(family = "Times New Roman", size = 22)
  f2 <- list(family = "Times New Roman", size = 18)
  date <- seq(from = as.Date("2016-01-01"), as.Date("2016-12-31"), by = "day")
  
  Qdoy <- table_doy(data, "trapa_ref")
  x1   <- list(titlefont = f, tickfont = f2, ticks = "outside", dtick = "M1", tickformat="%b")
  y1   <- list(title = "Q (mm day<sup>-1 </sup>)", titlefont = f2,  tickfont = f2,  ticks = "outside", zeroline = FALSE, range = c(0.01,10), dtick = 3)
  title <- list(text = "Reference", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.4, y = 1.14)
  title_a <- list(text = "Trapananda", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.05, y = 0.99)
  fig1 <- plot_ly(   x = date, y = Qdoy$Qalto, type = 'scatter', mode = 'lines', alpha = 0, alpha_stroke = 0)
  fig1 <- fig1 %>% add_trace( x = date, y = Qdoy$Qbajo,  fillcolor="rgba(55, 126, 184, 0.2)", fill = 'tonexty', alpha = 0, alpha_stroke = 0)
  fig1 <- fig1 %>% add_trace( x = date, y = Qdoy$mean, alpha = 1, alpha_stroke = 1, line = marker_1)
  fig1 <- fig1 %>% layout(xaxis = x1, yaxis = y1, showlegend = FALSE)
  fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)", barmode = 'stack')

  fig1 <- fig1 %>% layout(annotations = title_a)
  fig1
  
  Qdoy <- table_doy(data, "trapa_imp1")
  title2 <- list(text = "Impacted", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.6, y = 1.14)
  fig2 <- plot_ly(   x = date, y = Qdoy$Qalto, type = 'scatter', mode = 'lines', alpha = 0, alpha_stroke = 0)
  fig2 <- fig2 %>% add_trace( x = date, y = Qdoy$Qbajo,  fillcolor="rgba(55, 126, 184, 0.2)", fill = 'tonexty', alpha = 0, alpha_stroke = 0)
  fig2 <- fig2 %>% add_trace( x = date, y = Qdoy$mean, alpha = 1, alpha_stroke = 1, line = marker_1)
  fig2 <- fig2 %>% layout(xaxis = x1, yaxis = y1, showlegend = FALSE)
  fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)", barmode = 'stack')
  #fig2 <- fig2 %>% layout(annotations = title2)
  
  #Qdoy <- table_doy(data, "trapa_imp2")
  #fig2 <- fig2 %>% add_trace( x = date, y = Qdoy$mean, alpha = 0.5, alpha_stroke = 0.5, line = c(marker_1, dash = 'dot'))
  
  
  Qdoy <- table_doy(data, "port_ref")
  title_b <- list(text = "Portales", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.85)
  y3   <- list(title = "Q (mm day<sup>-1 </sup>)", titlefont = f2,  tickfont = f2,  ticks = "outside", zeroline = FALSE, range = c(0,30), dtick = 10)
  fig3 <- plot_ly(   x = date, y = Qdoy$Qalto, type = 'scatter', mode = 'lines', alpha = 0, alpha_stroke = 0)
  fig3 <- fig3 %>% add_trace( x = date, y = Qdoy$Qbajo,  fillcolor="rgba(77, 175, 74, 0.2)", fill = 'tonexty', alpha = 0, alpha_stroke = 0)
  fig3 <- fig3 %>% add_trace( x = date, y = Qdoy$mean, alpha = 1, alpha_stroke = 1, line = marker_2)
  fig3 <- fig3 %>% layout(xaxis = x1, yaxis = y3, showlegend = FALSE)
  fig3 <- fig3 %>% layout(plot_bgcolor="rgb(235, 235, 235)", barmode = 'stack')
  fig3 <- fig3 %>% layout(annotations = title_b)
  
  Qdoy <- table_doy(data, "port_imp1")
  fig4 <- plot_ly(   x = date, y = Qdoy$Qalto, type = 'scatter', mode = 'lines', alpha = 0, alpha_stroke = 0)
  fig4 <- fig4 %>% add_trace( x = date, y = Qdoy$Qbajo,  fillcolor="rgba(77, 175, 74, 0.2)", fill = 'tonexty', alpha = 0, alpha_stroke = 0)
  fig4 <- fig4 %>% add_trace( x = date, y = Qdoy$mean, alpha = 1, alpha_stroke = 1, line = marker_2)
  fig4 <- fig4 %>% layout(xaxis = x1, yaxis = y3, showlegend = FALSE)
  fig4 <- fig4 %>% layout(plot_bgcolor="rgb(235, 235, 235)", barmode = 'stack')
  
  #Qdoy <- table_doy(data, "port_imp2")
  #fig4 <- fig4 %>% add_trace( x = date, y = Qdoy$mean, alpha = 0.5, alpha_stroke = 0.5, line = c(marker_2, dash = 'dot'))
  
  Qdoy <- table_doy(data, "carr_ref")
  title_c <- list(text = "General Carrera", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.9)
  y5   <- list(title = "Q (mm day<sup>-1 </sup>)", titlefont = f2,  tickfont = f2,  ticks = "outside", zeroline = FALSE, range = c(0,30), dtick = 10)
  fig5 <- plot_ly(   x = date, y = Qdoy$Qalto, type = 'scatter', mode = 'lines', alpha = 0, alpha_stroke = 0)
  fig5 <- fig5 %>% add_trace( x = date, y = Qdoy$Qbajo,  fillcolor="rgba(228, 26, 28, 0.2)", fill = 'tonexty', alpha = 0, alpha_stroke = 0)
  fig5 <- fig5 %>% add_trace( x = date, y = Qdoy$mean, alpha = 1, alpha_stroke = 1, line = marker_3)
  fig5 <- fig5 %>% layout(xaxis = x1, yaxis = y5, showlegend = FALSE)
  fig5 <- fig5 %>% layout(plot_bgcolor="rgb(235, 235, 235)", barmode = 'stack')
  fig5 <- fig5 %>% layout(annotations = title_c)
  
  Qdoy <- table_doy(data, "carr_imp1")
  fig6 <- plot_ly(   x = date, y = Qdoy$Qalto, type = 'scatter', mode = 'lines', alpha = 0, alpha_stroke = 0)
  fig6 <- fig6 %>% add_trace( x = date, y = Qdoy$Qbajo,  fillcolor="rgba(228, 26, 28, 0.2)", fill = 'tonexty', alpha = 0, alpha_stroke = 0)
  fig6 <- fig6 %>% add_trace( x = date, y = Qdoy$mean, alpha = 1, alpha_stroke = 1, line = marker_3)
  fig6 <- fig6 %>% layout(xaxis = x1, yaxis = y3, showlegend = FALSE)
  fig6 <- fig6 %>% layout(plot_bgcolor="rgb(235, 235, 235)", barmode = 'stack')
  
  #Qdoy <- table_doy(data, "carr_imp2")
  #fig6 <- fig6 %>% add_trace( x = date, y = Qdoy$mean, alpha = 0.5, alpha_stroke = 0.5, line = c(marker_3, dash = 'dot'))
  
  Qdoy <- table_doy(data, "coyalto_ref")
  title_d <- list(text = "Coyhaique Alto ", font = f, showarrow = F, xref = "paper", yref = "paper", x = 0.03, y = 0.93)
  y7   <- list(title = "Q (mm day<sup>-1 </sup>)", titlefont = f2,  tickfont = f2,  ticks = "outside", zeroline = FALSE, range = c(0.01,10), dtick = 3)
  fig7 <- plot_ly(   x = date, y = Qdoy$Qalto, type = 'scatter', mode = 'lines', alpha = 0, alpha_stroke = 0)
  fig7 <- fig7 %>% add_trace( x = date, y = Qdoy$Qbajo,  fillcolor="rgba(255, 127, 0, 0.2)", fill = 'tonexty', alpha = 0, alpha_stroke = 0)
  fig7 <- fig7 %>% add_trace( x = date, y = Qdoy$mean, alpha = 1, alpha_stroke = 1, line = marker_4)
  fig7 <- fig7 %>% layout(xaxis = x1, yaxis = y7, showlegend = FALSE)
  fig7 <- fig7 %>% layout(plot_bgcolor="rgb(235, 235, 235)", barmode = 'stack')
  fig7 <- fig7 %>% layout(annotations = title_d)
  fig7 <- fig7 %>% layout(annotations = title)
  
  Qdoy <- table_doy(data, "coyalto_imp1")
  fig8 <- plot_ly(   x =date, y = Qdoy$Qalto, type = 'scatter', mode = 'lines', alpha = 0, alpha_stroke = 0)
  fig8 <- fig8 %>% add_trace( x = date, y = Qdoy$Qbajo,  fillcolor="rgba(255, 127, 0, 0.2)", fill = 'tonexty', alpha = 0, alpha_stroke = 0)
  fig8 <- fig8 %>% add_trace( x = date, y = Qdoy$mean, alpha = 1, alpha_stroke = 1, line = marker_4)
  fig8 <- fig8 %>% layout(xaxis = x1, yaxis = y1, showlegend = FALSE)
  fig8 <- fig8 %>% layout(plot_bgcolor="rgb(235, 235, 235)", barmode = 'stack')
  fig8 <- fig8 %>% layout(annotations = title2)
  
  
  #Qdoy <- table_doy(data, "coyalto_imp2")
  #fig8 <- fig8 %>% add_trace( x = date, y = Qdoy$mean, alpha = 0.5, alpha_stroke = 0.5, line = c(marker_4, dash = 'dot'))
  
  fig <- subplot(fig7, fig8, fig1, fig2, fig5, fig6, fig3, fig4,  nrows = 4, shareX = T, shareY = T, titleY = T, margin = c(0.01, 0.01, 0.01, 0.01))
  fig
  
  reticulate::use_miniconda('r-reticulate')
  reticulate::py_run_string("import sys")
  save_image(fig, file = "/home/rooda/Dropbox/Projects/Aysen_Hydrology/Figures/Figure_Hydrology.png", width = 1100, height = 1000, scale = 4)
