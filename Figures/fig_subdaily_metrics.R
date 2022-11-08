rm(list=ls())
cat("\014")  

library("plotly")
library("RColorBrewer")
setwd("/home/rooda/Dropbox/Projects/Aysen_Hydrology/Results")

files  <- list.files()
levels_sites <- c("IMP1", "IMP2", "REF")
data   <- read.csv(files[1])

for (i in 2:12) {
  data_i <- read.csv(files[i])
  data <- rbind(data, data_i)
  print(i)
}

data$Time <- as.POSIXct(data$Time)
data <- subset(data, data$Time > as.POSIXct("2020-01-01"))

data_a <- subset(data, substring(data$ID, 1, 4) == "Carr")
data_a$ID   <- as.factor(data_a$ID)
levels(data_a$ID) <- levels_sites

data_b <- subset(data, substring(data$ID, 1, 4) == "CoyA")
data_b$ID   <- as.factor(data_b$ID)
levels(data_b$ID) <- levels_sites

data_c <- subset(data, substring(data$ID, 1, 4) == "Port")
data_c$ID   <- as.factor(data_c$ID)
levels(data_c$ID) <- levels_sites

data_d <- subset(data, substring(data$ID, 1, 4) == "Trap")
data_d$ID   <- as.factor(data_d$ID)
levels(data_d$ID) <- levels_sites

f <- list(family = "Times New Roman", size = 22)
f2 <- list(family = "Times New Roman", size = 18)


# 1. Carrera ------------------------------- -------------------------------------------------------
x     <- list(title = "General Carrera", titlefont = f, tickfont = f2, ticks = "outside")
y     <- list(title = "Amplitude", titlefont = f, tickfont = f2,  ticks = "outside", zeroline = FALSE, type = "log")
fig1 <- plot_ly(data_a, y = ~AMP, x = ~ID, type = "box", color = ~ID, colors = brewer.pal(3, 'Dark2'), marker = list(size = 3, opacity = 0.1))
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = FALSE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")
fig1

y2 <- list(title = "MAFR", titlefont = f,  type = "log",  tickfont = f2, ticks = "outside", zeroline = FALSE)
fig2 <- plot_ly(data_a, y = ~MAFR, x = ~ID, type = "box", color = ~ID, colors = brewer.pal(3, 'Dark2'), marker = list(size = 3, opacity = 0.1))
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = FALSE)
fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y3 <- list(title = "MEFR", titlefont = f,  type = "log",  tickfont = f2, ticks = "outside", zeroline = FALSE)
fig3 <- plot_ly(data_a, y = ~MEFR, x = ~ID, type = "box", color = ~ID, colors = brewer.pal(3, 'Dark2'), marker = list(size = 3, opacity = 0.1))
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3, showlegend = FALSE)
fig3 <- fig3 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y5 <- list(title = "Ratio", titlefont = f, range = c(1.0, 1.3),  dtick = 0.1, tickfont = f2,ticks = "outside", zeroline = FALSE)
fig5 <- plot_ly(data_a, y = ~RATIO, x = ~ID, type = "box", color = ~ID, colors = brewer.pal(3, 'Dark2'), marker = list(size = 3, opacity = 0.1))
fig5 <- fig5 %>% layout(xaxis = x, yaxis = y5, showlegend = FALSE)
fig5 <- fig5 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

fig_a <- subplot(fig1, fig2, fig3, fig5, nrows = 4, shareX = T, shareY = F, titleY = T, margin = c(0.04, 0.04, 0.01, 0.01))

# 2. Coyhaique Alto ------------------------ -------------------------------------------------------
x     <- list(title = "Coyhaique Alto", titlefont = f, tickfont = f2, ticks = "outside")
y     <- list(titlefont = f, tickfont = f2,  ticks = "outside", zeroline = FALSE, type = "log")
fig1 <- plot_ly(data_b, y = ~AMP, x = ~ID, type = "box", color = ~ID, colors = brewer.pal(3, 'Dark2'), marker = list(size = 3, opacity = 0.1))
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = FALSE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y2 <- list( titlefont = f,  type = "log",  tickfont = f2, ticks = "outside", zeroline = FALSE)
fig2 <- plot_ly(data_b, y = ~MAFR, x = ~ID, type = "box", color = ~ID, colors = brewer.pal(3, 'Dark2'), marker = list(size = 3, opacity = 0.1))
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = FALSE)
fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y3 <- list(titlefont = f,  type = "log",  tickfont = f2, ticks = "outside", zeroline = FALSE)
fig3 <- plot_ly(data_b, y = ~MEFR, x = ~ID, type = "box", color = ~ID, colors = brewer.pal(3, 'Dark2'), marker = list(size = 3, opacity = 0.1))
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3, showlegend = FALSE)
fig3 <- fig3 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y5 <- list(titlefont = f, range = c(1.0 ,1.3),  dtick = 0.1, tickfont = f2,ticks = "outside", zeroline = FALSE)
fig5 <- plot_ly(data_b, y = ~RATIO, x = ~ID, type = "box", color = ~ID, colors = brewer.pal(3, 'Dark2'), marker = list(size = 3, opacity = 0.1))
fig5 <- fig5 %>% layout(xaxis = x, yaxis = y5, showlegend = FALSE)
fig5 <- fig5 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

fig_b <- subplot(fig1, fig2, fig3, fig5, nrows = 4, shareX = T, shareY = F, titleY = T, margin = c(0.04, 0.04, 0.01, 0.01))

# 3. Portales  ------------------------------- -------------------------------------------------------
x     <- list(title = "Portales", titlefont = f, tickfont = f2, ticks = "outside")
y     <- list( titlefont = f, tickfont = f2,  ticks = "outside", zeroline = FALSE, type = "log")
fig1 <- plot_ly(data_c, y = ~AMP, x = ~ID, type = "box", color = ~ID, colors = brewer.pal(3, 'Dark2'), marker = list(size = 3, opacity = 0.1))
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = FALSE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y2 <- list( titlefont = f,  type = "log",  tickfont = f2, ticks = "outside", zeroline = FALSE)
fig2 <- plot_ly(data_c, y = ~MAFR, x = ~ID, type = "box", color = ~ID, colors = brewer.pal(3, 'Dark2'), marker = list(size = 3, opacity = 0.1))
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = FALSE)
fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y3 <- list( titlefont = f,  type = "log",  tickfont = f2, ticks = "outside", zeroline = FALSE)
fig3 <- plot_ly(data_c, y = ~MEFR, x = ~ID, type = "box", color = ~ID, colors = brewer.pal(3, 'Dark2'), marker = list(size = 3, opacity = 0.1))
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3, showlegend = FALSE)
fig3 <- fig3 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y5 <- list( titlefont = f, range = c(1.0, 1.3),   dtick = 0.1, tickfont = f2,ticks = "outside", zeroline = FALSE)
fig5 <- plot_ly(data_c, y = ~RATIO, x = ~ID, type = "box", color = ~ID, colors = brewer.pal(3, 'Dark2'), marker = list(size = 3, opacity = 0.1))
fig5 <- fig5 %>% layout(xaxis = x, yaxis = y5, showlegend = FALSE)
fig5 <- fig5 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

fig_c <- subplot(fig1, fig2, fig3, fig5, nrows = 4, shareX = T, shareY = F, titleY = T, margin = c(0.04, 0.04, 0.01, 0.01))


# 4. Trapananda ------------------------------- -------------------------------------------------------
x     <- list(title = "Trapananda", titlefont = f, tickfont = f2, ticks = "outside")
y     <- list( titlefont = f, tickfont = f2,  ticks = "outside", zeroline = FALSE, type = "log")
fig1 <- plot_ly(data_d, y = ~AMP, x = ~ID, type = "box", color = ~ID, colors = brewer.pal(3, 'Dark2'), marker = list(size = 3, opacity = 0.1))
fig1 <- fig1 %>% layout(xaxis = x, yaxis = y, showlegend = FALSE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y2 <- list( titlefont = f,  type = "log",  tickfont = f2, ticks = "outside", zeroline = FALSE)
fig2 <- plot_ly(data_d, y = ~MAFR, x = ~ID, type = "box", color = ~ID, colors = brewer.pal(3, 'Dark2'), marker = list(size = 3, opacity = 0.1))
fig2 <- fig2 %>% layout(xaxis = x, yaxis = y2, showlegend = FALSE)
fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y3 <- list(titlefont = f,  type = "log",  tickfont = f2, ticks = "outside", zeroline = FALSE)
fig3 <- plot_ly(data_d, y = ~MEFR, x = ~ID, type = "box", color = ~ID, colors = brewer.pal(3, 'Dark2'), marker = list(size = 3, opacity = 0.1))
fig3 <- fig3 %>% layout(xaxis = x, yaxis = y3, showlegend = FALSE)
fig3 <- fig3 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

y5 <- list( titlefont = f, range = c(1.0, 1.3),  dtick = 0.1, tickfont = f2,ticks = "outside", zeroline = FALSE)
fig5 <- plot_ly(data_d, y = ~RATIO, x = ~ID, type = "box", color = ~ID, colors = brewer.pal(3, 'Dark2'), marker = list(size = 3, opacity = 0.1))
fig5 <- fig5 %>% layout(xaxis = x, yaxis = y5, showlegend = FALSE)
fig5 <- fig5 %>% layout(plot_bgcolor="rgb(235, 235, 235)")

fig_d <- subplot(fig1, fig2, fig3, fig5, nrows = 4, shareX = T, shareY = F, titleY = T, margin = c(0.04, 0.04, 0.01, 0.01))

figX <- subplot(fig_a, fig_b, fig_c, fig_d, nrows = 1, shareY = F, titleY = T, titleX = T)

reticulate::use_miniconda('r-reticulate')
save_image(figX, file = "/home/rooda/Dropbox/Projects/Aysen_Hydrology/Figure_Validation.png", width = 1200, height = 1000, scale = 4)
