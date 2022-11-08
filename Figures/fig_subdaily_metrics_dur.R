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
data_a$DUR[data_a$DUR > 5] <- 5
data_a$DUR

data_b <- subset(data, substring(data$ID, 1, 4) == "CoyA")
data_b$ID   <- as.factor(data_b$ID)
levels(data_b$ID) <- levels_sites
data_b$DUR[data_b$DUR > 5] <- 5

data_c <- subset(data, substring(data$ID, 1, 4) == "Port")
data_c$ID   <- as.factor(data_c$ID)
levels(data_c$ID) <- levels_sites
data_c$DUR[data_c$DUR > 5] <- 5

data_d <- subset(data, substring(data$ID, 1, 4) == "Trap")
data_d$ID   <- as.factor(data_d$ID)
levels(data_d$ID) <- levels_sites
data_d$DUR[data_d$DUR > 5] <- 5


f <- list(family = "Times New Roman", size = 22)
f2 <- list(family = "Times New Roman", size = 18)



marker_1 = list(color = brewer.pal(5, 'Accent')[1])
marker_2 = list(color = brewer.pal(5, 'Accent')[2])
marker_3 = list(color = brewer.pal(5, 'Accent')[3])
marker_4 = list(color = brewer.pal(5, 'Accent')[4])
marker_5 = list(color = brewer.pal(5, 'Accent')[5])

x1   <- list(title = "General Carrera", titlefont = f, tickfont = f2, ticks = "outside")
y1   <- list(title = "Count (n)", titlefont = f,  tickfont = f2,  ticks = "outside", zeroline = FALSE)
fig1 <- plot_ly(x = c("IMP1", "IMP2", "REF"), y = table(subset(data_a, DUR == 1)$ID), type = "bar", name = "1", marker = marker_1)
fig1 <- fig1 %>% add_trace(y = table(subset(data_a, DUR == 2)$ID), name = '2', marker = marker_2)
fig1 <- fig1 %>% add_trace(y = table(subset(data_a, DUR == 3)$ID), name = '3', marker = marker_3)
fig1 <- fig1 %>% add_trace(y = table(subset(data_a, DUR == 4)$ID), name = '4', marker = marker_4)
fig1 <- fig1 %>% add_trace(y = table(subset(data_a, DUR == 5)$ID), name = '>5', marker = marker_5)
fig1 <- fig1 %>% layout(xaxis = x1, yaxis = y1, showlegend = FALSE)
fig1 <- fig1 %>% layout(plot_bgcolor="rgb(235, 235, 235)", barmode = 'stack')

x2 <- list(title = "Coyhaique Alto", titlefont = f, tickfont = f2, ticks = "outside")
y2 <- list(titlefont = f,  tickfont = f2,  ticks = "outside", zeroline = FALSE)
fig2 <- plot_ly(x = c("IMP1", "IMP2", "REF"), y = table(subset(data_b, DUR == 1)$ID), type = "bar", name = "1", marker = marker_1, showlegend = FALSE)
fig2 <- fig2 %>% add_trace(y = table(subset(data_b, DUR == 2)$ID), name = '2', marker = marker_2)
fig2 <- fig2 %>% add_trace(y = table(subset(data_b, DUR == 3)$ID), name = '3', marker = marker_3)
fig2 <- fig2 %>% add_trace(y = table(subset(data_b, DUR == 4)$ID), name = '4', marker = marker_4)
fig2 <- fig2 %>% add_trace(y = table(subset(data_b, DUR == 5)$ID), name = '>5', marker = marker_5)
fig2 <- fig2 %>% layout(xaxis = x2, yaxis = y2)
fig2 <- fig2 %>% layout(plot_bgcolor="rgb(235, 235, 235)", barmode = 'stack')

x3 <- list(title = "Portales", titlefont = f, tickfont = f2, ticks = "outside")
fig3 <- plot_ly(x = c("IMP1", "IMP2", "REF"), y = table(subset(data_c, DUR == 1)$ID), type = "bar", name = "1", marker = marker_1, showlegend = FALSE)
fig3 <- fig3 %>% add_trace(y = table(subset(data_c, DUR == 2)$ID), name = '2', marker = marker_2)
fig3 <- fig3 %>% add_trace(y = table(subset(data_c, DUR == 3)$ID), name = '3', marker = marker_3)
fig3 <- fig3 %>% add_trace(y = table(subset(data_c, DUR == 4)$ID), name = '4', marker = marker_4)
fig3 <- fig3 %>% add_trace(y = table(subset(data_c, DUR == 5)$ID), name = '>5', marker = marker_5)
fig3 <- fig3 %>% layout(xaxis = x3, yaxis = y2)
fig3 <- fig3 %>% layout(plot_bgcolor="rgb(235, 235, 235)", barmode = 'stack')

x4 <- list(title = "Trapananda", titlefont = f, tickfont = f2, ticks = "outside")
fig4 <- plot_ly(x = c("IMP1", "IMP2", "REF"), y = table(subset(data_d, DUR == 1)$ID), type = "bar", name = "1", marker = marker_1, showlegend = FALSE)
fig4 <- fig4 %>% add_trace(y = table(subset(data_d, DUR == 2)$ID), name = '2', marker = marker_2)
fig4 <- fig4 %>% add_trace(y = table(subset(data_d, DUR == 3)$ID), name = '3', marker = marker_3)
fig4 <- fig4 %>% add_trace(y = table(subset(data_d, DUR == 4)$ID), name = '4', marker = marker_4)
fig4 <- fig4 %>% add_trace(y = table(subset(data_d, DUR == 5)$ID), name = '>5', marker = marker_5)
fig4 <- fig4 %>% layout(xaxis = x4, yaxis = y2)
fig4 <- fig4 %>% layout(plot_bgcolor="rgb(235, 235, 235)", barmode = 'stack')

fig <- subplot(fig1, fig2, fig3, fig4, nrows = 1, shareX = T, shareY = T, titleY = T, margin = c(0.01, 0.01, 0.01, 0.01))
fig

reticulate::use_miniconda('r-reticulate')
save_image(fig, file = "/home/rooda/Dropbox/Projects/Aysen_Hydrology/Figure_duration.png", width = 1200, height = 500, scale = 4)

