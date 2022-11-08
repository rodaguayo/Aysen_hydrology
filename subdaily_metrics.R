rm(list=ls())
cat("\014")  

library("hydropeak")

setwd("/home/rooda/Dropbox/Projects/Aysen_Hydrology/data")

# Datasets
datasets <- list(c("Trapananda/Trap_IMP1_FASE 4 (Q)_2016-09-29_to_2022-03-16.csv" , "Trap_IMP1"),
                 c("Trapananda/Trap_IMP2_FASE 4 (Q)_2020-01-12_to_2022-03-16.csv",  "Trap_IMP2"),
                 c("Trapananda/Trap_REF_FASE 4 (Q)_2016-06-29_to_2022-03-16.csv",   "Trap_REF"),
                 c("Carrera/Carr_IMP1_FASE 4 (Q)_2016-09-11_to_2022-03-18.csv",     "Carr_IMP1"),
                 c("Carrera/Carr_IMP2_FASE 4 (Q)_2020-08-11_to_2022-03-18.csv",     "Carr_IMP2"),
                 c("Carrera/Carr_REFb_FASE 4 (Q)_2017-09-05_to_2022-03-18.csv",     "Carr_REF"),
                 c("Portales/Port_IMP1_FASE 4 (Q)_2016-12-13_to_2022-03-14.csv",    "Port_IMP1"),
                 c("Portales/Port_IMP2_FASE 4 (Q)_2020-03-18_to_2022-03-14.csv",    "Port_IMP2"),
                 c("Portales/Port_REF_FASE 4 (Q)_2016-09-14_to_2022-03-14.csv",     "Port_REF"),
                 c("Coyhaique Alto/CoyAlt_IMP1_FASE 4 (Q)_2016-09-28_to_2022-03-15.csv",  "CoyAlt_IMP1"),
                 c("Coyhaique Alto/CoyAlt_IMP2_FASE 4 (Q)_2020-01-12_to_2022-03-15.csv",  "CoyAlt_IMP2"),
                 c("Coyhaique Alto/CoyAlt_REF_FASE 4 (Q)_2016-09-28_to_2022-03-15.csv",   "CoyAlt_REF"))

for (i in 1:12) {
time_series<- read.csv(datasets[[i]][1])[c("date_time1", "Q_est1")]
time_series$ID <- datasets[[i]][2]
time_series$Q_est1 <- round(time_series$Q_est1,2)


time_series$date_time1 <- as.POSIXct(time_series$date_time1,   tz = "UTC")
anyDuplicated(time_series$date_time1)

tstamp <- data.frame(x = seq( head(time_series$date_time1,1),  tail(time_series$date_time1, 1), by = "30 min"))
time_series <-merge(tstamp, time_series, by.x="x",by.y="date_time1",all.x = TRUE)
anyDuplicated(time_series$date_time1)

#format for hydropeak
colnames(time_series) <- c("Time", "Q", "ID")
time_series <- time_series[, c("ID", "Time", "Q")]
time_series$Time <- strftime(time_series$Time , format = "%d.%m.%Y %H:%M")
time_series<-time_series[!duplicated(time_series$Time), ]

anyDuplicated(time_series$Time)

time_series_v2 <- time_series

time_series_flow <- flow(time_series_v2, steplength = 30, full = TRUE, format = "%d.%m.%Y %H:%M",   tz = "UTC")
time_series_flow$Time <- strftime(time_series_flow$Time, format = "%d.%m.%Y %H:%M",   tz = "UTC")
results_final <- get_events(time_series_flow, mc.cores = 8L, steplength = 30, omit.na = TRUE, format = "%d.%m.%Y %H:%M",   tz = "UTC")
write.csv(results_final, paste0(datasets[[i]][2], ".csv"), row.names = FALSE)

print(i)
}
