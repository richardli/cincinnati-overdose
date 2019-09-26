# This script produces smaller csv files for data storage

library(data.table)
dir <- "../data/"
Data <- fread(paste0(dir, "Cincinnati_Fire_Incidents__CAD___including_EMS__ALS_BLS.csv"))
Data <- data.frame(Data)
Data <- Data[Data$CFD_INCIDENT_TYPE_GROUP %in% c("HEROIN OVERDOSE", "OVERDOSE / POSIONING (INGESTION)"), ]
write.csv(Data, paste0(dir, "Cincinnati_Fire_Incidents__CAD___including_EMS__ALS_BLS-ODonly.csv"), row.names = FALSE)


crime <- fread(paste0(dir, "PDI__Police_Data_Initiative__Crime_Incidents.csv"))
crime <- data.frame(crime)
crime <- crime[!is.na(crime$LATITUDE_X), ]
date_time <- as.character(crime$DATE_REPORTED)
date <- as.Date(date_time, "%m/%d/%Y %H:%M:%S %p")
time_start <- "2015-08-01"
time_end <- "2019-02-01"
subset <- intersect(which(date >= as.Date(time_start)), which(date < as.Date(time_end)))
crime <- crime[subset, ]
crime <- crime[, c("INSTANCEID", "DATE_REPORTED", "LONGITUDE_X", "LATITUDE_X")]
write.csv(crime, paste0(dir, "PDI__Police_Data_Initiative__Crime_Incidents.csv"), row.names = FALSE)

