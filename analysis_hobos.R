library(data.table)
library(lubridate)
library(dplyr)
library(stringr)

## Change .txt extension to .csv from the CMD 

# ren *.txt *.csv


## PROCESSING AND CLEANING RAW DATA

out_dir <- ("C:\\Users\\LENOVO\\Desktop\\Nova pasta (2)\\")
files <- list.files("C:\\Users\\LENOVO\\Desktop\\Nova pasta (2)\\", pattern=".csv", full.names=T)
names.files <- list.files("C:\\Users\\LENOVO\\Desktop\\Nova pasta (2)\\", pattern=".csv")


all_data <- lapply(files, fread,select=c(1:5), header = FALSE, skip=1, encoding = "UTF-8", dec=".")

for (i in 1:length(all_data)){
  all_data[[i]]<-cbind(all_data[[i]],names.files[i])}


## PROCESS RAW DATA ---------
# drop errors, merge into one large data.table, name columns, parse timestamp
all_data <- all_data[sapply(all_data, is.data.table)]
all_data <- rbindlist(all_data)
setnames(all_data, c("n", "date", "time", "temp","light", "file"))

all_data$DATE <- paste(all_data$date,"_",all_data$time) #this variable will be useful for sort and remove duplicates

all_data$light <- as.numeric(sub(",", "", all_data$light, fixed=TRUE))
all_data$temp <- as.numeric(sub(",", "", all_data$temp, fixed=TRUE))
all_data$file <- sub(".csv", "", all_data$file, fixed=TRUE)

# drop rows with missing data
all_data <- na.omit(all_data)

# parsing dates and time
all_data$date <- dmy(all_data$date)
all_data$time <- hms::as_hms(all_data$time)


# cleaning variables into a new variable with less levels. We must pay attention in this part because some dataloggers were translocated from their place to
# other places. Check the dataloggers code!!!!  
all_data$site <- ifelse(grepl("Quiaios1-",all_data$file),'QUD1',
                       ifelse(grepl("Quiaios2-",all_data$file),'QUD2',
                              ifelse(grepl("LSA1_",all_data$file),'LSA1',
                                     ifelse(grepl("LSA2_",all_data$file),'LSA2',
                                            ifelse(grepl("quarentenaFRIGO",all_data$file),'ESAC_Frigo',
                                                   ifelse(grepl("quarentenaTESTE",all_data$file),'ESAC_Teste',
                                                          ifelse(grepl("Faro1_",all_data$file),'FAR1',
                                                                 ifelse(grepl("Faro2_",all_data$file),'FAR2',
                                                                        ifelse(grepl("Tocha1-",all_data$file),'TOC1',
                                                                               ifelse(grepl("Tocha2-",all_data$file),'TOC2',
                                                                                      ifelse(grepl("SPMoel1-",all_data$file),'SPM1',
                                                                                             ifelse(grepl("_spm1",all_data$file),'SPM1',
                                                                                                    ifelse(grepl("SPMoel2-",all_data$file),'SPM2',
                                                                                                           ifelse(grepl("20415974_",all_data$file),'SJD_JAEL2',
                                                                                                                  ifelse(grepl("20415975_",all_data$file),'SJD_JAEL1',
                                                                                                                         ifelse(grepl("SJacinto1-",all_data$file),'SJD1',
                                                                                                                                ifelse(grepl("SJacinto2_",all_data$file),'SJD2',all_data$file)))))))))))))))))






write.csv2(all_data, paste(out_dir, "temp.csv", sep="/"), row.names=FALSE)


# drop date duplicates
all_data <- all_data %>% distinct(DATE, site, .keep_all = TRUE) 


#all_data[, date:=floor_date(mdy_hms(date), "minute")] # floor date to nearest minute




# filtering by site

sjd2 <- all_data %>%
        filter(str_detect(file, "10640611"))


## SAVE PROCESSED DATA ------
# write data in one big file
write.csv(all_data, paste(out_dir, "all_data.csv", sep="/"), 
          row.names=FALSE)





# drop rows with missing data
#all_data <- all_data[complete.cases(all_data),]
