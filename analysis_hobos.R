# Part of this script was inspired and based on https://rpubs.com/cgb-grupstra/moorea-hobo-20190314


library(data.table)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(cowplot)

## Change .txt extension to .csv from the CMD 

# ren *.txt *.csv


## PROCESSING AND CLEANING RAW DATA

out_dir <- ("C:\\Users\\LENOVO\\Google Drive\\Databases\\Hobos_data\\HOBOS\\")
files <- list.files("C:\\Users\\LENOVO\\Google Drive\\Databases\\Hobos_data\\", pattern = ".csv", full.names = T)
names.files <- list.files("C:\\Users\\LENOVO\\Google Drive\\Databases\\Hobos_data\\", pattern = ".csv")


all_data <- lapply(files, fread,select = c(1:5), header = FALSE, skip = 1, encoding = "UTF-8", dec = ".", sep = ";", fill = F)

for (i in 1:length(all_data)) {
  all_data[[i]] <- cbind(all_data[[i]],names.files[i])}


## PROCESSING RAW DATA ---------
# drop errors, merge into one large data.table, name columns, parse timestamp
all_data <- all_data[sapply(all_data, is.data.table)]
all_data <- rbindlist(all_data)
setnames(all_data, c("n", "date", "time", "temp","light", "file"))

all_data$DATE <- paste(all_data$date,"_",all_data$time) #this variable will be useful for sort and remove duplicates

all_data$light <- as.numeric(sub(",", "", all_data$light, fixed = TRUE))
all_data$temp <- as.numeric(sub(",", "", all_data$temp, fixed = TRUE))
all_data$file <- sub(".csv", "", all_data$file, fixed = TRUE)

# drop rows with missing data
all_data <- na.omit(all_data)

# parsing dates and time and spliting dates
all_data$date <- dmy(all_data$date)
all_data$time <- hms::as_hms(all_data$time)

all_data$YEAR <- year(all_data$date)
all_data$MONTH <- month(all_data$date)
all_data$DAY <- day(all_data$date)


# cleaning variables into a new variable with less levels. We must pay attention in this part because some dataloggers were translocated from their place to
# other places. Check the dataloggers code!!!!  
all_data$site <- ifelse(grepl("Quiaios1-",all_data$file),'QUD1',
                       ifelse(grepl("Quiaios2-",all_data$file),'QUD2',
                              ifelse(grepl("SerraBoaViagem1-",all_data$file),'SBV1',
                                     ifelse(grepl("SBV2_",all_data$file),'SBV2',
                                            ifelse(grepl("10640599",all_data$file),'SBV2',
                                                   ifelse(grepl("HOBO_SBV1",all_data$file),'SBV2',
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
                                                                                                                                       ifelse(grepl("SJD_2_HOBO",all_data$file),'SJD_JAEL2',
                                                                                                                                              ifelse(grepl("20415974_",all_data$file),'SJD_JAEL2',
                                                                                                                                                     ifelse(grepl("20415975_",all_data$file),'SJD_JAEL1', #I'm not sure
                                                                                                                                                            ifelse(grepl("SJacinto1-",all_data$file),'SJD1',
                                                                                                                                                                   ifelse(grepl("SJD1_",all_data$file),'SJD1',
                                                                                                                                                                          ifelse(grepl("SJD1_SEI1_",all_data$file),'SJD1',
                                                                                                                                                                              ifelse(grepl("SJD2",all_data$file),'SJD2',
                                                                                                                                                                                 ifelse(grepl("SJD2_",all_data$file),'SJD2',
                                                                                                                                                                                        ifelse(grepl("SJacinto2_",all_data$file),'SJD2',
                                                                                                                                                                                               ifelse(grepl("SEI1_HOBO",all_data$file),'SEI1',
                                                                                                                                                                                                      ifelse(grepl("SEI2_HOBO",all_data$file),'SEI2',
                                                                                                                                                                                                             ifelse(grepl("Esposende2-",all_data$file),'ESP2',all_data$file)))))))))))))))))))))))))))))





unique(all_data$site)
all_data$site <- as.factor(all_data$site)

all_data$site_merged <- ifelse(grepl("QUD1",all_data$site),'QUD',
                        ifelse(grepl("QUD2",all_data$site),'QUD',
                               ifelse(grepl("FAR1",all_data$site),'FAR',
                                      ifelse(grepl("FAR2",all_data$site),'FAR',
                                             ifelse(grepl("LSA1",all_data$site),'LSA',
                                                    ifelse(grepl("LSA2",all_data$site),'LSA',
                                                           ifelse(grepl("SBV1",all_data$site),'SBV',
                                                                  ifelse(grepl("SBV2",all_data$site),'SBV',
                                                                         ifelse(grepl("SEI1",all_data$site),'SEI',
                                                                                ifelse(grepl("SEI2",all_data$site),'SEI',
                                                                                       ifelse(grepl("SJD1",all_data$site),'SJD',
                                                                                              ifelse(grepl("SJD2",all_data$site),'SJD',
                                                                                                     ifelse(grepl("SPM1",all_data$site),'SPM',
                                                                                                            ifelse(grepl("SPM2",all_data$site),'SPM',
                                                                                                                   ifelse(grepl("TOC1",all_data$site),'TOC',
                                                                                                                          ifelse(grepl("TOC2",all_data$site),'TOC',
                                                                                                                                 ifelse(grepl("SJD_JAEL2",all_data$site),'SJD_JAEL2',
                                                                                                                                        ifelse(grepl("SJD_JAEL1",all_data$site),'SJD_JAEL1',
                                                                                                                                               ifelse(grepl("ESP",all_data$site),'ESP',
                                                                                                                                                      ifelse(grepl("ESAC_Frigo",all_data$site),'ESAC_Frigo',
                                                                                                                                                             ifelse(grepl("ESAC_Teste",all_data$site),'ESAC_Teste',all_data$site)))))))))))))))))))))

unique(all_data$site_merged)
all_data$site_merged <- as.factor(all_data$site_merged)



# adding two variables with the geographic coordinates of each HOBO
all_data$lat <- ifelse(grepl("QUD1",all_data$site),40.2241592,
                       ifelse(grepl("QUD2",all_data$site),40.22625410606060115,
                              ifelse(grepl("SBV1",all_data$site),40.20061062278523423,
                                     ifelse(grepl("SBV2",all_data$site),40.20103006892036035,
                                            ifelse(grepl("ESP2",all_data$site),41.50971512259314977,
                                                   ifelse(grepl("TOC1",all_data$site),40.34855876047194556,
                                                          ifelse(grepl("TOC2",all_data$site),40.34924270743018582,
                                                                 ifelse(grepl("FAR1",all_data$site),37.026209,
                                                                        ifelse(grepl("FAR2",all_data$site),37.029384,
                                                                               ifelse(grepl("LSA1",all_data$site),37.991534759,
                                                                                      ifelse(grepl("LSA2",all_data$site),37.992226568,
                                                                                             ifelse(grepl("SPM1",all_data$site),39.7593915,
                                                                                                    ifelse(grepl("SPM2",all_data$site),39.75861329512477482,
                                                                                                           ifelse(grepl("SEI1",all_data$site),40.492807072,
                                                                                                                  ifelse(grepl("SEI2",all_data$site),40.491341348,
                                                                                                                         ifelse(grepl("ESAC_Frigo",all_data$site),40.211873,
                                                                                                                                ifelse(grepl("ESAC_Teste",all_data$site),40.211873,
                                                                                                                                       ifelse(grepl("SJD_JAEL1",all_data$site),40.6724388895208,
                                                                                                                                              ifelse(grepl("SJD_JAEL2",all_data$site),40.673035,
                                                                                                                                                     ifelse(grepl("SJD1",all_data$site),40.67424688032262026,
                                                                                                                                                            ifelse(grepl("SJD2",all_data$site),40.67504833221530447,all_data$site)))))))))))))))))))))

all_data$lon <- ifelse(grepl("QUD1",all_data$site), -8.8887518,
                       ifelse(grepl("QUD2",all_data$site),-8.88808660606060563,
                              ifelse(grepl("SBV1",all_data$site),-8.8911566477474846,
                                     ifelse(grepl("SBV2",all_data$site),-8.89130474040791441,
                                            ifelse(grepl("ESP2",all_data$site),-8.78505684981873003,
                                                   ifelse(grepl("TOC1",all_data$site),-8.8208499891281118,
                                                          ifelse(grepl("TOC2",all_data$site),-8.8245842637479246,
                                                                 ifelse(grepl("FAR1",all_data$site),-8.005328,
                                                                        ifelse(grepl("FAR2",all_data$site),-8.004749,
                                                                               ifelse(grepl("LSA1",all_data$site),-8.851849324,
                                                                                      ifelse(grepl("LSA2",all_data$site),-8.852139557,
                                                                                             ifelse(grepl("SPM1",all_data$site),-9.0210281,
                                                                                                    ifelse(grepl("SPM2",all_data$site),-9.02144823488168335,
                                                                                                           ifelse(grepl("SEI1",all_data$site),-8.766142984,
                                                                                                                  ifelse(grepl("SEI2",all_data$site),-8.766761358,
                                                                                                                         ifelse(grepl("ESAC_Frigo",all_data$site),-8.453358,
                                                                                                                                ifelse(grepl("ESAC_Teste",all_data$site),-8.453358,
                                                                                                                                       ifelse(grepl("SJD_JAEL1",all_data$site),-8.74220332517403,
                                                                                                                                              ifelse(grepl("SJD_JAEL2",all_data$site),-8.741245,
                                                                                                                                                     ifelse(grepl("SJD1",all_data$site),-8.74094232750662847,
                                                                                                                                                            ifelse(grepl("SJD2",all_data$site),-8.74048588338050259,all_data$site)))))))))))))))))))))


all_data$lat <- as.numeric(all_data$lat)
all_data$lon <- as.numeric(all_data$lon)


# adding a variable informing about the working status of the HOBO. 
# value 1 -> working, 0 -> not working
all_data$status <- ifelse(grepl("QUD1",all_data$site), 1,
                       ifelse(grepl("QUD2",all_data$site),1,
                              ifelse(grepl("SBV1",all_data$site),0,
                                     ifelse(grepl("SBV2",all_data$site),0,
                                            ifelse(grepl("ESP2",all_data$site),0,
                                                   ifelse(grepl("TOC1",all_data$site),0,
                                                          ifelse(grepl("TOC2",all_data$site),0,
                                                                 ifelse(grepl("FAR1",all_data$site),1,
                                                                        ifelse(grepl("FAR2",all_data$site),1,
                                                                               ifelse(grepl("LSA1",all_data$site),1,
                                                                                      ifelse(grepl("LSA2",all_data$site),1,
                                                                                             ifelse(grepl("SPM1",all_data$site),0,
                                                                                                    ifelse(grepl("SPM2",all_data$site),0,
                                                                                                           ifelse(grepl("SEI1",all_data$site),0,
                                                                                                                  ifelse(grepl("SEI2",all_data$site),1,
                                                                                                                         ifelse(grepl("ESAC_Frigo",all_data$site),1,
                                                                                                                                ifelse(grepl("ESAC_Teste",all_data$site),1,
                                                                                                                                       ifelse(grepl("SJD_JAEL1",all_data$site),0,
                                                                                                                                              ifelse(grepl("SJD_JAEL2",all_data$site),0,
                                                                                                                                                     ifelse(grepl("SJD1",all_data$site),1,
                                                                                                                                                            ifelse(grepl("SJD2",all_data$site),1,all_data$site)))))))))))))))))))))

all_data$status <- as.factor(all_data$status)

                                                              
# dropping date duplicates
all_data <- all_data %>% distinct(DATE, site, .keep_all = TRUE) 



## ANALYZING THE DATA ----
## + Averages ---- 
# ++ daily means ----
daily_means <- all_data %>%
  filter(str_detect(site_merged, "SJD$") 
         | str_detect(site_merged, "SEI") 
         | str_detect(site_merged, "QUD") 
         | str_detect(site_merged, "LSA") 
         | str_detect(site_merged, "FAR")) %>%
  #droplevels() %>%
  #filter(site_merged != "SJD_") %>%
  droplevels() %>%
  mutate(temp1 = replace(temp, temp>50, NA)) %>% # To avoid removing temp values on lines with temp > 50?C, I replaced all values above 50?C by NA
  mutate(light1 =replace(light, light<=0, NA)) %>%
  group_by(YEAR, MONTH, DAY, site_merged) %>%
  summarise(meantemp = mean(temp1, na.rm = T),
            meanlight = mean(light1, na.rm = T))%>%
  mutate(MY = lubridate::ym(paste0(YEAR,"/",MONTH)))

# ++ monthly means ----
monthly_means <- daily_means %>%
  group_by(YEAR, MONTH, site_merged) %>%
  summarise(mean_month_temp = mean(meantemp, na.rm = T),
            mean_month_light = mean(meanlight, na.rm = T))%>%
  mutate(MY = lubridate::ym(paste0(YEAR,"/",MONTH)))



# plotting data
tempplot <- ggplot(monthly_means, aes(x=MY, y=mean_month_temp))+
  geom_point(aes(col=site_merged,fill=site_merged))+
  geom_smooth(aes(colour=site_merged))+
  scale_x_date(date_breaks= "1 year", date_labels = "%Y")+
  scale_y_continuous(breaks=seq(round(min(monthly_means$mean_month_temp)),round(max(monthly_means$mean_month_temp))))+
  theme_light()+
  #theme(legend.title = element_blank(),legend.position = "none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title= "Monthly temperature means", y="Monthly mean temperature (°C) with 95% CI", x="Date", col="Site", fill="Site")
tempplot

lightplot <- ggplot(monthly_means, aes(x=MY, y=mean_month_light))+
  geom_point(aes(col=site_merged,fill=site_merged))+
  geom_smooth(aes(colour=site_merged))+
  scale_x_date(date_breaks= "1 year", date_labels = "%Y")+
  scale_y_continuous(breaks=seq(0,round(max(monthly_means$mean_month_light)),5000))+
  #scale_color_manual(labels = c("Faro", "Santo André","Quiaios","Seixo","São Jacinto"))+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title= "Monthly sunlight means", y="Monthly mean sunlight (lux) with 95% CI", x="Date", col="Site", fill="Site")
lightplot


# + Ranges ----
# ++ daily ranges ----
daily_range <- all_data %>%
  filter(str_detect(site_merged, "SJD$") 
         | str_detect(site_merged, "SEI") 
         | str_detect(site_merged, "QUD") 
         | str_detect(site_merged, "LSA") 
         | str_detect(site_merged, "FAR")) %>%
  #droplevels() %>%
  #filter(site_merged != "SJD_") %>%
  droplevels() %>%
  mutate(temp1 = replace(temp, temp>50, NA)) %>% # To avoid removing temp values on lines with temp > 50?C, I replaced all values above 50?C by NA
  mutate(light1 =replace(light, light<=0, NA)) %>%
  na.omit() %>%
  group_by(YEAR, MONTH, DAY, site_merged) %>%
  summarise(min_temp = min(temp1), max_temp = max(temp1),
            min_light = min(light1), max_light = max(light1))%>%
  mutate(temp_range = max_temp-min_temp,
         light_range = max_light-min_light,
         MY = lubridate::ym(paste0(YEAR,"/",MONTH)))

# ++ average daily ranges per month ----
monthly_range <- daily_range %>%
  group_by(YEAR, MONTH, site_merged) %>%
  summarise(temp_mean_month_range = mean(temp_range, na.rm = T),
            light_mean_month_range = mean(light_range, na.rm = T))%>%
  mutate(MY = lubridate::ym(paste0(YEAR,"/",MONTH)))  


# plotting data
tempplot1 <- ggplot(monthly_range, aes(x=MY, y=temp_mean_month_range))+
  #geom_point(aes(col=site,fill=site))+
  geom_smooth(aes(colour=site_merged))+
  scale_x_date(date_breaks= "1 year", date_labels = "%Y")+
  scale_y_continuous(breaks=seq(round(min(monthly_range$temp_mean_month_range)),round(max(monthly_range$temp_mean_month_range))))+
  theme_light()+
  #theme(legend.title = element_blank(),legend.position = "none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title= "Daily temperature range", y="Mean daily temperature range (°C) with 95% CI", x="Date", col="Site", fill="Site")
tempplot1

lightplot1 <- ggplot(monthly_range, aes(x=MY, y=light_mean_month_range))+
  #geom_point(aes(col=site,fill=site))+
  geom_smooth(aes(colour=site_merged))+
  scale_x_date(date_breaks= "1 year", date_labels = "%Y")+
  scale_y_continuous(breaks=seq(0,round(max(monthly_range$light_mean_month_range)),20000))+
  #scale_color_manual(labels = c("Faro", "Santo André","Quiaios","Seixo","São Jacinto"))+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title= "Daily sunlight range", y="Mean daily sunlight range (lux) with 95% CI", x="Date", col="Site", fill="Site")
lightplot1


# + Max Min ----
# ++ average monthly max and min ----
maxmin_average <- daily_range %>%
  group_by(YEAR, MONTH, site_merged) %>%
  summarise(mean_min_temp = mean(min_temp, na.rm = T), mean_max_temp = mean(max_temp, na.rm = T),
            mean_min_light = mean(min_light, na.rm = T), mean_max_light = mean(max_light, na.rm = T))%>%
  mutate(MY = lubridate::ym(paste0(YEAR,"/",MONTH)))
  

# plotting data
tempplot2 <- ggplot(maxmin_average, aes(x=MY, y=mean_min_temp))+
  #geom_point(aes(col=site,fill=site))+
  geom_smooth(aes(colour=site_merged))+
  scale_x_date(date_breaks= "1 year", date_labels = "%Y")+
  scale_y_continuous(breaks=seq(round(min(maxmin_average$mean_min_temp)),round(max(maxmin_average$mean_min_temp))))+
  theme_light()+
  #theme(legend.title = element_blank(),legend.position = "none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title= "Average monthly minimum temperature", y="Mean monthly minimum temperature (°C) with 95% CI", x="Date", col="Site", fill="Site")
tempplot2

tempplot3 <- ggplot(maxmin_average, aes(x=MY, y=mean_max_temp))+
  #geom_point(aes(col=site,fill=site))+
  geom_smooth(aes(colour=site_merged))+
  scale_x_date(date_breaks= "1 year", date_labels = "%Y")+
  scale_y_continuous(breaks=seq(round(min(maxmin_average$mean_max_temp)),round(max(maxmin_average$mean_max_temp))))+
  theme_light()+
  #theme(legend.title = element_blank(),legend.position = "none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title= "Average monthly maximum temperature", y="Mean monthly maximum temperature (°C) with 95% CI", x="Date", col="Site", fill="Site")
tempplot3



lightplot2 <- ggplot(maxmin_average, aes(x=MY, y=mean_min_light))+
  #geom_point(aes(col=site,fill=site))+
  geom_smooth(aes(colour=site_merged))+
  scale_x_date(date_breaks= "1 year", date_labels = "%Y")+
  scale_y_continuous(breaks=seq(0,round(max(maxmin_average$mean_min_light)),100))+
  #scale_color_manual(labels = c("Faro", "Santo André","Quiaios","Seixo","São Jacinto"))+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title= "Average monthly minimum sunlight", y="Mean monthly minimum sunlight (lux) with 95% CI", x="Date", col="Site", fill="Site")
lightplot2

lightplot3 <- ggplot(maxmin_average, aes(x=MY, y=mean_max_light))+
  #geom_point(aes(col=site,fill=site))+
  geom_smooth(aes(colour=site_merged))+
  scale_x_date(date_breaks= "1 year", date_labels = "%Y")+
  scale_y_continuous(breaks=seq(0,round(max(maxmin_average$mean_max_light)),50000))+
  #scale_color_manual(labels = c("Faro", "Santo André","Quiaios","Seixo","São Jacinto"))+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title= "Average monthly maximum sunlight", y="Mean monthly maximum sunlight (lux) with 95% CI", x="Date", col="Site", fill="Site")
lightplot3




## SAVING PROCESSED DATA ------
# writting data in one big file

write.csv(monthly_means, paste(out_dir, "monthly_means_data_28092022.csv", sep="/"), 
          row.names=FALSE) # this file is still uncleaned for outliers! be carefull and clean it before analyze it
write.csv(monthly_range, paste(out_dir, "monthly_range_data_28092022.csv", sep="/"), 
          row.names=FALSE) # this file is still uncleaned for outliers! be carefull and clean it before analyze it
write.csv(maxmin_average, paste(out_dir, "monthly_maxmin_data_28092022.csv", sep="/"), 
          row.names=FALSE) # this file is still uncleaned for outliers! be carefull and clean it before analyze it


# exporting plots
png("monthly_means_28092022.png", width = 800, height = 800)
plot_grid(tempplot, lightplot, labels = c('A', 'B'), label_size = 12, nrow=2)
dev.off()

png("monthly_average_daily_range_28092022.png", width = 800, height = 800)
plot_grid(tempplot1, lightplot1, labels = c('A', 'B'), label_size = 12, nrow=2)
dev.off()

png("monthly_average_maxmin_28092022.png", width = 800, height = 800)
plot_grid(tempplot2,  tempplot3, lightplot2,lightplot3,  labels = c('A', 'B', 'C', 'D'), label_size = 12, nrow=2)
dev.off()





# drop rows with missing data
#all_data <- all_data[complete.cases(all_data),]

#all_data[, date:=floor_date(mdy_hms(date), "minute")] # floor date to nearest minute






