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
files <- list.files("C:\\Users\\LENOVO\\Google Drive\\Databases\\Hobos_data\\", pattern=".csv", full.names=T)
names.files <- list.files("C:\\Users\\LENOVO\\Google Drive\\Databases\\Hobos_data\\", pattern=".csv")


all_data <- lapply(files, fread,select=c(1:5), header = FALSE, skip=1, encoding = "UTF-8", dec=".")

for (i in 1:length(all_data)){
  all_data[[i]]<-cbind(all_data[[i]],names.files[i])}


## PROCESSING RAW DATA ---------
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
                                                                                                                                                                          ifelse(grepl("SJD2",all_data$file),'SJD2',
                                                                                                                                                                                 ifelse(grepl("SJD2_",all_data$file),'SJD2',
                                                                                                                                                                                        ifelse(grepl("SJacinto2_",all_data$file),'SJD2',
                                                                                                                                                                                               ifelse(grepl("SEI1_HOBO",all_data$file),'SEI1',
                                                                                                                                                                                                      ifelse(grepl("SEI2_HOBO",all_data$file),'SEI2',
                                                                                                                                                                                                             ifelse(grepl("Esposende2-",all_data$file),'ESP2',all_data$file))))))))))))))))))))))))))))





unique(all_data$site)
all_data$site <- as.factor(all_data$site)


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


# monthly trends
# filtering by site
sjd <- all_data %>%
  filter(str_detect(site, "SJD1") | str_detect(site, "SJD2")) %>%
  droplevels() %>%
  #filter(temp < 50) %>% # I want to keep only with temp values <50?c
  mutate(temp1 = replace(temp, temp>50, NA)) %>% # To avoid removing temp values on lines with temp > 50?C, I replaced all values above 50?C by NA
  mutate(light1 =replace(light, light<=0, NA)) %>% # To avoid use the night light values on mean estimation, I replaced all values equal or below to 0 by NA
  group_by(YEAR, MONTH) %>%
  summarize(TEMP_mean_dayly_x_month = mean(temp1, na.rm=T), # These are the average values of dayly temp and light by each month 
            LIGHT_mean_dayly_x_month = mean(light1, na.rm = T)) %>%
  mutate(MY = lubridate::ym(paste0(YEAR,"/",MONTH)))
sjd$site <- rep("sjd", nrow(sjd))


qud <- all_data %>%
  filter(str_detect(site, "QUD1") | str_detect(site, "QUD2")) %>%
  droplevels() %>%
  mutate(light1 =replace(light, light<=0, NA)) %>% # To avoid use the night light values on mean estimation, I replaced all values equal or below to 0 by NA
  group_by(YEAR, MONTH) %>%
  summarize(TEMP_mean_dayly_x_month = mean(temp, na.rm=T), # These are the average values of dayly temp and light by each month 
            LIGHT_mean_dayly_x_month = mean(light1, na.rm = T)) %>%
  mutate(MY = lubridate::ym(paste0(YEAR,"/",MONTH)))
qud$site <- rep("qud", nrow(qud))


far <- all_data %>%
  filter(str_detect(site, "FAR1") | str_detect(site, "FAR2")) %>%
  droplevels() %>%
  #filter(temp < 50) %>% # I want to keep only with temp values <50?c
  mutate(temp1 = replace(temp, temp>50, NA)) %>% # To avoid removing temp values on lines with temp > 50?C, I replaced all values above 50?C by NA
  mutate(light1 =replace(light, light<=0, NA)) %>% # To avoid use the night light values on mean estimation, I replaced all values equal or below to 0 by NA
  group_by(YEAR, MONTH) %>%
  summarize(TEMP_mean_dayly_x_month = mean(temp1, na.rm=T), # These are the average values of dayly temp and light by each month 
            LIGHT_mean_dayly_x_month = mean(light1, na.rm = T)) %>%
  mutate(MY = lubridate::ym(paste0(YEAR,"/",MONTH)))
far$site <- rep("far", nrow(far))

lsa <- all_data %>%
  filter(str_detect(site, "LSA1") | str_detect(site, "LSA2")) %>%
  droplevels() %>%
  #filter(temp < 50) %>% # I want to keep only with temp values <50?c
  #mutate(temp1 = replace(temp, temp>50, NA)) %>% # To avoid removing temp values on lines with temp > 50?C, I replaced all values above 50?C by NA
  mutate(light1 =replace(light, light<=0, NA)) %>% # To avoid use the night light values on mean estimation, I replaced all values equal or below to 0 by NA
  group_by(YEAR, MONTH) %>%
  summarize(TEMP_mean_dayly_x_month = mean(temp, na.rm=T), # These are the average values of dayly temp and light by each month 
            LIGHT_mean_dayly_x_month = mean(light1, na.rm = T)) %>%
  mutate(MY = lubridate::ym(paste0(YEAR,"/",MONTH)))
lsa$site <- rep("lsa", nrow(lsa))

sei <- all_data %>%
  filter(str_detect(site, "SEI1") | str_detect(site, "SEI2")) %>%
  droplevels() %>%
  filter(temp < 50) %>% # I want to keep only with temp values <50?c
  mutate(temp1 = replace(temp, temp>50, NA)) %>% # To avoid removing temp values on lines with temp > 50?C, I replaced all values above 50?C by NA
  mutate(light1 =replace(light, light<=0, NA)) %>% # To avoid use the night light values on mean estimation, I replaced all values equal or below to 0 by NA
  group_by(YEAR, MONTH) %>%
  summarize(TEMP_mean_dayly_x_month = mean(temp1, na.rm=T), # These are the average values of dayly temp and light by each month 
            LIGHT_mean_dayly_x_month = mean(light1, na.rm = T)) %>%
  mutate(MY = lubridate::ym(paste0(YEAR,"/",MONTH)))
sei$site <- rep("sei", nrow(sei))


all_data1 <- rbind(sjd,qud,far,lsa,sei)


# plotting data
tempplot <- ggplot(all_data1, aes(x=MY, y=TEMP_mean_dayly_x_month))+
  geom_point(aes(col=site,fill=site))+
  geom_smooth(aes(colour=site))+
  scale_x_date(date_breaks= "1 year", date_labels = "%Y")+
  scale_y_continuous(breaks=seq(round(min(all_data1$TEMP_mean_dayly_x_month)),round(max(all_data1$TEMP_mean_dayly_x_month))))+
  theme_light()+
  #theme(legend.title = element_blank(),legend.position = "none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title= "Monthly temperature means", y="Monthly mean temperature (°C) with 95% CI", x="Date", col="Site", fill="Site")
tempplot

lightplot <- ggplot(all_data1, aes(x=MY, y=LIGHT_mean_dayly_x_month))+
  geom_point(aes(col=site,fill=site))+
  geom_smooth(aes(colour=site))+
  scale_x_date(date_breaks= "1 year", date_labels = "%Y")+
  scale_y_continuous(breaks=seq(0,round(max(all_data1$LIGHT_mean_dayly_x_month)),5000))+
  #scale_color_manual(labels = c("Faro", "Santo André","Quiaios","Seixo","São Jacinto"))+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title= "Monthly sunlight means", y="Monthly mean sunlight (lux) with 95% CI", x="Date", col="Site", fill="Site")
lightplot




## SAVING PROCESSED DATA ------
# writting data in one big file

write.csv(all_data1, paste(out_dir, "monthly_means_data.csv", sep="/"), 
          row.names=FALSE) # this file is still uncleaned for outliers! be carefull and clean it before analyze it

# exporting plots
png("monthly_trends.png", width = 800, height = 800)
plot_grid(tempplot, lightplot, labels = c('A', 'B'), label_size = 12, nrow=2)
dev.off()





# drop rows with missing data
#all_data <- all_data[complete.cases(all_data),]

#all_data[, date:=floor_date(mdy_hms(date), "minute")] # floor date to nearest minute






