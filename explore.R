install.packages("rmarkdown")


install.packages("tidyverse", dependencies=TRUE)
install.packages("tidyverse")
install.packages("xlsx")

install.packages("maptools")
install.packages("raster")
install.packages("rgdal")

install.packages("ggmap")
install.packages("extrafont")

library(tidyverse)
library(readxl)
library(lubridate)
library(rmarkdown)
library(xlsx)
library(ggplot2)
library(broom)

library(maptools)
library(raster)
library(plyr)
library(rgdal)
library(ggmap)
library(scales)

library(extrafont)
font_import()
loadfonts(device = "win")

##### PRELIMINARY DATA CLEANING #####

incidents <- read_csv("C:/Users/Lily/Desktop/Data Analysis/Iraq War/ibc-incidents-2020-1-31.csv", skip = 11, col_names = TRUE) %>%
  mutate(loc1 = word(Location,-1)) %>%
  mutate(StartDate = dmy(`Start Date`)) %>%
  mutate(EndDate = dmy(`End Date`))

individuals <- read_xlsx("C:/Users/Lily/Desktop/Data Analysis/Iraq War/individuals_edit.xlsx",sheet = "individuals") %>%
  mutate(IBC = word(`IBC code`,1,sep = "-")) %>%
  distinct(IBC,.keep_all = TRUE) %>%
  select(11:12)

incidents_half <- incidents %>%
  left_join(individuals,by = c("IBC code" = "IBC")) %>%
  mutate(Gov_1 = ifelse(is.na(Gov),loc1,Gov)) %>%
  select(-"Start Date",-"End Date")



write.xlsx(incidents_half, "C:/Users/Lily/Desktop/Data Analysis/Iraq War/incidents_edit.xlsx", sheetName="Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)

write.csv(incidents_half,"C:/Users/Lily/Desktop/Data Analysis/Iraq War/incidents_edit.csv", row.names = TRUE)

distinct(incidents, loc1)
colnames(incidents_half)

incidents %>%
  filter(Governorate == "Al-Din")


##### FINAL DATA CLEANING #####


#Use new updated file
ByGov <- read_xlsx("C:/Users/Lily/Desktop/Data Analysis/Iraq War/incidents_edit.xlsx",sheet = "incidents_edit") %>%
  dplyr::mutate(Governorate = Gov_2) %>%
  dplyr::mutate(Year = year(StartDate)) %>%
  dplyr::group_by(Year,Governorate) %>%
  dplyr::summarize(Casualty = sum(`Reported Maximum`)) %>%
  dplyr::ungroup() %>%
  add_row(Year = 2003, Governorate = "Dohuk", Casualty = 0) %>%
  add_row(Year = 2004, Governorate = "Sulaymaniyah", Casualty = 0) %>%
  add_row(Year = 2004, Governorate = "Halabjah", Casualty = 0) %>%
  add_row(Year = 2007, Governorate = "Dohuk", Casualty = 0) %>%
  add_row(Year = 2007, Governorate = "Halabjah", Casualty = 0) %>%
  add_row(Year = 2009, Governorate = "Dohuk", Casualty = 0) %>%
  add_row(Year = 2009, Governorate = "Halabjah", Casualty = 0) %>%
  add_row(Year = 2010, Governorate = "Dohuk", Casualty = 0) %>%
  add_row(Year = 2010, Governorate = "Halabjah", Casualty = 0) %>%
  add_row(Year = 2012, Governorate = "Halabjah", Casualty = 0) %>%
  add_row(Year = 2013, Governorate = "Halabjah", Casualty = 0) %>%
  add_row(Year = 2014, Governorate = "Halabjah", Casualty = 0) %>%
  add_row(Year = 2015, Governorate = "Halabjah", Casualty = 0) %>%
  add_row(Year = 2015, Governorate = "Karbala", Casualty = 0) %>%
  add_row(Year = 2016, Governorate = "Halabjah", Casualty = 0) %>%
  add_row(Year = 2017, Governorate = "Qadissiya", Casualty = 0) %>%
  add_row(Year = 2017, Governorate = "Erbil", Casualty = 0) %>%
  add_row(Year = 2017, Governorate = "Sulaymaniyah", Casualty = 0) %>%
  add_row(Year = 2017, Governorate = "Halabjah", Casualty = 0) %>%
  add_row(Year = 2017, Governorate = "Babil", Casualty = 0) %>%
  add_row(Year = 2017, Governorate = "Dohuk", Casualty = 0) %>%
  add_row(Year = 2017, Governorate = "Missan", Casualty = 0) %>%
  add_row(Year = 2017, Governorate = "Wassit", Casualty = 0) %>%
  dplyr::mutate(Governorate= fct_recode(Governorate,"Border" = "Border/Between Governorates")) %>%
  dplyr::filter(!Governorate %in% c("Nationwide excluding Baghdad","Outside Iraq","Northern Iraq","Southern Iraq","South-Central Iraq")) %>%
  droplevels %>%
  dplyr::mutate(Region = fct_collapse(Governorate
                               ,Northern = c("Salah Al-Din","Kirkuk","Sulaymaniyah","Halabjah","Erbil","Dohuk","Ninawa")
                               ,Southern = c("Missan","Thi-Qar","Muthanna","Basra")
                               ,Western = c("Anbar")
                               ,Eastern = c("Diyala")
                               ,Central = c("Baghdad")
                               ,SouthCentral = c("Karbala","Babil","Wassit","Qadissiya","Najaf"))
  ) %>%
  dplyr::mutate(Governorate = as.factor(Governorate)) %>%
  dplyr::mutate(Year = as.factor(Year)) %>%
  dplyr::arrange(Year,Governorate)


#Check ByGov data frame
str(ByGov)
summary(ByGov)
head(ByGov)


levels(ByGov$Governorate)
names(ByGov)

#Remove unused object
remove(incidents)
remove(g)
remove(gadm,Iraq_UTM,NAME_1,UTM_Name,gov_3,ByGovUTM,Iraq_ByGov)
remove(reorder_within,scale_x_reordered,scale_y_reordered)
remove(ByGovUTMProp)
remove(theme_opts)
remove(theme.porttheme,theme.smallmult)
remove(Iraq_ByGov_Winz)
remove(ByGovUTMWithRank)

##### THEME #####

theme.universal <-  
  theme(text = element_text(family = "Franklin Gothic Medium", color = "#444444")) +
  theme(plot.title = element_text(size = 24)) +
  theme(plot.subtitle = element_text(size = 18)) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.text = element_text(size = 12))


##### CHLOROPHLET #####

#get iraq outline map
gadm <- readRDS("C:/Users/Lily/Desktop/Data Analysis/Iraq War/gadm36_IRQ_1_sp.rds")

#check iraq outline map
plot(gadm)

#Store in a dataframe
Iraq_UTM <- spTransform(gadm, CRS("+init=EPSG:4667")) 

#check
head(Iraq_UTM)

#Map Iraq_UTM province name with ByGov governorate

#Get Iraq_UTM province name
NAME_1 <- as.data.frame(Iraq_UTM@data$NAME_1)

#create Iraq_UTM province IDs
UTM_Name <- mutate(NAME_1,id = rownames(NAME_1))

#get ByGov Governorate
gov_1 <- as.data.frame(distinct(ByGov,Governorate) %>% dplyr::arrange(Governorate))

#create ByGov Governorate IDs
gov_id <- c("1","9","10","2","00","13","12","6","7","14","8","15","3","5","00","16","00","00","4","17","00","00","7","11","00","18")

#Merge ByGov Governorate with newly created IDs
gov_2 <- cbind(gov_1,gov_id) %>% dplyr::arrange(gov_id)

#join Iraq_UTM province with ByGov Governorate using IDs
gov_3 <- inner_join(UTM_Name,gov_2, by = c("id" = "gov_id")) %>%
  dplyr::rename(NAME_1 = `Iraq_UTM@data$NAME_1`)

#Check
head(gov_3,30)

#Join with ByGov data
ByGovUTM <- inner_join(ByGov,gov_3,by = "Governorate") %>%
  dplyr::select(Year,Governorate,NAME_1,Casualty)

#Check
head(ByGovUTM)

#Map ByGovUTM with Iraq_UTM, to join data with the polygon
Iraq_UTM@data$id <- rownames(Iraq_UTM@data)
Iraq_UTM@data <- join(Iraq_UTM@data, ByGovUTM, by="NAME_1")
Iraq_ByGov <- tidy(Iraq_UTM)
Iraq_ByGov <- join(Iraq_ByGov,Iraq_UTM@data, by="id")


#Windsorization to narrow the gap
summary(ByGovUTM)

bench <- 498 + 1.5*IQR(ByGovUTM$Casualty)

Iraq_ByGov_Winz <- Iraq_ByGov

Iraq_ByGov_Winz$Casualty[Iraq_ByGov_Winz$Casualty > bench] <- bench

#Check data range after windsorization
boxplot(Iraq_ByGov_Winz$Casualty)

ggplot(Iraq_ByGov_Winz,aes(x = Year, y = Casualty)) + geom_boxplot(aes(group = Year))



#Create clorophlet
ggplot() + 
  geom_polygon(data = Iraq_ByGov_Winz, aes(x = long, y = lat, group = group, fill = Casualty), color = "black", size = 0.25) + 
  theme(aspect.ratio=1) +
  scale_fill_distiller(name="Casualty", palette = "Reds", direction = 1) +
  facet_wrap(.~Year,ncol = 5) +
  labs(title = "Spread of Casualty Year by Year") +
  theme.universal +
  theme(strip.text.x = element_text(size = 12)) +
  theme(legend.title = element_text(size = 11)) +
  theme(legend.text = element_text(size = 9)) +
  theme(axis.text = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(panel.grid = element_blank())
  


##### RANK CHART #####

ByGovWithRank <- ByGov %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(rank = min_rank(desc(Casualty))) %>%
  dplyr::ungroup()

levels(ByGovWithRank$Region)
as.data.frame(ByGovWithRank)


#NOTE: tidy this chart,highlight top 5
ByGovWithRank %>%
  dplyr::group_by(Governorate) %>%
  dplyr::summarize(Casualty = sum(Casualty)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(Casualty)) %>%
  dplyr::mutate(top_5_flag = ifelse(Governorate %in% c("Baghdad","Ninawa","Anbar","Diyala","Salah Al-Din"),T,F)) %>%
  ggplot(aes(x = reorder(Governorate,Casualty),y = Casualty)) +
  geom_bar(aes(fill = top_5_flag),stat = "identity") +
  geom_text(aes(label = comma(Casualty)), hjust = 1.1, color = "#FFFFFF", size = 4) +
  labs(title = "Iraq's Deadliest Governorate") +
  labs(subtitle = "From 2003 until 2017, in order of casualty") +
  scale_fill_manual(values = c("#999999","#CE1126")) +
  scale_y_log10() +
  scale_x_discrete(expand = c(0,0)) +
  theme.universal +
  theme(axis.text.x = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  coord_flip() +
  theme(legend.position = "none")
  



ByGovWithRank %>%
  filter(rank <= 10) %>%
  dplyr::mutate(gov_flag = ifelse(Governorate %in% c("Baghdad","Anbar","Salah Al-Din","Diyala","Ninawa"),T,F)) %>%
  dplyr::mutate(gov_top_labels = ifelse(gov_flag == T, Region,"other")) %>%

  ggplot(aes(x = Year, y = rank, group = Governorate)) +
  geom_line(aes(color = gov_top_labels, alpha = gov_flag), size = 2) +
  geom_point(aes(color = gov_top_labels, alpha = gov_flag), size = 2.3) +
  geom_point(color = "#FFFFFF", alpha = .8, size = .3) +
  #scale_y_log10(breaks = c(100,1000,10000)) +
  theme.universal +
  geom_text(data = ByGovUTMWithRank %>% filter(Year == "2017", rank <= 10), aes(label = Governorate, x = '2017') , hjust = -.05, color = "#888888", size = 4, family = "Franklin Gothic Medium") +
  geom_text(data = ByGovUTMWithRank %>% filter(Year == "2003", rank <= 10), aes(label = Governorate, x = '2003') , hjust = 1.05, color = "#888888", size = 4, family = "Franklin Gothic Medium") +
  scale_x_discrete(expand = c(.2, .2)) +
  scale_y_reverse(breaks = c(1,5,9)) +
  scale_alpha_discrete(range = c(.4,.9)) +
  labs(title = "Governorate Ranking by Casualty\nyear-over-year from 2003 to 2017") +
  labs(subtitle = "(Governorate ranks, by Casualty)") +
  labs(x = "Year", y = "Rank") +
  theme(panel.grid.major.x = element_line(color = "#F3F3F3")) +  
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom",legend.title = element_blank()) +
  scale_color_manual(values = c("#000000","#007A3D","#DACF2B","#CE1126","#BBBBBB"), labels = c("Western", "Central", "Eastern","Northern","Non Top 5")) +
  guides(alpha = FALSE)
  

