install.packages("rmarkdown")


install.packages("tidyverse", dependencies=TRUE)
install.packages("tidyverse")
install.packages("xlsx")

install.packages("maptools")
install.packages("raster")
install.packages("rgdal")

install.packages("ggmap")
install.packages("showtext")
install.packages("extrafont")

install.packages("zoo")

library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
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


#add font using extrafont
#font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()  #vector of font family names


#add the font using showtext
font_files() #Check font in system
font_add("Franklin Gothic Medium","framd.ttf", italic = "framdit.ttf")
showtext_auto()

windowsFonts(sans=windowsFont("Franklin Gothic Medium"))


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
InitialData <- read_xlsx("incidents_edit.xlsx",sheet = "incidents_edit") %>%
  dplyr::mutate(Governorate = Gov_2) %>%
  dplyr::mutate(StartDate = as_date(StartDate)) %>%
  dplyr::mutate(Governorate= fct_recode(Governorate,"Border" = "Border/Between Governorates")) %>%
  dplyr::filter(!Governorate %in% c("Nationwide excluding Baghdad","Outside Iraq","Northern Iraq","Southern Iraq","South-Central Iraq")) %>%
  droplevels
  

str(InitialData)

#Yearly-level data  
ByGov <- InitialData %>%
  dplyr::mutate(Year = year(StartDate)) %>%
  #dplyr::mutate(MonthYear = as.factor(substr(as.character(StartDate),1,7))) %>%
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

#use extrafont
theme.universal <-  
  theme(text = element_text(family = "Franklin Gothic Medium", color = "#444444"),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
  )
  
 
##### CHLOROPHLET #####

#get iraq outline map
gadm <- readRDS("gadm36_IRQ_1_sp.rds")

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
#edit IDs
gov_id <- c("1","9","10","2","00","13","12","6","7","14","8","15","3","5","16","4","17","7","11","00","18")

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
  

ggplot() + 
  geom_polygon(data = Iraq_ByGov_Winz, aes(x = long, y = lat, group = group, fill = Casualty), color = "black", size = 0.25) + 
  theme(aspect.ratio=1) +
  scale_fill_distiller(name="Casualty", palette = "Reds", direction = 1) +
  facet_wrap(.~Year,ncol = 5) +
  labs(title = "Spread of Casualty Year by Year") +
  theme.universal +
  theme(
    strip.text.x = element_text(size = 12),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )



##### YEARLY CHART #####

timeline <- data.frame(start_date=as_date(c("2003-03-20", "2011-12-19", "2014-01-01")), end_date=as_date(c("2011-12-18", "2013-12-31", "2017-12-09")), event=c("U.S. Invasion & Insurgency", "Protests & Insurgency", "ISIS War"))



InitialData %>%
  dplyr::group_by(StartDate) %>%
  dplyr::summarize(Casualty = sum(`Reported Maximum`)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = StartDate, y = Casualty)) +
  geom_rect(timeline, mapping = aes(xmin = start_date, ymin = 0, xmax = end_date, ymax = Inf, fill = event),inherit.aes = FALSE, alpha = 0.15) +
  geom_text(timeline,mapping = aes(x = start_date, y = 2000, label = event), size = 3, angle = 90, nudge_x = 50) +
  geom_line(color = "#CE1126",stat = "identity") +
  geom_smooth(color = "#007A3D") +
  #scale_y_continuous(expand = c(.05, .05)) +
  scale_y_log10() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme.universal +
  theme(axis.title.x = element_blank(),
        axis.text = element_text(size = 10),
        panel.grid = element_blank(),
        legend.position = "none") +
  labs(title = "Iraq's Casualty Overtime") +
  scale_fill_manual(values = c("#000000","#FFFFFF","#CE1126"))


#Backup yearly chart
ByGov %>%
  dplyr::group_by(Year) %>%
  dplyr::summarize(Casualty = sum(Casualty)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = Year, y = Casualty, label = Casualty)) +
  geom_bar(stat = "identity",fill = "#CE1126") +
  geom_text(aes(label = paste(round(Casualty/1000, 0), "K", sep="")), vjust = 1.1, color = "#FFFFFF", size = 4) + 
  #set line size
  geom_vline(xintercept = 9.5, color = "#444444", size = .5) +
  geom_vline(xintercept = 11.5, color = "#444444",size = .5) +
  #geom_hline(yintercept = 32000) +
  scale_y_continuous(expand = c(.08, .08), labels = scales::unit_format(unit = "k", scale = 1e-3)) +
  theme.universal +
  labs(title = "Iraq's Casualty by Year") +
  theme(panel.grid = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(axis.ticks = element_line(color = "#444444")) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(panel.grid = element_blank()) +
  annotate("text", x = 5, y = 35000, label = "US Invasion, Insurgency, and Civil War", color = "#444444", family = "Franklin Gothic Medium") +
  annotate("text", x = 10.55, y = 35000, label = "Insurgency\n& Protest", color = "#444444", family = "Franklin Gothic Medium") +
  annotate("text", x = 13.55, y = 35000, label = "ISIS War", color = "#444444", family = "Franklin Gothic Medium")


#Backup monthly chart
InitialData %>%
  dplyr::mutate(MonthYear = as.factor(substr(as.character(StartDate),1,7)) %>%
  dplyr::mutate(Year = as.factor(year(StartDate)))) %>%
  dplyr::group_by(MonthYear,Year) %>%
  dplyr::summarize(Casualty = sum(`Reported Maximum`)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = MonthYear, y = Casualty)) +
  geom_line(stat = "identity", group = 1) +
  scale_x_discrete(expand = c(.02, .02), breaks = c(
    "2003-03","2003-06","2003-09","2003-12"
    ,"2004-03","2004-06","2004-09","2004-12"
    ,"2005-03","2005-06","2005-09","2005-12"
    ,"2006-03","2006-06","2006-09","2006-12"
    ,"2007-03","2007-06","2007-09","2007-12"
    ,"2008-03","2008-06","2008-09","2008-12"
    ,"2009-03","2009-06","2009-09","2009-12"
    ,"2010-03","2010-06","2010-09","2010-12"
    ,"2011-03","2011-06","2011-09","2011-12"
    ,"2012-03","2012-06","2012-09","2012-12"
    ,"2013-03","2013-06","2013-09","2013-12"
    ,"2014-03","2014-06","2014-09","2014-12"
    ,"2015-03","2015-06","2015-09","2015-12"
    ,"2016-03","2016-06","2016-09","2016-12"
    ,"2017-03"
  )) +
  scale_y_continuous(expand = c(.05, .05)) +
  theme.universal +
  theme(axis.text.x = element_text(angle = 90,size = 12, lineheight = 3, margin(t = .1, r = .5, b = .1, l = .5), family = "Franklin Gothic Medium")) +
  theme(axis.title.x = element_blank())



##### RANK CHART #####

ByGovWithRank <- ByGov %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(rank = min_rank(desc(Casualty))) %>%
  dplyr::ungroup()

levels(ByGovWithRank$Region)
as.data.frame(ByGovWithRank)


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
  theme.universal +
  theme(axis.text.x = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_line(color = "#444444")) +
  theme(panel.grid = element_blank()) +
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
  theme.universal +
  geom_text(data = ByGovWithRank %>% filter(Year == "2017", rank <= 10), aes(label = Governorate, x = '2017') , hjust = -.05, color = "#888888", size = 4, family = "Franklin Gothic Medium") +
  geom_text(data = ByGovWithRank %>% filter(Year == "2003", rank <= 10), aes(label = Governorate, x = '2003') , hjust = 1.05, color = "#888888", size = 4, family = "Franklin Gothic Medium") +
  scale_x_discrete(expand = c(.2, .2)) +
  scale_y_reverse(breaks = c(1,5,10)) +
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
  