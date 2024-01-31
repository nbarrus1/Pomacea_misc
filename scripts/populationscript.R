#Script Title: Apple Snail Populations at LILA
#Date first created: 1/30/2024
#purpose: The purpose of this script is to formalize some of the population
#level analyses that I've only explored in excel.  These include timeseries of
#populations through time and the relationship between percapita reproduction and
#estimated population size.

#----------------------------
##general start to scripting##
#----------------------------

rm(list = ls())

######github#####
#note, only needed after 90 days from 1/16/2024

#  usethis::create_github_token()  
#  gitcreds::gitcreds_set()

#####check for r updates#####
#note, updateing may take some time so plan accordingly

require(installr)

check.for.updates.R()

#updateR() #only if needed

#######check for package updates#####
#note, updateing may take some time so plan accordingly

old.packages()

update.packages()

#------------------------------------
#####load packages#####
#------------------------------------

library(tidyverse)
library(here)
library(readxl)
library(lubridate)
library(lemon)

theme_set(theme_bw(base_size = 15))

#-----------------------------------
#####Load Data####
#-----------------------------------


eggdata.master <- read_excel("C:/Users/nbarr/OneDrive/Pictures/Documents/FIU/LAB_data/LILA/eggmass_2018-2021/EggMassSurvey/EggMassData_v1.12_nb.xlsx",
                             sheet = 2)%>% 
  mutate(Year = year(Date),
         Month = month(Date),
         Day = day(Date))

#load in the newer data collected from the FIU lab and then reformat for merging.
#note) this will need to be done by copying and pasting the code from lines 25-34
#for each additional year of surveys done in FIU.
#note 2) the North transect was renamed to be called T1 transect and South transect
#was renamed to be called the T2 transect (see metadata in the FAU data file "EggMassData_v1.12_nb.xlsx")
#note 3) the P. paludosa data for M4 is removed because Paludosa have been extirpated
#from Cell M4 (see code lines 39-41)

#2022

eggdata.2022 <- read_excel("C:/Users/nbarr/OneDrive/Pictures/Documents/FIU/LAB_data/LILA//2022/Egg Masses/LILA_EMS_2022_EMC.xlsx",
                           sheet = 1) %>% 
  rename(Cell = Macrocosm,
         Notes = Comment) %>% 
  gather(`Pompal Ridge`,`Pompal Slough`,`Pommac Ridge`,`Pommac Slough`, key = "group", value = "Count") %>% 
  separate(group, sep = " ", c("Pomacea_Species","Habitat")) %>% 
  mutate(Pomacea_Species = if_else(Pomacea_Species == "Pompal", true = "Paludosa",
                                   false = "Maculata"),
         Transect = if_else(Transect == "NORTH", true = "T1",
                            false = "T2"),
         Cell = if_else(Cell == "M4" & Pomacea_Species == "Paludosa",true = "extirpated",
                        false = paste(Cell))) %>% 
  filter(Cell != "extirpated")

#2023

eggdata.2023 <- read_excel("C:/Users/nbarr/OneDrive/Pictures/Documents/FIU/LAB_data/LILA/2023/Egg Masses/LILA_EMS_2023_EMC.xlsx",
                           sheet = 1) %>% 
  rename(Cell = Macrocosm,
         Notes = Comment) %>% 
  gather(`Pompal Ridge`,`Pompal Slough`,`Pommac Ridge`,`Pommac Slough`, key = "group", value = "Count") %>% 
  separate(group, sep = " ", c("Pomacea_Species","Habitat")) %>% 
  mutate(Pomacea_Species = if_else(Pomacea_Species == "Pompal", true = "Paludosa",
                                   false = "Maculata"),
         Transect = if_else(Transect == "NORTH", true = "T1",
                            false = "T2"),
         Cell = if_else(Cell == "M4" & Pomacea_Species == "Paludosa",true = "extirpated",
                        false = paste(Cell))) %>% 
  filter(Cell != "extirpated")


#merge the newer data collected from the FIU lab to the eggdata.master (the older FAU data)

eggdata.master <- eggdata.master %>% 
  bind_rows(eggdata.2022) |> 
  bind_rows(eggdata.2023)

#check the data by year to see if the merge was successful 

table(eggdata.master$Year,eggdata.master$Transect)
table(eggdata.master$Year,eggdata.master$Cell)

#check the counts
hist(eggdata.master$Count)

#check for NAs
table(is.na(eggdata.master$Cell))
table(is.na(eggdata.master$Transect))
table(is.na(eggdata.master$Habitat))
table(is.na(eggdata.master$Pomacea_Species))
table(is.na(eggdata.master$Count))

#the merge looks good when there are equal numbers of transects T1 and T2 by year
#note) there were additional transects walked 2019 and 2021 to check for egg clutches
#in other habitats
#note 2) there should be no NA in other wards lines 57-61 should give all FALSE when run

#####add the Area data from the transects ##### 

#Manually create this dataframe since this info is in the Meta Data and the size of 
#the data is not large 
#note) See Meta data to corroborate the actual areas of each transect

transectarea <- tibble(Cell = c("M4","M4","M3","M3","M2","M2","M1","M1"),
                       Transect = c("T1","T2","T1","T2","T1","T2","T1","T2"),
                       Area_m= c(1408,1268,1368,1264,1340,1276,1320,1292)) %>% 
  group_by(Cell) %>% 
  summarise(tot.area.m = sum(Area_m),
            tot.area.ha = tot.area.m/10000)


slougharea <- tibble(Wetland = c("M1", "M2", "M3", "M4"),
                     Area_ha = c(2.49,2.39,2.44,2.46))

#density data

invt.2018.summer <- read_csv("C:/Users/nbarr/OneDrive/Pictures/Documents/FIU/LAB_data/LILA/2018/LILA_TT_2018_INVT_SUMMER.csv",
                             na = ".")
invt.2019.spring <- read_csv("C:/Users/nbarr/OneDrive/Pictures/Documents/FIU/LAB_data/LILA/2019/Throw Trapping/Spring/LILA_TT_2019_INVT_SPRING.csv",
                             na = ".")
invt.2019.summer <- read_csv("C:/Users/nbarr/OneDrive/Pictures/Documents/FIU/LAB_data/LILA/2019/Throw Trapping/Summer/LILA_TT_2019_INVT_SUMMER.csv",
                             na = ".")
invt.2020.spring <- read_csv("C:/Users/nbarr/OneDrive/Pictures/Documents/FIU/LAB_data/LILA/2020/Throw Trapping/Spring/LILA_TT_2020_INVT_SPRING.csv",
                             na = ".")
invt.2020.summer <- read_csv("C:/Users/nbarr/OneDrive/Pictures/Documents/FIU/LAB_data/LILA/2020/Throw Trapping/Summer/LILA_TT_2020_INVT_SUMMER.csv",
                             na = ".")
invt.2021.spring <- read_csv("C:/Users/nbarr/OneDrive/Pictures/Documents/FIU/LAB_data/LILA/2021/Throw Trapping/Spring/LILA_TT_2021_INVT_SPRING.csv",
                             na = ".")
invt.2021.summer <- read_csv("C:/Users/nbarr/OneDrive/Pictures/Documents/FIU/LAB_data/LILA/2021/Throw Trapping/Summer/LILA_TT_2021_INVT_SUMMER.csv",
                             na = ".")
invt.2022.spring <- read_excel("C:/Users/nbarr/OneDrive/Pictures/Documents/FIU/LAB_data/LILA/2022/Throw Trapping/Spring/LILA_TT_2022_INVT_SPRING.xlsx",
                               na = ".")
invt.2022.summer <- read_excel("C:/Users/nbarr/OneDrive/Pictures/Documents/FIU/LAB_data/LILA/2022/Throw Trapping/Summer/LILA_TT_2022_INVT_SUMMER.xlsx",
                               na = ".")
invt.2023.spring <- read_excel("C:/Users/nbarr/OneDrive/Pictures/Documents/FIU/LAB_data/LILA/2023/Throw Trapping/Spring/LILA_TT_2023_INVT_SPRING.xlsx",
                               na = ".")
invt.2023.summer <- read_excel("C:/Users/nbarr/OneDrive/Pictures/Documents/FIU/LAB_data/LILA/2023/Throw Trapping/Summer/LILA_TT_2023_INVT_SUMMER.xlsx",
                               na = ".")


invt_master <- invt.2018.summer |> 
  bind_rows(invt.2019.spring)|> 
  bind_rows(invt.2019.summer) |> 
  bind_rows(invt.2020.spring)|> 
  bind_rows(invt.2020.summer) |> 
  bind_rows(invt.2021.spring)|> 
  bind_rows(invt.2021.summer) |> 
  bind_rows(invt.2022.spring)|> 
  bind_rows(invt.2022.summer) |> 
  bind_rows(invt.2023.spring)|> 
  bind_rows(invt.2023.summer) 



#-------------------------------------
#####population time series####
#-------------------------------------

# note, these data will be summarized by water year not calendar year. 
#Water year in this case refers to June of the previous to May of the current year
#and reflects the water obtained through seasonal rains in Southern Florida. 
#That is most of the rain falls in the wet season of the previous year and is what 
#influenced the likelihood of drying in the next season.

#-------------------------------------------
#Egg Clutch Densitey Time Series#######
#-------------------------------------------

#merge the transect area data to the count data, then calculate densities of egg masses
#per hectare on the transects. While renaming the data.frame to eggdensity

eggdensity <- eggdata.master %>% 
  select(-Notes) %>% 
  group_by(Year,Month,Day,Cell,Pomacea_Species) %>% 
  summarise(tot.eggs = sum(Count)) %>% 
  left_join(transectarea, by = "Cell") %>% 
  mutate(egg.ha = tot.eggs/tot.area.ha,
         wateryr = case_when(Year == 2019~2018,
                             Year == 2020~2019,
                             Year == 2021~2020,
                             Year == 2022~2021,
                             Year == 2023~2022)) |> 
  ungroup() %>% 
  group_by(wateryr, Cell, Pomacea_Species) %>% 
  summarise(n = n(),
            ave.egg.ha = mean(egg.ha, na.rm = T),
            sd.egg.ha = sd(egg.ha, na.rm = T)) %>%
  mutate(se = sd.egg.ha/sqrt(n))

#create and assign the written out scientific names for Maculata and Paludosa for the plots

pom_labels <- c("Pomacea maculata", "Pomacea paludosa")
names(pom_labels) <- c("Maculata", "Paludosa")

#create the time series plot for the apple snail egg clutch densities 
#note) to specify the superscript in the y axis plot you need to use the function
#expression. When using the expression function the text does not need to be in quotes,
#spaces need to be coded as "~".  Google "customizing axis titles in R" to get a blog post
#on how to work with expressions if you have further questions.
#note 2) the labeller function will use our specified scientific names for the panel titles
#instead of the "Maculata" and "Paludosa" from lines 104 and 105


eggdensity %>% 
  ggplot(aes(x = wateryr, y = ave.egg.ha, shape = Cell, fill = Cell))+
  geom_line(size = 1)+
  geom_point(size = 3.5, color = "black")+
  facet_rep_grid(~Pomacea_Species, labeller = labeller(Pomacea_Species = pom_labels))+
  theme_classic()+
  scale_x_continuous(breaks = c(2018,2019,2020,2021,2022))+
  labs(x = NULL, y = expression(Average~Clutch~Density~(ha^-1)))+
  scale_shape_manual(values = c(22,22,21,21))+
  scale_fill_manual(values = c("black","white","black","white"))+
  theme(strip.text = element_text(face = "italic",size = 14),
        legend.title = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text( angle = 60, vjust = 0.5),
        legend.text = element_text(size = 14))

#------------------------------------------
####Apple Snail Densities####
#------------------------------------------

apple_density <-invt_master |> 
  filter(Species == "POMPAL"| Species == "POMMAC") |> 
  filter(Throw %in% c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)) |> 
  mutate(wateryr = case_when(Session == "Summer 2018" | Session == "Spring 2019" ~ 2018,
                             Session == "Summer 2019" | Session == "Spring 2020" ~ 2019,
                             Session == "Summer 2020" | Session == "Spring 2021" ~ 2020,
                             Session == "Summer 2021" | Session == "Spring 2022" ~ 2021,
                             Session == "Summer 2022" | Session == "Spring 2023" ~ 2022,
                             Session == "Summer 2023"  ~ 2023)) |> 
  group_by(Species,Wetland,wateryr) |> 
  summarise(count = sum(Count), .groups = "drop") |> 
  complete(Species,Wetland,wateryr, fill = list (count = 0)) |> 
  mutate(remove = if_else(Species == "POMPAL"& Wetland == "M4",
                          true = 1,
                          false =0)) |> 
  left_join(slougharea, by = "Wetland") |> 
  filter(remove != 1) |> 
  select(-remove) |> 
  arrange(Species, Wetland, wateryr) |> 
  group_by(Species, Wetland, wateryr) |> 
  mutate(density = count/28,
         density = if_else(density ==0, true = 0.01, false = density),
         density.ha = density*10000,
         N = density.ha * Area_ha) |> 
  mutate(`N+1` = lead(apple_density$N))

#create the time series plot for the apple snail densities 
#note) to specify the superscript in the y axis plot you need to use the function
#expression. When using the expression function the text does not need to be in quotes,
#spaces need to be coded as "~".  Google "customizing axis titles in R" to get a blog post
#on how to work with expressions if you have further questions.
#note 2) the labeller function will use our specified scientific names for the panel titles
#instead of the "Maculata" and "Paludosa" from lines 104 and 105

pom_labels <- c("Pomacea maculata", "Pomacea paludosa")
names(pom_labels) <- c("POMMAC", "POMPAL")


apple_density %>% 
  filter(wateryr != 2023) |> 
  #ggplot(aes(x = wateryr, y = N, shape = Wetland, fill = Wetland))+
  ggplot(aes(x = wateryr, y = density.ha, shape = Wetland, fill = Wetland))+
  geom_line(size = 1)+
  geom_point(size = 3.5, color = "black")+
  facet_rep_wrap(~Species, labeller = labeller(Species = pom_labels), scales = "free_y")+
  theme_classic()+
  scale_x_continuous(breaks = c(2018,2019,2020,2021,2022,2023))+
  labs(x = "Water Year", y = expression(Density~(ha^-1)))+
  #labs(x = "Water Year", y = "Population Size")+
  scale_shape_manual(values = c(22,22,21,21))+
  scale_fill_manual(values = c("black","white","black","white"))+
  theme(strip.text = element_text(face = "italic",size = 14),
        legend.title = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text( angle = 60, vjust = 0.5),
        legend.text = element_text(size = 14))


#---------------------------------
##### Percapita reproducton###
#---------------------------------

percap <- eggdensity |> 
  mutate(Species = if_else(Pomacea_Species == "Maculata", true = "POMMAC", false = "POMPAL")) |> 
  rename(Wetland = Cell) |> 
  select(-Pomacea_Species) |> 
  left_join(apple_density, by = c("wateryr","Wetland","Species")) |> 
  mutate(per_rep = ave.egg.ha/N)

percap %>% 
  ggplot(aes(x = N, y = per_rep, shape = Wetland, fill = Wetland, label = as.character(wateryr)))+
  #geom_line(size = 1)+
  geom_point(size = 3.5, color = "black")+
  facet_rep_wrap(~Species, labeller = labeller(Species = pom_labels),
                 scales = "free")+
  theme_classic()+
  geom_text(vjust = 1, hjust = -0.2)+
  labs(x = "Population Size", y = "Percapita Reproduction")+
  #scale_x_continuous(breaks = c(2018,2019,2020,2021,2022,2023))+
  #labs(x = expression(Density~(ha^-1)), y = "Percapita Reproduction")+
  scale_shape_manual(values = c(22,22,21,21))+
  scale_fill_manual(values = c("black","white","black","white"))+
  theme(strip.text = element_text(face = "italic",size = 14),
        legend.title = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text( angle = 60, vjust = 0.5),
        legend.text = element_text(size = 14))
