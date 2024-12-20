#To run this code specify the directory to save the dataset to
setwd("D:/Rdirectory_24/R-Practice")


#Required packages for Geospatial Graphplot
install.packages(c("sf", "ggmap", "tmap", "rnaturalearth"))
install.packages("devtools")

devtools::install_github("ropensci/rnaturalearthhires")

#Running Necessary Libraries
library(tidyverse) # gplot2, tibble, tidyr, readr, dplyr, stringr, purr, and forcats
library(sf)
library(ggmap)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(devtools)

# obtaining UK boundaries
uk <- ne_states(country = "united kingdom", returnclass = "sf")
#data exploring
unique(uk$region) # to cross match the region names with the dataset
unique(uk$name)   # to cross match the subregion names with the dataset

#--------------Data source----------------------
#source: https://www.data.gov.uk/dataset/723c243d-2f1a-4d27-8b61-cdb93e5b10ff/uk-greenhouse-gas-emissions-local-authority-and-regional
#Data Description: 2005 to 2022 local authority greenhouse gas emissions dataset
#CSV file was save and under"local-authority-ghg-uk.csv" in the specified directory
#------------Data--wrangling -----------
UKghglocal<-read.csv('local-authority-ghg-uk.csv')
#--------------------------------------------------------------
#-------by Region~All sectors Co2e (year 2022) (Final Report Visualisation)--------------------

#recoding of region names was conducted in order to match regions with UK map data
Region_uk_all<-
  UKghglocal %>% 
  drop_na() %>%
  filter(Calendar.Year=='2022') %>% 
  group_by(LA.GHG.Sector,Region, Calendar.Year) %>%
  summarise(Co2.Emission.kt=sum(Territorial.emissions..kt.CO2e.)) %>% 
  rename(region=Region) %>% 
  mutate(region=recode(region,'East of England'='Eastern',
                       'London'='Greater London',
                       'Scotland'='Highlands and Islands',
                       'Wales'='West Wales and the Valley'))
#Data Exploration
unique(Region_uk_all$region)


#Left join data with shapefile
uk_region_all_map <- uk %>%
  left_join(Region_uk_industry, by = "region")

#exploring data to make sure of the join operation
unique(uk_region_indus_map$region)

#to add single labels for each region
region_labels2 <- uk_region_all_map %>%
  group_by(region) %>%
  summarize(geometry = st_union(geometry), .groups = 'drop')  # Combine geometries

#Enhance Label colour for contrast effect
region_labels2e <- region_labels2 %>%
  mutate(label_color = ifelse(region %in% c("Northern Ireland", 
                                            "East","South West"),
                              "white", "black"))  # Use white text for dark regions

# Plot
ggplot(uk_region_all_map) +
  geom_sf(aes(fill = Co2.Emission.kt)) +  # Heatmap fill
  geom_sf_text(data = region_labels2e, 
               aes(label = region, color = label_color), 
               size = 2.7, check_overlap = TRUE) +  # Label with dynamic color
  scale_fill_viridis_c(name = "CO2e Emissions (kt)", option = "plasma") +
  scale_color_identity() +  # Use color from the data (no scale applied)
  labs(title = "Geospatial Heatmap of CO2e Emissions in the UK (2022)",
       subtitle = "Combined Sectors Emissions",
       caption = "Data Source: UK Greenhouse Gas Emissions: Local Authority and Regional") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = 'bold', hjust = 1.45),
    plot.subtitle = element_text(size = 13, hjust = 0.099),
    plot.caption = element_text(face = 'bold'),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right")

#--------------------------------------------------------------------------------
#Region~agriculture co2e emissions for 2022

#Explore the region names in the dataset
unique(UKghglocal$Region)
#recoding of region names was conducted in order to match regions with UK map data
Region_uk_agri<-
  UKghglocal %>% 
  drop_na() %>%
  filter(Calendar.Year=='2022',LA.GHG.Sector=='Agriculture') %>% 
  group_by(LA.GHG.Sector,Region, Calendar.Year) %>%
  summarise(Co2.Emission.kt=sum(Territorial.emissions..kt.CO2e.)) %>% 
  rename(region=Region) %>% 
  mutate(region=recode(region,'East of England'='Eastern',
                'London'='Greater London',
                'Scotland'='Highlands and Islands',
                'Wales'='West Wales and the Valley'))
  
#Exploration
unique(Region_uk_agri$region)

# Left join data with shapefile
uk_region_agri_map <- uk %>%
  left_join(Region_uk_agri, by = "region")

#exploring data to make sure of the join operation
unique(uk_region_agri_map$region)
#adding unique region label on the map
region_labels <- uk_region_agri_map %>%
  group_by(region) %>%
  summarize(geometry = st_union(geometry), .groups = 'drop')  # Combine geometries


#plotting by region level
ggplot(uk_region_agri_map) +
  geom_sf(aes(fill = Co2.Emission.kt)) +  # Color fill based on emissions
  geom_sf_text(data = region_labels, 
               aes(label = region), 
               size = 2.5, color = "black", 
               check_overlap = TRUE) +  # City labels
  scale_fill_viridis_c(name = "CO2e Emissions (kt)", 
                       option = "plasma") + 
  labs(title = "Geospatial Heatmap of CO2e Emissions in the UK (2022)",
       subtitle = "Agriculture Sector by Region",
       caption = "UK Local and Regional CO2e Emissions by Sector") +
  theme_minimal()+
  theme(
    plot.title = element_text(size=18, face = 'bold', hjust=0.9),
    plot.subtitle = element_text(size = 13, hjust=0.09),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right")

##------------------------------------------------------------------------------
#Region~Industry co2e emissions for 2022

#recoding of region names was conducted in order to match regions with UK map data
Region_uk_industry<-
  UKghglocal %>% 
  drop_na() %>%
  filter(Calendar.Year=='2022',LA.GHG.Sector=='Industry') %>% 
  group_by(LA.GHG.Sector,Region, Calendar.Year) %>%
  summarise(Co2.Emission.kt=sum(Territorial.emissions..kt.CO2e.)) %>% 
  rename(region=Region) %>% 
  mutate(region=recode(region,'East of England'='Eastern',
                       'London'='Greater London',
                       'Scotland'='Highlands and Islands',
                       'Wales'='West Wales and the Valley'))

unique(Region_uk_industry$region)

# Left join data with shapefile
uk_region_indus_map <- uk %>%
  left_join(Region_uk_industry, by = "region")

#exploring data to make sure of the join operation
unique(uk_region_indus_map$region)

region_labels1 <- uk_region_indus_map %>%
  group_by(region) %>%
  summarize(geometry = st_union(geometry), .groups = 'drop')  # Combine geometries


#plotting by region level
ggplot(uk_region_indus_map) +
  geom_sf(aes(fill = Co2.Emission.kt)) +  # Color fill based on emissions
  geom_sf_text(data = region_labels1, 
               aes(label = region), 
               size = 2.5, color = "black", 
               check_overlap = TRUE) +  # City labels
  scale_fill_viridis_c(name = "CO2e Emissions (kt)", option = "plasma") +
  labs(title = "Geospatial Heatmap of CO2e Emissions in the UK (2022)",
       subtitle = "Industry Sector",
       caption = "UK Local and Regional CO2e Emissions by Secto") +
  theme_minimal()+
  theme(
    plot.title = element_text(size=18, face = 'bold', hjust=0.9),
    plot.subtitle = element_text(size = 13, hjust=0.09),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right")

#3.-------by Region All sectors (Final Report Visualisation)--------------------
#by Region all co2e emissions for 2022
#recoding of region names was conducted in order to match regions with UK map data

Region_uk_all<-
  UKghglocal %>% 
  drop_na() %>%
  filter(Calendar.Year=='2022') %>% 
  group_by(LA.GHG.Sector,Region, Calendar.Year) %>%
  summarise(Co2.Emission.kt=sum(Territorial.emissions..kt.CO2e.)) %>% 
  rename(region=Region) %>% 
  mutate(region=recode(region,'East of England'='Eastern',
                       'London'='Greater London',
                       'Scotland'='Highlands and Islands',
                       'Wales'='West Wales and the Valley'))
#Data Exploration
unique(Region_uk_all$region)


#Left join data with shapefile
uk_region_all_map <- uk %>%
  left_join(Region_uk_industry, by = "region")

#exploring data to make sure of the join operation
unique(uk_region_indus_map$region)

#to add si
region_labels2 <- uk_region_all_map %>%
  group_by(region) %>%
  summarize(geometry = st_union(geometry), .groups = 'drop')  # Combine geometries

#plotting by region level
ggplot(uk_region_all_map) +
  geom_sf(aes(fill = Co2.Emission.kt)) +  # Color fill based on emissions
  geom_sf_text(data = region_labels2, 
               aes(label = region), 
               size = 2.7, color = "black", 
               check_overlap = TRUE) +  # City labels
  scale_fill_viridis_c(name = "CO2e Emissions (kt)", option = "plasma") +
  labs(title = "Geospatial Heatmap of CO2e Emissions in the UK (2022)",
       subtitle = "Combined Sectors",
       caption = "Data Source: UK Greenhouse Gas Emissions: Local Authority and Regional") +
  theme_minimal()+
  theme(
    plot.title = element_text(size=18, face = 'bold', hjust=0.9),
    plot.subtitle = element_text(size = 13, hjust=0.09),
    plot.caption = element_text(face = 'bold'),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right")

#------------Label colour change for contrast ---Improved - Report chart

#Adding a color column for regions where labels are hard to see
region_labels2 <- uk_region_all_map %>%
  group_by(region) %>%
  summarize(geometry = st_union(geometry), .groups = 'drop')  # Combine geometries

region_labels2e <- region_labels2 %>%
  mutate(label_color = ifelse(region %in% c("Northern Ireland", 
                                            "East","South West"),
                              "white", "black"))  # Use white text for dark regions

# Plot
ggplot(uk_region_all_map) +
  geom_sf(aes(fill = Co2.Emission.kt)) +  # Heatmap fill
  geom_sf_text(data = region_labels2e, 
               aes(label = region, color = label_color), 
               size = 2.7, check_overlap = TRUE) +  # Label with dynamic color
  scale_fill_viridis_c(name = "CO2e Emissions (kt)", option = "plasma") +
  scale_color_identity() +  # Use color from the data (no scale applied)
  labs(title = "Geospatial Heatmap of CO2e Emissions in the UK (2022)",
       subtitle = "Combined Sectors Emissions",
       caption = "Data Source: UK Greenhouse Gas Emissions: Local Authority and Regional") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = 'bold', hjust = 1.45),
    plot.subtitle = element_text(size = 13, hjust = 0.099),
    plot.caption = element_text(face = 'bold'),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right")


##-----------------------------------------------------


#sub region level plotting ----- 

#Aggregating the gas emission by local authority for 2022 
#Co2e=Co2 Equivalent (CH4+CO2 gas emissions)
#Agriculture Sector

name_uk_agri<-
  UKghglocal %>% 
  filter(Calendar.Year=='2022', 
         LA.GHG.Sector=='Agriculture') %>% 
  group_by(Local.Authority, Calendar.Year) %>%
  summarise(Co2e_Emissions=sum(Territorial.emissions..kt.CO2e.)) %>%
  rename(name=Local.Authority) %>%
  mutate(name = recode(name,
                                  "Newry, Mourne and Down" = "Strabane",
                                  "North Ayrshire" = "Fermanagh",
                                  "North Devon" = "Dungannon",
                                  "North East Derbyshire" = "Armagh",
                                  "North East Lincolnshire" = "Newry and Mourne",
                                  "North Hertfordshire" = "Flintshire",
                                  "North Kesteven" = "Cheshire West and Chester",
                                  "North Lanarkshire" = "Wrexham",
                                  "North Lincolnshire" = "Shropshire",
                                  "North Norfolk" = "Powys",
                                  "North Northamptonshire" = "Herefordshire",
                                  "North Somerset" = "Monmouthshire",
                                  "Northumberland" = "Scottish Borders",
                                  "Norwich" = "Limavady",
                                  "Nottingham" = "Coleraine",
                                  "Nuneaton and Bedworth" = "Moyle",
                                  "Oadby and Wigston" = "Larne",
                                  "Oldham" = "Carrickfergus",
                                  "Orkney Islands" = "Newtownabbey",
                                  "Oxford" = "Belfast",
                                  "Pembrokeshire" = "North Down",
                                  "Pendle" = "Ards",
                                  "Perth and Kinross" = "Down",
                                  "Peterborough" = "Clackmannanshire",
                                  "Plymouth" = "Stirling",
                                  "Portsmouth" = "Falkirk",
                                  "Powys" = "West Lothian",
                                  "Preston" = "Edinburgh",
                                  "Reading" = "Midlothian",
                                  "Redbridge" = "East Lothian",
                                  "Redcar and Cleveland" = "North Tyneside",
                                  "Redditch" = "South Tyneside",
                                  "Reigate and Banstead" = "Sunderland",
                                  "Renfrewshire" = "Durham",
                                  "Rhondda Cynon Taf" = "Hartlepool",
                                  "Ribble Valley" = "Redcar and Cleveland",
                                  "Richmond upon Thames" = "North Yorkshire"))
 

# Left join data with shapefile
uk_name_agri_map <- uk %>%
  left_join(name_uk_agri, by = "name")

names(uk_name_agri_map)

#plotting by sub-region level
ggplot(uk_name_agri_map) +
  geom_sf(aes(fill = Co2e_Emissions)) +  # Color fill based on emissions
  geom_sf_text(aes(label = name), size = 2, color = "black", check_overlap = TRUE) +  # City labels
  #geom_sf_text(data = region_labels1, aes(label = region), size = 2.5, color = "black",check_overlap = TRUE) +  # City labels
  scale_fill_viridis_c(name = "CO2e Emissions (kt)", option = "plasma") +
  labs(title = "Geospatial Heatmap of CO2e Emissions in the UK (2022)",
       subtitle = "Industry Sector",
       caption = "UK Local and Regional CO2e Emissions by Secto") +
  theme_minimal()+
  theme(
    plot.title = element_text(size=18, face = 'bold', hjust=0.9),
    plot.subtitle = element_text(size = 13, hjust=0.09),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right")

#--------------------------------------

#calculating total for CO2e emission for agriculture for 2022
UKghglocal %>% 
  drop_na() %>%
  filter(Calendar.Year=='2022', LA.GHG.Sector=='Agriculture') %>% 
  summarise(Co2.Emission.kt=sum(Territorial.emissions..kt.CO2e.)) %>% 
  view()

#total co2e emissions in the Uk for Agriculture sector for 2022
49581.55

#contribution of emissions by subregion for 2022 for Agriculture sector
UKghglocal %>% 
  filter(Calendar.Year=='2022', LA.GHG.Sector=='Agriculture') %>% 
  group_by(Local.Authority) %>% 
  summarise(Co2e_Emission_kt=sum(Territorial.emissions..kt.CO2e.)) %>% 
  mutate(Percentage=(Co2e_Emission_kt/49581.55)*100) %>% 
  view()


#gas emissions Across all sectors by sub-regions for 2022 & population

name_uk_all<-
  UKghglocal %>%
  drop_na() %>% 
  filter(Calendar.Year=='2022') %>% 
  group_by(Local.Authority) %>% 
  summarize(co2total=sum(Territorial.emissions..kt.CO2e.),
            population_in_thousand=first(Mid.year.Population..thousands.), 
            .groups = 'drop') %>% 
  rename(name=Local.Authority)

  
uk_name_all_map <- uk %>%
  left_join(name_uk_all, by = "name")

#plotting by sub-region level
ggplot(uk_name_all_map) +
  geom_sf(aes(fill = co2total)) +  # Color fill based on emissions
  geom_sf_text(aes(label = name), size = 2, color = "black", check_overlap = TRUE) +  # City labels
  #geom_sf_text(data = region_labels1, aes(label = region), size = 2.5, color = "black",check_overlap = TRUE) +  # City labels
  scale_fill_viridis_c(name = "CO2e Emissions (kt)", option = "plasma") +
  labs(title = "Geospatial Heatmap of CO2e Emissions in the UK (2022)",
       subtitle = "All Sector",
       caption = "UK Local and Regional CO2e Emissions by Sector") +
  theme_minimal()+
  theme(
    plot.title = element_text(size=18, face = 'bold', hjust=0.9),
    plot.subtitle = element_text(size = 13, hjust=0.09),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right")


