
#Firstly opening the packages for data cleaning

library(readxl) #use the install excel read package
library(tidyverse)
library(janitor)
library(dplyr)

# Identify the working Directory
setwd("~/Rdirectory") #set the working director for the file



#read the data from the downloaded dataset
GHG_total <- read_excel("GHG_rawdata.xlsx", 
                        sheet = "GHG total", 
                        range = "A8:Y42")


#Exploring the data
View(GHG_total)
str(GHG_total)

#Changing "industry name" to "years" for clarity (Janitor package)
GHG_total<-rename(GHG_total, "year"="Industry name")
#Cleaning names of variables (Janitor Package)
GHG_total<-clean_names(GHG_total)
view(GHG_total)


#Saving my data (always coming back here to save different version as I progress)
view(GHG_total)
save(GHG_total,file = "GHG_total_1.RData")
write.csv(GHG_total,"GHG_total_1.csv", row.names = FALSE )


#created a unique identifier for GHG_data for analytic purposes
#For GHG_total I chose "Combined" function in year and industry

GHG_total$gas_type<-"Combined"
view(GHG_total)

#reording the variable for visual appeal
GHG_total <- GHG_total[, c("year", "gas_type", 
                          setdiff(names(GHG_total), 
                                   c("Year", "gas_type")))]
view(GHG_total)
#remove repeated column "year"
GHG_total<-GHG_total[,-3]


#............importing CO2_Sheet......................
#read the data from the downloaded dataset
Co2_total <- read_excel("GHG_rawdata.xlsx", 
                        sheet = "Co2", 
                        range = "A7:Y41")
#Exploring data
view(Co2_total)
str(Co2_total)

#Changing the "industry name" to "Year" & name cleaning for the variable
Co2_total<-rename(Co2_total, "year"="Industry name")
Co2_total<-clean_names(Co2_total)
view(Co2_total)

#Creating Unique Identifier for analytic purposes
Co2_total$gas_type<-"Co2"
view(Co2_total)
#Rearrange variable view  (using dplyr)
Co2_total<-Co2_total %>% 
  select(year,gas_type,everything()) %>% 
  view()

#saving the data
save(Co2_total, file = "Co2_total_1.RData")
write.csv(Co2_total, file = "Co2_total_1.csv")

#Saving my data (always coming back here to save different version as I progress)
view(GHG_total)
save(GHG_total,file = "Co2_total_1.RData")
write.csv(GHG_total,"Co2_total_1.csv", row.names = FALSE )

#........ Importing CH4 data ...........

CH4_total<-read_excel("GHG_rawdata.xlsx", 
                      sheet = "CH4", 
                      range = "A7:Y41" )
#Exploring the data view
view(CH4_total)
str(CH4_total)

#changing the name from "Industry name to "Years" & variable name cleaning
CH4_total<-rename(CH4_total,"Year"="Industry name")
CH4_total<-clean_names(CH4_total)


#creating Unique variable Identifier for analytic purposes
CH4_total$gas_type<-"CH4"
view(CH4_total)

#rearrange variable view
CH4_total<-CH4_total %>% 
  select(Year,gas_type,everything()) %>% 
  view()
#Saving my data (always coming back here to save different version as I progress)
view(GHG_total)
save(GHG_total,file = "CH4_total_1.RData")
write.csv(GHG_total,"CH4_total_1.csv", row.names = FALSE )

#........ Importing N2O data ...........

N2O_total<-read_excel("GHG_rawdata.xlsx", 
                      sheet = "N2O", 
                      range = "A7:Y41" )
#Exploring the data view
view(N2O_total)
str(N2O_total)

#changing the name from "Industry name to "Years" & variable name cleaning
N2O_total<-rename(N2O_total,"Year"="Industry name")
N2O_total<-clean_names(N2O_total)


#creating Unique variable Identifier for analytic purposes
N2O_total$gas_type<-"N2O"
view(N2O_total)

#rearrange variable view
N2O_total<-N2O_total %>% 
  select(Year,gas_type,everything()) %>% 
  view()

#Saving my data (always coming back here to save different version as I progress)
view(GHG_total)
save(GHG_total,file = "N2O_total_1.RData")
write.csv(GHG_total,"N2O_total_1.csv", row.names = FALSE )


#........ Importing HFC data ...........

HFC_total<-read_excel("GHG_rawdata.xlsx", 
                      sheet = "HFC", 
                      range = "A7:Y41" )
#Exploring the data view
view(HFC_total)
str(HFC_total)

#changing the name from "Industry name to "Years" & variable name cleaning
HFC_total<-rename(HFC_total,"Year"="Industry name")
HFC_total<-clean_names(HFC_total)


#creating Unique variable Identifier for analytic purposes
HFC_total$gas_type<-"HFC"
view(HFC_total)

#rearrange variable view
HFC_total<-HFC_total %>% 
  select(Year,gas_type,everything()) %>% 
  view()

#Saving my data (always coming back here to save different version as I progress)
view(GHG_total)
save(GHG_total,file = "HFC_total_1.RData")
write.csv(GHG_total,"HFC_total_1.csv", row.names = FALSE )


#........ Importing PFC data ...........

PFC_total<-read_excel("GHG_rawdata.xlsx", 
                      sheet = "PFC", 
                      range = "A7:Y41" )
#Exploring the data view
view(PFC_total)
str(PFC_total)

#changing the name from "Industry name to "Years" & variable name cleaning
PFC_total<-rename(PFC_total,"Year"="Industry name")
PFC_total<-clean_names(PFC_total)


#creating Unique variable Identifier for analytic purposes
PFC_total$gas_type<-"PFC"
view(PFC_total)

#rearrange variable view
PFC_total<-PFC_total %>% 
  select(Year,gas_type,everything()) %>% 
  view()

#Saving my data (always coming back here to save different version as I progress)
view(GHG_total)
save(GHG_total,file = "PFC_total_1.RData")
write.csv(GHG_total,"PFC_total_1.csv", row.names = FALSE )

#........ Importing SF6 data ...........

SF6_total<-read_excel("GHG_rawdata.xlsx", 
                      sheet = "SF6", 
                      range = "A7:Y41" )
#Exploring the data view
view(SF6_total)
str(SF6_total)

#changing the name from "Industry name to "Years" & variable name cleaning
SF6_total<-rename(SF6_total,"Year"="Industry name")
SF6_total<-clean_names(SF6_total)


#creating Unique variable Identifier for analytic purposes
SF6_total$gas_type<-"SF6"
view(SF6_total)

#rearrange variable view
SF6_total<-SF6_total %>% 
  select(Year,gas_type,everything()) %>% 
  view()

#Saving my data (always coming back here to save different version as I progress)
view(GHG_total)
save(GHG_total,file = "SF6_total_1.RData")
write.csv(GHG_total,"SF6_total_1.csv", row.names = FALSE )

#........ Importing NF3 data ...........

NF3_total<-read_excel("GHG_rawdata.xlsx", 
                      sheet = "NF3", 
                      range = "A7:Y41" )
#Exploring the data view
view(NF3_total)
str(NF3_total)

#changing the name from "Industry name to "Years" & variable name cleaning
NF3_total<-rename(NF3_total,"Year"="Industry name")
NF3_total<-clean_names(NF3_total)


#creating Unique variable Identifier for analytic purposes
NF3_total$gas_type<-"NF3"
view(NF3_total)

#rearrange variable view
NF3_total<-NF3_total %>% 
  select(Year,gas_type,everything()) %>% 
  view()
#Saving my data (always coming back here to save different version as I progress)
view(GHG_total)
save(GHG_total,file = "NF3_total_1.RData")
write.csv(GHG_total,"NF3_total_1.csv", row.names = FALSE )

# combining into one single dataframe using dplyr

library(dplyr)

#further exploration of the data shows so VARSET requires change

#GHG
GHG_total<-rename(GHG_total,
                  "total_gas_emissions"="total_greenhouse_gas_emissions" )
#Co2
Co2_total<-rename(Co2_total,
                  "total_gas_emissions"="total_co2_gas_emissions" )

#CH4
CH4_total<-rename(CH4_total,
                  "total_gas_emissions"="total_ch4_gas_emissions" )
#HFC
HFC_total<-rename(HFC_total,
                  "total_gas_emissions"="total_hfc_gas_emissions" )
#N2O
N2O_total<-rename(N2O_total,
                  "total_gas_emissions"="total_n2o_gas_emissions" )
#NF3
NF3_total<-rename(NF3_total,
                  "total_gas_emissions"="total_nf3_gas_emissions" )
#PFC
PFC_total<-rename(PFC_total,
                  "total_gas_emissions"="total_pfc_gas_emissions" )
#SF6
SF6_total<-rename(SF6_total,
                  "total_gas_emissions"="total_sf6_gas_emissions" )


#Combining the data by using bind
all_gases_emission<-bind_rows(GHG_total,
                              Co2_total,
                              CH4_total,
                              HFC_total,
                              N2O_total,
                              NF3_total,
                              PFC_total,
                              SF6_total)

# Exploring the combined data
str(all_gases_emission)
glimpse(all_gases_emission)
head(all_gases_emission)


#saving the combined data
save(all_gases_emission,file = "all_gases_emissions_1.RData")
write.csv(all_gases_emission,file = "all_gases_emission_1.csv")

view(all_gases_emission)

###Short name

emissions_data<-all_gases_emission



#viewing the data we can see that it is 
#relatively clean no NA or other


#### Exploring the data using GGPLOT tests

#line chart 1
all_gases_emission %>%
  filter(gas_type!="Combined") %>% 
  ggplot(aes(year, total_gas_emissions/1000,
             colour = gas_type,
             fill = "gas_type"))+
  geom_point(aes(colour = gas_type, 
                 size = total_gas_emissions/1000, 
                 alpha = 0.00001))+
  geom_line()+
  labs(x="Years",
       y="Total Emissions in\nMillion Tonnes",
       title = )
  theme_bw()

  #line chart 2
all_gases_emission %>%
    filter(gas_type != "Combined") %>% 
    ggplot(aes(year, total_gas_emissions/1000,
               colour = gas_type,
               fill = "gas_type"))+
    geom_point(aes(colour = gas_type))+
    geom_line()+
    labs(x="Years",
         y="Total Emissions in\nMillion Tonnes",
         title = )+
  theme_bw()
#line chart _using group
all_gases_emission %>%
  filter(gas_type!="Combined") %>% 
  ggplot(aes(year, manufacturing/1000, colour=gas_type))+
  geom_line()+
  labs(x="Years",
       y="Total Emissions in\nMillion Tonnes",
       title = )
theme_bw()

#test multiple geom points
all_gases_emission %>%
  filter(gas_type=="Co2") %>% 
  ggplot(aes(x=year))+
  geom_point(aes(y = manufacturing/1000, 
                 color = "Manufacturing"))+
  geom_point(aes(y = agriculture_forestry_and_fishing/1000, 
                 color = "Agriculture, Forestry, and Fishing")) +
  labs(
    x = "Year",
    y = "Emissions",
    color = "Sector",
    title = "Gas Emission Cross different Sectors")+
  ylim(0,150)

#Area Chart
all_gases_emission %>%
  filter(gas_type != "Combined") %>% 
  ggplot(aes(year, total_gas_emissions/1000,
             fill = "gas_type"))+
  geom_area(aes(fill = gas_type, alpha = 0.5))+
  labs(x="Years",
       y="Total Emissions in\nMillion Tonnes",
       title = "Area Chart Trend" )+
  theme_bw()


#bar_Chart 1 -- not good
all_gases_emission %>%
  filter(gas_type!="Combined") %>%
  filter(year=="2023") %>% 
  ggplot(aes(x=gas_type, y=total_gas_emissions/1000))+
  geom_bar(stat = "identity")

#Stacked bar char 
all_gases_emission %>%
  filter(gas_type!="Combined") %>%
  ggplot(aes(x=year, y=total_gas_emissions/1000))+
  geom_bar(stat = "identity",
           alpha=0.5, 
           aes(colour=gas_type, fill = gas_type))+
  geom_smooth(method = "lm") #not necessary

#bar chart over 10 years 10 period  
all_gases_emission %>%
  filter(year >= 1990 & year <= 2000) %>% 
  filter(gas_type!="Combined") %>%
  ggplot(aes(x=year, y=total_gas_emissions/1000, 
             colour = gas_type))+
  geom_bar(stat = "identity",position = position_dodge(),
           alpha=0.5, 
           aes( fill = gas_type))


all_gases_emission %>%
  filter(gas_type!="Combined") %>%
  ggplot(aes(x=year, y=total_gas_emissions/1000))+
  geom_bar(stat = "identity",
           alpha=0.3, 
           aes(colour=gas_type, fill = gas_type))+
  geom_smooth(method = "lm", se=F)+
  labs(x="Years",
       y="Gas Emissions in \n Million Tonnes",
       title = "Trends in Greenhouse Gas Emissions\n by Type from 1990 to 2023")+
  theme_bw()


#Summary Gas_Total
summary(all_gases_emission$total_gas_emissions)  

## Trend line by Gas_Type
all_gases_emission %>%
  filter(gas_type!="Combined") %>% 
  ggplot(aes(year,manufacturing/1000,
             colour=gas_type))+
  geom_point(size=3, alpha=0.3)+
  geom_smooth(method = "lm")+
  facet_wrap(~gas_type)+
  labs(x="Year", 
       y="Manufacturing Emmissions\n in million Tonnes",
       title = "Trend of Manufacturing Emission\n by gas 1990 to 2023")


### Boxplot ###### 
all_gases_emission %>%
  filter(gas_type!="Combined") %>%
  ggplot(aes(year, total_gas_emissions/1000))+
  geom_boxplot(aes(gas_type, 
                   colour=gas_type, 
                   fill=gas_type))+
  coord_flip()+
  theme_bw()

####box plot excluding high emission gases
emissions_data %>%
  filter(!gas_type%in%c("Combined", "Co2", 
                        "CH4","N2O", "HFC")) %>% 
  ggplot(aes(year,total_gas_emissions/1000))+
  geom_boxplot(aes(gas_type, colour = gas_type,
                   fill = gas_type))+
  coord_flip()


####  29/10/2024
install.packages("plotly")
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)


###Stacked area chart

#creating stacked data
stacked_data <- emissions_data %>%
  filter(gas_type!="Combined") %>% 
  group_by(year, gas_type) %>%
  summarise(total_emission = sum(total_gas_emissions/1000, 
                                 na.rm = TRUE))

# Stacked area plot#
ggplot(stacked_data, aes(x = year, y = total_emission, fill = gas_type)) +
  geom_area(alpha = 0.6) +
  labs(title = "Stacked Area Chart of Emissions by Gas Type",
       x = "Year", y = "Total Emissions\nin Million tonnes") +
  theme_minimal()


#histogram
ggplot(emissions_data, aes(total_gas_emissions,
                           fill = gas_type)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  labs(title = "Histogram of Emission Values by Gas Type",
       x = "Emission Value", y = "Frequency") +
  theme_minimal()


## invistigation, majorty of industry produce emission
hist(emissions_data$total_gas_emissions/1000,
     breaks = 3, 
     main = "Histogram of Gas Emission Rates\n All Sectors")

emissions_data %>%
  filter(gas_type!="Combined") %>% 
  ggplot(aes(year))+
  geom_point(aes(y=manufacturing/1000, colour=manufacturing))+
  geom_point(aes(y=total_gas_emissions/1000))

#pivot long and pivot wide
#variables to objects and header sector

# Restructure data using pivot_longer
sectors_data <- emissions_data %>%
  pivot_longer(
    cols = 3:26,
    names_to = "sector",
    values_to = "emissions")
write.csv(sectors_data,file = "sectors_data.csv", 
          row.names = FALSE)

# Create sector name mapping
sector_mapping <- c(
  "agriculture_forestry_and_fishing" = "Agriculture",
  "mining_and_quarrying" = "Mining",
  "manufacturing" = "Manufacturing",
  "electricity_gas_steam_and_air_conditioning_supply" = "Energy",
  "water_supply_sewerage_waste_management_and_remediation_activities" = "Water & Waste",
  "construction" = "Construction",
  "wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles" = "Retail & Auto",
  "transport_and_storage" = "Transport",
  "accommodation_and_food_services" = "Hospitality",
  "information_and_communication" = "ICT",
  "financial_and_insurance_activities" = "Finance",
  "real_estate_activities" = "Real Estate",
  "professional_scientific_and_technical_activities" = "Professional",
  "administrative_and_support_service_activities" = "Admin Services",
  "public_administration_and_defence_compulsory_social_security" = "Public Admin",
  "education" = "Education",
  "human_health_and_social_work_activities" = "Healthcare",
  "arts_entertainment_and_recreation" = "Arts & Rec",
  "other_service_activities" = "Other Services",
  "activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use" = "Households",
  "consumer_expenditure_note_4" = "Consumer Exp",
  "consumer_expenditure_not_travel" = "Consumer Non-Travel",
  "consumer_expenditure_travel" = "Consumer Travel",
  "total_gas_emissions"="Total"
)

### Sector naming improvements
sectors_data_rename <- emissions_data %>%
  pivot_longer(
    cols = 3:26,
    names_to = "sector",
    values_to = "emissions") %>% 
  mutate(sector=sector_mapping[sector])

sectors_data1 <- emissions_data %>%
  pivot_longer(
    cols = 3:26,
    names_to = "sector",
    values_to = "emissions") %>% 
  mutate(sector=sector_mapping[sector])

#######
sectors_data_rename

##### Saving dataset

getwd()
setwd("~/Rdirectory")

write.csv(sectors_data_rename, 
          file = "sector_data_rename1.csv", 
          row.names = FALSE)
########################################################333

### Renaming Wide data set
#saving the combined data


emissions_data1<-emissions_data %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")

###RENAMING VARIABLE NAMES
<-clean_names(GHG_total)

GHG_total<-select(GHG_total, year,gas_type, everything())

GHG_total1<-GHG_total %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")

Co2_total1<-Co2_total %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")

CH4_total1<-CH4_total %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")

HFC_total1<-HFC_total %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")

N2O_total1<-N2O_total %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")

NF3_total1<-NF3_total %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")

PFC_total1<-PFC_total %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")

SF6_total1<-SF6_total %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")

all_gases_emission1<-all_gases_emission %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")


# deleting repeated datasets

rm(CH4_data_long)
rm(CH4_data_wide)
rm(Co2_data_long)
rm(Co2_data_wide)
rm(HFC_data_long)
rm(HFC_data_wide)
rm(N2O_data_long)
rm(N2O_data_wide)
rm(NF3_data_long)
rm(NF3_data_wide)
rm(PFC_data_long)
rm(PFC_data_wide)
rm(SF6_data_long)
rm(SF6_data_wide)
