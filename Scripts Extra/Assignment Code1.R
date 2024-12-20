glimpse(sectors_data1)
str(sectors_data1)

all_gases_emission2<-
  all_gases_emission1[,c(-24,-25)]

emissions_data2<-
  emissions_data1[,c(-24,-25)]

CH4_total2<-CH4_total1[,c(-24,-25)]
Co2_total2<-Co2_total1[,c(-24,-25)]
GHG_total2<-GHG_total1[,c(-24,-25)]
HFC_total2<-HFC_total1[,c(-24,-25)]
N2O_total2<-N2O_total1[,c(-24,-25)]
NF3_total2<-NF3_total1[,c(-24,-25)]
PFC_total2<-PFC_total1[,c(-24,-25)]
SF6_total2<-SF6_total1[,c(-24,-25)]


sectors_data2<-emissions_data2 %>% 
  pivot_longer(
    cols = 3:24, 
    names_to = "Sector",
    values_to = "Emissions")


write.csv(sectors_data2, file = "sector_data2.csv",
          row.names = FALSE)
####### Spread the Total Value across sectors to
#calculate percentage

CH4_total2_test<-CH4_total2 %>% 
  select("year", "gas_type","Total", everything() )

CH4_data3<-CH4_total2_test %>% 
  pivot_longer(
    cols = 4:24,
    names_to = "Sector",
    values_to = "Emissions")
########## Co2 data
Co2_reorder<-Co2_total2 %>% 
  select("year", "gas_type","Total", everything() )

Co2_data3<-Co2_reorder %>% 
  pivot_longer(
    cols = 4:24,
    names_to = "Sector",
    values_to = "Emissions")
####### GHG ########
GHG_reorder<-GHG_total2 %>% 
  select("year", "gas_type","Total", everything() )

GHG_data3<-GHG_reorder %>% 
  pivot_longer(
    cols = 4:24,
    names_to = "Sector",
    values_to = "Emissions")

####### HFC ########
HFC_reorder<-HFC_total2 %>% 
  select("year", "gas_type","Total", everything() )

HFC_data3<-HFC_reorder %>% 
  pivot_longer(
    cols = 4:24,
    names_to = "Sector",
    values_to = "Emissions")

#########N2O################
N2O_reorder<-N2O_total2 %>% 
  select("year", "gas_type","Total", everything() )

N2O_data3<-N2O_reorder %>% 
  pivot_longer(
    cols = 4:24,
    names_to = "Sector",
    values_to = "Emissions")
#########NF3################
NF3_reorder<-NF3_total2 %>% 
  select("year", "gas_type","Total", everything() )

NF3_data3<-NF3_reorder %>% 
  pivot_longer(
    cols = 4:24,
    names_to = "Sector",
    values_to = "Emissions")
#########PFC################
PFC_reorder<-PFC_total2 %>% 
  select("year", "gas_type","Total", everything() )

PFC_data3<-PFC_reorder %>% 
  pivot_longer(
    cols = 4:24,
    names_to = "Sector",
    values_to = "Emissions")
#########SF6################
SF6_reorder<-SF6_total2 %>% 
  select("year", "gas_type","Total", everything() )

SF6_data3<-SF6_reorder %>% 
  pivot_longer(
    cols = 4:24,
    names_to = "Sector",
    values_to = "Emissions")

#Combining the data by using bind
sectors_data3<-bind_rows(GHG_data3,
                              Co2_data3,
                              CH4_data3,
                              HFC_data3,
                              N2O_data3,
                              NF3_data3,
                              PFC_data3,
                              SF6_data3)
#### Arrange varaible order
sectors_data3<- select(sectors_data3,
                       "year",
                       "gas_type",
                       "Sector",
                       "Emissions",
                       "Total")

sector_data4<-sectors_data3 %>% 
  mutate(percentage = (Emissions/Total)*100)
sector_data4$percentage<-
  round(sector_data4$percentage, 2)

########## Removing unnecessary dataframs#

rm(all_gases_emission)
rm(all_gases_emission1)
rm(CH4_total)
rm(CH4_total1)
rm(CH4_total2_test)
rm(Co2_total)
rm(Co2_total1)
rm(Co2_reorder)
rm(emissions_data)
rm(emissions_data1)
rm(GHG_total)
rm(GHG_total1)
rm(GHG_reorder)
rm(HFC_total)
rm(HFC_total1)
rm(HFC_reorder)
rm(left_joint_data)
rm(N2O_total)
rm(N2O_total1)
rm(N2O_reorder)
rm(NF3_total)
rm(NF3_total1)
rm(NF3_reorder)
rm(PFC_total)
rm(PFC_total1)
rm(PFC_reorder)
rm(SF6_total)
rm(SF6_total1)
rm(SF6_reorder)


rm(sectors_data)
rm(sectors_data_rename)
rm(sector_data3)
rm(sectors_data1)

emission_data_wide<-emissions_data2
CH4_data_long<-CH4_data3
CH4_data_wide<-CH4_total2
Co2_data_long<-Co2_data3
Co2_data_wide<-Co2_total2
GHG_data_long<-GHG_data3
GHG_data_wide<-GHG_data3
HFC_data_long<-HFC_data3
HFC_data_wide<-HFC_total2
N2O_data_long<-N2O_data3
N2O_data_wide<-N2O_total2
NF3_data_long<-NF3_data3
NF3_data_wide<-NF3_total2
PFC_data_long<-PFC_data3
PFC_data_wide<-PFC_total2
SF6_data_long<-SF6_data3
SF6_data_wide<-SF6_total2
stacked_data_long<-stacked_data
sector_percentage_of_total<-sector_data_with_percentage
sector_with_total<-sector_data_with_total

rm(sector_data_with_percentage)
rm(sector_with_tota)
rm(sector_data_with_total)
rm(CH4_data3)
rm(CH4_total2)
rm(Co2_data3)
rm(Co2_total2)
rm(emissions_data2)
rm(GHG_data3)
rm(GHG_total2)
rm(HFC_data3)
rm(HFC_total2)
rm(N2O_data3)
rm(N2O_total2)
rm(NF3_data3)
rm(NF3_total2)
rm(PFC_data3)
rm(PFC_total2)
rm(SF6_data3)
rm(SF6_total2)
rm(stacked_data)
rm(sector_data4)
rm(sectors_data3)
rm(sector_data_percentage)
rm(sectors_data2)
