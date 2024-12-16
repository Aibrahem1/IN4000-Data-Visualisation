library(ggplot2)
library(tidyverse)
library(ggExtra)
library(plotly)
library(scales)
library(patchwork)



# graphs for the assignment

#The y Scale is is going to be change from Thousand tonnes 
#to Million tonnes to show the reader the scale of emissions
factor<-1000 # to adjust the y Scale view

all_gases_emission2 %>%
  filter(gas_type!="Combined") %>%  #all gases
  ggplot(aes(x=year, y=Total/factor))+
  geom_point(size=3,aes(colour = gas_type,
                        alpha = 0.1))+
  scale_y_log10()+
  geom_line(aes(colour=gas_type))+
  theme_minimal()+
  labs(x="Year", y="Gas Emissions in\nMiillion Tonnes",
       colour="GAS Type")
  

#---- test
all_gases_emission2 %>%
  filter(gas_type!="Combined") %>%  #all gases
  ggplot(aes(x=year, y=Total/factor))+
  geom_point(size=3,aes(colour = gas_type,
                        alpha = 0.1))+
  scale_y_log10()+
  geom_line(aes(colour=gas_type))+
  theme_gray(base_size = 11,
             base_family = "",
             base_line_size = 22,
             base_rect_size = 22)+
  labs(x="Year", y="Gas Emissions in\nMiillion Tonnes",
       colour="GAS Type")

## Plotly #### 

Graph1<-all_gases_emission2 %>%
  filter(gas_type!="Combined") %>%  #all gases
  ggplot(aes(x=year, y=Total/factor))+
  geom_point(size=3,aes(colour = gas_type,
                        alpha = 0.1))+
  geom_line(aes(colour=gas_type))+
  theme_minimal()+
  labs(x="Year", y="Gas Emissions in\nMiillion Tonnes",
       colour="GAS Type")


ggplotly(Graph1)


all_gases_emission %>%  #low Emission gases
  filter(!gas_type %in% c("Combined", "Co2", "CH4")) %>% 
  ggplot(aes(x=year, y=total_gas_emissions/factor))+
  geom_point(size=3,aes(colour = gas_type, alpha=0.5))+
  geom_line(aes(colour=gas_type))+
  theme_minimal()


##### plotly was used#
ggplotly(all_gases_emission2 %>%  #lHigh Emission Gases
  filter(!gas_type %in% c("HFC", 
                          "N2O", 
                          "NF3",
                          "PFC",
                          "SF6",
                          "Combined")) %>% 
  ggplot(aes(x=year, y=Total))+
  geom_point(size=3,aes(colour = gas_type, alpha=0.5))+
  geom_line(aes(colour=gas_type))+
  theme_minimal())


######

# 1. Total emissions trend over time
total_trend <- ggplot(emission_data_wide, aes(year, total)) +
  geom_line(color="#2c3e50", size=1) +
  geom_smooth(method="loess", color="#e74c3c") +
  labs(title="Total Gas Emissions Over Time (1990-2023)",
       x="Year", y="Total Emissions") +
  theme_minimal() +
  scale_y_continuous(labels=comma)

view(emissions_data)


ggplot(emissions_data, aes(year,
                           total_gas_emissions, 
                           colour = gas_type))+
  geom_line()+
  geom_smooth(mehod="loess", colour="#e74c3c")+
  scale_y_continuous(labes=comma)

view(filter(sectors_data_rename, 
       gas_type!="Combined",
       sector!="Total"))

#filterd no Total in sector and Combined gas_type
sectors_data_rename %>%
  filter(gas_type!="Combined" | sector!="Total") %>% 
  ggplot(aes(year,emissions/1000, 
             colour = sector ))+
  geom_point() 

rm(filter_sector_data)

view(sectors_data)

sectors_data_rename %>% 
  filter(sector=="manufacturing")
  ggplot(aes(year,emissions/1000))+
  geom_point(aes(colour = sector)))



library(ggplot2)
library(ggridges)
library(tidyverse)

emission_data_wide %>%
  filter(gas_type!="Combined") %>% 
  ggplot(aes(Total/1000, gas_type))+
  geom_density_ridges(scale=1)

emission_data_wide %>%
  filter(gas_type!="Combined") %>% 
  ggplot(aes(Total/1000, gas_type))+
  geom_density_ridges(rel_min_height=0.00004)

emission_data_wide %>%
  filter(gas_type!="Combined") %>% 
  ggplot(aes(Total/1000, gas_type))+
  geom_density_ridges(fill="lightblue",
                      colour=6,
                      linetype=1,
                      lwd=0.5)

emission_data_wide %>%
  filter(gas_type!="Combined") %>% 
  ggplot(aes(Total/1000, gas_type))+
  geom_density_ridges(scale=1, aes(fill = gas_type))+
  scale_colour_viridis_b()



emission_data_wide %>%
  filter(gas_type!="Combined") %>% 
  ggplot(aes(Total/1000, gas_type, fill = stat(x)))+
  geom_density_ridges_gradient()+
  scale_fill_viridis_b(name = "Total",
                       option = "C")


emission_data_wide %>%
  filter(!gas_type%in% c("Combined", "Co2", "CH4")) %>% 
  ggplot(aes(Total/1000, gas_type, fill=stat(x)))+
  geom_density_ridges_gradient()


# Plotting the data
sector_percentage_of_total %>%
  filter(year == 2023) %>%
  ggplot(aes(x = "", y = percentage, fill = Sector)) +
  geom_col() +
  coord_polar(theta = "y") +  # Ensure "theta" is in quotes
  labs(title = "Gas Emissions by Type in 2023", 
       y = "Percentage", 
       fill = "sector") +
  theme_minimal()+
  scale_color_brewer()

write.csv(sector_percentage_of_total,
          file = "Sector%.csv", row.names = FALSE)

sector_percentage_of_total %>% 
  filter(year %in% c(1990, 2000, 2010, 2020)) %>%  # Filter for the specified years
  ggplot(aes(x = "", y = percentage, fill = Sector)) +
  geom_col(width = 1) +  # Create a bar plot with all bars having equal width
  coord_polar(theta = "y") +  # Convert the bar plot into a pie chart
  facet_wrap(~ year) +  # Create separate charts for each year
  labs(title = "Sector-wise Percentage Distribution Over Years",
       fill = "Sector") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+

library(viridis)  

sector_percentage_of_total %>%
  filter(year %in% c(1990, 2000, 2010, 2020)) %>%  # Filter for the specified years
  ggplot(aes(x = "", y = percentage, fill = Sector)) +
  geom_col(width = 1, colour="white", size=0.1) +  # Create a bar plot with all bars having equal width
  coord_polar(theta = "y") +  # Convert the bar plot into a pie chart
  facet_wrap(~ year) +  # Create separate charts for each year
  labs(title = "Sector-wise Percentage Distribution Over Years",
       fill = "Sector") +
  scale_fill_viridis_d() +  # Apply a colorblind-friendly palette
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

sector_percentage_of_total %>%
  filter(year %in% c(1990, 2000, 2010, 2020)) %>%  # Filter for the specified years
  ggplot(aes(x = "", y = percentage, fill = Sector)) +
  geom_col(width = 1,lty="dashed" ,colour="gray", size=0.1) +  # Create a bar plot with all bars having equal width
  coord_polar(theta = "y") +  # Convert the bar plot into a pie chart
  facet_wrap(~ year) +  # Create separate charts for each year
  labs(title = "Sector-wise Percentage Distribution Over Years",
       fill = "Sector") +
  scale_fill_discrete() +  # Apply a colorblind-friendly palette
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

library(viridis)

#### make pie chart 
sector_percentage_of_total %>% 
  filter(year="2023") %>% 
  ggplot(aes(x="",y=percentage, fill = Sector))+
  geom_col()



sector_percentage_of_total %>%
  filter(year %in% c(1990, 2000, 2010, 2020)) %>%  # Filter for the specified years
  ggplot(aes(x = "", y = percentage, fill = Sector)) +
  geom_col(width = 1, colour="white", size=0.1) +  # Create a bar plot with all bars having equal width
  coord_polar(theta = "y") +  # Convert the bar plot into a pie chart
  facet_wrap(~ year) +  # Create separate charts for each year
  labs(title = "Sector-wise Percentage Distribution Over Years",
       fill = "Sector") +
  scale_fill_viridis_d() +  # Apply a colorblind-friendly palette
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

sector_percentage_of_total %>%
  filter(year %in% c(1990, 2000, 2010, 2020)) %>%  # Filter for the specified years
  ggplot(aes(x = "", y = percentage, fill = gas_type)) +
  geom_col(width = 1,lty="dashed" ,colour="gray", size=0.1) +  # Create a bar plot with all bars having equal width
  coord_polar(theta = "y") +  # Convert the bar plot into a pie chart
  facet_wrap(~ year) +  # Create separate charts for each year
  labs(title = "Sector-wise Percentage Distribution Over Years",
       fill = "Sector") +
  scale_fill_viridis_d() +  # Apply a colorblind-friendly palette
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# making Subset of data ### 

Data_subset<-sector_with_total %>% 
  filter(gas_type!="Combined",Sector=="Total", 
         year%in%c("1990","2000"))
