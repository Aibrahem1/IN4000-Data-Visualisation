# Assignment Graphs options

#-------- Visualizations tests 
#-----------------------------------------------------

# Stacked Area Plot: Emissions by Sector Over Time
# Purpose: Visualizes each sector's share in total emissions over time.
# ASSERT: Reveal cumulative trends, Emphasize each sector’s impact over time.
# Grammar of Graphics: 'geom_area' for cumulative display, fill aesthetic for sectors.

#Option 1
#Stack area chart showing all sectors
ggplot(GHG_data_long, aes(x = year,
                          y = Emissions/1000, #Emissions in Million Tones 
                          fill = Sector)) +
  geom_area(position = "stack") +
  labs(title = "Sector Emissions in the UK,\nfrom 1990 to 2023", 
       x = "Year",
       y = "Total Emissions\nin Million Tonnes") +
  theme_minimal() +
  scale_fill_viridis_d()  # Colorblind-friendly palette for stacked area plot

#Option2 
#stacked area chart showing top sectors, data manipulation 
#top10ghg data
ggplot(top10ghg, aes(x = year,
                     y = Emissions/1000, #Emissions in Million Tones 
                     fill = Sector)) +
  geom_area(position = "stack") +
  labs(title = "Top emitting sectors\nin the UK,1990 to 2023",
       x = "Year",
       y = "Total Emissions\nin Million Tonnes") +
  theme_minimal() +
  scale_fill_viridis_d()+
  geom_vline(xintercept = 2015, 
             lty="dashed")+
  geom_vline(xintercept = 2020, lty="dashed", color="red")+
  annotate("text", x = 2015, y = max(top10ghg$Emissions) / 1000, 
           label = "Paris Agreement\n 2015", angle = 90, vjust = -0.5, hjust = -0.1,
           color = "white", size = 3.5)+
  annotate("text", x = 2020, y = max(top10ghg$Emissions) / 1000, 
           label = "Covid-19", angle = 90, vjust = -0.5, hjust = -0.1,
           color = "white", size = 3.5)

#--- Stacked bar chart 4 decades with facet
sector_data %>% # Step 1: Filter out values less than 1 or equal to zero
  filter(year%in%c("1990","2000","2010","2020"),  
         gas_type != "Combined", #exclude Combined
         Percentage > 1) %>% # Keep only values greater than 1
  arrange(desc(Percentage)) %>% 
  select(year,sector, gas_type, Percentage) %>% 
  ggplot(aes(x = sector, y = Percentage, fill = gas_type)) +  # Flip x and y
  geom_bar(stat = "identity", 
           position = "identity", 
           width = 0.7, 
           color = "white")+
  facet_wrap(.~year)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_viridis_d()+
  scale_y_log10()+
  labs(x= "Sector",
       y="Percentage of Emissions",
       title = "Percentage of Emissions by Sector and Gas Type in 2023")

#-----------------------------------------------------
# Heatmap: Sectoral Emissions by Year
# Purpose: Highlights annual emissions for each sector.
# ASSERT: Emphasize high and low emission years; simple heatmap layout.
# Grammar of Graphics: 'geom_tile' for heatmap, fill aesthetic representing emissions intensity.
library(viridis)
library(dplyr)
library(ggplot2)
library(forcats)
library(viridis)

ggplot(GHG_data_long, aes(x = year, y = Sector, fill = Emissions)) +
  geom_tile(color = "white") +  # Adding white borders for readability
  labs(title = "Sectoral Emissions by Year", x = "Year", y = "Sector") +
  scale_fill_viridis(name = "Emissions", option = "C") +  # Continuous viridis scale for gradient in heatmap
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability

# Reording the tiles view#
GHG_data_long %>%
  mutate(Sector = fct_reorder(Sector, 
                              Emissions, 
                              .desc = TRUE)) %>%
  ggplot(aes(x = year, y = Sector, fill = Emissions/1000)) +
  geom_tile(color = "white") +  # Adding white borders for readability
  labs(title = "Total Greenhouse Gas Emissions by Sector from 1990 to 2023\n(Intensity by Emission Levels)", x = "Year", y = "Sector") +
  scale_fill_viridis(name = "Emissions\nMillion Tonnes", option = "C") +  # Continuous viridis scale for gradient in heatmap
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 17),
        axis.text = element_text(face = "bold"),
        axis.text.y = element_text(angle = 20))


