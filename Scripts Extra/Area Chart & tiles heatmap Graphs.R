# Assignment Graphs options
#Required Libraries
library(tidyverse)
library(viridis)
#-----------------------------------------------------

# Stacked Area Plot: Emissions by Sector Over Time
# Purpose: Visualizes each sector's share in total emissions over time.
# ASSERT: Reveal cumulative trends, Emphasize each sector’s impact over time.
# Grammar of Graphics: 'geom_area' for cumulative display, fill aesthetic for sectors.

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
#Adding key events (Paris Agreement 2015 and Covid 2019)
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


