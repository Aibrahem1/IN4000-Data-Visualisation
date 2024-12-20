#Final Report visualisations
#----------(Figure : Pie chart --- Consumer footprint by gas type over 3 decades)-----------
#1. Load "vis_data10.RData"

#2. Run following Libraries
library(tidyverse)
library(paletteer)
#3. Graph plotting
Consumer_data_long %>%
  filter(year %in% c(1990, 2000, 2010, 2020) & 
           gas_type != "Combined") %>% 
  mutate(`Consumer EXP` = recode(`Consumer EXP`,
                                 "Non_travel_percentage" = "Consumer(non-travel)%",
                                 "Travel_percentage" = "Consumer(travel)%")) %>% 
  ggplot(aes(x = "", y = Percentage, fill = `Consumer EXP`)) +
  geom_bar(stat = "identity", width = 2, colour = "white") +
  coord_polar("y", start = 0) +
  facet_grid(year ~ gas_type) +
  labs(title = "Consumer GHG footprint by Gas Type in the UK Over Three decades", 
       x = NULL, 
       y = NULL,
       fill="Consumer EXP",
       caption = 'Data source: UK Office for National Statistics (ONS) - UK Environmental Accounts') +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3.5, colour = "white",
            fontface="bold")+
  theme(axis.line = element_blank(),
        legend.title = element_text(hjust = 0.5,
                                    size = 9,
                                    face="bold",
                                    family="sans"),
        legend.position = "top",
        legend.text = element_text(size = 10),
        plot.caption.position = 'plot',
        plot.caption = element_text(size = 10),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  size = 13, 
                                  face="bold",
                                  family = "sans"),
        strip.text.x = element_text(size=12,
                                    colour = "black", 
                                    face = "bold",
                                    family = "sans"),
        strip.text.y = element_text(size = 10,
                                    colour = "black",
                                    face="bold",
                                    family = "sans"))+
  scale_color_paletteer_d("ggthemes::wsj_red_green")+
  scale_fill_paletteer_d("ggthemes::wsj_red_green")

#----------(Figure : tiles-heatmap --- total greenhouse emissions from 1990 to 2023)------
#1. Load "vis_data10.RData"

#2. Run following Libraries
library(viridis)
library(dplyr)
library(ggplot2)
library(forcats)
library(viridis)
#3. Graph plotting
GHG_data_long %>%
  mutate(Sector = fct_reorder(Sector, 
                              Emissions, 
                              .desc = TRUE)) %>%
  ggplot(aes(x = year, y = Sector, fill = Emissions/1000)) +
  geom_tile(color = "white") +  # Adding white borders for readability
  labs(title = "Total Greenhouse Gas Emissions by Sector from 1990 to 2023\n(Intensity by Emission Levels)", 
       x = "Year", 
       y = "Sector",
       caption = 'Data source: UK Office for National Statistics (ONS) - UK Environmental Accounts') +
  scale_fill_viridis(name = "Emissions\nMillion Tonnes", option = "C") +  # Continuous viridis scale for gradient in heatmap
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 17),
        axis.text = element_text(face = "bold"),
        axis.text.y = element_text(angle = 20),
        plot.caption.position = 'plot',
        plot.caption = element_text(size = 10))


#----------(Figure : Stacked Area Chart: top emitting Sectors in the UK 1990-2023)------
#1. Load "vis_data10.RData"

#2. Run following Libraries
library(tidyverse)
library(viridis)
#3. Graph plotting

#stacked area chart showing top sectors, data manipulation 
#top10ghg data
ggplot(top10ghg, aes(x = year,
                     y = Emissions/1000, #Emissions in Million Tones 
                     fill = Sector)) +
  geom_area(position = "stack") +
  labs(title = "UK's Highest Emission Sectors Trend Over Time (1990-2023)",
       x = "Year",
       y = "Total Emissions\nin Million Tonnes",
       caption = 'Data source: UK Office for National Statistics (ONS) - UK Environmental Accounts' ) +
  theme_minimal() +
  theme(plot.title = element_text(face = 'bold', size = 17),
        plot.caption.position = 'plot',
        plot.caption = element_text(size = 10))+
  scale_fill_viridis_d()+
  geom_vline(xintercept = 2015, 
             lty="dashed")+
  geom_vline(xintercept = 2020, lty="dashed", color="red")+
  annotate("text", x = 2015, y = max(top10ghg$Emissions) / 1000, 
           label = "Paris Agreement\n          2015", 
           angle = 90, vjust = -0.5, hjust = -0.1,
           color = "white", size = 3.5)+
  annotate("text", x = 2020, y = max(top10ghg$Emissions) / 1000, 
           label = "Covid-19", angle = 90, vjust = -0.5, hjust = -0.1,
           color = "white", size = 3.5)

#----------(Figure : Geospatial Heatmap by Region in the UK 2022)-----------
#1. Load "geospatial_data.RData" or "vis_data10.RData"
#For details about data mutations and wrangling refer to "Geospatial Heatmap Graphs.R"
#2. Run following Libraries
library(tidyverse) # gplot2, tibble, tidyr, readr, dplyr, stringr, purr, and forcats
library(sf)
library(ggmap)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(devtools)
#3. Graph plotting
ggplot(uk_region_all_map) +
  geom_sf(aes(fill = Co2.Emission.kt)) +  # Heatmap fill
  geom_sf_text(data = region_labels2e, 
               aes(label = region, color = label_color), 
               size = 2.7, check_overlap = TRUE) +  # Label with dynamic color
  scale_fill_viridis_c(name = "CO2e Emissions (kt)", option = "plasma") +
  scale_color_identity() +  # Use color from the data (no scale applied)
  labs(title = "Geospatial Heatmap of CO2e Emissions by Region in the UK (2022)",
       subtitle = "Combined Sectors Emissions",
       caption = "Data Source: UK Greenhouse Gas Emissions: Local Authority and Regional") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 17, face = 'bold'),
    plot.subtitle = element_text(size = 13),
    plot.caption = element_text(size = 10),
    plot.caption.position = 'plot',
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right")

#----------(Figure :line chart gas types emissions trend 1990 to 2023 scaled)-----------
#1. Load "vis_data10.RData"

#2. Run following Libraries
library(tidyverse)
#3. Graph plotting
emissions_data %>% #dataframe 
  filter(gas_type!="Combined") %>% 
  ggplot(aes(x=year, y=Total/1000, colour = gas_type))+
  geom_point(size=2, alpha=0.5)+
  geom_vline(xintercept = 2013, 
             lty="dashed")+
  geom_line(alpha=0.5,
            size=1,
            lty="solid")+
  #geom_smooth(method = "lm", se=TRUE)+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))+
  scale_colour_viridis_d()+ #option 1 scale colour virdis_d 
  #scale_color_brewer(palette = "Dark2")+ #option 2 brewer
  labs(title = "Evolution of Greenhouse Gas Emissions by Type in the UK (1990–2023)",
       subtitle = "Analysis of Atmospheric Emissions from Key Greenhouse Gases Across Sectors",
       color = "Gas Type",
       x= "Years",
       y= "Emissions in Million Tonnes\nScaled (log10)",
       caption = "Data source: UK Office for National Statistics (ONS) - UK Environmental Accounts")+
  theme_minimal(base_size = 12) +  # Clean theme with larger base font
  theme(plot.title = element_text(face = "bold", 
                                  hjust = 0.5, # Centered title
                                  size = 19),
        plot.subtitle = element_text(hjust = 0.5, 
                                     size = 12),  # Subtitle under title
        axis.title = element_text(size = 14, 
                                  face = "bold"),  # Bold axis titles
        axis.text = element_text(size = 14),  # Larger axis text
        legend.position = "bottom",  # Position legend to the right
        legend.title = element_text(face = "bold", 
                                    size = 14),  # Bold legend title
        legend.text = element_text(size = 14),# Adjust legend text size
        panel.grid.major = element_line(color = "gray80", 
                                        size = 0.5),  # Light gridlines
        panel.grid.minor = element_blank(),
        panel.background =element_blank()) + # Remove minor grid lines
  theme(plot.caption.position = "plot",
        plot.caption = element_text(size = 10))+
  theme(
    panel.background = element_rect(fill = "snow", 
                                    color = "black"),  # Plot area background
    plot.background = element_rect(fill = "white"),  # Entire plot background
    panel.grid.major = element_line(color = "grey"),  # Major gridlines
    panel.grid.minor = element_blank())+  # Remove minor gridlines
  annotate("text", 
           x = 2013, 
           y = 0.0006, 
           label = "Drop in NF3", 
           color = "red", 
           size = 4, 
           hjust = 0,
           vjust=1,
           angle=90) # label for NF3 gas drop
