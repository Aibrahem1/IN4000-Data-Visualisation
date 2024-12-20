#This code uses 'vis_data_cleaningv1.RData'

#Required Libraries to run the code

library(tidyverse)
library(paletteer)

#---- Data Manipulation for the plot 
Consumer_data_long %>%
  filter(year %in% c(1990, 2000, 2010, 2020), 
           gas_type != "Combined") %>%
  mutate(`Consumer EXP` = recode(`Consumer EXP`,
                                 "Non_travel_percentage" = "Consumer(non-travel)%",
                                 "Travel_percentage" = "Consumer(travel)%")) %>% 
  view()


#---------- Assignment Pie chart --------- 
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
        plot.caption = element_text(size = 10,
                                    face = 'bold'),
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
#scale_fill_paletteer_d("ggsci::alternating_igv")
#scale_fill_brewer(palette = "Set2")


# Pie Chart Theme 0
Consumer_data_long %>%
  filter(year %in% c(1990, 2000, 2010, 2020) & 
           gas_type != "Combined") %>%
  mutate(`Consumer EXP` = recode(`Consumer EXP`,
                                 "Non_travel_percentage" = "Consumer(non-travel)%",
                                 "Travel_percentage" = "Consumer(travel)%")) %>% 
  ggplot(aes(x = "", y = Percentage, fill = `Consumer EXP`)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar("y", start = 0) +
  facet_grid(year ~ gas_type) +
  labs(title = "Customer Expenditure by Gas Type in the UK Over Three decades", 
       x = NULL, 
       y = NULL,
       fill="Consumer EXP") +
  theme_void() +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, colour = "black",
            fontface="bold") +
  theme(legend.title = element_blank(),
        panel.spacing = unit(0.1, "lines"), # Reduce space between pies
        plot.title = element_text(hjust = 0.5,  # Center and adjust title size
                                  size = 16, 
                                  face="bold"),
        strip.text.x = element_text(size=10,
                                    colour = "darkred", 
                                    face = "bold"),
        strip.text.y = element_text(size = 10,
                                    colour = "darkgreen",
                                    face="bold")) +
  scale_fill_brewer(palette = "Accent")

#------- Pie Chart theme 1

Consumer_data_long %>%
  filter(year %in% c(1990, 2000, 2010, 2020) & 
           gas_type != "Combined") %>%
  mutate(`Consumer EXP` = recode(`Consumer EXP`,
                                 "Non_travel_percentage" = "Consumer(non-travel)%",
                                 "Travel_percentage" = "Consumer(travel)%")) %>% 
  ggplot(aes(x = "", y = Percentage, fill = `Consumer EXP`)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar("y", start = 0) +
  facet_grid(year ~ gas_type) +
  labs(title = "Customer Expenditure by Gas Type in the UK\nOver Three decades", 
       x = NULL, 
       y = NULL,
       fill="Consumer EXP") +
  theme_void() +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, colour = "black",
            fontface="bold") +
  theme(legend.title = element_text(size = 9,
                                    hjust = 0.5,
                                    colour="royalblue4",
                                    face = "bold"),
        panel.spacing = unit(0, "lines"), # Reduce space between pies
        plot.title = element_text(hjust = 0.5,  # Center and adjust title size
                                  size = 16, 
                                  face="bold",
                                  colour = "royalblue4"),
        strip.text.x = element_text(size=13,
                                    colour = "seagreen", 
                                    face = "bold",
                                    family = "mono"),
        strip.text.y = element_text(size = 13,
                                    colour = "seagreen",
                                    face="bold",
                                    family = "mono")) +
  scale_fill_brewer(palette = "Set2")

# Pie Chart Theme 2 (colourblind friendly /formal)
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
       caption = 'Data Source:') +
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
        plot.caption = element_text(size = 10,
                                    face = 'bold'),
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
  #scale_fill_paletteer_d("ggsci::alternating_igv")
  #scale_fill_brewer(palette = "Set2")


## Pie Chart Theme 3 

Consumer_data_long %>%
  filter(year %in% c(1990, 2000, 2010, 2020,2023) & 
           gas_type != "Combined") %>%
  mutate(`Consumer EXP` = recode(`Consumer EXP`,
                                 "Non_travel_percentage" = "Consumer(non-travel)%",
                                 "Travel_percentage" = "Consumer(travel)%")) %>% 
  ggplot(aes(x = "", y = Percentage, fill = `Consumer EXP`)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar("y", start = 0) +
  facet_grid(gas_type ~ year) +
  labs(title = "Customer Expenditure by Gas Type in the UK\nOver Three decades", 
       x = NULL, 
       y = NULL,
       fill="Consumer EXP") +
  theme_void() +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, colour = "black",
            fontface="bold") +
  theme(legend.title = element_text(size = 9,
                                    hjust = 0.5,
                                    colour="royalblue4",
                                    face = "bold"),
        panel.spacing = unit(0, "lines"), # Reduce space between pies
        plot.title = element_text(hjust = 0.5,  # Center and adjust title size
                                  size = 16, 
                                  face="bold",
                                  colour = "royalblue4"),
        strip.text.x = element_text(size=13,
                                    colour = "royalblue2", 
                                    face = "bold",
                                    family = "mono"),
        strip.text.y = element_text(size = 13,
                                    colour = "royalblue2",
                                    face="bold",
                                    family = "mono")) +
  scale_fill_manual(
    values = c(
      "Consumer(non-travel)%" = "khaki3",  #  for non-travel
      "Consumer(travel)%" = "tomato"))      #  for travel
 

## Pie Chart Theme 4
install.packages("paletteer")
library(paletteer)

Consumer_data_long %>%
  filter(year %in% c(1990, 2000, 2010, 2020,2023) & 
           gas_type != "Combined") %>%
  mutate(`Consumer EXP` = recode(`Consumer EXP`,
                                 "Non_travel_percentage" = "Consumer(non-travel)%",
                                 "Travel_percentage" = "Consumer(travel)%")) %>% 
  ggplot(aes(x = "", y = Percentage, fill = `Consumer EXP`)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar("y", start = 0) +
  facet_grid(gas_type ~ year) +
  labs(title = "Customer Expenditure by Gas Type in the UK\nOver Three decades", 
       x = NULL, 
       y = NULL,
       fill="Consumer EXP") +
  theme_void() +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, colour = "black",
            fontface="bold") +
  theme(legend.title = element_text(size = 9,
                                    hjust = 0.5,
                                    colour="royalblue4",
                                    face = "bold"),
        panel.spacing = unit(0, "lines"), # Reduce space between pies
        plot.title = element_text(hjust = 0.5,  # Center and adjust title size
                                  size = 16, 
                                  face="bold",
                                  colour = "royalblue4"),
        strip.text.x = element_text(size=13,
                                    colour = "royalblue2", 
                                    face = "bold",
                                    family = "mono"),
        strip.text.y = element_text(size = 13,
                                    colour = "royalblue2",
                                    face="bold",
                                    family = "mono")) +
  scale_fill_paletteer_d("nationalparkcolors::Acadia")+
  scale_colour_paletteer_d("nationalparkcolors::Acadia")
