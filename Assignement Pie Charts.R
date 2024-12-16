#Pie Chart Consumer Expenidtures

#---- Data Manipulation for the blot 
Consumer_data_long %>%
  filter(year %in% c(1990, 2000, 2010, 2020), 
           gas_type != "Combined") %>%
  mutate(`Consumer EXP` = recode(`Consumer EXP`,
                                 "Non_travel_percentage" = "Consumer(non-travel)%",
                                 "Travel_percentage" = "Consumer(travel)%")) %>% 
  view()


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
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar("y", start = 0) +
  facet_grid(gas_type ~ year) +
  labs(title = "Consumer Expenditure by Gas Type in the UK\nOver Three decades", 
       x = NULL, 
       y = NULL,
       fill="Consumer EXP") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3, colour = "black",
            fontface="bold")+
  theme(axis.line = element_blank(),
        legend.title = element_text(hjust = 0.5,
                                    size = 13,
                                    face="bold",
                                    family="sans"),
        legend.position = "bottom",
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  size = 16, 
                                  face="bold",
                                  family = "sans"),
        strip.text.x = element_text(size=10,
                                    colour = "black", 
                                    face = "bold",
                                    family = "sans"),
        strip.text.y = element_text(size = 10,
                                    colour = "black",
                                    face="bold",
                                    family = "sans"))+
  scale_fill_brewer(palette = "Set2")


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
 
