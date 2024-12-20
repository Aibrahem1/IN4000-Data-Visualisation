#---------------------bar charts-------------------------------#
#Required Libraries
library(tidyverse)
library(paletteer)
library(forcats)
############################################################
#1. Assignment Bar Chart

sector_data %>% 
  filter(gas_type!='Combined', year=='2023', Percentage!=0) %>% 
  select(sector, gas_type, Percentage, emissions) %>%
  mutate(sector = fct_reorder(sector, emissions, .desc = FALSE)) %>%  
  ggplot(aes(x=sector, y=Percentage, fill = gas_type))+
  geom_bar(stat = 'identity', width = 0.7, position = 'identity')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(size=3,face='bold', aes(
    label =paste0(round(Percentage, 2),'%'),
    hjust = ifelse(gas_type %in% c("NF3", "PFC"), 1.2, -0.2),
    colour = ifelse(gas_type%in%c('NF3','PFC'), 'white', 'black')))+ 
  facet_grid(.~gas_type)+
  coord_flip()+
  scale_fill_viridis_d()+
  scale_color_identity()+
  labs(title = 'Analysis of Gas Composition by Sector for the Year 2023:Comprehensive Breakdown of Emission Sources',
       caption = 'Data source: UK Office for National Statistics (ONS) - UK Environmental Accounts',
       y=NULL,
       x='Sector',
       fill='Gas Type')+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(face = 'bold', angle = 0),
        axis.title.y = element_text(size=13),
        strip.text = element_text(size = 13))



# Barchart: for 2023 with facet ~gas theme minimal

sector_data %>% 
  filter(year == "2023", 
         gas_type != "Combined") %>% 
  select(sector, gas_type, Percentage) %>% 
  ggplot(aes(x = sector, y = Percentage, fill = gas_type)) +
  geom_bar(stat = "identity", 
           position = "identity", 
           width = 0.7, 
           color = "white") +
  geom_text(aes(
    label = round(Percentage, 2),
    hjust = ifelse(gas_type %in% c("NF3", "PFC"), 1.2, -0.2),  # Position inside for NF3 and PFC
    color = ifelse(gas_type %in% c("NF3", "PFC"), "white", "black")  # White for NF3 and PFC, black for others
  ), size = 3) +
  coord_flip() +
  facet_grid(. ~ gas_type) +
  scale_fill_manual(values = c("goldenrod", 
                               "darkorange", 
                               "skyblue", "tomato", 
                               "purple", "green",
                               "pink")) +
  scale_color_identity() +  # Use the color values set in geom_text directly
  labs(
    title = "Emissions by Sector and Gas Type (2023)",
    x = "Sector",
    y = "Total Emissions",
    fill = "Gas Type"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"))

  
# Position inside for NF3 and PFC
# White for NF3 and PFC, black for others
######################################################################
#barchar 2:  facet by gas type using viridis colour for accessibility
#white colour fill for NF3 and PFC 
sector_data %>% 
  filter(year == "2023", 
         gas_type != "Combined") %>% 
  select(sector, gas_type, Percentage) %>% 
  ggplot(aes(x = sector, y = Percentage, fill = gas_type)) +
  geom_bar(stat = "identity", 
           position = "identity", 
           width = 0.7, 
           color = "white") +
  geom_text(aes(
    label = paste0(round(Percentage, 2),"%"),
    hjust = ifelse(gas_type %in% c("NF3", "PFC"), 1.2, -0.2),  # Position inside for NF3 and PFC
    color = ifelse(gas_type %in% c("NF3", "PFC"), "white", "black")), # White for NF3 and PFC, black for others
    size = 3) +  
  coord_flip() +
  facet_grid(. ~ gas_type) +
  scale_fill_viridis_d() +
  scale_color_identity() +  # Use the color values set in geom_text directly
  labs(
    title = "Emissions by Sector and Gas Type (2023)",
    x = "Sector",
    y = "Total Emissions",
    fill = "Gas Type"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"))
################################################################
# barchart3: Same as above different theme + bold label
sector_data %>% 
  filter(year == "2023", 
         gas_type != "Combined") %>% 
  select(sector, gas_type, Percentage) %>% 
  ggplot(aes(x = sector, y = Percentage, fill = gas_type)) +
  geom_bar(stat = "identity", 
           position = "identity", 
           width = 0.7, 
           color = "white") +
  geom_text(aes(
    label = paste0(round(Percentage, 2), "%"),  # Append % to each value
    hjust = ifelse(gas_type %in% c("NF3", "PFC"), 1.2, -0.2),  # Position inside for NF3 and PFC
    color = ifelse(gas_type %in% c("NF3", "PFC"), "white", "black")), # White for NF3 and PFC, black for others
    size = 3,
    fontface="bold") +  
  coord_flip() +
  facet_grid(. ~ gas_type) +
  scale_fill_viridis_d() +
  scale_color_identity() +  # Use the color values set in geom_text directly
  labs(
    title = "Emissions by Sector and Gas Type (2023)",
    x = "Sector",
    y = "Total Emissions",
    fill = "Gas Type"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"))
####################################################################
# barchart4 : Same as above bw theme + bold + black geom text

sector_data %>% 
  filter(year == "2023", 
         gas_type != "Combined") %>% 
  select(sector, gas_type, Percentage) %>% 
  ggplot(aes(x = sector, y = Percentage, fill = gas_type)) +
  geom_bar(stat = "identity", 
           position = "identity", 
           width = 0.7, 
           color = "white") +
  geom_text(aes(
    label = paste0(round(Percentage, 2), "%"),  # Append % to each value
    hjust = ifelse(gas_type %in% c("NF3", "PFC"), 1.2, 0)),  # Position inside for NF3 and PFC
    size = 3,
    fontface = "bold",    # Set text to bold
    color = "black") +    # Set color to black for all text
  coord_flip() +
  facet_grid(. ~ gas_type) +
  scale_fill_viridis_d() +
  labs(
    title = "Emissions by Sector and Gas Type (2023)",
    x = "Sector",
    y = "Total Emissions",
    fill = "Gas Type") +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"))

######################################################################
#barchart4: Stack bar chart (not good)
sector_data %>% 
  # Step 1: Filter out values less than 1 or equal to zero
  filter(year == "2023", 
         gas_type != "Combined", 
         Percentage > 1) %>%  # Keep only values greater than 1
  select(sector, gas_type, Percentage) %>% 
  ggplot(aes(x = gas_type, y = Percentage, fill = sector)) +  # Flip x and y
  geom_bar(stat = "identity", 
           position = "identity", 
           width = 0.7, 
           color = "white") +
  
  # Add labels with percentages, but only if Percentage > 1
  geom_text(aes(
    label = paste0(round(Percentage, 2), "%"),
    hjust = ifelse(gas_type %in% c("NF3", "PFC"), 1.2, -0.2),
    color = ifelse(gas_type %in% c("NF3", "PFC"), "white", "black")
  ), size = 3) +
  
  facet_grid(. ~ gas_type, scales = "free") +  # Facet along x-axis by gas type
  scale_fill_viridis_d() +
  scale_color_identity() +  # Use color values set in geom_text directly
  
  # Customize labels and theme
  labs(
    title = "Emissions by Sector and Gas Type (2023)",
    x = "Gas Type",
    y = "Total Emissions (%)",
    fill = "Sector"
  ) +
  
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Adjust x-axis text for readability
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines")
  )
####################################################################
#Barchart 5: 

#^Data
sector_data %>% # Step 1: Filter out values less than 1 or equal to zero
  filter(year == "2023", 
         gas_type != "Combined", #exclude Combined
         Percentage > 1) %>% # Keep only values greater than 1
  arrange(desc(Percentage)) %>% 
  select(year,sector, gas_type, Percentage) %>% 
  view()
#^data: Data for multiple years to be plotted
sector_data %>% # Step 1: Filter out values less than 1 or equal to zero
  filter(year%in%c("1990","2000","2010","2020"),  
         gas_type != "Combined", #exclude Combined
         Percentage > 1) %>% # Keep only values greater than 1
  arrange(desc(Percentage)) %>% 
  select(year,sector, gas_type, Percentage) %>% 
  view()

#@Chart #### not facet ~year 2023
sector_data %>% # Step 1: Filter out values less than 1 or equal to zero
  filter(year == "2023", 
         gas_type != "Combined", 
         Percentage > 1) %>% # Keep only values greater than 1
  arrange(desc(Percentage)) %>% 
  select(sector, gas_type, Percentage) %>% 
  ggplot(aes(x = sector, y = Percentage, fill = gas_type)) +  # Flip x and y
  geom_bar(stat = "identity", 
           position = "identity", 
           width = 0.7, 
           color = "white")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_viridis_d()+
  scale_y_log10()+
  labs(x= "Sector",
       y="Percentage of Emissions",
       title = "Percentage of Emissions by Sector and Gas Type in 2023")
##############################################################
#Barchart5: Stacked bar chart ovver 3 decades with facet stat identity
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

#validations
sector_data %>% # Step 1: Filter out values less than 1 or equal to zero
  filter(gas_type != "Combined", #exclude Combined
         emissions > 1) %>% # Keep only values greater than 1
  arrange(year,desc(emissions)) %>% 
  view()
########################################################
#Barchart 6- cluter bar by emissions comparing 4 different years 
#with facet position dodge
sector_data %>% # Step 1: Filter out values less than 1 or equal to zero
  filter(year%in%c("1990","2020"),  
         gas_type != "Combined", #exclude Combined
         emissions > 10) %>% # Keep only values greater than 1
  arrange(desc(emissions)) %>% 
  select(year,sector, gas_type, emissions) %>% 
  ggplot(aes(x = sector, y = emissions, fill = gas_type)) +  # Flip x and y
  geom_bar(stat = "identity", 
           position = position_dodge(preserve = "single"), 
           width = 0.7, 
           color = "white")+
  facet_wrap(.~year, nrow = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_viridis_d()+
  scale_y_log10()+
  labs(x= "Sector",
       y="Percentage of Emissions",
       title = "Percentage of Emissions by Sector and Gas Type in 2023")
#############################################################333
#Barchart 7- cluter bar by percentage comparing 4 different years 
#with facet position dodge
sector_data %>% # Step 1: Filter out values less than 1 or equal to zero
  filter(year%in%c("1990","2020"),  
         gas_type != "Combined", #exclude Combined
         Percentage > 1) %>% # Keep only values greater than 1
  arrange(desc(Percentage)) %>% 
  select(year,sector, gas_type, Percentage) %>% 
  ggplot(aes(x = sector, y = Percentage, fill = gas_type)) +  # Flip x and y
  geom_bar(stat = "identity", 
           position = position_dodge(preserve = "single"), 
           width = 0.7, 
           color = "white")+
  facet_wrap(.~year, nrow = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_viridis_d()+
  scale_y_log10()+
  labs(x= "Sector",
       y="Percentage of Emissions",
       title = "Percentage of Emissions by Sector and Gas Type in 2023")

#############################################################

sector_data %>% # Step 1: Filter out values less than 1 or equal to zero
  filter(year%in%c("1990","2000","2010","2020"),  
         gas_type != "Combined", #exclude Combined
         Percentage > 0) %>% # Keep only values greater than 1
  arrange(desc(Percentage)) %>% 
  select(year,sector, gas_type, Percentage) %>%
  group_by(sector) %>% 
  ggplot(aes(x = sector, y = Percentage, fill = gas_type)) +  # Flip x and y
  geom_bar(stat = "identity", 
           position = "identity", 
           width = 0.7, 
           color = "white")+
  facet_wrap(.~year)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_viridis_d()+
  #scale_y_log10()+
  labs(x= "Sector",
       y="Percentage of Emissions",
       title = "Percentage of Emissions by Sector and Gas Type in 2023")

#############################################################
#---Clustered bar chart
sector_data %>% #Step 1: Filter out values less than 1 or equal to zero
  filter(year == "2023", 
         gas_type != "Combined", 
         Percentage > 1) %>% # Keep only values greater than 1
  arrange(desc(Percentage)) %>% 
  select(sector, gas_type, Percentage) %>% 
  ggplot(aes(x = sector, y = Percentage, fill = gas_type)) +  # Flip x and y
  geom_bar(stat = "identity", 
           position = position_dodge(preserve = "single"), 
           width = 0.7, 
           color = "white")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_viridis_d()+
  scale_y_log10()

#############################################################
# ------ composite of gases contribution for each sector 2023
sector_data %>% 
  filter(year == "2023", 
         gas_type != "Combined", 
         emissions > 1) %>% #   # Step 1: Filter out values less than 1
  select(sector, gas_type, emissions) %>%
  ggplot(aes(x=sector,
             y=emissions/1000,
             fill=gas_type))+
  geom_bar(stat = "identity", width=0.7,
           position = "identity")+
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))+
  scale_y_log10()+
  scale_fill_viridis_d()

#############################################################
## not accurate
sector_data %>% 
  filter(year == "2023", 
         gas_type != "Combined", 
         Percentage > 1) %>% #   # Step 1: Filter out values less than 1
  select(sector, gas_type, Percentage) %>%
  ggplot(aes(x=sector,
             y=Percentage,
             fill=gas_type))+
  geom_bar(stat = "identity", width=0.7,
           position = "identity")+
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))


#############################################################
sector_data %>% 
  filter(year == "2023", 
         gas_type != "Combined") %>% 
  select(sector, gas_type, Percentage) %>% 
  ggplot(aes(x = sector, y = Percentage, fill = gas_type)) +
  geom_bar(stat = "identity", 
           position = "identity", 
           width = 0.7, 
           color = "white") +
  geom_text(aes(
    label = paste0(round(Percentage, 2), "%"),  # Append % to each value
    hjust = ifelse(gas_type %in% c("NF3", "PFC"), 1.2, -0.2),  # Position inside for NF3 and PFC
    color = ifelse(gas_type %in% c("NF3", "PFC"), "white", "black")), # White for NF3 and PFC, black for others
    size = 3,
    fontface="bold") +  
  coord_flip() +
  facet_grid(. ~ gas_type) +
  scale_fill_viridis_d() +
  scale_color_identity() +  # Use the color values set in geom_text directly
  labs(
    title = "Emissions by Sector and Gas Type (2023)",
    x = "Sector",
    y = "Total Emissions",
    fill = "Gas Type"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"))
##----------------------testing ----------------------------


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