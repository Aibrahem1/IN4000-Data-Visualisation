#-------- Visualizations tests 
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
  labs(title = "Emissions in Million Tonnes\nby Sector Over Time", 
       x = "Year",
       y = "Total Emissions") +
  theme_minimal() +
  scale_fill_viridis_d()  # Colorblind-friendly palette for stacked area plot

#stacked area chart showing top sectors, data manipulation 
#top10ghg data
ggplot(top10ghg, aes(x = year,
                     y = Emissions/1000, #Emissions in Million Tones 
                     fill = Sector)) +
  geom_area(position = "stack") +
  labs(title = "Top Emitting Sectors\nin the UK,1990 to 2023",
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

#line trend chart with plotly
library(plotly) # to used plotly before the graph use ggplotly


ggplotly(emissions_data %>%
  filter(gas_type!="Combined") %>%
  ggplot(aes(x=year, y=Total, colour = gas_type))+ #divide by factor
  geom_line(lty="dashed",alpha=0.4)+
  geom_point()+
  geom_line(aes(colour = gas_type))+
  scale_y_log10()+ # Scaling Values for clarity 1
  scale_colour_viridis_d()+   # Colour accessibility
  labs(x="Year", y="Total Gas Emissions\nlog Million Tonnes",
       title = "Trend of Greenhouse Emissions by gas\nFrom 1990-2023"))
  #scale_y_continuous(trans = "log") #Scaling values 2




install.packages("GGally")
library(GGally)
library(tidyverse)

# ----------------- Parallel coordinates chart ---------------
emissions_data %>% 
  filter(gas_type!="Combined") %>% 
  ggparcoord(columns = c(3:23), 
           groupColumn = 2)+ # colored by gas_type
  geom_vline(xintercept =c(1:23), lty="dashed", 
             colour="grey")+
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1),
        panel.background = element_blank())
#Colored by year
emissions_data %>% 
  filter(gas_type!="Combined") %>% 
  ggparcoord(columns = c(3:23), 
             groupColumn = 1)+ # colored by year
  geom_vline(xintercept =c(1:23), lty="dashed", 
             colour="grey")+
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1),
        panel.background = element_blank())

#-------------Spider chart ----------------------------------
library(ggradar)
library(scales) #run install.packages("scales") first if you haven't install it yet
library(tidyverse) #run install.packages("tidyverse") first if you haven't install it yet
library(ggradar)
#• We first use the mutate_if function to scale all numeric columns.
#• Then we create a new variable called new_region where we have the names of the regions but with
#spaces changes to underscores using the str_replace_all function.
#• Then we group the data by the new region column.
#• Finally, we can calculate the mean of every numeric column.
#• The final data is passed to the ggradar function.

#------- data manuipulation to pipe the data into GGRADAR
emissions_data %>%
  filter(gas_type=="Co2" | gas_type=="CH4") %>%
  mutate() %>% 
  select(2:23) %>%
  mutate_if(is.numeric,rescale) %>% 
  group_by(gas_type) %>% 
  summarise_if(is.numeric,mean) %>%
  ggradar(axis.label.size = 3,
          legend.text.size = 8,
          axis.line.colour ="grey")

#### Manually scaling by dividing on total to obtain value betwen 0-1

# data to pip-into the spider graph (2023 precentage gases) 

emissions_data %>%
  mutate(across(3:23, ~ (. / Total) * 100)) %>% #scaling values between 0-1 by dividing overtotal
  filter(gas_type!="Combined", # filtered to excluded combined gases
         year=="1990") %>% # filter for a specific year
  select(2:23) %>% # selected colums for the spiderchart
  group_by(gas_type) %>%
  view()

# Spider graph 1 1990 contribution of gases by sector
emissions_data %>%
  mutate(across(3:23, ~ (. / Total) * 100)) %>% #scaling values between 0-1 by dividing overtotal
  filter(gas_type!="Combined", # filtered to excluded combined gases
         year=="1990") %>% # filter for a specific year
  select(2:23) %>% # selected colums for the spiderchart
  group_by(gas_type) %>%
  ggradar(axis.label.size = 3,
          legend.text.size = 8,
          axis.line.colour ="grey")
#---problems are some data is overlaping and cannot see their related 
#lines

#----- Spider graph 2 for 1990 adjusting
emissions_data %>%
  mutate(across(3:23, ~ . / Total)) %>% #scaling values between 0-1 by dividing overtotal
  filter(gas_type!="Combined", # filtered to excluded combined gases
         year=="1990") %>% # filter for a specific year
  select(2:23) %>% # selected colums for the spiderchart
  group_by(gas_type) %>% 
  drop_na() %>% 
  ggradar(axis.label.size = 3,      # Reduce axis label size
          legend.text.size = 8,     # set the legend text size
          axis.line.colour ="grey", # Set a lighter color for gridlines
          group.line.width = 1,     # Reduce line width of each group
          group.point.size = 3,     # Set point size (optional)
          grid.label.size = 3,      # Adjust the size of grid labels (0%, 50%, 100%)
          legend.position = "right")+ # change legend position from left tow right
  scale_colour_viridis_d()+
  ggtitle("Relative Emissions by Sector\nfor Different Gases in 1990") +
  theme(
    plot.title = element_text(size = 14), # Adjust title size here
    plot.caption = element_text(size = 10, hjust = 1))+ # Adjust caption size here
  labs(caption = "Amin")

#-------------
# Spider graph 2 for 2023
emissions_data %>%
  mutate(across(3:23, ~ . / Total)) %>% #scaling values between 0-1 by dividing overtotal
  filter(gas_type!="Combined", # filtered to excluded combined gases
         year=="2023") %>% # filter for a specific year
  select(2:23) %>% # selected colums for the spiderchart
  group_by(gas_type) %>% 
  ggradar(axis.label.size = 3,      # Reduce axis label size
          legend.text.size = 8,     # set the legend text size
          axis.line.colour ="grey", # Set a lighter color for gridlines
          group.line.width = 1,     # Reduce line width of each group
          group.point.size = 3,     # Set point size (optional)
          grid.label.size = 3,      # Adjust the size of grid labels (0%, 50%, 100%)
          legend.position = "right")+ # change legend position from left tow right
  scale_colour_viridis_d()+
  ggtitle("Relative Emissions by Sector\nfor Different Gases in 2023") +
  theme(
    plot.title = element_text(size = 14), # Adjust title size here
    plot.caption = element_text(size = 10, hjust = 1))+ # Adjust caption size here
  labs(caption = "Amin")


#create facet for 1990 and 2023 to show comparisons for spider

