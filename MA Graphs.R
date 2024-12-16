# Load the dataset
load("C:\\Users\\HP\\Downloads\\Assignment_datav6.RData")
ls()

# Load necessary libraries
library(ggplot2)
# Install and load viridis package if not already installed
# install.packages("viridis")
library(viridis)

# Line Plot: Emissions Over Time by Sector
# Purpose: Shows trends in emissions across different sectors over time.
# ASSERT: Articulate (tracking sectoral emissions over time), Simplify (only essential variables used),
#         Sequence (x-axis for time), Emphasize (different colors for each sector to differentiate).
# Grammar of Graphics: 'geom_line' as geometry, color aesthetic for 'Sector', and minimal theme for simplicity.

ggplot(GHG_data_long, aes(x = year, y = Emissions, color = Sector)) +
  geom_line(size = 1) +# Line thickness for better visibility
  geom_vline(xintercept = 2015, lty="dashed")+
  labs(title = "Sector-wise Emissions Over Time", x = "Year", y = "Emissions") +
  theme_minimal() +
  theme(legend.position = "bottom") +  # Position legend for easier access
  scale_color_viridis_d()  # Colorblind-friendly palette with viridis
#--------------------------------------------------

# Filter data for the year 1990
data_1990 <- subset(GHG_data_long, year == 1990)

# Bar Plot: Emissions by Sector for a Specific Year
# ASSERT: Emphasize sectoral emissions; Simple bar chart without additional variables.
# Grammar of Graphics: 'geom_bar' for categorical comparison, reordered by descending emissions.
ggplot(data_1990, aes(x = reorder(Sector, -Emissions), y = Emissions/1000, fill = Sector)) +
  geom_bar(stat = "identity", width = 0.7) +  # Adjust bar width for clarity
  labs(title = "Total Emissions by Sector in 1990", x = "Sector", y = "Emissions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +  # Rotate x-axis labels
  scale_fill_viridis_d()  # Colorblind-friendly palette for bar plot

#-----------------------------------------------------

# Stacked Area Plot: Emissions by Sector Over Time
# Purpose: Visualizes each sector's share in total emissions over time.
# ASSERT: Reveal cumulative trends, Emphasize each sector’s impact over time.
# Grammar of Graphics: 'geom_area' for cumulative display, fill aesthetic for sectors.

ggplot(GHG_data_long, aes(x = year,
                          y = Emissions/1000, #Emissions in Million Tones 
                          fill = Sector)) +
  geom_area(position = "stack") +
  labs(title = "Emissions in Million Tonnes\nby Sector Over Time", 
       x = "Year",
       y = "Total Emissions") +
  theme_minimal() +
  scale_fill_viridis_d()  # Colorblind-friendly palette for stacked area plot
#-----------------------------------------------------
# Heatmap: Sectoral Emissions by Year
# Purpose: Highlights annual emissions for each sector.#
# ASSERT: Emphasize high and low emission years; simple heatmap layout.
# Grammar of Graphics: 'geom_tile' for heatmap, fill aesthetic representing emissions intensity.

ggplot(GHG_data_long, aes(x = year, y = Sector, fill = Emissions)) +
  geom_tile(color = "white") +  # Adding white borders for readability
  labs(title = "Sectoral Emissions by Year", x = "Year", y = "Sector") +
  scale_fill_viridis(name = "Emissions", option = "C") +  # Continuous viridis scale for gradient in heatmap
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability
#-----------------------------------------------------
GHG_data_long %>% 
  filter(year=="1990") %>% 
  ggplot(aes(x = reorder(Sector, -Emissions), y = Emissions, fill = Sector)) +
  geom_bar(stat = "identity", width = 0.7) +  # Adjust bar width for clarity
  labs(title = "Total Emissions by Sector in 1990", x = "Sector", y = "Emissions") +
  theme_minimal() +
  scale_y_log10()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +  # Rotate x-axis labels
  scale_fill_viridis_d()  # Colorblind-friendly palette for bar plot

#-----------------------------------------------------