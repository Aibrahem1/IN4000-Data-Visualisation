#Trend of gas_emissions by type
emissions_data %>% #dataframe 
  filter(gas_type!="Combined") %>% 
  ggplot(aes(x=year, y=Total/1000, colour = gas_type))+
  geom_point(size=2, alpha=0.5)+
  geom_vline(xintercept = 2013, 
             lty="dashed")+
  geom_line(alpha=0.5,
            size=1,
            lty="solid")+
  geom_smooth(method = "lm", se=TRUE)+
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
                                  size = 25),
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
        plot.caption = element_text(hjust = 0, size = 14))+
  theme(
    panel.background = element_rect(fill = "grey100", 
                                    color = "black"),  # Plot area background
    plot.background = element_rect(fill = "snow2"),  # Entire plot background
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

#-------------BOXPLOT ----------------
#--(need to add the median value)

sector_data %>%
  filter(gas_type != "Combined") %>%         #filter out the combined GHG
  ggplot(aes(gas_type, emissions/1000)) +   # Main boxplot with adjusted outlier size and shape
  geom_boxplot(varwidth = TRUE, 
               aes(fill = gas_type), 
               outlier.size = 2, 
               outlier.shape = 8) + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +   # Logarithmic y-axis with better breaks and labels
  scale_fill_brewer(palette = "Set3") +   # Custom color palette
  ggtitle("Distribution of Greenhouse Gas Emissions by Type (1990–2023)") +   # Title and axis labels
  xlab("Gas Type") + 
  ylab("Emissions (log scale)") +
  stat_summary(fun = median, 
               geom = "point", 
               shape = 4, 
               size = 3, 
               color = "black") +   # Add statistical summary (median as a point)
  theme(legend.position = "none") +   # Remove legend or reposition it (choose one, remove legend shown here)
  # flip the coordinates for horizontal layout
  coord_flip()+
  geom_text(
    data = sector_data %>%
      filter(gas_type != "Combined") %>%
      group_by(gas_type) %>%
      summarize(median_value = median(emissions / 1000)),  # Calculate medians
    aes(x = gas_type, y = median_value, label = round(median_value, 2)),  # Position labels
    vjust = -0.5, 
    size = 3, 
    color = "black")

sector_data %>%
  filter(gas_type != "Combined") %>%
  group_by(gas_type) %>%
  summarize(median_value = median(emissions)) %>% 
  view()

emissions_data %>%
  group_by(gas_type) %>%
  summarize(
    total_emissions = median(Total, na.rm = TRUE))
#---------------------Box plot 2 with emissions data---------------------------------
#data to be ploted

emissions_data %>%
  filter(gas_type != "Combined") %>% 
  view()
#data to be used for geom text
emissions_data %>%
  filter(gas_type != "Combined") %>%
  group_by(gas_type) %>%
  summarize(median_value = mean(Total / 1000)) %>% 
  view()
#^^^^^^ the BOXPLOT 2
emissions_data %>%
  filter(gas_type != "Combined") %>%         #filter out the combined GHG
  ggplot(aes(gas_type, Total/1000)) +   # Main boxplot with adjusted outlier size and shape
  geom_boxplot(varwidth = TRUE, 
               aes(fill = gas_type), 
               outlier.size = 2, 
               outlier.shape = 8) + 
  labs(caption = "Data source: UK Office for National Statistics (ONS) - UK Environmental Accounts\nNote: Mean values shown above each box represent emissions in Million Tonnes (MT) over the years")+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +   # Logarithmic y-axis with better breaks and labels
  scale_fill_brewer(palette = "Set3") +   # Custom color palette
  ggtitle("Distribution of Greenhouse Gas Emissions by Type (1990–2023)") +   # Title and axis labels
  xlab("Gas Type") + 
  ylab("Emissions (log scale)") +
  stat_summary(fun = median, 
               geom = "point", 
               shape = 4, 
               size = 3, 
               color = "black") +   # Add statistical summary (median as a point)
  theme(legend.position = "none", # Remove legend or reposition it (choose one, remove legend shown here)
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 20,
                                  face = "bold"),
        plot.margin = margin(t = 1, r = 40, b = 1, l = 40),
        plot.caption = element_text(face = "bold")) +   
  coord_flip()+   # flip the coordinates for horizontal layout
  geom_text(data = emissions_data %>%
      filter(gas_type != "Combined") %>%
      group_by(gas_type) %>%
      summarize(median_value = mean(Total / 1000)),  # Calculate means
    aes(x = gas_type, y = median_value, 
        label = ifelse(gas_type == "NF3",  
        paste0(round(median_value, 4), " MT"),    # 4 decimal places for NF3
        paste0(round(median_value, 2), " MT"))),  # 2 decimal places for others
    vjust = -4, 
    size = 3, 
    color = "black")
# Specify the x and y coordinates for the note
  #annotate("text",x = 5, y = 100,
   #        label = "Note: NF3 emissions have 4 decimal places for clarity.",
   #        size = 3, color = "red", hjust = 2.9)

