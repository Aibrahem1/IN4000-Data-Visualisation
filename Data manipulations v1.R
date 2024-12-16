
#i chose a stacked area chart#
#to avoid over crowding the graph I dedcide to aggregate data
#I will present top 10 sectors & others
#Others is the sum of lowest 10 sectors

#-------------------------top10ghg data---------------------
#I have rerrange data to identify the top 10 sectors and the lowest 10
GHG_data_long %>% 
  arrange(year,desc(Emissions)) %>% # arrange by year asce and emissinons desc
  write.csv("top10ghg.csv", row.names = FALSE ) #save for exploration

#I conducted data exploration
GHG_data_long %>% 
  arrange(year,desc(Emissions)) %>% # arrange by year asce and emissinons desc
  view()

#using dplyr I filter out other sectors to aggregate
other_sectors_row <- GHG_data_long %>% #the dataset
  arrange(year,desc(Emissions)) %>% #additional and not really needed
  filter(Sector %in% c("Education", "Healthcare", "Hospitality", 
                       "Admin Services", "Professional", "Arts & Rec", 
                       "ICT", "Other Services", "Real Estate", 
                       "Finance", "Households")) %>% #filtered out sectors to add
  group_by(year, gas_type) %>% #grouping by year and gas_type
  summarize(
    Sector = "Other Sectors",
    Emissions = sum(Emissions, na.rm = TRUE), #sum emissions
    Total = first(Total),  # Assuming Total is consistent within each year
    Percentage = sum(Percentage, na.rm = TRUE)) %>% #sum percentage
  view()

#I filtered out the sectors which will be aggregated
GHG_filtered <- GHG_data_long %>%
  arrange(year,desc(Emissions)) %>% 
  filter(!Sector %in% c("Education", "Healthcare", "Hospitality", 
                        "Admin Services", "Professional", "Arts & Rec", 
                        "ICT", "Other Services", "Real Estate", 
                        "Finance", "Households")) %>% 
  view()
#combined the 2 data frames using rowbind

top10ghg<- 
  bind_rows(GHG_filtered, other_sectors_row) %>%
  arrange(year, desc(Emissions)) %>%
  view()
#### testing graph stacked area 
ggplot(top10ghg, aes(x = year,
                          y = Emissions/1000, #Emissions in Million Tones 
                          fill = Sector)) +
  geom_area(position = "stack") +
  labs(title = "Emissions in Million Tonnes\nby Sector Over Time", 
       x = "Year",
       y = "Total Emissions") +
  theme_minimal() +
  scale_fill_viridis_d()  # Colorblind-friendly palette for stacked area plot
  
            

#mutate(
    Sector = ifelse(row_number() %in% 11:21, "Others", Sector),  # Create a new sector label "Others" for rows 11 to 21
    Emissions = ifelse(Sector == "Others", 
                       sum(Emissions[row_number() %in% 11:21], na.rm = TRUE), 
                       Emissions),     # Aggregate Emissions for the "Others" sector across rows 11 to 21
    Percentage = ifelse(Sector == "Others", 
                        sum(Percentage[row_number() %in% 11:21], na.rm = TRUE), 
                        Percentage)) %>%      # Aggregate Percentage for the "Others" sector across rows 11 to 21
  distinct(Sector, .keep_all = TRUE) #removing duplidats


  scale_fill_viridis_d()