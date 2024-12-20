
#i chose a stacked area chart#
#to avoid over crowding the graph I dedcide to aggregate data
#I will present top 10 sectors & others
#Others is the sum of lowest 10 sectors

#-------------------------Top10ghg data---------------------
#I have rerrange data to identify the top 10 sectors and the lowest 10
GHG_data_long %>% 
  arrange(year,desc(Emissions)) %>% # arrange by year ascending order and emissions descending order
  write.csv("top10ghg.csv", row.names = FALSE ) #save for exploration

#I conducted data exploration
GHG_data_long %>% 
  arrange(year,desc(Emissions)) %>% # arrange by year asce and emissions desc
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

#I filtered out other the aggregated sectors from the GHG_Data_long
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

#Removed the mutated data as it will not be using the visualisation or the analysis
rm(GHG_filtered)
rm(other_sectors_row)