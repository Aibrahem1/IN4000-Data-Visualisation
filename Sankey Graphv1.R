library(networkD3)
library(ggalluvial)
library(htmlwidgets)
library(ggplot2)
library(plotly)


####### Don't Use this one#
nodes <- data.frame(name = c("Agriculture", 
                             "Mining", 
                             "Manufacturing", 
                             "Energy", 
                             "Water & Waste", 
                             "Construction", 
                             "Retail & Auto", 
                             "Transport", 
                             "Hospitality", 
                             "ICT",
                             "Finance",
                             "Real Estate",
                             "Professional",
                             "Admin Services",
                             "Public Admin",
                             "Education",
                             "Healthcare",
                             "Arts & Rec",
                             "Other Services",
                             "Households",
                             "Consumer Exp",
                             "Total"))
links <- data.frame(
  source = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21),
  target = c(22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 
             22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22),
  value = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 
            110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220)

  sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "TWh", fontSize = 12, nodeWidth = 30)


sector_percentage_of_total %>% 
  filter(year==2023 & gas_type=="Combined") %>% 
  view()

#### Use this one #######
library(networkD3)

# Nodes data frame remains the same
nodes <- data.frame(name = c("Agriculture 10%", 
                             "Mining  3%", 
                             "Manufacturing  15%", 
                             "Energy   14%", 
                             "Water & Waste  5%", 
                             "Construction  2%", 
                             "Retail & Auto  3%", 
                             "Transport  17%", 
                             "Hospitality  1%", 
                             "ICT  0.15%",
                             "Finance  0.05%",
                             "Real Estate  0.19%",
                             "Professional  0.31%",
                             "Admin Services  0.64%",
                             "Public Admin  0.90%",
                             "Education  0.51",
                             "Healthcare 1%",
                             "Arts & Rec  0.23",
                             "Other Services  0.18%",
                             "Households 0.02",
                             "Consumer Exp  24%",
                             "Total"))

# Create the links data frame
# Note: We connect each node (0 to 20) to the "Total" node, which is index 21
links <- data.frame(
  source = 0:20,  # Indices of all source nodes (from 0 to 20)
  target = rep(21, 21),  # Connect all sources to the "Total" node (index 21)
  value = c(10.10, 3.37, 15.10, 14.41, 
            5.11, 2.31, 2.64, 17.23, 0.85, 0.15,
            0.05, 0.19, 0.31, 0.64, 0.90, 
            0.51, 1.25, 0.23, 0.18, 0.02, 24.46)  # Values for each connection
)

# Create the Sankey diagram
sankeyNetwork(Links = links,
              Nodes = nodes, 
              Source = "source", 
              Target = "target",
              Value = "value", 
              NodeID = "name", 
              units = "%", 
              fontSize = 12, 
              nodeWidth = 50)


sankey<- sankeyNetwork(Links = links,
                       Nodes = nodes, 
                       Source = "source", 
                       Target = "target",
                       Value = "value", 
                       NodeID = "name", 
                       units = "%", 
                       fontSize = 12, 
                       nodeWidth = 30)
sankey

sankey_with_title <- htmlwidgets::prependContent(
  sankey,
  htmltools::tags$(
    "Gas Emission Distribution Across Sectors in 2023")
)

sankey_with_title

sankey_with_title_and_caption <- htmlwidgets::appendContent(
  sankey_with_title,
  htmltools::tags$p("Note: This diagram shows the contribution of various sectors to the total gas emissions in 2023, expressed as a percentage.")
)

sankey_with_title_and_caption

########## Alternative way

# Sample data for the alluvial plot
data_alluvial <- data.frame(
  source = c("Agriculture", "Mining", "Manufacturing", "Energy", "Water & Waste", 
             "Construction", "Retail & Auto", "Transport", "Hospitality", "ICT",
             "Finance", "Real Estate", "Professional", "Admin Services", "Public Admin",
             "Education", "Healthcare", "Arts & Rec", "Other Services", "Households", 
             "Consumer Exp"),
  target = rep("Total", 21),
  value = c(10.10, 3.37, 15.10, 14.41, 5.11, 2.31, 2.64, 17.23, 0.85, 0.15, 
            0.05, 0.19, 0.31, 0.64, 0.90, 0.51, 1.25, 0.23, 0.18, 0.02, 24.46))


ggplot(data_alluvial, aes(axis1 = source, axis2 = target, y = value)) +
  geom_alluvium(aes(fill = source), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  labs(title = "My Custom Sankey Diagram Title", y = "Value") +
  theme_minimal()
