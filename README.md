# Final Report Visualizations

![Visualization Overview](https://github.com/Aibrahem1/IN4000-Data-Visualisation/blob/main/Graphs/IN4000%20Readme%20Overview.png)

This repository contains R scripts for generating various visualizations related to greenhouse gas emissions and consumer behavior in the UK. These scripts leverage data from the UK Office for National Statistics (ONS) to provide insights through multiple graphical representations.

## Visualizations

### 1. **Pie Chart**: Consumer Footprint by Gas Type Over Three Decades
![Pie Chart Preview](https://github.com/Aibrahem1/IN4000-Data-Visualisation/blob/main/Graphs/pie%20chart.png?raw=true)
- **Description**: Displays consumer greenhouse gas (GHG) footprint by gas type for the years 1990, 2000, 2010, and 2020.
- **Dependencies**:
  - `tidyverse`
  - `paletteer`
- **Features**:
  - Polar bar chart with percentages labeled.
  - Faceted by year and gas type.
  - Styled for clarity and presentation.

### 2. **Heatmap**: Total Greenhouse Gas Emissions by Sector (1990–2023)
![Heatmap Preview](https://github.com/Aibrahem1/IN4000-Data-Visualisation/blob/main/Graphs/heatmap.png)
- **Description**: A tile-based heatmap showing emission intensity across sectors over the years.
- **Dependencies**:
  - `viridis`
  - `dplyr`
  - `ggplot2`
  - `forcats`
- **Features**:
  - Color gradient representing emission levels.
  - Sector names reordered by emission levels.
  - Clear labeling and intuitive color scaling.

### 3. **Stacked Area Chart**: Top Emitting Sectors in the UK (1990–2023)
![Stacked Area Chart Preview](https://github.com/Aibrahem1/IN4000-Data-Visualisation/blob/main/Graphs/area%20chart.png)
- **Description**: Tracks the emission trends of the highest emitting sectors over time.
- **Dependencies**:
  - `tidyverse`
  - `viridis`
- **Features**:
  - Highlights key historical events (e.g., Paris Agreement and COVID-19).
  - Emissions displayed in million tonnes.
  - Color-coded sectors for easy differentiation.


### 4. **Geospatial Heatmap**: CO2e Emissions by Region in the UK (2022) 
![Geospatial Heatmap Preview](https://github.com/Aibrahem1/IN4000-Data-Visualisation/blob/main/Graphs/geospatial%20map.png)
- **Description**: A map visualizing CO2e emissions by region in the UK.
- **Dependencies**:
  - `tidyverse`
  - `sf`
  - `ggmap`
  - `tmap`
  - `rnaturalearth`
- **Features**:
  - Geospatial representation of emissions intensity.
  - Regional labels and dynamic coloring.
  - Minimalist styling for better focus on data.

### 5. **Line Chart**: Gas Types Emissions Trend (1990–2023)
![Line Chart Preview](https://github.com/Aibrahem1/IN4000-Data-Visualisation/blob/main/Graphs/line%20chart.png?raw=true)
- **Description**: Displays scaled trends for different gas types over time.
- **Dependencies**:
  - `tidyverse`
- **Features**:
  - Log-scaled y-axis for a clear view of emission changes.
  - Highlighted historical drops and events.
  - Smooth trendlines and annotations for significant points.

## Setup Instructions

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd <repository-folder>
   ```
2. Load the required datasets:
   - `vis_data10.RData`
   - `geospatial_data.RData`
3. Install the necessary R libraries:
   ```R
   install.packages(c("tidyverse", "paletteer", "viridis", "sf", "ggmap", "tmap", "rnaturalearth"))
   ```

## Data Sources
- UK Office for National Statistics (ONS) - UK Environmental Accounts.
- UK Greenhouse Gas Emissions: Local Authority and Regional data.

## Contribution
Feel free to contribute by:
- Improving scripts.
- Adding new visualizations.
- Providing feedback and suggestions.


## Acknowledgments
Special thanks to the UK ONS for providing comprehensive environmental accounts data.


---

Visualizations are critical for understanding complex data trends. Explore the repository and share your feedback to improve the presentation and insights further!
