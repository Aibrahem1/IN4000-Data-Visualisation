# Final Report Visualizations

![Visualization Overview](https://via.placeholder.com/1200x400?text=Final+Report+Visualizations)

This repository contains R scripts for generating various visualizations related to greenhouse gas emissions and consumer behavior in the UK. These scripts leverage data from the UK Office for National Statistics (ONS) to provide insights through multiple graphical representations.

## Visualizations

### 1. **Pie Chart**: Consumer Footprint by Gas Type Over Three Decades
![Pie Chart Preview](https://via.placeholder.com/800x400?text=Pie+Chart+Preview)
- **Description**: Displays consumer greenhouse gas (GHG) footprint by gas type for the years 1990, 2000, 2010, and 2020.
- **Dependencies**:
  - `tidyverse`
  - `paletteer`
- **Features**:
  - Polar bar chart with percentages labeled.
  - Faceted by year and gas type.
  - Styled for clarity and presentation.

### 2. **Heatmap**: Total Greenhouse Gas Emissions by Sector (1990–2023)
![Heatmap Preview](https://via.placeholder.com/800x400?text=Heatmap+Preview)
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
![Stacked Area Chart Preview](https://via.placeholder.com/800x400?text=Stacked+Area+Chart+Preview)
- **Description**: Tracks the emission trends of the highest emitting sectors over time.
- **Dependencies**:
  - `tidyverse`
  - `viridis`
- **Features**:
  - Highlights key historical events (e.g., Paris Agreement and COVID-19).
  - Emissions displayed in million tonnes.
  - Color-coded sectors for easy differentiation.

### 4. **Geospatial Heatmap**: CO2e Emissions by Region in the UK (2022)
![Geospatial Heatmap Preview](https://via.placeholder.com/800x400?text=Geospatial+Heatmap+Preview](https://github.com/Aibrahem1/IN4000-Data-Visualisation/blob/5ef858d20cf0819f929d9e3afee48132b161f859/Graphs/Geo-spatial%20heatmap%20by%20region%20-Co2e%20Emissions%20in%20the%20UK%202022.pdf)](https://github.com/Aibrahem1/IN4000-Data-Visualisation/blob/main/Graphs/Geo-spatial%20heatmap%20by%20region%20-Co2e%20Emissions%20in%20the%20UK%202022.pdf))
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
![Line Chart Preview](https://via.placeholder.com/800x400?text=Line+Chart+Preview)
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

## License
This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Acknowledgments
Special thanks to the UK ONS for providing comprehensive environmental accounts data.

---

_Visualizations are critical for understanding complex data trends. Explore the repository and share your feedback to improve the presentation and insights further!
