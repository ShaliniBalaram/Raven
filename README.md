# RAVEN Flood Modeling Project

## Lumped vs. Semi-Distributed Hydrological Models for Flood Prediction

This project compares lumped and semi-distributed hydrological models for flood prediction on the Nith River watershed using the Raven modeling framework. The analysis includes comprehensive data acquisition, model setup, calibration, validation, and flood event analysis.

## Project Structure

The project follows an organized directory structure:

```
/
├── scripts/            # R analysis and modeling scripts
│   └── main.R         # Main script controlling workflow
├── data/               # Raw data and temporary files
├── models/             # Model configuration files
│   ├── input_files/   # Parameters and calibration files
│   ├── lumped/        # Lumped model files
│   └── distributed/   # Semi-distributed model files
├── outputs/            # All generated outputs  
│   ├── data/          # Processed data (CSV files)
│   ├── plots/         # Generated visualizations
│   ├── models/        # Model input summaries
│   ├── results/       # Model run results & metrics
│   ├── flood_analysis/ # Flood event analysis outputs
│   └── presentations/ # Presentation materials
└── documentation/      # Project documentation
```

## Configuration and Inputs

### Configuring Inputs

All inputs are configured in the `PROJECT_CONFIG` section of `main.R`:

```r
PROJECT_CONFIG <- list(
  STATION_ID = "02GA010",           # Hydrometric station ID
  START_DATE = "2015-01-01",       # Analysis start date
  END_DATE = "2020-12-31",         # Analysis end date
  WATERSHED_LAT = 43.38,           # Watershed latitude
  WATERSHED_LON = -80.65,          # Watershed longitude
  SEARCH_RADIUS = 50,              # Weather station search radius (km)
  WATERSHED_AREA = 1000.0,         # Watershed area (km²)
  WATERSHED_NAME = "Nith River",   # Watershed name
  OUTLET_STATION = "02GA010",      # Outlet station ID
  TARGET_NSE = 0.6,                # Target Nash-Sutcliffe Efficiency
  TARGET_BIAS = 10.0,              # Target bias percentage
  FLOOD_PERCENTILE = 0.95,         # Percentile for identifying flood events
  MIN_EVENT_DURATION = 2,          # Minimum flood event duration (days)
  EVENT_SEPARATION_DAYS = 3        # Days between separate flood events
)
```

Modify these parameters to analyze different watersheds, time periods, or adjust flood identification thresholds.

## Setup and Installation

1. **Clone or download** this repository

2. **Setup the environment**: Source the main script in R to set up all required directories and packages:

   ```r
   source("scripts/main.R")
   ```

3. **Install required packages**: Run the setup functions to install and configure dependencies:

   ```r
   setup_packages()
   setup_hydat_database()
   setup_raven_executable()
   ```
   
   The first run will take some time as it downloads and configures the HYDAT database.

## Running the Analysis

### Full Workflow

To run the complete analysis workflow:

```r
source("scripts/main.R")
main()
```

This will execute all steps:
1. Setup directories and packages
2. Download hydrometric and weather data
3. Process watershed characteristics
4. Configure and run lumped and distributed models
5. Analyze model performance and flood events
6. Generate comparison plots and reports

### Individual Components

You can run specific components of the workflow:

```r
source("scripts/main.R")

# Data setup only
setup_project_data()

# Run only the flood event analysis
run_flood_analysis_only()

# Create presentation materials
create_presentation_materials()
```

## Generated Outputs

### Data Outputs (`/outputs/data/`)
- Flow and weather time series data (CSV)
- Station metadata
- Statistical summaries

### Visualizations (`/outputs/plots/`)
- Hydrograph time series
- Temperature and precipitation plots
- Flow duration curves

### Model Results (`/outputs/results/`)
- Calibrated model outputs
- Performance metrics
- Hydrographs for both model types

### Flood Analysis (`/outputs/flood_analysis/`)
- Identified flood events
- Event-specific performance metrics
- Model comparison statistics
- Flood visualization plots

## Required Packages

### Core Packages (Required)
- **tidyhydat** v0.5.5+ - Hydrometric data access
- **weathercan** v0.6.2+ - Weather data access
- **dplyr** v1.0.0+ - Data manipulation
- **ggplot2** v3.3.0+ - Visualization
- **sf** v1.0.0+ - Spatial data handling
- **stringr** v1.4.0+ - String manipulation

### Optional Packages (Enhanced Features)
- **RavenR** v2.1.4+ - Raven model interface
- **terra** v1.5.0+ - Spatial data processing
- **elevatr** v0.4.2+ - Elevation data access
- **plotly** v4.10.0+ - Interactive visualizations
- **rmarkdown** v2.14+ - Documentation generation

## Data Sources

- **Hydrometric data**: Environment Canada HYDAT database, accessed via tidyhydat
- **Weather data**: Environment Canada weather stations, accessed via weathercan
- **Elevation data**: SRTM via elevatr package (optional)

## Troubleshooting

If you encounter issues:

1. **Missing packages**: Run `setup_packages()` to install all dependencies

2. **Data download errors**: Check internet connection and try:
   ```r
   setup_hydat_database(force = TRUE)  # Force re-download HYDAT database
   ```

3. **Raven executable errors**: Verify installation with:
   ```r
   setup_raven_executable()
   ```

4. **Missing output directories**: Run:
   ```r
   setup_directories()
   ```

## Author and Contact Information

**Shalini Balaram**  
Indian Institute of Technology, Madras  
Email: shalinib0204@gmail.com

For questions, suggestions, or collaboration opportunities related to this project, please contact the author directly via email.


