# RAVEN Technical Guide

## Modeling Framework Overview

The RAVEN project utilizes the Raven hydrological modeling framework, a flexible tool designed for water resource applications. This document provides technical details on the model implementation, parameters, and analytical approaches.

## Model Types

### Lumped Model

The lumped model treats the watershed as a single unit with uniform parameters:

- **Structure**: Single HRU (Hydrologic Response Unit)
- **Parameters**: Watershed-averaged values
- **Inputs**: Spatially averaged precipitation and temperature
- **Processes**: 
  - Snowmelt (Degree-day method)
  - Evapotranspiration (Penman-Monteith)
  - Infiltration (HBV)
  - Surface runoff (SCS-CN)
  - Baseflow (Linear reservoir)

### Semi-Distributed Model

The semi-distributed model divides the watershed into multiple sub-basins and HRUs:

- **Structure**: Multiple HRUs defined by:
  - Elevation bands
  - Land use classes
  - Soil types
- **Parameters**: Spatially variable parameters for each HRU
- **Inputs**: Distributed precipitation and temperature (IDW interpolation)
- **Processes**: Same as lumped model but applied to each HRU independently

## Data Sources and Processing

### Hydrometric Data

- **Source**: Environment Canada HYDAT database
- **Station**: 02GA010 (Nith River at New Hamburg)
- **Variables**: Daily discharge (m³/s)
- **Processing**: 
  - Quality control checks
  - Missing data assessment
  - Conversion to mm/day for model comparison

### Weather Data

- **Source**: Environment Canada weather stations
- **Variables**: Daily precipitation, min/max temperature
- **Processing**:
  - Quality control checks
  - Spatial interpolation for distributed model
  - Generation of potential evapotranspiration 

### Digital Elevation Model

- **Source**: SRTM 30m resolution
- **Processing**:
  - Void filling
  - Sink removal
  - Stream network delineation
  - Watershed boundary extraction
  - Elevation band classification

### Land Use and Soil Data

- **Sources**: National land cover database
- **Processing**:
  - Reclassification to model-specific classes
  - Extraction of parameters for each class
  - Overlay with watershed boundary

## Model Calibration

### Calibration Strategy

- **Period**: 2015-2017
- **Validation Period**: 2018-2020
- **Warm-up Period**: 1 year
- **Calibration Algorithm**: DDS (Dynamically Dimensioned Search)
- **Objective Function**: 
  - Primary: NSE (Nash-Sutcliffe Efficiency)
  - Secondary: Log-NSE and PBIAS
- **Parameters Calibrated**:
  - Soil moisture storage capacities
  - Infiltration parameters
  - Snow melt factors
  - Routing coefficients
  - Baseflow recession constants

### Performance Metrics

- **NSE**: Nash-Sutcliffe Efficiency
- **PBIAS**: Percent Bias
- **KGE**: Kling-Gupta Efficiency
- **RSR**: RMSE-observations standard deviation ratio
- **R²**: Coefficient of determination

## Flood Event Analysis

### Event Identification

- **Threshold Method**: Flow exceeding 95th percentile
- **Minimum Duration**: 2 days
- **Separation Criteria**: Flow must drop below threshold for at least 3 days

### Event Metrics

- **Peak Flow**: Magnitude and timing error
- **Event Volume**: Total flow volume during event
- **Event Duration**: Hours/days of high flow
- **Rise Time**: Time from event start to peak
- **Recession Characteristics**: Analyzed using recession curve analysis

## Model Output Files

### Hydrograph Files

- **Format**: CSV
- **Variables**: 
  - Date/time
  - Observed flow
  - Simulated flow
  - Flow components (surface runoff, baseflow, etc.)

### Performance Summary Files

- **Format**: CSV
- **Variables**:
  - Overall metrics (NSE, PBIAS, etc.)
  - Seasonal metrics
  - Flow percentile-based metrics
  - Event-specific metrics

### Diagnostic Output

- **Water Balance Components**: 
  - Precipitation
  - Evapotranspiration
  - Runoff components
  - Storage changes
- **State Variables**:
  - Soil moisture
  - Snowpack
  - Groundwater storage

## Running Advanced Analyses

### Sensitivity Analysis

```r
# Example code for sensitivity analysis
run_sensitivity_analysis(
  parameters = c("SOIL_DEPTH", "BASEFLOW_COEFF", "MELT_FACTOR"),
  ranges = list(c(100, 2000), c(0.01, 0.5), c(2, 8)),
  samples = 100,
  output_metric = "NSE"
)
```

### Uncertainty Analysis

```r
# Example code for Monte Carlo uncertainty analysis
run_uncertainty_analysis(
  iterations = 1000,
  confidence_level = 0.95,
  output_dir = OUTPUT_RESULTS_DIR
)
```

### Climate Change Scenario Analysis

```r
# Example code for climate scenario analysis
run_climate_scenarios(
  temperature_deltas = c(1, 2, 3, 4),  # °C
  precipitation_factors = c(0.9, 1.0, 1.1, 1.2),  # Multipliers
  output_dir = file.path(OUTPUT_DIR, "climate_scenarios")
)
```

## Reference Parameter Values

| Parameter | Description | Typical Range | Units |
|-----------|-------------|--------------|-------|
| SOL_DEPTH | Soil depth | 500-2000 | mm |
| SOL_AWC | Available water capacity | 0.05-0.25 | mm/mm |
| CN2 | SCS curve number | 40-95 | - |
| KSAT | Saturated hydraulic conductivity | 0.1-150 | mm/hr |
| ALPHA_BF | Baseflow alpha factor | 0.01-1.0 | 1/day |
| SMFMX | Snow melt factor, maximum | 2.0-8.0 | mm/°C/day |
| SMFMN | Snow melt factor, minimum | 1.0-6.0 | mm/°C/day |
| SURLAG | Surface runoff lag coefficient | 0.5-24 | hr |
| ESCO | Soil evaporation compensation factor | 0.1-1.0 | - |
| EPCO | Plant uptake compensation factor | 0.1-1.0 | - |

---

Last Updated: June 7, 2025
