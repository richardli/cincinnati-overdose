# Suspected Heroin-related Overdose in Cincinnati
## Data

## Quick Start
For a quick replication of the main analysis, set working directory to the  `codes/` folder and run the following command in R. 

```
source('baseModel.R')
```

The working space is saved in the `fitted-model` folder, and a collection of figures in `figures` folder. Some maps are produced with multiple color schemes. Running the whole script takes less than 20 minutes on a 6-core Macbook Pro.

## Details of the R scripts

### Data processing
+ ``countPoint.R``: function for preprocessing geo-codede data
+ ``readCin-bg.R``: Read the data files and calculate covariates at the Block Group level.
+ ``readCin-sna.R``: Read the data files and calculate covariates at the Neighborhood level.

### Model fitting 
+ ``formulalist.R``: set up the formulas for model specification.
+ ``baseModel.R``: Model fitting.
+ ``vis-data.R``: Visualization of the data.
+ ``vis-fit.R``: Visualization of the fitted model.

### Additional scripts for the data repository
+ ``filterEMS.R``: process the raw spreadsheets from Cincinnati open data website and save smaller copies in the data repository.
+ ``get_park_bus.R``: process park and bus stop shapefiles.
+ ``get_zoning.R``: process zoning data
+ ``getBlockGEO.R``: process block group shapefiles and link to neighborhoods.

### Additional analysis script
These scripts will take a long time to run since they fit multiple models.
+ ``sensitivity-EMS.R``: sensitivity analysis comparing different EMS call inclusions.
+ ``sensitivity-blockgroup.R``: sensitivity analysis comparing different block group definitions.
+ ``compareModel.R``: sensitivity analysis comparing different models.

