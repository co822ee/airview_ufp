# airview_ufp

Version 0.1.0

Building models for UFP and/or BC in EXPANSE. 


## Project organization

```
.
├── .gitignore
├── CITATION.md
├── LICENSE.md
├── README.md
├── config             <- Configuration files (HW)
├── data               <- All project data, ignored by git
│   ├── processed      <- The final, canonical data sets for modeling. (PG)
│   ├── raw            <- The original, immutable data dump. (RO)
│   └── temp           <- Intermediate data that has been transformed. (PG)
├── results
│   ├── figures        <- Figures for the manuscript or reports (PG)
│   └── output         <- Other output for the manuscript or reports (PG)
└── src                <- Source code for this project (HW)

```

## src
### Pipeline for creating other city-wide data (v2)
Based upon the previous data processing step for Basel, this pipeline was built for processing raw mobile data points (to convert data points to 60m road segments by snapping the data points to the nearest road segment). Detailed steps can be found in the description in the previous section.

`process_pipeline.R`: to process the raw ts file to mean of mean 
raw ts snapped to closest road segments:

```
in_dir <- "data/processed/"
fnames <- list.files(in_dir, '_rawPtsLines_v3.gpkg')
```

`src/00_process/01_process_predictors.R`: to process the predictor variables' values (extracted and downloaded from GEE) for all cities 

`src/00_process/01_eda_stat_meanOfmean.R`: to combinne the predictor vairables and means of means for all cities

* output:  `data/temp/stat_new/meanOfmean_predictors_cities.Rdata` -> This output is being used for developing LUR models, further processed by a function script `src/01_LUR/fun_read_mobileData.R`.

`src/00_process/02_process_stat.R`: to generate some statistics of the mobile data (using the mean of means)

* output:  `data/temp/stat_new/campaign_stat_cities.Rdata`
* output:  `data/temp/stat_new/campaign_stat_cities2.Rdata`
* output:  `data/temp/stat_new/campaignRoadInfo_cities.Rdata`

`src/00_process/03_process_stat.R`: to process the mean of means and predictor variables for all cities 

`src/deconvolution/00_test_deconv.R`: to generate the deconvolution data for the mobile data (from the 1-s data)

* output: `data/processed/deconv[city].Rdata1`
* output: `results/output/deconv_cor_[city].csv`


#### Mobile data

 filename | description  | R script creating the file
 --- | --- | ---
 data/processed/final_mobileData/[city]_stat2_v3.rds    |  line segments with mean of mean | process_pipeline.R, fun_04_prodStat.R
 data/temp/stat_new/meanOfmean_predictors_cities.Rdata  | datafram with mean of mean and predictor variables | 01_eda_stat_meanOfmean.R
 newStat_pathfname <- "data/temp/stat_new/campaign_stat_cities.Rdata" | dataframe with mean (on driving days and road segment UIDs) with all cities included and information of campaigns available | 01_process_stat.R
 campaignRoadInfo_pathfname <- "data/temp/stat_new/campaignRoadInfo_cities.Rdata"  | dataframe with information on whether a road segment in a city has mobile data measured in both campaigns or either of the campaign | 01_process_stat.R

#### Predictor variables
```
  predictor_pathfname <- paste0('data/temp/road_centroidPts/ptsPredictors_', city, '.Rdata')  
  # ==> output predictor variables (cleaned)
  spatialPts_pathfname <- paste0('data/temp/road_centroidPts/ptsUID_', city, '.Rdata')
  # ==> output centroid points (sf dataframe) with UID and city available 
  spatialLines_pathfname <- paste0('data/temp/road_centroidPts/linesUID_', city, '.Rdata')
  # ==> output line segment (sf dataframe) with UID and city available
  
```

`01_process_stat.R`
`data/temp/stat_new/campaign_stat_cities.Rdata`: contains the averages on driving days and road segments from all cities.

`01_eda_stat_meanOfmean.R`
`data/temp/stat_new/meanOfmean_predictors_cities.Rdata`: contains all the mean of mean and predictor variables 

### Visualization

`vis_mobileData.R`: Provides visualizations specific to mobile data analysis, including trends and statistical summaries across different cities.

`vis_paper_3-3_modelPerformance_externalValidation.R`: Generates visualizations for model performance and external validation, focusing on geographical and statistical data representations across various international datasets.

## External evaluation

`vis_external_validation_new.r`: Visualizes external validation results, including scatter plots and error matrices for different models and locations.

`vis_paper_3-3_modelPerformance_externalValidation.R`: This script is responsible for generating comprehensive visualizations that assess the measurements from the external evaluation data. It focuses on presenting both geographical and statistical data representations, which are crucial for understanding model behavior across different international datasets (e.g. Switzerland, Rome, Amsterdam, Augsburg, Claire). The code for vusalizing the results was outdated (which were commented out). The updated code for creating visual for external validation can be found in the vis_external_validation_new.r. 


### src/01_LUR
`03_LUR_5fold_v1.r`: 5-fold CV using different subset of training and validation (city-specific, pooled, pooled deconv)

`01_process_predictors.R`: Processes predictor data files from GEE for various cities.

`03_LUR_5fold_v1.r`: Executes a 5-fold cross-validation strategy to evaluate LUR models, accommodating different subsets of training and validation data, including city-specific, pooled, and pooled deconvolution scenarios.

`03_LUR_city.R`: Analyzes and models local urban regression (LUR) specifically tailored to individual city datasets.

`03_LUR_global_LOAOCV_v1.R`: Implements a global Leave-One-Area-Out Cross-Validation (LOAOCV) strategy for LUR models to assess model robustness across different geographic areas.

`04_LUR_global-model_performance.R`: Evaluates the performance of global LUR models, providing insights into their effectiveness and accuracy across multiple regions.


### src/02_vis

`eda_ts_decomposition.R`: process and explore making the model temporally detrended.

## License

This project is licensed under the terms of the [GNU License](/LICENSE.md)

## Citation

Please [cite this project as described here](/CITATION.md).
