
# About the project

This repository is part of the publication "TITLE" by Rentier et al. (2024) [link to publication] and contains the information needed to reproduce the analysis and visualisations of the research.
We kindly refer to the original paper for more information on the purpose of the scripts in this repository and a detailed description of the method.

### How to cite
Please cite Rentier et al. (2024) if you wish to (re)use any data or scripts from this repository. If you have any questions regarding this matter, feel free to contact us [link to contact details].
Recommended citation:

_(insert citation)_

# Getting started
### Prerequisites

```required_packages.R```
	Run this to get all packages needed to run the scripts
### Load data
To run the visualisations, all necessary data is stored in the ```Data``` folder. To load these, run:

```Load_data.r ```

### Visualisation
To reproduce the figures from the paper (and more!) run: 

```visualisation_temp.R ```
	for all visualisations regarding the temperature(difference)
 
```visualisation_treeline.R```
	for all visualisations regarding the treeline

### Other scripts
We have also included the scripts to run the significance tests :
```significance.R```

and a script to run some exploratory analysis to get a feel of the data:
```exploratory_data_analysis.R```
	


# Data
### Original data
Five different paleoclimate models form the basis of our analysis. Their raw data is needed to run the "treeline_extraction.ipynb". This data is not ours and can be downloaded on their respective websites:

- CHELSA-TRaCE21K
	- **Present**: CHELSA_TraCE21K_bio01_20_V1.0.tif
	- **LGM:** CHELSA_TraCE21K_bio01_-190_V1.0.tif
	- https://chelsa-climate.org/chelsa-trace21k/
- WorldClim 1
	- **Present**: ccmidbi_6ka_2.5.tif
	- **LGM**: cclgmbi_21ka_2.5.tif
	- https://www.worldclim.com/paleo-climate1
- Beyer et al.
	- **Present:** Late_Quaternary_Environment_-20.nc
	- **LGM**: Late_Quaternary_Environment_-21020.nc
	- https://springernature.figshare.com/articles/dataset/Metadata_record_for_High-resolution_terrestrial_climate_bioclimate_and_vegetation_for_the_last_120_000_years/12436484
- EcoClimate
	- **Present**: bio#CCSM_Modern(1950-1999)bio1.bil
	- **LGM**: bio#baseline_Modern(1950-1999)#CCSM_LGM (21ka)bio1.bil
	- https://www.dropbox.com/scl/fo/f51yih41uqtwqdvk9ln3g/AFEGNMU2fmt4n9eDVwEwXhg?rlkey=68pu5xu9mja968hu5h9peqzzy&e=1&dl=0
- PALEO-PGEM-series
	- **Present**: PALEO-PGEM-Series_bio1_mean_-20.nc
	- **LGM**: PALEO-PGEM-Series_bio1_mean_-21020 (21ka).nc
	- https://figshare.com/articles/dataset/PALEO-PGEM-Series_a_spatial_time_series_of_the_global_climate_over_the_last_5_million_years_Plio-Pleistocene_/20379663

In addition, you will need the following two datasets:
- GMBA mountain inventory 
	- v2.0, level 3
	- https://ilias.unibe.ch/goto_ilias3_unibe_cat_1000515.html
- GMTED global elevation data
	- 30 arcsec resolution
	- https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Grid_ZipFiles/mn30_grd.zip

### Derived/produced data
Data products from our analysis are stored in the Data folder. This data is ours and needs to be credited when used for other research:

- delta_t
  	- description
  
- delta_t_diff
  	- description
  
- mountains_xy
  	- description
  
- **TO ADD: suzettes biome**
  	- description
 
# Funding
This research has been developed for the [PPF-Alpine project](https://mountainsinmotion.w.uib.no/) and is funded by the [Trond Mohn Stiftelse](https://mohnfoundation.no/prosjekt/suzette-flantua/) and [University of Bergen](https://www.uib.no/en/bio)

# Contact
If you have any questions regarding the project, this repository or the publication, please do not hesitate to contact us! 
**E-mail:** eline.s.rentier [_at_] gmail.com


