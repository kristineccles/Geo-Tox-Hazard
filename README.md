## High throughput environmental chemical hazard mapping tool ##

This GitHub page contains the code and input data for the high throughput environmental chemical hazard mapping tool. Input data are obtained from the [US EPA National Air Toxics Assessment](https://www.epa.gov/national-air-toxics-assessment/2014-national-air-toxics-assessment) (NATA) and the [NTP Integrated Chemical Environment](https://ice.ntp.niehs.nih.gov) (ICE-TOX21). More information on the assays used can be found on the [US EPA CompTox Dashboard](https://comptox.epa.gov/dashboard)

This Shiny app complements the paper entitled "A geospatial modeling approach to quantifying the risk of exposure to environmental chemical mixtures via a common molecular initiating event". This paper demonstrates how environmental chemical concentrations of chemicals can be integrated with hazard data from TOX21 high throughput assays using a common molecular initiating event (the assay) to spatial patterns of risk from exposure to complex mixtures.


### Code ###
Key elements of the analysis code are as follows:

app.R - an R script used to render the Shiny app. This consists of the ui (user interface) and server code with several plotting functions required to render the Shiny app. 

GeoToxHazard-App folder - contains all of the files needed to run the shiny app

* nata_exposure.R - an R script that integrates and merges the data from NATA and ICE- TOX21 and formats the results for use within the Shiny environment. The raw input files are too large to add to GitHub but can be made available upon request. The output files are saved in the GeoToxHazard-App folder. Input data generated from this file include: 
  * assay_list.csv - a comma-separated values data sheet that lists the names of the assays generated from the ICE TOX21 data for input into the drop-down menu
  * nata_tox21_sp.rds - a (spatial) R data file that contains the data needed for the first map that integrates chemicals that a
  * localG_sp.rds - a (spatial) R data file that contains the results from the Cluster Analysis (the Getis and Ord's Gi* cluster). Correcting for >1000 multiple comparisons, only values are that are > 3.886 or < -3.886 are statistically significant. In the map, hot spots (clusters of high RQ values) are indicated in red, and cold spots (clusters of low RQ values) are indicated in blue
  * heatmap_df.csv a comma-separated values data sheet containing information on the chemical potency indicated by the Activity Concentration Cut Off (ACC) by assay and chemical. 
