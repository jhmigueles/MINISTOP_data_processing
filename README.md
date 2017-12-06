# MINISTOP_data_processing
Accelerometer data processing of the MINISTOP study by Jairo H. Migueles.

We used the functions implemented in the GGIR package (https://cran.r-project.org/web/packages/GGIR/) to process the data. Additionally, we used the AddCounts function to import the ActiGraph's activity counts to the GGIR functions (https://github.com/jhmigueles/MINISTOP_data_processing/blob/master/R/AddCounts.R) and modified the g.part5 function (https://github.com/jhmigueles/MINISTOP_data_processing/blob/master/R/AddCounts.R) for specific purposes of this study, i.e., to calculate  the within-participant SD and coefficient of variation of every metric.
