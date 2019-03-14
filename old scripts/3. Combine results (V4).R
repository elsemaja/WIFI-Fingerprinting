###############################################################################
#                                                                             #
#   WIFI | CREATE TIBBLE/DATAFRAME WITH RESULTS | VERSION 3.0 | by ELSE       #
#                                                                             #
#   combine all the predicted values in one table or dataframe for comparison #
#                                                                             #
###############################################################################


# Load the resultsVSactuals
resultsFLOOR <- readRDS(file = "resultsFLOOR(V4).rds")
resultsBUILDING <- readRDS(file = "resultsBUILDING(V4).rds")
resultsLATITUDE <- readRDS(file = "resultsLATITUDE(V4).rds")
resultsLONGITUDE <- readRDS(file = "resultsLONGITUDE(V4).rds")



# combine the predicted results and the corresponding errors in a tibble or datafrme ---
resultsALL <- data.frame(resultsFLOOR,resultsBUILDING,resultsLATITUDE,resultsLONGITUDE)

# store the complete file
saveRDS(resultsALL, file = "resultsALL(4).rds")


