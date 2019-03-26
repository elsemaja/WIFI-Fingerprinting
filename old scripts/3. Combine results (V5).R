###############################################################################
#                                                                             #
#   WIFI | CREATE TIBBLE/DATAFRAME WITH RESULTS | VERSION 5.0 | by ELSE       #
#                                                                             #
#   combine all the predicted values in one table or dataframe for comparison #
#                                                                             #
###############################################################################


# Load the resultsVSactuals
resultsFLOOR <- readRDS(file = "resultsFLOOR(V5).rds")
resultsBUILDING <- readRDS(file = "resultsBUILDING(V5).rds")
resultsLATITUDE <- readRDS(file = "resultsLATITUDE(V5).rds")
resultsLONGITUDE <- readRDS(file = "resultsLONGITUDE(V5).rds")



# combine the predicted results and the corresponding errors in a tibble or datafrme ---
resultsALL <- data.frame(resultsFLOOR,resultsBUILDING,resultsLATITUDE,resultsLONGITUDE)

# store the complete file
saveRDS(resultsALL, file = "resultsALL(5).rds")


