# Mobility Choices Analyse
Analysation Application for Mobility Choices Project

## Requirements
- R
- RStudio

## Installation
- Open `/implementation/mcanalyse/mcanalyse.Rproj` in RStudio
- Install all packages listed in `global.R, updateMongoSchemaScript.R` and `cache.R` (`library(LIB_NAME)`) with the following command
```
install.packages(c("R.cache","dplyr","foreach","data.table","lubridate","sp", .......))
```

- Adapt URLs for Database in following files:
  * autoCacheReload.R
  * updateMongoSchemaScript.R
  * databaseconnector.R

- Adapt URL to your Backend in "/helper/rest.R"
