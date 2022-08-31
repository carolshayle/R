#all underscores within the folder name and data names were removed before running the code
#remove all spaces between the column names i.e  Current Region was converted to CurrentRegion and Previous Region to PreviousRegion

# installing the required libraries 
install.packages("readxl") 
install.packages("tidyverse")
install.packages("writexl")
install.packages("dplyr")
install.packages("readr")
install.packages("lubridate") 

#Combining the three sheets:2019,2020,2021
library(readxl)
library(tidyverse)
path <- "E:\\TestQuestions_CCCM_IMO"
setwd(path)
fn.import<- function(x) read_excel("Displacement_Data.xlsx", sheet=x)
sheet = excel_sheets("Displacement_Data.xlsx")
data_frame = lapply(setNames(sheet, sheet),  fn.import )
data_frame = bind_rows(data_frame, .id="Sheet")
print (data_frame)
view(data_frame)

#saving the merged document to a local directly
library("writexl")
write_xlsx(data_frame,"E:\\TestQuestions_CCCM_IMO\\DisplacedMerged.xlsx")
           
# reading individual files and merging the DisplacedMerged file with the RegionList file into a ConsolidatedFile
library(dplyr)
library(readr)

DisplacementMerged <- read_excel("DisplacedMerged.xlsx",sheet = 1)
RegionList <- read_excel("RegionList.xlsx",sheet = 1)
view(DisplacementMerged)
view(RegionList)

joined_tibble <- left_join(DisplacementMerged, RegionList, 
                           by = c("CurrentRegion" = "Code"))
glimpse(joined_tibble)

#exporting the consolidated file to a local directory
library("writexl")
write_xlsx(joined_tibble,"E:\\TestQuestions_CCCM_IMO\\ConsolidatedSheet.xlsx")

#extracting year and month
library(lubridate)
Year<-year(joined_tibble$Arrival) # Only years are extracted

Month<-month(joined_tibble$Arrival) # Only months are extracted
view(Year)
view(Month)

#append Year column to Consolidated Sheet to create new consolidated sheet
joined_tibble_Year <- cbind(joined_tibble,Year)
view(joined_tibble_Year)

#append month column to the new consolidated sheet 
joined_tibble_Month <- cbind(joined_tibble_Year,Month)
view(joined_tibble_Month)

#saving the extracted month and year to file
library("writexl")
write_xlsx(joined_tibble_Month,"E:\\TestQuestions_CCCM_IMO\\Extracted_Month_Year.xlsx")
