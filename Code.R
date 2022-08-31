# installing the required libraries 
install.packages("readxl") 
install.packages("tidyverse")


library(readxl)
library(tidyverse)

# specifying the path for file
path <- "E:\\TestQuestions_CCCM_IMO"

# set the working directory 
setwd(path)

# accessing all the sheets 
sheet = excel_sheets("Displacement_Data.xlsx")

# applying sheet names to dataframe names
data_frame = lapply(setNames(2019, 2020), 
                    function(x) read_excel("Displacement_Data.xlsx", sheet=1)

# attaching all dataframes together
data_frame = bind_rows(data_frame, .id="CurrentDistrict")

# printing data of all sheets
print (data_frame)
view(data_frame)