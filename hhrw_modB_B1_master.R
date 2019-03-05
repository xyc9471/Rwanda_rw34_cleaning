#------------------------------------------------------------------------------ #
#                                                                               #
#                                    DIME                                       #
#                   Data Cleaning for Rwanda Household Info                     #
#                             for Module B and B1                               #
#                                MASTER SCRIPT                                  #
#                                                                               #
#                                                                               #
#------------------------------------------------------------------------------ #

# PURPOSE:    Set-up configurations and run scripts that are used to clean data 
#             collected for Rwanda Irrigation Project
# NOTES:      

# WRITTEN BY: Yuchen Xiang

#                                                     Last modified in Nov 2018

# PART 0: Clear boiler plate --------------------------------------------------

  rm(list=ls())

# PART 1: Setup work directory ------------------------------------------------

  if (Sys.getenv("USERNAME") %in% c("wb501238", "WB501238")) {
    
    projectFolder  <- "C:/Users/WB501238/Documents/GitHub/Rwanda_rw34_cleaning"
    
  }

  if (Sys.getenv("USERNAME") %in% c("wb546716", "WB546716")) {
    
    projectFolder  <- "C:/Users/WB546716/Documents/GitHub/Rwanda_rw34_cleaning"
    
  }

  dataWorkFolder    <- file.path(projectFolder, "DataWork")
  followupFolder    <- file.path(dataWorkFolder, "followup1")
  Data              <- file.path(followupFolder, "DataSets")
  DeidentifiedData  <- file.path(Data, "Deidentified")
  InterData         <- file.path(Data, "Intermediate")

# PART 2: Load packages -------------------------------------------------------
  
  packages <- c("haven",
                "sjmisc",
                "stringr",
                "reshape",
                "reshape2",
                "data.table",
                "gtools",
                "dplyr")

  sapply(packages, function(x) {
    print(x)
      if (x %in% installed.packages() == FALSE) {
        install.packages(x, dependencies = TRUE) 
      }
      library(x, 
              character.only = T)
    }
  )
  
# PART 3: Load data files -----------------------------------------------------

  hh_rw34 <- read_dta(file.path(DeidentifiedData,
                                "fup1_rw34_incoming_data_toclean.dta"))
  
  