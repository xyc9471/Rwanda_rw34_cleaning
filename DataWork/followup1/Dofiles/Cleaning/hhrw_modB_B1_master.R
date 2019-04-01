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


# PART 0: Clear boiler plate --------------------------------------------------

  rm(list=ls())

# PART 1: Setup work directory ------------------------------------------------

  if (Sys.getenv("USERNAME") %in% c("wb501238", "WB501238")) {
    
    projectFolder  <- "C:/Users/WB501238/Documents/GitHub/Rwanda_rw34_cleaning"
    
    }

  if (Sys.getenv("USERNAME") %in% c("wb546716", "WB546716")) {
    
    projectFolder  <- "C:/Users/WB546716/Documents/GitHub/Rwanda_rw34_cleaning"
    
    }


  projectFolder     <- file.path("C:/Users/WB546716/Documents/GitHub/Rwanda_rw34_cleaning")
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

# PART 4: The order of running the other r-scripts ----------------------------
  
  # 1) Running hhrw_modB_rmdups.R     # remove dups before any other steps
  # 2) Running hhrw_modB_reshape.R
  # 3) Running hhrw_modB1_rmdups.R
  # 4) Running hhrw_modB1_reshape.R

# PART 5: Merging the reshaped module b and b1 into one dataset ---------------
  
  modB_long  <- read_dta(file.path(InterData,
                                  "modB_long.dta"))
    
  modB1_long <- read_dta(file.path(InterData,
                                   "modB1_long.dta"))
  
  modB_B1_long <- merge(modB_long, modB1_long, by = c("id_05", "member_id"), all = T)   

  # Both modB and B1 has variable named "new_age", then the merged dataset will automatically
  # relabel them as new_age.x new_age.y, which cannot be exported into dta file
  # so it is necessary to rename them before exporting
  
  names(modB_B1_long)[names(modB_B1_long)=="new_age.x"] <- "new_age_b"
  names(modB_B1_long)[names(modB_B1_long)=="new_age.y"] <- "new_age_b1"
  
# PART 6: Export the reshaped and merged module b and b1 dataset --------------
  
  write_dta(modB_B1_long, file.path(InterData, "modB_B1_long.dta"))
    