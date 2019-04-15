#------------------------------------------------------------------------------ #
#                                                                               #
#                                    DIME                                       #
#                   Data Cleaning for Rwanda Household Info                     #
#                     Merging B and B1 into one dataset                         #
#                                                                               #
#                                                                               #
#------------------------------------------------------------------------------ #

# PURPOSE:    Set-up configurations and run scripts that are used to clean data 
#             collected for Rwanda Irrigation Project
# NOTES:      

# WRITTEN BY: Yuchen Xiang

# PART 1: Load the data --------------------------------------------------------

  modb <- read_dta(file.path(InterData,
                             "modB_long.dta"))
  
  modb1 <- read_dta(file.path(InterData,
                              "modB1_long.dta"))
  
# PART 2: Removing B1 empty observations --------------------------------------

  modb1 <- as.data.frame(modb1)

  modb1 = modb1[!is.na(modb1$b1hh_14a),]

  modb1 = modb1[!is.na(modb1$b1hh_05), ]

# PART 3: Generate member_id for new members ---------------------------------
  
  # Module B has current total number of members
  # Module B1 has ID of newly added member
  # To avoid duplicates in ID variables, add ID +16
  
  # Step 1: Getting the new ID +16
  
    modb1$member_id <- modb1$member_id + 16
    
# PART 4: Append module B and B1 ---------------------------------------------
  
    modb[1:88] <- lapply(modb[1:88], as.numeric)
    modB_B1_long <- rbind.fill(modb, modb1)

# PART 5: Export the module B and B1 -----------------------------------------
    
    write_dta(modB_B1_long, file.path(InterData, "modB_B1_long.dta"))
    