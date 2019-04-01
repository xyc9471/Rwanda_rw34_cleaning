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
  # To know what is the number of newly added member in this household
  # We can calculate it by: the total number of members (B) + member_id of the newly added member (B1)
  # For example, in module B we know family xxx has 5 members, and in module B1 we there is one
  # newly added member and his/her ID is 1, then we know he/she is the 6th member in this household
  
  # Step 1: Getting the current total number of members
  
    modb <- select(modb, c(id_05, member_id, pl_hhmembnumber))
  
  # Step 2: Merge it to mod b1
  
    new_modb1 <- merge(modb1, modb, by.x = c("id_05", "member_id"), all.x = TRUE)
  
  # Step 3: Generate member_id for newly added member

    new_modb1$member_id <- as.integer(new_modb1$member_id) # convert it to numeric
    new_modb1$pl_hhmembnumber <- as.integer(new_modb1$pl_hhmembnumber)

    new_modb1$member_id <- new_modb1$member_id + new_modb1$pl_hhmembnumber

    new_modb1 = subset(new_modb1, select = -c(pl_hhmembnumber)) # delete the pl_hhmembnumber

# PART 4: Export the merged module B and B1
    
    modB_B1_long <- merge(modB_long, new_modb1, by = c("id_05", "member_id"), all.x = TRUE)
