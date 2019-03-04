#------------------------------------------------------------------------------ #
#                                                                               #
#                                    DIME                                       #
#                   Data Cleaning for Rwanda Household Info                     #
#         Removing Duplicated IDs and Correcting Var Names - Module B           #
#                                Yuchen Xiang                                   #
#                                                                               #
#------------------------------------------------------------------------------ #

# PART 1: Subsetting the data  --------------------------------------------------

  which(colnames(hh_rw34)=="pl_hhmembnumber") # 32, starting point of module B
  which(colnames(hh_rw34)=="old_membpresent") # 1031, ending point of module B
  hh_rw <- hh_rw34[c(7,32:1031)] # keeping only id_05 and mod_b 

# PART 2: Correcting Variable Names ---------------------------------------------
  
# 1) correct the names  ---------------------------------------------------------
  
  find_var(hh_rw, "v") # check for variables with the wrong names (starting with letter "v")
  
  df <- hh_rw[names(hh_rw)[substr(names(hh_rw),1,1) == "v"]] # subset the variables with the wrong names
  
  for (var in hh_rw[names(hh_rw)[substr(names(hh_rw),1,1) == "v"]]) {
    label <- str_replace(attr(var,"label"), "HH", "hh") # the right names are stored in attibutes
    list <- append(list, label) # extract the attribute labels in a list, then convert all HH to hh
  } 
  list[-1] # deleting the funtion element
  names(hh_rw)[substr(names(hh_rw),1,1) == "v"] <- as.character(list) # replace the old names with the correct names
  
  # double check if we successfully corrected variable names
  names(hh_rw)[substr(names(hh_rw),1,1) == "v"] # double check: character(0) means all names replaced
  names(hh_rw)[substr(names(hh_rw),1,1) == "H"] # double check: character(0) means HH is changed to hh
  
# PART 3: Removing Duplicated Household ID ("id_05")-----------------------------

# 1) Check for duplicates in "id_05"  -------------------------------------------

  duplicated(hh_rw) # returns 0 duplicated columns
  duplicated(hh_rw$id_05) # returns 2 duplicated rows in id_05
  n_occur <- data.frame(table(hh_rw$id_05))
  n_occur[n_occur$Freq > 1,] # locate the dups
  #    Var1 Freq
  #16  6021    2
  #157 6183    2

# 2) Remove the first column of dups of "id_05" ---------------------------------
  
  hh_rw <- hh_rw[!duplicated(hh_rw$id_05),]
  
  # check for obs, should be 2 obs fewer than original data set






