#------------------------------------------------------------------------------ #
#                                                                               #
#                                    DIME                                       #
#                   Data Cleaning for Rwanda Household Info                     #
#         Module B1: Removing Duplicated IDs and Correcting Var Names           #
#                                Yuchen Xiang                                   #
#                                                                               #
#------------------------------------------------------------------------------ #

# PART 1: Subsetting the data  --------------------------------------------------

  which(colnames(hh_rw34)=="b1hh_14a") # 1033, starting point of module B1
  which(colnames(hh_rw34)=="hh_15a_check") # 1474, ending point of module B1
  hhrw_b1 <- hh_rw34[c(7,1044:1470)] # keeping only id_05 and module B1 

# PART 2: Correcting Names ------------------------------------------------------
  
# 1) subsetting the variables whose name needs to be corrected ------------------
  
  df <- hhrw_b1[names(hhrw_b1)[substr(names(hhrw_b1),1,1) == "v"]] # so we know how many names need to be corrected

# 2) extract attributes labels and replace the names ----------------------------
  
  for (var in hhrw_b1[names(hhrw_b1)[substr(names(hhrw_b1),1,1) == "v"]]) {
    label <- str_replace(attr(var,"label"), "B1HH", "b1hh") # the right names are stored in attibutes
    list <- append(list, label) # extract the attribute labels in a list, then convert all B to b, HH to hh
  } 
  
  list <- list[-1] # deleting the funtion element
                   # check if the number of elements in list matches the number of vars in df
                   # they should MATCH
  
  names(hhrw_b1)[substr(names(hhrw_b1),1,1) == "v"] <- as.character(list) # replace the old names with the correct names
  
  # double check if we successfully corrected variable names
  names(hhrw_b1)[substr(names(hhrw_b1),1,1) == "v"] # double check: character(0) means all names replaced

# PART 3: Remove Duplicated IDs -------------------------------------------------
  
# 1) Check for duplicates in "id_05"  -------------------------------------------
  
  duplicated(hhrw_b1) # returns 1 duplicated columns
  which(duplicated(hhrw_b1))
  duplicated(hh_rw$id_05) # returns 2 duplicated rows in id_05
  n_occur <- data.frame(table(hh_rw$id_05))
  n_occur[n_occur$Freq > 1,] # locate the dups
  #    Var1 Freq
  #16  6021    2
  #157 6183    2
  
  # 2) Remove the first column of dups of "id_05" ---------------------------------
  
  hh_rw <- hh_rw[!duplicated(hh_rw$id_05),]
  
  # check for obs, should be 2 obs fewer than original data set
  
  