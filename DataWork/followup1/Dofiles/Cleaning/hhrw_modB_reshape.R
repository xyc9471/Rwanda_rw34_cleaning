#------------------------------------------------------------------------------ #
#                                                                               #
#                                    DIME                                       #
#                   Data Cleaning for Rwanda Household Info                     #
#                     Reshaping to long format - Module B                       #
#                                Yuchen Xiang                                   #
#                                                                               #
#------------------------------------------------------------------------------ #

# PART 1: Sorting data and extract variables by group  -------------------------------

# 1) Sorting the table ---------------------------------------------------------------
  
  hhrw=hh_rw[4:1001] # subset anything else EXCEPT id_05 and variables DO NOT needed to be reshaped
  hhrw_ordered <- hhrw[,mixedsort(colnames(hhrw))] # sort variables needed to be reshaped in alphabetical and ascending order
  hhrw_ordered=cbind(hh_rw[1:3],hhrw_ordered) # combine the sorted table with id_05 and variables DO NOT needed to be reshaped

# 2) Extracting the variables under each group ---------------------------------------
  
# Step 1: deleting the last integer indicating the family member, left with, for example, hh_07_ or hh_07
  colnames(hhrw_ordered)[4:1001] = substr(colnames(hhrw_ordered)[4:1001], 1, 
                                          nchar(colnames(hhrw_ordered)[4:1001])-2)

# Step 2: deleting the last underscore ("_")

  for (i in 1:1001) {
    x=colnames(hhrw_ordered)[i]
    if (substring(x,nchar(x))=="_") {
      colnames(hhrw_ordered)[i] = substr(colnames(hhrw_ordered)[i], 1, 
                                         nchar(colnames(hhrw_ordered)[i])-1)
      }
    } # returns variable names as dups of hh_07 / age / etc....
      # this is for extracting the variables under each group
  
# Step 3: extracting all names into a list 
  varnames = colnames(hhrw_ordered) # extracting the names into a list
  varnames <- varnames[duplicated(varnames)] # keeps the vars needed to be reshaped
  varnames <- varnames[!duplicated(varnames)] # removes all dups, which left with each group name

# PART 2: reshape the data into long format  -----------------------------------------
  
# 1) reshape the first variable, which returns a table with "id_05", "member_id", first_var -----
  
  subdf=hhrw_ordered[,which(colnames(hhrw_ordered)==varnames[1])] 
  subdf=cbind(hhrw_ordered[,1],subdf)
  colnames(subdf)[1]="id_05"
  colnames(subdf)[2:length(colnames(subdf))]=1:(length(colnames(subdf))-1)
  # extracting every variable from ordered data set that has the same name as the first name in varnames list
  # subset id_05 and all variables under first group
  # rename those variables with member number 1:12
  
  modB_long <- reshape(data = subdf, 
                      idvar = "id_05", 
                      varying = list(c(2:length(colnames(subdf)))), 
                      direction="long", 
                      v.names = c(varnames[1]),
                      timevar = "member_id",
                      sep="_") 
  # reshape the subsetted data with id_05 and variables in first group
  # returns a long-format data frame with id_05, member_id, first_group
  # We did this because empty data frame cannot be merged, so it is necessary to have a non-empty frame first

# 2) reshape the rest variables into long format, and merge into the data frame above  ----------
  
  for (var in 2:length(varnames)) {   # repeat the steps in PART 3, 1), but this time in a loop
    subdf=hhrw_ordered[,which(colnames(hhrw_ordered)==varnames[var])]
    subdf=cbind(hhrw_ordered[,1],subdf)
    colnames(subdf)[1]="id_05"
    colnames(subdf)[2:length(colnames(subdf))]=1:(length(colnames(subdf))-1)
  
    modB_long1 <- reshape(data = subdf, 
                          idvar = "id_05", 
                          varying = list(c(2:length(colnames(subdf)))), 
                          direction="long", 
                          v.names = c(varnames[var]),
                          timevar = "member_id",
                          sep="_") # for variables under each group, 
                                   # we reshape once, and name it as hhrw_long1
    
    modB_long <- merge(modB_long, modB_long1, by = c("id_05", "member_id"), all = T)
    } # then we merge the reshaped data frame to hhrw_long by "id_05" and "member_id"  
      # returns a data frame with "id_05", "member_id", all other variables reshaped into long format

# PART 3: merging other variables that do not need to be reshaped -------------------------------
  
  # Since there are variables like "pl_hhmembnumber", "hh_rosterold_count", "b1hh_14", "old_membpresent",
  # they are household level information and do not need to be reshaped into long format
  # so we can just merge them back to the reshaped data file by "id_05"
  
# 1) Subset those variables with "id_05" --------------------------------------------------------
  
  hhrw_unique <- select(hh_rw34, c("id_05", 
                                   "pl_hhmembnumber", 
                                   "hh_rosterold_count", 
                                   "b1hh_14", 
                                   "old_membpresent"))
  
# 2) Merge to the long-format data frame hhrw_long ----------------------------------------------
  
  modB_long <- merge(modB_long, hhrw_unique, by = c("id_05"), all = T) 
  # final long-format data frame of orginal data set - module B

  
# PART 4: removing empty rows from 13:16 --------------------------------------------------------
  
  modB_long = modB_long[!modB_long$member_id > 12,]
  
# PART 5: export the long-format data file ------------------------------------------------------
  
  write_dta(modB_long, file.path(InterData, "modB_long.dta"))
  
  