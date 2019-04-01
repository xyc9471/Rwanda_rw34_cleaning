#------------------------------------------------------------------------------ #
#                                                                               #
#                                    DIME                                       #
#                   Data Cleaning for Rwanda Household Info                     #
#                     Module B1: Reshaping to long format                       #
#                                Yuchen Xiang                                   #
#                                                                               #
#------------------------------------------------------------------------------ #

# PART 1: Sorting data and extract variables by group  -------------------------------

# 1) Sorting the table ---------------------------------------------------------------
  hhrwb1=hhrw_b1[4:435] # subset anything else EXCEPT id_05 and variables DO NOT needed to be reshaped
  hhrwb1_exp = hhrw_b1[,c(1:3,436:443)] # subset id_05 and variables DO NOT needed to be reshaped
  hhrwb1_ordered <- hhrwb1[,mixedsort(colnames(hhrwb1))] # sort variables needed to be reshaped in alphabetical and ascending order
  hhrwb1_ordered=cbind(hhrwb1_exp[1:11],hhrwb1_ordered) # combine the sorted table with id_05 and variables DO NOT needed to be reshaped

# 2) Extracting the variables under each group ---------------------------------------
  
# Step 1: deleting the last integer indicating the family member, left with, for example, hh_07_ or hh_07
  colnames(hhrwb1_ordered)[12:443] = substr(colnames(hhrwb1_ordered)[12:443], 1, 
                                          nchar(colnames(hhrwb1_ordered)[12:443])-2)
  
# Step 2: deleting the last underscore ("_")
  
  for (i in 1:443) {
    x=colnames(hhrwb1_ordered)[i]
    if (substring(x,nchar(x))=="_") {
      colnames(hhrwb1_ordered)[i] = substr(colnames(hhrwb1_ordered)[i], 1, 
                                         nchar(colnames(hhrwb1_ordered)[i])-1)
    }
  } # returns variable names as dups of hh_07 / age / etc....
  # this is for extracting the variables under each group
  
# Step 3: extracting all names into a list 
  varnames = colnames(hhrwb1_ordered) # extracting the names into a list
  varnames <- varnames[duplicated(varnames)] # keeps the vars needed to be reshaped
  varnames <- varnames[!duplicated(varnames)] # removes all dups, which left with each group name
  
# PART 2: reshape the data into long format  -----------------------------------------

# 1) reshape the first variable, which returns a table with "id_05", "member_id", first_var -----
  
  subdf=hhrwb1_ordered[,which(colnames(hhrwb1_ordered)==varnames[1])] 
  subdf=cbind(hhrwb1_ordered[,1],subdf)
  colnames(subdf)[1]="id_05"
  colnames(subdf)[2:length(colnames(subdf))]=1:(length(colnames(subdf))-1)
  # extracting every variable from ordered data set that has the same name as the first name in varnames list
  # subset id_05 and all variables under first group
  # rename those variables with member number 1:12
  
  modB1_long <- reshape(data = subdf, 
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
    subdf=hhrwb1_ordered[,which(colnames(hhrwb1_ordered)==varnames[var])]
    subdf=cbind(hhrwb1_ordered[,1],subdf)
    colnames(subdf)[1]="id_05"
    colnames(subdf)[2:length(colnames(subdf))]=1:(length(colnames(subdf))-1)
    
    modB1_long1 <- reshape(data = subdf, 
                          idvar = "id_05", 
                          varying = list(c(2:length(colnames(subdf)))), 
                          direction="long", 
                          v.names = c(varnames[var]),
                          timevar = "member_id",
                          sep="_") # for variables under each group, 
    # we reshape once, and name it as hhrw_long1
    
    modB1_long <- merge(modB1_long, modB1_long1, by = c("id_05", "member_id"), all = T)
  } # then we merge the reshaped data frame to hhrw_long by "id_05" and "member_id"  
  # returns a data frame with "id_05", "member_id", all other variables reshaped into long format

# PART 3: merging other variables that do not need to be reshaped -------------------------------
  
  # Since there are variables like "b1hh_14a", "hh_01" etc
  # they are household level information and do not need to be reshaped into long format
  # so we can just merge them back to the reshaped data file by "id_05"
  
# 1) Merge to the long-format data frame hhrw_long ----------------------------------------------
  
  modB1_long <- merge(modB1_long, hhrwb1_exp, by = c("id_05"), all = T) 
  # final long-format data frame of orginal data set - module B
  
# PART 4: removing empty rows from 13:16 --------------------------------------------------------
  
  modB1_long = modB1_long[!modB1_long$member_id > 12,]
  
# PART 5: removing empty column new_age
  
  modB1_long <- select(modB1_long, -c(new_age))
  
# PART 6: export the long-format data file ------------------------------------------------------
  
  write_dta(modB1_long, file.path(InterData, "modB1_long.dta"))
    
  