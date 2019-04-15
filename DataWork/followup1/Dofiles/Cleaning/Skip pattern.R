#------------------------------------------------------------------------------ #
#                                                                               #
#                                    DIME                                       #
#                   Data Cleaning for Rwanda Household Info                     #
#                             for Module B and B1                               #
#                                Skip Patterns                                  #
#                                                                               #
#                                                                               #
#------------------------------------------------------------------------------ #
# PURPOSE:    Set-up configurations and run scripts that are used to clean data 
#             collected for Rwanda Irrigation Project
# NOTES:      This is for checking skip patterns for module B and B1

# WRITTEN BY: Yuchen Xiang

### PART 1: Load Data Sets ------------------------------------------------------------

    modbb1 <- read_dta(file.path(InterData,
                                "modB_B1_long.dta"))
### PART 2: Delete empty rows
    
    modbb1[1:179] <- lapply(modbb1[1:179], as.numeric)
    hh_bb1 <- as.data.frame(modbb1)
    hh_bb1 <- subset(hh_bb1, !is.na(b1hh_14a) | !is.na(hh_roster_index))
    
### PART 3: Check for skip patterns starting with module b -----------------------------


  # 1. If a person is still a member
  summary(hh_bb1$hh_13c[hh_bb1$hh_13b == 0], basic = T) # if a person is not, hh_13c should range 1 to 4
  # result:
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  # 1.00    1.00    3.00    2.54    4.00    4.00     129  no odd values here
  summary(hh_bb1$hh_13c[hh_bb1$hh_13b == 1], basic = T) # if a person is still a member, hh_13c should all be NA
  # result:
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  # NA      NA      NA     NaN      NA      NA    3082 # no odd values here
  # no other reasons, so hh_13c_other all NA

# For labor age >= 16
  hh_bb1 <- as.data.frame(hh_bb1)
  grp <- c("hh_10", "hh_10_other", "hh_10a", "hh_10a_alert", "hh_10_18a", "hh_10_18a_other", "hh_10_18A")

  # 2. Primary Activity (income source)
  summary(hh_bb1$hh_10[hh_bb1$hh_07 < 16])
  # Result:
  #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #-77.000   1.000   1.000   2.345   1.000  16.000    1569   # -77 indicates other
  summary(hh_bb1$hh_10_other[hh_bb1$hh_10 == -77], basic = T)
  # contains non-numeric values 
  
  # 3. Income
  summary(hh_bb1$hh_10a)
  # Result:
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  # -88   30000   60000  128711  144000 1200000    3030 
  summary(hh_bb1$hh_10a_alert[hh_bb1$hh_10a > 100000]) # if income > 100000, hh_10a_alert = 1
  # Result:
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  # 1       1       1       1       1       1    3030 
  summary(hh_bb1$hh_10a_alert[hh_bb1$hh_10a < 100000]) 
  # Result:
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #  NA      NA      NA     NaN      NA      NA    3150  # no odd values here
  
  # 4. Primary Activity from September to February (income source)
  summary(hh_bb1$hh_10_18a)
  # Result:
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  # -77.000   1.000   1.000   2.302   1.000  16.000    1569 
  summary(hh_bb1$hh_10_18a_other[hh_bb1$bb_10a_18a == -77])
  # hh_10_18a_other all character
  
  # 5. Income September to February
  summary(hh_bb1$hh_10_18A)

# For hh_10_18a = 2
  # 6. Who worked the most
  