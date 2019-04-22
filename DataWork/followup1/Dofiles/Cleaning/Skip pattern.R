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
    modbb1 <- as.data.frame(modbb1)
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

### For labor age >= 16
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
  
    summary(hh_bb1$hh_10a[hh_bb1$hh_10a > 100000]) 
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 100800  150000  240000  321379  441000 1200000    3030 
  
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
  
    summary(hh_bb1$hh_10_18a_other[hh_bb1$hh_10_18a == -77])
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # NA      NA      NA     NaN      NA      NA    1574 
  
  # 5. Income September to February
    summary(hh_bb1$hh_10_18A)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88   15000   30000   51018   52000  450000    3042 

# For people who worked as an agriculture laborer for another farmer(hh_10_18a = 2)
  # 6. Who worked the most
    summary(hh_bb1$hh_10a_18a_2[hh_bb1$hh_10_18a != 2])
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # NA      NA      NA     NaN      NA      NA    3146
    
  # 7. Relation to person worked for most  
    summary(hh_bb1$hh_10a_18a_2[hh_bb1$hh_10_18a == 2])
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -77.000   2.000   3.000   3.479   8.000   8.000    1569
    
    summary(hh_bb1$hh_10a_18a_2[hh_bb1$hh_10_18a == -77])
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # NA      NA      NA     NaN      NA      NA    1574
    
    summary(hh_bb1$hh_10a_18a_2[hh_bb1$hh_10_18a == -88])
    summary(hh_bb1$hh_10a_18a_2[hh_bb1$hh_10_18a == -66]) # both return NA
  
  # 8. Other relation to person worked for most (hh_10_18a == -77)
    summary(hh_bb1$hh_10a_18a_2_oth)
    # no anomalies here
  
  # 9. Day s worked
    summary(hh_bb1$hh_10a_18a_3)
  
  # 10. Days worked on plots in CA
    summary(hh_bb1$hh_10a_18a_4)
    # Result:
    #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88.000   0.000   6.000   6.046  16.000  30.000    3154 
    
    which(hh_bb1$hh_10a_18a_4 > hh_bb1$hh_10a_18a_3)
    # returns NA
    
 # 11. Days worked on plots OUTSIDE CA
    summary(hh_bb1$hh_10a_18a_5)
    # Result: 
    #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88.00    0.00   10.00   11.29   21.00   70.00    3154 
    
 # 12. Earning by working for hh_10A_18a_1
    summary(hh_bb1$hh_10a_18a_6)
    # Result:
    #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #  -88   10000   15000   18180   25000   60000    3146 

# For Season B 2018 (February - May/June)
 # 13. Primary Activity during Season 18 B
    summary(hh_bb1$hh_10_18b)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -77.000   1.000   1.000   2.312   1.000  16.000    1569 

 # 14. Other Activity (hh_10_18b == -77)
    summary(hh_bb1$hh_10_18b_other[hh_bb1$hh_10_18b == -77])
    # return complete NA    