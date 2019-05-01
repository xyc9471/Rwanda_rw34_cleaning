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
    
### PART 2: Delete empty rows ---------------------------------------------------------
    
    modbb1[1:179] <- lapply(modbb1[1:179], as.numeric)
    hh_bb1 <- as.data.frame(modbb1)
    hh_bb1 <- subset(hh_bb1, !is.na(b1hh_14a) | !is.na(hh_roster_index))
    
### PART 3: Module B: Check for skip patterns -----------------------------------------

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

### For people who worked as an agriculture laborer for another farmer(hh_10_18a = 2) - Primary Activity

### For Season A 2018 (September - Feburary)
    
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

### For Season B 2018 (February - May/June)
    
 # 13. Primary Activity during Season 18 B
    summary(hh_bb1$hh_10_18b)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -77.000   1.000   1.000   2.312   1.000  16.000    1569 

 # 14. Other Activity (hh_10_18b == -77)
    summary(hh_bb1$hh_10_18b_other[hh_bb1$hh_10_18b == -77])
    # return complete NA   
    
 # 15. Earnings duing Season 18 B
    summary(hh_bb1$hh_10b)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88   11500   29000   46304   50000  400000    3043 
    
 # 16. Season 18b Relationship
    summary(hh_bb1$hh_10a_18b_2)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -77.000   2.500   3.000   3.606   8.000   8.000    3148 
    
    summary(hh_bb1$hh_10a_18b_2_oth[hh_bb1$hh_10a_18b_2 == -77])
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # NA      NA      NA     NaN      NA      NA    3149

 # 17. Days worked in Season 18b
    summary(hh_bb1$hh_10a_18b_3)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88      10      20      13      30      60    3148 
    
 # 18. Days worked in plots in CA
    summary(hh_bb1$hh_10a_18b_4)
    # Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
    #-88.00     0.00     6.00   335.36    16.25 21000.00     3155 
    
    summary(hh_bb1$hh_10a_18b_4[hh_bb1$hh_10a_18b_3 == -66])
    summary(hh_bb1$hh_10a_18b_4[hh_bb1$hh_10a_18b_3 == -88])
    # both return NA
    
 # 19. Days worked outside plots in CA
    summary(hh_bb1$hh_10a_18b_5)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #-88.00    2.00    9.50   11.06   25.75   60.00    3155 
    
    summary(hh_bb1$hh_10a_18b_5[hh_bb1$hh_10a_18b_3 == -66])
    summary(hh_bb1$hh_10a_18b_5[hh_bb1$hh_10a_18b_3 == -88])
    # both return NA
    
 # 20. Earnings from working for (hh_10a_18a_1)
    summary(hh_bb1$hh_10a_18b_6)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88    8000   15000   17842   28000   50000    3148 

### For Season C (June - August/September)
    
 # 21. Primary Activity during Season 18 C
    summary(hh_bb1$hh_10_18c)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -77.000   1.000   1.000   2.796   2.000  16.000    1569 
    
 # 22. Other Activity (hh_10_18c == -77)
    summary(hh_bb1$hh_10_18c_other[hh_bb1$hh_10_18c == -77])
    # return complete NA   
    
 # 23. Earnings duing Season 18 B
    summary(hh_bb1$hh_10c)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88   10000   20000   39817   45000  400000    3060 
    
 # 24. Season 18b Relationship
    summary(hh_bb1$hh_10a_18c_2)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -77.00    2.00    3.00    3.34    8.00    8.00    3172 
    
    summary(hh_bb1$hh_10a_18c_2_oth[hh_bb1$hh_10a_18c_2 == -77])
    # all NA
    
 # 25. Days worked in Season 18b
    summary(hh_bb1$hh_10a_18c_3)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #-88.000   5.000  10.000   6.936  20.000  56.000    3172 
    
 # 26. Days worked in plots in CA
    summary(hh_bb1$hh_10a_18c_4)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 0.00    2.00   10.00   10.84   15.50   56.00    3176 

 # 27. Days worked outside plots in CA
    summary(hh_bb1$hh_10a_18c_5)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 0.00    0.00    0.00    4.93    5.50   50.00    3176 
    
 # 28. Earnings from working for (hh_10a_18a_1)
    summary(hh_bb1$hh_10a_18c_6)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88    4600   10000   11807   16750   40000    3172 
    
### For Secondary Activity 
    
### For Season 18A (September - Feburary)
    
 # 29. Secondary activity during Season 18 A
    summary(hh_bb1$hh_11_18a)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -77.000   2.000   3.000   3.785   7.000  12.000    3014 
    
 # 30. Other secondary activity
    summary(hh_bb1$hh_11_18a_other[hh_bb1$hh_11_18a==-77])
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # NA      NA      NA     NaN      NA      NA    3016
    
 # 31. Earning from secondary activity
    summary(hh_bb1$hh_11a)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #-88   20000   45000   73506   80000 2000000    3014 
    
 # 32. Relationship to this person
    summary(hh_bb1$hh_11a_18a_2)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #-77.000   3.000   3.000   3.987   8.000   8.000    3141 
    
 # 33. Other relationship
    summary(hh_bb1$hh_11a_18a_2_oth[hh_bb1$hh_11a_18a_2 == -77])
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # NA      NA      NA     NaN      NA      NA    3147
    
    summary(hh_bb1$hh_11a_18a_2_oth)
    # all NA
    
 # 34. Days worked for them
    summary(hh_bb1$hh_11a_18a_3)
    #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88.000   5.000  10.000   3.859  25.000  60.000    3141 
    
 # 35. Days worked on CA plots
    summary(hh_bb1$hh_11a_18a_4)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #-88.000   0.000   0.000   3.866   5.500  35.000    3152 
    
    summary(hh_bb1$hh_11a_18a_4[hh_bb1$hh_11a_18a_3 == -66]) # refuse to answer
    summary(hh_bb1$hh_11a_18a_4[hh_bb1$hh_11a_18a_3 == -88]) # dont know
    # both should return NA
    
 # 36. Days worked outside CA plots
    summary(hh_bb1$hh_11a_18a_5)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #-88.00    3.50   10.00   11.67   20.00   60.00    3152 
    
 # 37. Earnings from working for hh_11a_18b_1
    summary(hh_bb1$hh_11a_18a_6)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #-88    4000   10000   12846   20000   48000    3141 
    
### For Season 18B (Feburary - May/June)
    
 # 38. Secondary activity during Season 18 B
    summary(hh_bb1$hh_11_18b)
    # Result:
    #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -77.000   2.000   5.000   3.732   7.000  10.000    3014 
    
 # 39. Other secondary activity
    summary(hh_bb1$hh_11_18b_other[hh_bb1$hh_11_18b==-77])
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # NA      NA      NA     NaN      NA      NA    3016
    
 # 40. Earning from secondary activity
    summary(hh_bb1$hh_11b)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88   10000   18000   30983   35000  600000    3050
    
 # 41. Relationship to this person
    summary(hh_bb1$hh_11a_18b_2)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -77.000   3.000   3.000   4.055   8.000   8.000    3146 
    
 # 42. Other relationship
    summary(hh_bb1$hh_11a_18b_2_oth[hh_bb1$hh_11a_18b_2 == -77])
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # NA      NA      NA     NaN      NA      NA    3147
    
    summary(hh_bb1$hh_11a_18b_2_oth)
    # all NA
    
 # 43. Days worked for them
    summary(hh_bb1$hh_11a_18b_3)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88.0     5.0    12.0   114.8    25.0  8000.0    3146 
    
 # 44. Days worked on CA plots
    summary(hh_bb1$hh_11a_18b_4)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88.000   0.000   0.000   1.703   5.000  35.000    3155 
    
    summary(hh_bb1$hh_11a_18b_4[hh_bb1$hh_11a_18b_3 == -66]) # refuse to answer
    summary(hh_bb1$hh_11a_18b_4[hh_bb1$hh_11a_18b_3 == -88]) # dont know
    # both should return NA
 
 # 45. Days worked outside CA plots
    summary(hh_bb1$hh_11a_18b_5)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88.00    3.00   10.00   10.34   18.50   87.00    3155 
    
 # 46. Earnings from working for hh_11a_18b_1
    summary(hh_bb1$hh_11a_18b_6)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #-88    4000   10000   13096   19000   70000    3146 
    
# For season C 2018 (June - August/September)
    
 # 47. Secondary Activity during season C
    summary(hh_bb1$hh_11_18c)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -77.000   2.000   5.000   5.141   7.000  16.000    3014 
    
 # 48. Other secondary activity
    summary(hh_bb1$hh_11_18c_other[hh_bb1$hh_11_18c==-77])  
    # all NA    
 
 # 49. Earning from secondary activity
    summary(hh_bb1$hh_11c)  
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88    9500   16000   36283   30000  800000    3080 
    
 # 50. Relationship to this person
    summary(hh_bb1$hh_11a_18c_2) 
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -77.000   3.000   8.000   3.231   8.000   8.000    3180 
  
 # 51. Other relationship
    summary(hh_bb1$hh_11a_18c_2_oth[hh_bb1$hh_11a_18c_2 == -77])
    summary(hh_bb1$hh_11a_18c_2_oth)
    # both return NA
  
 # 52. Days worked for them
    summary(hh_bb1$hh_11a_18c_3)  
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88.000   5.000   8.000   1.436  13.000  36.000    3180 
  
 # 53. Days worked on CA plots
    summary(hh_bb1$hh_11a_18c_4)
    #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88.000   0.000   0.000   2.743   8.500  30.000    3184 

    summary(hh_bb1$hh_11a_18c_4[hh_bb1$hh_11a_18c_3 == -66]) # refuse to answer
    summary(hh_bb1$hh_11a_18c_4[hh_bb1$hh_11a_18c_3 == -88]) # dont know
    # both should return NA
    
 # 54. Days worked outside CA plots
    summary(hh_bb1$hh_11a_18c_5)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88.00    3.50   10.00   11.67   20.00   60.00    3152 
    
 # 55. Earnings from working for hh_11a_18c_1
    summary(hh_bb1$hh_11a_18c_6)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88    3750    6000    8301   10000   30000    3180 

### For Migration 
    
 # 56. Did pl_hhmembername migrate? 
    summary(hh_bb1$hh_12)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 0.0000  0.0000  0.0000  0.0727  0.0000  1.0000    2834 
    
 # 57. Which year and month left? 
    summary(hh_bb1$hh_12a)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 0.000   0.000   0.000   3.214   6.500  12.000    3191 
    
    summary(hh_bb1$hh_12a[hh_bb1$hh_12 != 1])
    # all NA

 # 58. Which year and month returned?
    summary(hh_bb1$hh_12b)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 1.00   10.00   14.00   12.18   15.00   15.00    3191 
    
    summary(hh_bb1$hh_12b[hh_bb1$hh_12 != 1])
    # all NA

 # 59. Any new members?
    summary(hh_bb1$b1hh_14)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 0.0000  0.0000  0.0000  0.1379  0.0000  1.0000     129 

### PART 4: Module B1: Check for skip patterns -----------------------------------------
    
 # 60. How many new members?
    summary(hh_bb1$b1hh_14a)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 1.000   1.000   1.000   1.527   2.000   5.000    3090 
    
    summary(hh_bb1$b1hh_14a[hh_bb1$b1hh_14 != 1])
    # should return NA    
 
 # 61. Relationship to HH head
    summary(hh_bb1$b1hh_05)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 1.000   3.000   3.000   3.868   6.000   7.000    3090 
    
 # 62. New member's sex
    summary(hh_bb1$b1hh_06)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 1.000   1.000   2.000   1.504   2.000   2.000    3090 

 # 63. New member's age
    summary(hh_bb1$b1hh_07)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 0.000   0.000   3.000   9.264  17.000  49.000    3090 

 # 64. Age over 18
    summary(hh_bb1$age_ovr18)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 0.0000  0.0000  0.0000  0.2481  0.0000  1.0000    3090 

 # 65. Highest level of education completed
    summary(hh_bb1$b1hh_08)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 1.000   2.000   2.000   2.255   2.500   5.000    3164 

 # 66. Currenty in school? b1hh_03
    summary(hh_bb1$b1hh_09)    
    #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 0.000   0.000   0.000   0.234   0.000   1.000    3172 
    
 # 67. Primary activity from feb 2018 to nov 2018?
    summary(hh_bb1$b1hh_10)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #-77.000   1.000   2.000   4.328   9.000  16.000    3158 
    
 # 68. Other activity
    summary(hh_bb1$b1hh_10_other[hh_bb1$b1hh_10 == -77])
    # all NA
    summary(hh_bb1$b1hh_10_other)
    # all NA
    
 # 69. Earnings from this source from feb 2018 to nov 2018
    summary(hh_bb1$b1hh_10a)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88   50000   50000   61416   65000  150000    3212  

 # 70. Earnings more than 100,0000
    summary(hh_bb1$b1hh_10a_alert[hh_bb1$b1hh_10a > 100000])
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 1       1       1       1       1       1    3212 
    summary(hh_bb1$b1hh_10a_alert)
    # same as above
    
    
### For people who worked as an agriculture laborer for another farmer(hh_10_18a = 2) - Primary Activity
    
### For Season A 2018 (September - Feburary)
    
 # 71. Primary Activity during season 18A
    summary(hh_bb1$b1hh_10_18a)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -77.000   1.000   4.000   3.098   9.000  16.000    3158 
    
    summary(hh_bb1$b1hh_10_18a_other[hh_bb1$b1hh_10_18a == -77])
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # NA      NA      NA     NaN      NA      NA    3160 
    
 # 72. Income September to February
    summary(hh_bb1$b1hh_10_18A)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # NA      NA      NA     NaN      NA      NA    3219 
    
 # 73. Any anomalies in b1hh_10A_18a series?
    summary(hh_bb1$b1hh_10a_18a_2[hh_bb1$b1hh_10_18a != 2])
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # NA      NA      NA     NaN      NA      NA    3215
    
 # 74. b1hh_03 Relation to the person worked for most  
    summary(hh_bb1$b1hh_10a_18a_2[hh_bb1$b1hh_10_18a == 2])
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #  8       8       8       8       8       8    3158 
    
    summary(hh_bb1$b1hh_10a_18a_2[hh_bb1$b1hh_10_18a == -77])
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # NA      NA      NA     NaN      NA      NA    3160
    
    summary(hh_bb1$b1hh_10a_18a_2[hh_bb1$b1hh_10_18a == -88])
    summary(hh_bb1$b1hh_10a_18a_2[hh_bb1$b1hh_10_18a == -66]) # both return NA
    
 # 75. Other relation to person worked for most (hh_10_18a == -77)
    summary(hh_bb1$b1hh_10a_18a_2_oth)
    # all NA
    
 # 76. b1hh_03 Days worked
    summary(hh_bb1$b1hh_10a_18a_3)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 15.00   18.75   24.00   25.75   31.00   40.00    3215 
    
 # 77. b1hh_03 Days worked on plots in CA
    summary(hh_bb1$b1hh_10a_18a_4)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 0.00   11.25   21.50   20.75   31.00   40.00    3215 
    
    which(hh_bb1$b1hh_10a_18a_4 > hh_bb1$b1hh_10a_18a_3)
    # returns NA
    
 # 78. Days worked on plots OUTSIDE CA
    summary(hh_bb1$b1hh_10a_18a_5)
    # Result: 
    #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #   0       0       0       5       5      20    3215
    
 # 79. Earning by working for b1hh_10A_18a_1
    summary(hh_bb1$b1hh_10a_18a_6)
    # Result:
    #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 12000   18000   20000   21000   23000   32000    3215 
    
### For Season B 2018 (February - May/June)
    
 # 80. Primary Activity during Season 18 B
    summary(hh_bb1$b1hh_10_18b)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -77.000   1.000   4.000   4.377   9.000  16.000    3158 
    
 # 81. Other Activity (hh_10_18b == -77)
    summary(hh_bb1$b1hh_10_18b_other[hh_bb1$b1hh_10_18b == -77])
    # return complete NA   
    
 # 82. Earnings duing Season 18 B
    summary(hh_bb1$b1hh_10b)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88   14000   20000   20273   22000   50000    3212 
    
 # 83. Season 18b Relationship
    summary(hh_bb1$b1hh_10a_18b_2)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 8       8       8       8       8       8    3215 
    summary(hh_bb1$b1hh_10a_18b_2_oth[hh_bb1$b1hh_10a_18b_2 == -77])
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # NA      NA      NA     NaN      NA      NA    3215
    
 # 84. Days worked in Season 18b
    summary(hh_bb1$b1hh_10a_18b_3)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 10.00   13.75   23.50   22.75   32.50   34.00    3215 
    
 # 85. Days worked in plots in CA
    summary(hh_bb1$b1hh_10a_18b_4)
    #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 0.00    7.50   12.50   14.75   19.75   34.00    3215 
    
 # 86. Days worked outside plots in CA
    summary(hh_bb1$b1hh_10a_18b_5)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #  0       0       0       8       8      32    3215 
    
 # 87. Earnings from working for (hh_10a_18a_1)
    summary(hh_bb1$b1hh_10a_18b_6)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 8000   17000   20000   18000   21000   24000    3215 
    
### For Season C (June - August/September)
    
 # 88. Primary Activity during Season 18 C
    summary(hh_bb1$b1hh_10_18c)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -77.000   1.000   7.000   4.426  10.000  14.000    3158 
    
 # 89. Other Activity (hh_10_18c == -77)
    summary(hh_bb1$b1hh_10_18c_other[hh_bb1$b1hh_10_18c == -77])
    # return complete NA   
    
 # 90. Earnings duing Season 18 B
    summary(hh_bb1$b1hh_10c)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -88    4956   18000   16832   25000   40000    3212 
    
 # 91. Season 18b Relationship
    summary(hh_bb1$b1hh_10a_18c_2)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 8       8       8       8       8       8    3217
    
 # 92. Days worked in Season 18b
    summary(hh_bb1$b1hh_10a_18c_3)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #10.00   13.75   17.50   17.50   21.25   25.00    3217 
    
 # 93. Days worked in plots in CA
    summary(hh_bb1$b1hh_10a_18c_4)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 0.0     2.5     5.0     5.0     7.5    10.0    3217 
    
 # 94. Days worked outside plots in CA
    summary(hh_bb1$b1hh_10a_18c_5)
    #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 0.00    6.25   12.50   12.50   18.75   25.00    3217 
    
 # 95. Earnings from working for (hh_10a_18a_1)
    summary(hh_bb1$b1hh_10a_18c_6)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #10000   12000   14000   14000   16000   18000    3217 
    
    
### Secondary Activity

### For Season A 2018 (September - Feburary)
    
 # 96. Primary Activity during season 18A
    summary(hh_bb1$b1hh_11_18a)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 10      10      10      10      10      10    3218 
    
 # 97. Income September to February
    summary(hh_bb1$b1hh_11_18A)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # NA      NA      NA     NaN      NA      NA    3219 
    
 # 98. Any anomalies in b1hh_10A_18a series?
    summary(hh_bb1$b1hh_11a_18a_2[hh_bb1$b1hh_11_18a != 2])
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # NA      NA      NA     NaN      NA      NA    3219
    
 # 99. b1hh_03 Relation to the person worked for most  
    summary(hh_bb1$b1hh_11a_18a_2[hh_bb1$b1hh_11_18a == 2])
    # all NA
    
 # 100. Other relation to person worked for most (hh_10_18a == -77)
    summary(hh_bb1$b1hh_11a_18a_2_oth)
    # all NA
    
 # 101. b1hh_03 Days worked
    summary(hh_bb1$b1hh_11a_18a_3)
    # Result:
    # NA
    
 # 102. b1hh_03 Days worked on plots in CA
    summary(hh_bb1$b1hh_11a_18a_4)
    # Result:
    # NA
    
    which(hh_bb1$b1hh_11a_18a_4 > hh_bb1$b1hh_11a_18a_3)
    # returns NA
    
 # 103. Days worked on plots OUTSIDE CA
    summary(hh_bb1$b1hh_11a_18a_5)
    # Result: 
    # NA
    
 # 104. Earning by working for b1hh_10A_18a_1
    summary(hh_bb1$b1hh_11a_18a_6)
    # Result:
    #  NA
    
### For Season B
    
 # 105. Secondary activity during Season 18 B
    summary(hh_bb1$b1hh_11_18b)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 2       2       2       2       2       2    3218 
    
 # 106. No other activities
    
 # 107. Earning from secondary activity
    summary(hh_bb1$b1hh_11b)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 7000    7000    7000    7000    7000    7000    3218 
    
 # 108. Relationship to this person
    summary(hh_bb1$b1hh_11a_18b_2)
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #  1       1       1       1       1       1    3218 
    
 # 109. Other relationship
    summary(hh_bb1$b1hh_11a_18b_2_oth[hh_bb1$b1hh_11a_18b_2 == -77])
    # Result:
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # NA      NA      NA     NaN      NA      NA    3218
    
    summary(hh_bb1$b1hh_11a_18b_2_oth)
    # all NA
    
 # 110. Days worked for them
    summary(hh_bb1$b1hh_11a_18b_3)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 10      10      10      10      10      10    3218 
    
 # 111. Days worked on CA plots
    summary(hh_bb1$b1hh_11a_18b_4)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 0       0       0       0       0       0    3218 
    
 # 112. Days worked outside CA plots
    summary(hh_bb1$b1hh_11a_18b_5)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 10      10      10      10      10      10    3218 
    
 # 113. Earnings from working for hh_11a_18b_1
    summary(hh_bb1$b1hh_11a_18b_6)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 7000    7000    7000    7000    7000    7000    3218 
    
### For season C 2018 (June - August/September)
    
 # 114. Secondary Activity during season C
    summary(hh_bb1$b1hh_11_18c)
    #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #  2       2       2       2       2       2    3218
    
 # 115. Other secondary activity
    summary(hh_bb1$b1hh_11_18c_other[hh_bb1$b1hh_11_18c==-77])  
    # all NA    
    
 # 116. Earning from secondary activity
    summary(hh_bb1$b1hh_11c)  
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 7700    7700    7700    7700    7700    7700    3218 
    
 # 117. Relationship to this person
    summary(hh_bb1$b1hh_11a_18c_2) 
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 1       1       1       1       1       1    3218 
    
 # 118. No other types of relationship

    
 # 119. Days worked for them
    summary(hh_bb1$b1hh_11a_18c_3)  
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 11      11      11      11      11      11    3218 
    
 # 120. Days worked on CA plots
    summary(hh_bb1$b1hh_11a_18c_4)
    #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #   0       0       0       0       0       0    3218 
  
    
 # 121. Days worked outside CA plots
    summary(hh_bb1$b1hh_11a_18c_5)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 11      11      11      11      11      11    3218 
    
 # 122. Earnings from working for hh_11a_18c_1
    summary(hh_bb1$b1hh_11a_18c_6)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 7700    7700    7700    7700    7700    7700    3218 

 # 123. Did b1hh_03 migrate?
    summary(hh_bb1$b1hh_12)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 0       0       0       0       0       0    3212   
    # Nobody migrates
    summary(hh_bb1$b1hh_12a)
    summary(hh_bb1$b1hh_12b)  # should all be NA
    
 # 124. Primarily Responsible person
    summary(hh_bb1$hh_15a_check[hh_bb1$hh_15 != hh_bb1$hh_15a])
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # 1.000   1.000   1.000   1.868   1.000  18.000    3090 

    
    