#------------------------------------------------------------------------------ #
#                                                                               #
#                                    DIME                                       #
#                   Data Cleaning for Rwanda Household Info                     #
#                                Yuchen Xiang                                   #
#                                                                               #
#------------------------------------------------------------------------------ #

### Loading Data Sets -----------------------------------------------------------
library(haven)
hh_rw34 <- read_dta("C:/Users/WB546716/Documents/Data Cleaning/Irrigation_Rwanda_Cleaning/fup1_rw34_incoming_data_toclean.dta")
View(hh_rw34)

### Cleaning --------------------------------------------------------------------
# 1. Subsetting the data 
which(colnames(hh_rw34)=="old_membpresent") # 1031
which(colnames(hh_rw34)=="pl_hhmembnumber") # 32
hh_rw <- hh_rw34[c(7,32:1031)] # keeping only id_05 and mod_b
View(hh_rw)

# 2. correct the names
install.packages("sjmisc")
library(sjmisc)
library(stringr)
find_var(hh_rw, "v") 
df <- hh_rw[names(hh_rw)[substr(names(hh_rw),1,1) == "v"]]
for (var in hh_rw[names(hh_rw)[substr(names(hh_rw),1,1) == "v"]]) {
  label <- str_replace(attr(var,"label"), "HH", "hh") # convert HH to hh
  list <- append(list, label)
} 
names(hh_rw)[substr(names(hh_rw),1,1) == "v"] <- as.character(list) 
names(hh_rw)[substr(names(hh_rw),1,1) == "v"] # double check: character(0) means all names replaced
names(hh_rw)[substr(names(hh_rw),1,1) == "H"] # double check: character(0) means HH is changed to hh

# 3. check for duplicates
duplicated(hh_rw) # returns 0 duplicated columns
duplicated(hh_rw$id_05) # returns 2 duplicated rows in id_05
n_occur <- data.frame(table(hh_rw$id_05))
n_occur[n_occur$Freq > 1,] # locate the dups
#    Var1 Freq
#16  6021    2
#157 6183    2

# 1) remove the dups of id_05
hh_rw <- hh_rw[!duplicated(hh_rw$id_05),] # check for obs, should be 2 obs fewer than original data set

# 4. converting to long format
library(reshape)
library(reshape2)
library(data.table)

which(colnames(hh_rw)=="hh_13b_1") # 8
which(colnames(hh_rw)=="hh_12b_12") # 951
which(colnames(hh_rw)=="sex_1") # 969
which(colnames(hh_rw)=="sex_12") # 980
which(colnames(hh_rw)=="age_1") # 953
which(colnames(hh_rw)=="age_12") # 964
which(colnames(hh_rw)=="member_present_1") #985
which(colnames(hh_rw)=="member_present_16") #1000

hhrw_long <- reshape(as.data.frame(hhrw_long), direction="long",
                     varying=c(969:980,953:964,985:1000),
                     v.names=c("gender","age", "memb_present"))

hhrw_long <- reshape(as.data.frame(hh_rw), direction="long",
                     varying=c(8:951),
                     v.names=c("memb_id"))

hhrw_long <- gather(hh_rw,
                    key = member,
                    value = answer,
                    hh_13b_1:hh_13b_12)

duplicated(hh_rw$id_05)

write.csv(hhrw_long, "C:/Users/WB546716/Documents/Data Cleaning/Irrigation_Rwanda_Cleaning/hhrw_long.csv" )
