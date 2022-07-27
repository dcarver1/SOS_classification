###
# generate input for the cape may dataset 
# carverd@colostate.edu 
# 20220125 
###



# read in content 
maySamples <- read_csv("data/cape_may/Cape MaySOSforPullman.csv")
dim(maySamples)
allVals <- read_csv("data/cape_may/Current_Export 10.10.2019.csv")
# subset based on acc_num 
d2 <- allVals[allVals$ACC_NUM %in% maySamples$`Accn Number`, ]

# export classification 
write_csv(x = d2, file = "data/capeMay2019.csv" )



# need to make a manual edit to drop special characters in voucher --------


