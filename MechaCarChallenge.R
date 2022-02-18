#############################################
###                                       ###
###     Module 15 Challenge Code          ###
###                                       ###
#############################################

### Deliverable 1 ###

# load dplyr
library(dplyr)

# read in csv
mechacar <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

# linear regression
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mechacar))

### Deliverable 2 ###

# read in csv
suspension <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

#summary df
total_summary <- suspension %>% summarize(Mean = mean(PSI),
                                          Median = median(PSI),
                                          Variance = var(PSI),
                                          SD = sd(PSI))

#lot summary df
lot_summary <- suspension %>% group_by(Manufacturing_Lot) %>%
                              summarize(Mean = mean(PSI),
                                        Median = median(PSI),
                                        Variance = var(PSI),
                                        SD = sd(PSI))

### Deliverable 3 ###

# Perform t-test on Lot 1
t.test(subset(suspension,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)

# Perform t-test on Lot 2
t.test(subset(suspension,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)

# Perform t-test on Lot 3
t.test(subset(suspension,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)
