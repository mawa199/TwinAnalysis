rm(list = ls())

##########################################################################
#
# Based on https://datacolada.org/98
# "DrivingdataAll with font.xlsx" downloaded from ResearchBox.
# setwd() to be set to the folder of that file.
#
# Objectives:
# Identify Calibri and Cambria twins.
# Compare signing conditions of twins.
# Understand how the desired effect was built into the data.
#
# Martin Wallmeier, 01.09.2021
#
###########################################################################

# folder with file "DrivingdataAll with font.xlsx"
# insert path
setwd("... Data/")   

library("readxl")
a <- read_excel("DrivingdataAll with font.xlsx", sheet = "Data")

df <- a

#######################################################################
#
# Identify twin pairs using a simple search algorithm
#
#######################################################################

# identifier for car availability
df$N_cars <- df$car1 * 1000 + df$car2 * 100 + df$car3 * 10 + df$car4

# subsets
sub_cambria <- subset(df, font == "Cambria")
sub_calibri <- subset(df, font == "Calibri")
# subset of cambria obs. that have not yet been matched with a cambria obs.
sub_cambria_not_yet_assigned <- sub_cambria

# initialize twin variables in calibri subset
sub_calibri$twin_identified <- F
sub_calibri$twin_id <- NA
sub_calibri$twin_condition <- NA
sub_calibri$twin_baseline_car1 <- NA
sub_calibri$twin_baseline_car2 <- NA
sub_calibri$twin_baseline_car3 <- NA
sub_calibri$twin_baseline_car4 <- NA
sub_calibri$twin_update_car1 <- NA
sub_calibri$twin_update_car2 <- NA
sub_calibri$twin_update_car3 <- NA
sub_calibri$twin_update_car4 <- NA
sub_calibri$twin_baseline_average <- NA
sub_calibri$twin_diff_average <- NA

# nb. of twins identified
N_twins <- 0

repeat{
  # for each dataset in the calibri subset, search for a twin in the cambria subset
  for (i in 1:nrow(sub_calibri)){
    
    if (sub_calibri$twin_identified[i] == F) { # start search process
      
      # calibri values to be matched
      calibri_base_car1 <- sub_calibri$baseline_car1[i]
      calibri_base_car2 <- sub_calibri$baseline_car2[i]
      calibri_base_car3 <- sub_calibri$baseline_car3[i]
      calibri_base_car4 <- sub_calibri$baseline_car4[i]
      calibri_N_cars <- sub_calibri$N_cars[i]
      
      # Potential matches are cambria cases not yet matched.
      # The potential matches are narrowed down successively.
      df_potential_cambria_twins <- sub_cambria_not_yet_assigned
      
      # potential cambria matches have baseline values that are larger by at most 1000
      if (!is.na(calibri_base_car1)) {
        df_potential_cambria_twins <- subset(df_potential_cambria_twins, baseline_car1 >= calibri_base_car1 &
                                               baseline_car1 <= (calibri_base_car1 + 1000) &
                                               N_cars == calibri_N_cars)
      } 
      
      # if there is a car2, its baseline value must also lie in the +1000 distance
      if (!is.na(calibri_base_car2)) {
        df_potential_cambria_twins <- subset(df_potential_cambria_twins, baseline_car2 >= calibri_base_car2 &
                                               baseline_car2 <= (calibri_base_car2 + 1000) &
                                               N_cars == calibri_N_cars)
      }
      # same for car3
      if (!is.na(calibri_base_car3)){
        df_potential_cambria_twins <- subset(df_potential_cambria_twins, baseline_car3 >= calibri_base_car3 &
                                               baseline_car3 <= (calibri_base_car3 + 1000) &
                                               N_cars == calibri_N_cars)
      }  
      # same for car4
      if (!is.na(calibri_base_car4)) {
        df_potential_cambria_twins <- subset(df_potential_cambria_twins, baseline_car4 >= calibri_base_car4 &
                                               baseline_car4 <= (calibri_base_car4 + 1000) &
                                               N_cars == calibri_N_cars)
      }  
      
      if (nrow(df_potential_cambria_twins) == 0) {
        print(paste0("Warning: no possible twin exists for id ", sub_calibri$id[i]))
      }
      if (nrow(df_potential_cambria_twins) == 1) { # unique match!
        
        # note the matching twin characteristics
        sub_calibri$twin_identified[i] <- T
        sub_calibri$twin_id[i] <- df_potential_cambria_twins$id[1]
        sub_calibri$twin_condition[i] <- df_potential_cambria_twins$condition[1]
        sub_calibri$twin_baseline_car1[i] <- df_potential_cambria_twins$baseline_car1[1]
        sub_calibri$twin_baseline_car2[i] <- df_potential_cambria_twins$baseline_car2[1]
        sub_calibri$twin_baseline_car3[i] <- df_potential_cambria_twins$baseline_car3[1]
        sub_calibri$twin_baseline_car4[i] <- df_potential_cambria_twins$baseline_car4[1]
        sub_calibri$twin_baseline_average[i] <- df_potential_cambria_twins$baseline_average[1]
        sub_calibri$twin_update_car1[i] <- df_potential_cambria_twins$update_car1[1]
        sub_calibri$twin_update_car2[i] <- df_potential_cambria_twins$update_car2[1]
        sub_calibri$twin_update_car3[i] <- df_potential_cambria_twins$update_car3[1]
        sub_calibri$twin_update_car4[i] <- df_potential_cambria_twins$update_car4[1]
        sub_calibri$twin_diff_average[i] <- df_potential_cambria_twins$diff_average[1]
        
        # take the match out of the data frame with the not-yet-matched cambria cases
        sub_cambria_not_yet_assigned <- subset(sub_cambria_not_yet_assigned, 
                                               id != df_potential_cambria_twins$id[1])
      } 
      
      rm(calibri_base_car1, calibri_base_car2, calibri_base_car3, calibri_base_car4,
         calibri_N_cars, df_potential_cambria_twins) 
    } 
    
  } # of for i= 
  
  N_twins_new <- sum(sub_calibri$twin_identified, na.rm = T)
  print(paste0("Result after this search round: ", N_twins_new, " matches identified"))
  if (N_twins_new == N_twins){ # no new match found
    break
  } else N_twins <- N_twins_new 
  
} # of repeat

print(N_twins_new)
df_twins <- subset(sub_calibri, twin_identified == T)

df_twins$factor_conditions <- with(df_twins, interaction(condition,  twin_condition))

df_twins$diff1 <- df_twins$update_car1 - df_twins$baseline_car1
df_twins$twin_diff1 <- df_twins$twin_update_car1 - df_twins$twin_baseline_car1


###################################################################
#
# count Calibri drivers with 1 to 4 cars (overall and twin sample)
# 
###################################################################

twin_pair_ids <- c(df_twins$id, df_twins$twin_id)
sub_twins <- subset(df, id %in% twin_pair_ids)
sub_twins_calibri <- subset(sub_twins, font == "Calibri")

table(sub_calibri$car_count)
table(sub_twins_calibri$car_count)


# standard deviations for car_count groups
tapply(sub_calibri$diff_average, sub_calibri$car_count, sd)

# car_count and combination of Calibri/Cambria condition
#table(df_twins$car_count, df_twins$factor_conditions)

###################################################################
#
# distribution among Bottom and Top
# 
###################################################################

table(df_twins$condition, df_twins$twin_condition)

###########################################
#
# compute group mean differences
#
###########################################

print_subgroup_means <- function(ids){
  a_sub <- subset(a, id %in% ids)
  mean_top <- mean(a_sub$diff_average[a_sub$condition == "Sign Top"])
  mean_bottom <- mean(a_sub$diff_average[a_sub$condition == "Sign Bottom"])
  res <- c(mean_top, mean_bottom, mean_top-mean_bottom)
  return(res)
}

# effect in sample of identified twins
twin_pair_ids <- c(df_twins$id, df_twins$twin_id)
print_subgroup_means(twin_pair_ids)

# effect in full sample
all_ids <- c(a$id)
print_subgroup_means(all_ids)

# split conditions combined
sub <- subset(df_twins, factor_conditions == "Sign Top.Sign Bottom" |
                factor_conditions == "Sign Bottom.Sign Top")
twin_pair_ids <- c(sub$id, sub$twin_id)
print_subgroup_means(twin_pair_ids)

# split condition Bottom/Top
sub <- subset(df_twins, factor_conditions == "Sign Bottom.Sign Top")
twin_pair_ids <- c(sub$id, sub$twin_id)
print_subgroup_means(twin_pair_ids)

# split condition Top/Bottom
sub <- subset(df_twins, factor_conditions == "Sign Top.Sign Bottom")
twin_pair_ids <- c(sub$id, sub$twin_id)
print_subgroup_means(twin_pair_ids)

# effect in sample of equal-condition twins
sub <- subset(df_twins, factor_conditions == "Sign Bottom.Sign Bottom" |
                factor_conditions == "Sign Top.Sign Top")
twin_pair_ids <- c(sub$id, sub$twin_id)
print_subgroup_means(twin_pair_ids)

# effect in sample of Bottom/Bottom twins
sub <- subset(df_twins, factor_conditions == "Sign Bottom.Sign Bottom")
twin_pair_ids <- c(sub$id, sub$twin_id)
print_subgroup_means(twin_pair_ids)

# effect in sample of Top/Top twins
sub <- subset(df_twins, factor_conditions == "Sign Top.Sign Top")
twin_pair_ids <- c(sub$id, sub$twin_id)
rm(sub)
print_subgroup_means(twin_pair_ids)

#############################################
#
# quadrant counts equal condition subsample
#
#############################################

# Bottom/Bottom
sub <- subset(df_twins, factor_conditions == "Sign Bottom.Sign Bottom")
length(sub$id[sub$diff_average <= 25000 & sub$twin_diff_average <= 25000])
length(sub$id[sub$diff_average > 25000 & sub$twin_diff_average <= 25000]) 
length(sub$id[sub$diff_average > 25000 & sub$twin_diff_average > 25000])
length(sub$id[sub$diff_average <= 25000 & sub$twin_diff_average > 25000]) 
 
# Top/Top
sub <- subset(df_twins, factor_conditions == "Sign Top.Sign Top")
length(sub$id[sub$diff_average <= 25000 & sub$twin_diff_average <= 25000]) 
length(sub$id[sub$diff_average > 25000 & sub$twin_diff_average <= 25000])
length(sub$id[sub$diff_average > 25000 & sub$twin_diff_average > 25000])
length(sub$id[sub$diff_average <= 25000 & sub$twin_diff_average > 25000]) 


########################################################################
#
# plot average miles driven for Calibri vs. Cambria twin
# 1st plot: Calibri top and Cambria top
# 2nd plot: Calibri bottom and Cambria bottom
# 3rd/4th plot: Split condition
#
#########################################################################

par(mfrow = c(1, 1))

# Define colors for the 4 condition combinations
# ordering: 1) Bottom-Bottom, 2) Top-Bottom, 3) Bottom-Top, 4) Top-Top
colors_base <- c("black", "red", "blue", "blue")
colors <- colors_base[as.numeric(df_twins$factor_conditions)]

# Define shapes
shapes_base = c(16, 16, 16, 16) 
shapes <- shapes_base[as.numeric(df_twins$factor_conditions)]

## plot cases with twin condition Top-Top
# set size of symbols for the other conditions to 0.
size_base = c(0, 0, 0, 1)
size <- size_base[as.numeric(df_twins$factor_conditions)]


plot(df_twins$diff_average, df_twins$twin_diff_average, 
     col = colors, pch = shapes, cex = size,
     xlab = " Calibri twin: average miles driven",
     ylab = " Cambria twin: average miles driven", 
     main = "Sign Top both for Calibri and Cambria twin (N=1315)")
#abline(0,1)
abline(h=25000)
abline(v=25000)
# legend("topright", inset=c(-0.02,0),legend = levels(df_twins$factor_conditions),
#        col =  colors_base, pch = shapes_base, cex=0.5)

## plot cases with twin condition Bottom-Bottom
size_base = c(1, 0, 0, 0)
size <- size_base[as.numeric(df_twins$factor_conditions)]

plot(df_twins$diff_average, df_twins$twin_diff_average, 
     col = colors, pch = shapes, cex = size,
     xlab = " Calibri twin: average miles driven",
     ylab = " Cambria twin: average miles driven",
     main = "Sign Bottom both for Calibri and Cambria twin (N=1426)")
#abline(0,1)
abline(h=25000)
abline(v=25000)

## plot cases with split twin conditions
# Define symbol size
size_base = c(0.0, 1, 1, 0.0)
size <- size_base[as.numeric(df_twins$factor_conditions)]
# split twin pooled
plot(df_twins$diff_average, df_twins$twin_diff_average, 
     col = "black", pch = shapes, cex = size,
     xlab = " Calibri twin: average miles driven",
     ylab = " Cambria twin: average miles driven",
     main = "Split twin condition (N=248)")
abline(v=25000)
abline(h=25000)

plot(df_twins$diff_average, df_twins$twin_diff_average, 
     col = colors, pch = shapes, cex = size,
     xlab = " Calibri twin: average miles driven",
     ylab = " Cambria twin: average miles driven",
     main = "Split twin condition (N=248)",
     sub = "red: Calibri Top/Cambria Bottom; blue: Bottom/Top")
rect(0,25000,25000,50000,col="grey", border=F)
rect(25000,0,50000,25000,col="grey", border=F)
par(new=T)
plot(df_twins$diff_average, df_twins$twin_diff_average, 
     col = colors, pch = shapes, cex = size,
     xlab = " Calibri twin: average miles driven",
     ylab = " Cambria twin: average miles driven",
     main = "Split twin condition (N=248)",
     sub = "red: Calibri Top/Cambria Bottom; blue: Bottom/Top")
text(50000,50000,"Q1", cex=1.5)
text(50000,0,"Q2", cex=1.5)
text(0,0,"Q3", cex=1.5)
text(0,50000,"Q4", cex=1.5)

################################################################
#
# Histograms of *average* miles driven for Calibri drivers in 
# different conditions:
#
# Calibri top and bottom, depending on condition of the Cambria twin 
#
################################################################

par(mfrow = c(2, 2))
# plot 1: Calibri top when twin is also top
sub <- subset(df_twins, factor_conditions == "Sign Top.Sign Top")
hist(sub$diff_average,breaks=30,
     xlim=c(0,55000), main='Calibri top when twin is top (N=1315)',
     xlab='',xaxt='n',col='dodgerblue',las=1)
mtext(side=1,line=2.5,font=1," Miles Driven (average per car)",cex=1)
axis(side=1, at=c(0,25000,50000, 55000))

# plot 2: Calibri top when twin is bottom
sub <- subset(df_twins, factor_conditions == "Sign Top.Sign Bottom")
hist(sub$diff_average,breaks=30,
     xlim=c(0,55000), main='Calibri top when twin is bottom (N=123)',
     xlab='',xaxt='n',col='dodgerblue',las=1)
mtext(side=1,line=2.5,font=1," Miles Driven (average per car)",cex=1)
axis(side=1, at=c(0,25000,50000, 55000))

# plot 3: Calibri bottom when twin is also bottom
sub <- subset(df_twins, factor_conditions == "Sign Bottom.Sign Bottom")
hist(sub$diff_average,breaks=30,
     xlim=c(0,55000), main='Calibri bottom when twin is bottom (N=1426)',xlab='',xaxt='n',col='dodgerblue',las=1)
mtext(side=1,line=2.5,font=1," Miles Driven (average per car)",cex=1)
axis(side=1, at=c(0,25000,50000, 55000))

# plot 4: Calibri bottom when twin is top
sub <- subset(df_twins, factor_conditions == "Sign Bottom.Sign Top")
hist(sub$diff_average,breaks=30,
     xlim=c(0,55000), main='Calibri bottom when twin is top (N=125)',xlab='',xaxt='n',col='dodgerblue',las=1)
mtext(side=1,line=2.5,font=1," Miles Driven (average per car)",cex=1)
axis(side=1, at=c(0,25000,50000, 55000))


################################################################
#
# Histograms *car#1 miles* driven for Calibri drivers in 
# different conditions:
#
# Calibri top and bottom, depending on condition of the Cambria twin 
#
################################################################

# par(mfrow = c(2, 2))
# # plot 1: Calibri top when twin is also top
# sub <- subset(df_twins, factor_conditions == "Sign Top.Sign Top")
# hist(sub$diff1,breaks=30,
#      xlim=c(0,55000), main='Calibri top with Cambria top twin',xlab='',xaxt='n',col='dodgerblue',las=1)
# mtext(side=1,line=2.5,font=1,"Miles Driven car#1",cex=1)
# axis(side=1, at=c(0,25000,50000, 55000))
# 
# # plot 2: Calibri top when twin is bottom
# sub <- subset(df_twins, factor_conditions == "Sign Top.Sign Bottom")
# hist(sub$diff1,breaks=30,
#      xlim=c(0,55000), main='Calibri top with Cambria bottom twin',xlab='',xaxt='n',col='dodgerblue',las=1)
# mtext(side=1,line=2.5,font=1,"Miles Driven car#1",cex=1)
# axis(side=1, at=c(0,25000,50000, 55000))
# 
# # plot 3: Calibri bottom when twin is also bottom
# sub <- subset(df_twins, factor_conditions == "Sign Bottom.Sign Bottom")
# hist(sub$diff1,breaks=30,
#      xlim=c(0,55000), main='Calibri bottom with Cambria bottom twin',xlab='',xaxt='n',col='dodgerblue',las=1)
# mtext(side=1,line=2.5,font=1,"Miles Driven car#1",cex=1)
# axis(side=1, at=c(0,25000,50000, 55000))
# 
# # plot 4: Calibri bottom when twin is top
# sub <- subset(df_twins, factor_conditions == "Sign Bottom.Sign Top")
# hist(sub$diff1,breaks=30,
#      xlim=c(0,55000), main='Calibri bottom with Cambria top twin',xlab='',xaxt='n',col='dodgerblue',las=1)
# mtext(side=1,line=2.5,font=1,"Miles Driven car#1",cex=1)
# axis(side=1, at=c(0,25000,50000, 55000))


