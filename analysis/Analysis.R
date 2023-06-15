# setwd("Job/ARCA/course-open-science/code/attachment/01-attachment-base/")
# rm(list = ls())

# new comment


## altro commit



reverse_items <- function(array, rangelikert){
  # check range
  if (length(rangelikert) != 2){
    
  }
  
  # define min and max
  minvalue <- rangelikert[1]
  maxvalue <- rangelikert[2]
  
  adj <- minvalue + maxvalue
  # reverse the item values
  rev_array <- abs(array - adj)
  
  return(rev_array)
}

my_data <-readxl::read_excel("data/data-raw/att-quest.xlsx")
my_data <- as.data.frame(my_data)

str(my_data)

# Data munging

my_data$id = factor(my_data$id)
my_data$grade = factor(my_data$grade)
my_data$gender<-factor(my_data$gender)

# Compute questionnaire score

#  - ssm
ssm_items <- c("ssm1", "ssm3", "ssm4", "ssm9", "ssm10", "ssm13", "ssm15")
for (i in ssm_items) {
  my_data[i] <- reverse_items(array = my_data[i], rangelikert = c(1,4))
}
# my_data$ssm_tot <- (my_data$ssm1 +my_data$ssm2+ my_data$ssm3 +my_data$ssm4 + my_data$ssm5 +my_data$ssm6+my_data$ssm7 + my_data$ssm8+my_data$ssm9+
  # my_data$ssm10+ my_data$ssm11+my_data$ssm12+my_data$ssm13+my_data$ssm14 + my_data$ssm15)/15
my_data$ssm_tot <- rowMeans(my_data[startsWith(names(my_data), "ssm")])

# - sdq
sdq_items <- c("sdq7", "sdq11", "sdq14", "sdq21", "sdq25")
for (i in sdq_items) {
  my_data[i] <- reverse_items(array = my_data[i], rangelikert = c(0,2))
}
# my_data$sdq7 = abs(my_data$sdq7 - 2)
# my_data$sdq11 = abs(my_data$sdq11 - 2)
# my_data$sdq14 = abs(my_data$sdq14 - 2)
# my_data$sdq21 = abs(my_data$sdq21 - 2)
# my_data$sdq25 = abs(my_data$sdq25 - 2)

my_data$int_tot<-my_data$sdq3+my_data$sdq6+my_data$sdq8 +my_data$sdq11+ my_data$sdq13+my_data$sdq14 + 
  my_data$sdq16+my_data$sdq19+my_data$sdq23+my_data$sdq24
my_data$ext_tot<-my_data$sdq2+my_data$sdq5+ my_data$sdq7+my_data$sdq10 +my_data$sdq12+my_data$sdq15 +
  my_data$sdq18+my_data$sdq21+ my_data$sdq22 +my_data$sdq25

my_data =my_data[, c("id", "grade", "gender", "ssm_tot", "int_tot", "ext_tot")]

# Descriptive stat
summary(my_data)
table(my_data$gender, my_data$grade) 

library('ggplot2')
theme_set(theme_bw())

ggplot(my_data) +
  geom_bar(aes(x = ext_tot), fill = "firebrick", alpha = .8)
sd(my_data$ext_tot)

ggplot(my_data) +
  geom_bar(aes(x = ext_tot), fill = "firebrick", alpha = .8)+
  facet_grid(grade~gender)

tapply(my_data$ext_tot, INDEX = list(my_data$gender, my_data$grade), FUN = mean)
tapply(my_data$ext_tot, INDEX = list(my_data$gender, my_data$grade), FUN = sd)

ggplot(my_data) +
  geom_jitter(aes(x = ssm_tot, y = ext_tot), width= .01, height = .2) +
  facet_grid(grade~gender)

# Models
fit <-lm(ext_tot ~ ssm_tot * gender * grade, data = my_data)
summary(fit)

car::Anova(fit)
ext_emmeans <- emmeans::emtrends(fit, pairwise ~ grade*gender, var = 'ssm_tot', adjust = "mvt")
ext_emmeans

ggplot(as.data.frame(effects::effect("ssm_tot * gender * grade", fit, xlev = 20))) +
  geom_ribbon(aes(x= ssm_tot, ymin = lower, ymax=upper, fill =gender), alpha = .4) +
  geom_line(aes(x= ssm_tot, y = fit, color =gender), size = 1.2) +
  geom_rug(data = my_data[my_data$gender == 0,],
           aes(x= ssm_tot, color = gender, y = ext_tot), size = 1.2, alpha = .7,
           position = position_jitter(width = .02, height = 0), sides = "t") +
  geom_rug(data = my_data[my_data$gender == 1,],
           aes(x= ssm_tot, color = gender, y = ext_tot), size = 1.2, alpha = .7,
           position = position_jitter(width = .02, height = 0), sides = "b") +
  facet_grid(grade~.) +
  theme(legend.position = "top")



