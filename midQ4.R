#import dataset
library(readr)
BRFSS  = read_csv(file = 'https://raw.githubusercontent.com/kijohnson/ADA-Fall-2021/master/BRFSS2017_10percent_v.csv')

#get median of height by sex
with(BRFSS, by(ht_meters, SEX, median, na.rm=T))

#use plot to show median of height by sex
library(ggplot2)
ggplot(BRFSS, aes(x=SEX, y=ht_meters)) +
  geom_boxplot(aes(group = SEX)) +
  scale_x_continuous(breaks = c(1,2), limits = c(0,3), labels=c('Male',"Female")) +
  ylab('Height (meters)')