#0
library('readr')
BRFSS = read_csv(file = 'https://raw.githubusercontent.com/kijohnson/ADA-Fall-2021/master/BRFSS2017_10percent_v.csv')
sex_fa = factor(BRFSS$SEX, labels = c('male','female','unknown'))
BRFSS$sex_fa = sex_fa
table(BRFSS$sex_fa)
#1
library(ggplot2)
t1 = table(BRFSS$income)
inname = names(t1)
incount = as.numeric(t1)
d1 = data.frame(inname, incount)
d2 = as.data.frame(t1) #t2 equals to t1
ggplot(data = d1, aes(x = reorder(inname,-incount), y = incount)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 45))

#2
ggplot(data = BRFSS, aes(x = ht_meters, y = wtkg, color = age_cat)) +
  geom_point()

#3
ggplot(data = BRFSS, aes(x = ht_meters, y = wtkg, color = sex_fa)) +
  geom_point() +
  facet_grid( ~ sex_fa)

#4
income.employed.count<-as.data.frame(table(BRFSS$employed, BRFSS$income))
colnames(income.employed.count)<-c("employed", "income", "freq")
ggplot(data = income.employed.count, aes(x = income, y = employed, size = freq))+
  geom_point() +
  theme(axis.text.x = element_text(angle = 45))

#5
BRFSS$diabetes_short <- factor(
  BRFSS$diabetes,
  levels = c("Yes", "Yes, but female told only during pregnancy", 
             "No", "No, pre-diabetes or borderline diabetes",
             "Don't know/Not Sure", "Refused"),
  labels = c(
    "Yes",
    "Yes, pregnancy",
    "No",
    "No, pre-diabetes",
    "Unknown",
    "Refused")
)
ggplot(data = BRFSS, aes(x = wtkg, y = diabetes_short,fill = diabetes_short)) +
  geom_violin() +
  geom_boxplot() +
  xlab('Weight[kg]') + ylab('Diabetes Status') +
  guides(color = F) +
  scale_fill_brewer(palette="Set1") + scale_color_brewer(palette="Set1", name="Diabetes Status") +
  theme_bw()
  
  
