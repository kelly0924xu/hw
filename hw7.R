#install.packages('odds.n.ends')
library('odds.n.ends')
library('dplyr')
library('ggplot2')
library('readr')

#1
BRFSS  = read_csv(file = 'https://raw.githubusercontent.com/kijohnson/ADA_Spring_2019/master/BRFSS2017_10percent_v2.csv')
BRFSS = BRFSS[,-1]
#2
summary(BRFSS)
#3
BRFSS$seatb_cat = ifelse(BRFSS$seatbelt == 'Always',0,ifelse(BRFSS$seatbelt %in% c("Never",'Sometimes','Seldom','Nearly always'),1,NA))
with(BRFSS, table(seatb_cat,seatbelt,useNA = 'always'))
#4
ggplot(data = BRFSS, aes(x=seatb_cat,y=bmi,group = seatb_cat)) +
  geom_boxplot()
#5
BRFSS_ex = na.omit(BRFSS)
BRFSS_ex<-BRFSS_ex[which(BRFSS_ex$sex!="Refused"),]
BRFSS_ex<-BRFSS_ex[which(BRFSS_ex$age_cat!="Don't know/refused/missing"),]
#6
#both appropriate
#7
onlybmi = with(BRFSS_ex,glm(seatb_cat ~ bmi, family = 'binomial'))
odds.n.ends(onlybmi)
#8
inagesex = with(BRFSS_ex,glm(seatb_cat ~ bmi + sex + age_cat, family = 'binomial'))
odds.n.ends(inagesex)
#9
plot(inagesex, which=4, id.n=5, col="red")
#10
outsider = c(589,7032,9454,11700,36038)
BRFSS_noout = BRFSS_ex[-outsider,]
inagesex_noout = with(BRFSS_noout,glm(seatb_cat ~ bmi + sex + age_cat, family = 'binomial'))
odds.n.ends(inagesex_noout)
#11
table(BRFSS_ex$seatb_cat)
set.seed(1)
BRFSS_zero = sample_n(BRFSS_ex[which(BRFSS_ex$seatb_cat==0),], size=4744,)
BRFSS_balance = rbind(BRFSS_zero,BRFSS_ex[which(BRFSS_ex$seatb_cat==1),])
table(BRFSS_balance$seatb_cat)
#12
#a
inagesex2 = with(BRFSS_balance,glm(seatb_cat ~ bmi + sex + age_cat, family = 'binomial'))
#b
#4914 and 4574
#c
odds.n.ends(inagesex2)
#d
g1 = odds.n.ends(inagesex, rocPlot=TRUE, predProbPlot=TRUE)
g2 = odds.n.ends(inagesex2, rocPlot=TRUE, predProbPlot=TRUE)



