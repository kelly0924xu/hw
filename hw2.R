library(readr)

#1
C1survey = read_csv(file = 'https://raw.githubusercontent.com/kijohnson/ADA-Fall-2021/master/Class%201%20Survey%20Fall%202021.csv')
#2
dim(C1survey)
print("28 people 27 questions")
#3
names(C1survey)[22] = ('bday')
names(C1survey)[23] = ('bmonth')
#4
table(sapply(C1survey,class))
#5
C1survey$bday = as.numeric(C1survey$bday)
C1survey$bmonth = as.numeric(C1survey$bmonth)

summary(C1survey$bday)
sum(is.na(C1survey$bmonth))

median(C1survey$bday,na.rm=T)
median(C1survey$bmonth,na.rm=T)
#6
x = C1survey$bmonth
bseason = ifelse((x>11|x<3),'winter',ifelse((x>=3&x<6),'spring',(ifelse(x>5&x<9,'summer','fall')
                                                             )
                                          )
                 )
table(bseason)

bseason = factor(bseason,levels=c('spring','summer','fall','winter'))
levels(bseason)

plot(C1survey$bmonth,bseason,yaxt='n')
axis(2,at=c(1,2,3,4),label=c('spring','summer','fall','winter'))

##factor VS as.factor
##?levels mistake: substitute or rearrange?
