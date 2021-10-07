library(mice)
library(VIM)
library(lattice)

#load BRFSS dataset and discard variables that will not be used
BRFSS <- read.csv(
  "https://raw.githubusercontent.com/kijohnson/ADA-FAll-2020/master/BRFSS2017_10percent_v2.csv")

BRFSS_k<-BRFSS[c("rowID", "diabetes", "sex", "X_AGE80", "ht_meters", "wtkg")] #keeps only these variables

BRFSS_k$diab_bin[
  BRFSS_k$diabetes=="No"]<-0 #Assign 0 to those who responded no to the diabetes question

BRFSS_k$diab_bin[
  BRFSS_k$diabetes=="Yes"]<-1 #Assign 1 to those who responded yes to the diabetes question

BRFSS_k$diab_bin<-as.factor(BRFSS_k$diab_bin) #make this variable categorical

class(BRFSS_k$diab_bin)

#remove diabetes from dataset
BRFSS_k<-BRFSS_k[ , !(names(BRFSS_k) %in% c("diabetes"))]

#We want the dataset to only contain variables used for the imputation.

#1
md.pattern(BRFSS_k)
#2
marginplot(BRFSS_k[,c('ht_meters','wtkg')],col = c('blue','red','black'),cex=1,cex.lab=1,
           cex.numbers = 0.7, pch = 20) #marginplot need three colors 
#3
pbox(BRFSS_k, pos=3, numbers = F)
#4

#All of steps 1-6 are performed to allow calculation of bmi from imputed height and weight. If you have no calculated variables from other variables in your analyses, you can skip these steps. This is tricky to figure out so the code is provided to you and explained.

#1. In the first line of code, we are simply creating a BMI variable in the R environment and assigning it a value of NA. We are doing this because we want this variable in step 2 to be in the starting dataset created in step 2 that we are using for imputation.
bmi<-NA 

#2. We are adding this variable to the BRFSS_k dataset using cbind and creating a new dataset that we will use for imputation.
BRFSS_i<-cbind(BRFSS_k, bmi) 

#3. In this line of code, we are running the imputation with maxit set to 0. What this does is allow us to extract the imputation settings—specifically the regression methods that will be used to impute each variable (referred to sometimes as a dry run). But because maxit is set to 0, the imputation runs super fast. The default maxit is 5 for models used to impute missing values to converge, which is much slower, especially for large datasets. Since we only want the methods used for imputation and not the imputation datasets, we can set it to 0 to get it to run fast. When we print the ini object, we get the imputation settings including the methods used to impute each variable. For BMI, the reason it does not come up with an imputation method is because there are no values for BMI (they are all NA as set in steps 1 and 2) for the algorithm to determine which method to use. If you type class(BMI), it comes up with a class of “logical”, a class that is not assigned an imputation method by default in the MICE algorithm. If we would have left the original BMI in the dataset where there were values calculated from height and weight, a method of PMM would have been assigned but this is an issue to impute BMI missing values because it would not correspond to BMI calculated from the imputed height and weight. So we take care of this in the subsequent lines of code, resulting in imputed datasets with bmi calculated from height and weight.
ini<-mice(BRFSS_i, maxit=0) 
ini

#4. In the next step, mice::complete(ini), we extract the complete data and methods from the first imputed dataset, which is needed to assign a method for bmi in steps 5 and 6. The reason we use mice:: before the complete function is to ensure that the complete function comes from the mice package.This is only necessary if you have different packages installed and libraries open with functions named the same.
mice::complete(ini) 

#5. In this step, we are assigning the imputation methods extracted (ini$meth) in step 4 to an object called meth.
meth<-ini$meth

#6. In this step, we are assigning a method with the below formula to calculate bmi, the tilde I indicates a passive imputation method (i.e. use the formula in parentheses to ‘impute the values for BMI’). We should specify a formula for variables that are derived from other variables. The reason why we go to the trouble of creating bmi now is so that it is present in each imputed dataset rather than after is because we would have to add the calculated bmi to each imputed dataset, which would create a lot of extra data management in order to create pooled estimates in analyses using this variable from the imputed datasets.
meth["bmi"]<-"~I(wtkg/(ht_meters*ht_meters))"


#7. Here we actually do 5 imputations (m=5, the default) using the methods assigned to each variable in steps 5 and 6. You can actually skip all of steps 1-6 if you have no variables that you are deriving from other variables and/or you are happy with the default imputation methods. We set a seed so we can create the exact same imputation datasets every time we run the code below.
imp <- mice(BRFSS_i, meth=meth, seed=10000) 

#8. Here we just look at the imputation results as we did in step 3. Only the first window that comes up when you run imp is important. Notice that the method used to calculate bmi is now shown in contrast to step 3 above.
imp

#9. In this final step, the code extracts the complete data from the first imputed dataset, for observations in the original dataset where either height or weight were missing (and as a consequence bmi). We can examine them to make sure they look reasonable.
mice::complete(imp)[is.na(BRFSS_k$ht_meters)|is.na(BRFSS_k$wtkg),]

#5
##yes

#6
stripplot(x=imp, data = wtkg ~.imp, jit=T, pch=20, xlab = "1=No Impute, Imputation Number")
stripplot(x=imp, data =ht_meters ~.imp, jit=T, pch=20, xlab = "1=No Impute, Imputation Number")

#7
fit = with(imp,lm(ht_meters~sex))
pool(fit)
summary(pool(fit))
complete_fit = with(BRFSS_k,lm(ht_meters~sex))
summary(complete_fit)
