#pulling the data for all 4 years and unifying the date column 
library(data.table)

data_2016 <- read.csv('/Users/pareshraut/Desktop/R/2016_brooklyn.csv',skip=4)
colnames(data_2016) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')
data_2016 <- data.table(data_2016)
data_2016 <- data_2016[ , date := as.Date(date,format = '%m/%d/%Y')]

data_2017 <- read.csv('/Users/pareshraut/Desktop/R/2017_brooklyn.csv',skip=4)
colnames(data_2017) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')
data_2017 <- data.table(data_2017)
data_2017 <- data_2017[ , date := as.Date(date,format = '%m/%d/%y')]

data_2018 <- read.csv('/Users/pareshraut/Desktop/R/2018_brooklyn.csv',skip=4)
colnames(data_2018) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')
data_2018 <- data.table(data_2018)
data_2018 <- data_2018[ , date := as.Date(date,format = '%m/%d/%y')]
data_2018$price = (gsub("\\$", "", data_2018$price))

data_2019 <- read.csv('/Users/pareshraut/Desktop/R/2019_brooklyn.csv',skip=4)
colnames(data_2019) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')
data_2019 <- data.table(data_2019)
data_2019 <- data_2019[ , date := as.Date(date,format = '%m/%d/%y')]

data_2020 <- read.csv('/Users/pareshraut/Desktop/R/2020_brooklyn.csv',skip=6)
colnames(data_2020) <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')
data_2020 <- data.table(data_2020)
data_2020 <- data_2020[ , date := as.Date(date,format = '%m/%d/%y')]


main_data <- rbind(data_2016,data_2017,data_2018,data_2019,data_2020)
sapply(main_data,class)
# ----------------------------------------------------------------
#clearing white spaces and trailing spaces from my data
main_data$neighborhood <- gsub('\\s+', '', main_data$neighborhood)
main_data$aptnum <- gsub('\\s+', '', main_data$aptnum)
main_data$bldclasscat <- gsub('\\s+', '', main_data$bldclasscat)
main_data$taxclasscurr <- gsub('\\s+', '', main_data$taxclasscurr)
main_data$bldclasscurr <- gsub('\\s+', '', main_data$bldclasscurr)
main_data$address <- gsub('\\s+', '', main_data$address)
main_data$taxclasssale <- gsub('\\s+', '', main_data$taxclasssale)
main_data$bldclasssale <- gsub('\\s+', '', main_data$bldclasssale)



# taxclasscurr,resunits,comunits,totunits,landsqft,grosssqft
# ,price are all of character type but we might need them to be of numeric type 

cols <- c("taxclasscurr","resunits","comunits","totunits","taxclasssale")

# Change class of certain columns


main_data$landsqft <- as.numeric(gsub(",", "", main_data$landsqft))
main_data$grosssqft <- as.numeric(gsub(",", "", main_data$grosssqft))
main_data$price <- as.numeric(gsub(",", "", main_data$price))



main_data <- data.table(main_data)
main_data_trans <- main_data[ ,(cols) := lapply(.SD, as.numeric), .SDcols = cols ]
main_data_trans <- subset(main_data_trans, select = -c(easement))

# summary(main_data_trans)

#following the filtering instructions according to part 1 
patterns <- c('^A','^R')

library(dplyr)
main_data_trans_sel <- filter(main_data_trans,grepl(paste(patterns,collapse = '|'),main_data_trans$bldclasssale))

main_data_trans_sel <- filter(main_data_trans_sel,((grepl('^1$',main_data_trans_sel$resunits)) & (grepl('^1$',main_data_trans_sel$totunits)))) 

main_data_trans_sel <- filter(main_data_trans_sel, grosssqft > 0 | is.na(grosssqft))

main_data_trans_sel <- filter(main_data_trans_sel, !is.na(price))

main_data_trans_sel <- filter(main_data_trans_sel, !(price==0))



# unique(main_data_trans_sel$resunits)
# main_data_trans_sel$log_price <- log(main_data_trans_sel$price)
# main_data_trans_sel <- filter(main_data_trans_sel, !is.infinite(log_price))

# creating a columns year age and month 

main_data_trans_sel$year <- format(as.Date(main_data_trans_sel$date, format="%Y-%m-%d"),"%Y")

main_data_trans_sel$age <- as.numeric(main_data_trans_sel$year) - main_data_trans_sel$yrbuilt

main_data_trans_sel$month <- format(as.Date(main_data_trans_sel$date, format="%Y-%m-%d"),"%m")

#creating a column of log prices 
main_data_trans_sel$log_price<-log(main_data_trans_sel$price)

#changing ages to 0 for all the observations where yrbult = year of sale
main_data_trans_sel$age[main_data_trans_sel$age == 2016] <- 0
main_data_trans_sel$age[main_data_trans_sel$age == 2017] <- 0
main_data_trans_sel$age[main_data_trans_sel$age == 2018] <- 0
main_data_trans_sel$age[main_data_trans_sel$age == 2019] <- 0
main_data_trans_sel$age[main_data_trans_sel$age == 2020] <- 0

# ---------------------------------------------------------------------------------
#understanding distribution of the price to remove outliers
library(ggplot2)
library(ggspatial)
library(ggthemes)

ggplot(main_data_trans_sel, aes(x=price)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot


ggplot(main_data_trans_sel, aes(x=log_price)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

# ----------------------------------------------------------------------------------
#finding correlation between numeric variables and response variable 
main_data_trans_sel_corr <- subset(main_data_trans_sel, select = -c(neighborhood,bldclasscat,bldclasscurr,address,date,bldclasssale,aptnum))
# sapply(main_data_trans_sel_corr,class)
cols <- c("year","month")
main_data_trans_sel_corr <- main_data_trans_sel_corr[ ,(cols) := lapply(.SD, as.numeric), .SDcols = cols ]
# install.packages("reshape2")
library(reshape2)
corr_mat <- round(cor(main_data_trans_sel_corr),2)
# corr_mat
# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
head(melted_corr_mat)
# plotting the correlation heatmap
library(ggplot2)
ggp <-ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +geom_tile()
ggp + scale_fill_gradient(low = "white", high = "black")


# ----------------------------------------------------------------
#we create a new df and begin analysing neighborhood and zipcodes, filter the zipcodes which are 0 
library(tidyr)
main_data_trans_sel_test <- main_data_trans_sel
main_data_trans_sel_test <- filter(main_data_trans_sel_test, !(zip==0))


ggplot(main_data_trans_sel_test, aes(x=neighborhood)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


neigh_df <- aggregate(main_data_trans_sel_test$neighborhood, by=list(main_data_trans_sel_test$neighborhood), FUN=length)
zip_df <- aggregate(main_data_trans_sel_test$zip, by=list(main_data_trans_sel_test$zip), FUN=length)

# head(neigh_df)
# head(zip_df)

library(dplyr)
neigh_df %>% arrange(desc(x))
zip_df %>% arrange(desc(x))

#creating bins for neighborhoods and zipcodes based on the mean prices 

data_group <- main_data_trans_sel %>%                                 # Group data
  group_by(neighborhood) %>%
  dplyr::summarize(mean_price = mean(price)) %>% 
  as.data.frame()


data_group_zip <- main_data_trans_sel_test %>%                                 # Group data
  group_by(zip) %>%
  dplyr::summarize(mean_price = mean(price)) %>% 
  as.data.frame()


# data_group_zip_neigh <- main_data_trans_sel %>%                                 # Group data
#   group_by(neighborhood,zip) %>%
#   dplyr::summarize(mean_price = mean(price)) %>% 
#   as.data.frame()

#finding the 10% cut points for binning 
quantile(data_group$mean_price,  probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
quantile(data_group_zip$mean_price,  probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

#labeling the bins as numeric values
data_group<- data_group %>% mutate(nbh_bin = cut(mean_price, breaks=c(0,488537.7,555073.1, 667332.1,773755.6,916847.6, 1058186.6,1206267.7,1336532.9,1702547.4,3230790.3), labels = FALSE))
data_group_zip<- data_group_zip %>% mutate(zip_bin = cut(mean_price, breaks=c(0,442930.1,630000.2,667860.7,755532.4,833674.9, 878921.9,1135898.0,1162418.5,1312107.6,2032829.6), labels = FALSE))

#clean bins
data_group<- subset(data_group, select = -c(mean_price))
data_group_zip<- subset(data_group_zip, select = -c(mean_price))

# data_group_cat&cat_bin <- c(5,4,8,9,3,7,6,1,2)

#megre with main data frame and filter the outlying prices 
main_data_trans_sel_test <- merge(x = main_data_trans_sel_test, y = data_group, by = "neighborhood")
main_data_trans_sel_test <- merge(x = main_data_trans_sel_test, y = data_group_zip, by = "zip")
main_data_trans_sel_test <- filter(main_data_trans_sel_test, log_price > 11)
main_data_trans_sel_test <-  main_data_trans_sel_test %>% drop_na(c(nbh_bin, landsqft, grosssqft, age, block, lot, zip_bin, bldclasscat, yrbuilt))


# ----------------------------------------------------------------
#building the model 
model <- lm(sqrt(price) ~ factor(nbh_bin)+landsqft+sqrt(grosssqft)+age+block+lot+factor(zip_bin)+factor(bldclasscat)+yrbuilt+I(age^2), data = main_data_trans_sel_test)
summary(model)

#calculating the rmse of the untransformed response variable 
RSS <- c(crossprod((model$residuals)^2))
MSE <- RSS / length(model$residuals)
RMSE <- sqrt(MSE)

RMSE

#testing for violations of ols regression
require(lmtest)
hist(model$residuals)
ks.test(model$residuals/summary(model)$sigma,pnorm)
plot(model$fitted.values,model$residuals)
bptest(model)


saveRDS(list(model=model, data=main_data_trans_sel_test), file='pareshraut.RDS') 
# readRDS('pareshraut.RDS')

# library(sandwich)
# rob_sum <- coeftest(model, vcov = vcovHC(model, type = 'HC0'))
# rob_sum
# hist(rob_sum$residuals)
# plot(log_price~factor(neighborhood),data=main_data_trans_sel)
# model <- lm(log_price ~ factor(neighborhood)+sqrt(landsqft)+sqrt(grosssqft)+age,data = main_data_trans_sel)
# model1 <- lm(price ~ factor(neighborhood) , data = data_2016)
# landsqft+grosssqft+age
# plot(log(main_data_trans_sel$price),(main_data_trans_sel$age))
# plot(data_poverty1$proppov,log(data_poverty1$propnet))
# plot(sqrt(data_poverty1$proppov),log(data_poverty1$propnet))
# factor(bldclasssale)
# model2 <- lm(price ~ landsqft+grosssqft+age+block+lot, data= main_data_trans_sel)
# summary(model2)
# model3 <- lm(price ~ factor(taxclasscurr), data= main_data_trans_sel)
# summary(model3)
# hist(main_data_trans_sel$price)
# na.action(model)
# unique(data_test_new$bldclasscat)
# count(which(! complete.cases(main_data_trans_sel_test$price)))
# summary(data_group)

# PART 2----------------------------------PART 2--------------------------------- PART 2

#creating a test data frame and changing the month and year col to numeric 
datap2 <- main_data_trans_sel_test
datap2$month <- as.numeric(datap2$month)
datap2$year <- as.numeric(datap2$year)
library(dplyr)


#creating a variable of quarter and year to identify which year and quarter does the observation belong in 
datap2 <- datap2 %>%
  mutate(quarter = case_when(year == 2016 & (month == 3 | month ==2 | month == 1)  ~ 'q1 2016', 
                           year == 2016 & (month == 4 | month ==5 | month == 6) ~ 'q2 2016', 
                           year == 2016 & (month == 7 | month ==8 | month == 9)  ~ 'q3 2016', 
                           year == 2016 & (month == 10 | month ==11 | month ==12)  ~ 'q4 2016', 
                           year == 2017 & (month == 3 | month ==2 | month == 1)  ~ 'q1 2017', 
                           year == 2017 & (month == 4 | month ==5 | month == 6)  ~ 'q2 2017', 
                           year == 2017 & (month == 7 | month ==8 | month == 9)  ~ 'q3 2017', 
                           year == 2017 & (month == 10 | month ==11 | month ==12)  ~ 'q4 2017',
                           year == 2018 & (month == 3 | month ==2 | month == 1)  ~ 'q1 2018', 
                           year == 2018 & (month == 4 | month ==5 | month == 6)  ~ 'q2 2018', 
                           year == 2018 & (month == 7 | month ==8 | month == 9)  ~ 'q3 2018', 
                           year == 2018 & (month == 10 | month ==11 | month ==12)  ~ 'q4 2018',
                           year == 2019 & (month == 3 | month ==2 | month == 1)  ~ 'q1 2019', 
                           year == 2019 & (month == 4 | month ==5 | month == 6)  ~ 'q2 2019', 
                           year == 2019 & (month == 7 | month ==8 | month == 9)  ~ 'q3 2019', 
                           year == 2019 & (month == 10 | month ==11 | month ==12)  ~ 'q4 2019',
                           year == 2020 & (month == 3 | month ==2 | month == 1)  ~ 'q1 2020', 
                           year == 2020 & (month == 4 | month ==5 | month == 6)  ~ 'q2 2020', 
                           year == 2020 & (month == 7 | month ==8 | month == 9)  ~ 'q3 2020', 
                           year == 2020 & (month == 10 | month ==11 | month ==12)  ~ 'q4 2020',
                           #if none of the conditions satisfy from above assign "unknown"
                           TRUE ~ 'unknown'))

datap2$quarter <- as.factor(datap2$quarter)
datap2$quarter <- relevel(datap2$quarter, ref = 'q3 2020')


model1 <- lm(sqrt(price) ~ factor(nbh_bin)+landsqft+sqrt(grosssqft)+age+block+lot+factor(zip_bin)+factor(bldclasscat)+quarter, data = datap2)
summary(model1)
plot(model1)


# plot(model1)
# unique(main_data_trans_sel_test$neighborhood)
# class(datap2$month)



