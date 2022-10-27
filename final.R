install.packages("lme4")
install.packages("effects")
install.packages("corrplot")
install.packages("stargazer")
install.packages("car")
install.packages("MASS")
install.packages("ggplot2")
install.packages("texreg")
library(lme4)
library(effects)
library(corrplot)
library(stargazer)
library(car)
library(MASS)
library(ggplot2)
library(texreg)
library(dplyr)

Data <- read.csv2("~/Desktop/demo1.csv")

### Remove outliers ###

# create detect outlier function
#detect_outlier <- function(x) {
 # Quantile1 <- quantile(x, probs=.25)
  #Quantile3 <- quantile(x, probs=.75)
  #IQR = Quantile3-Quantile1
  #x > Quantile3 + (IQR*1.5) | x < Quantile1 - (IQR*1.5)}

# create remove outlier function
#remove_outlier <- function(dataframe,
                           # columns=names(dataframe)) {
  
  # for loop to traverse in columns vector
 # for (col in columns) {
    
    # remove observation if it satisfies outlier function
   # dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  #}
  
  # return dataframe
  #print("Remove outliers")
  #print(dataframe)
#}

#Data1 <- remove_outlier(Data, c('expd','income')); Data1
par(mfrow = c(2,2))  
hist(Data$math,col = "steelblue3", main ="Math Score")
hist(Data$time,col = "steelblue3", main ="Time on Studying")
hist(Data$school,col = "steelblue3", main ="The number of Classes")

#Descriptive Statistic

cor(Data[ ,3:5])
summary(Data[ ,3:5])

dev.off() 
K <- split(Data, Data$pro_code)
corrplot(cor(Data[3:5]), method = "number",diag = FALSE,type = "lower")

par(mfrow = c(2,3)) 
Z <- data.frame(K[1])
corrplot(cor(Z[3:4]), method = "number",diag = FALSE,type = "lower")

Z <- data.frame(K[2])
corrplot(cor(Z[3:4]), method = "number",diag = FALSE,type = "lower")

Z <- data.frame(K[3])
corrplot(cor(Z[3:4]), method = "number",diag = FALSE,type = "lower")

Z <- data.frame(K[4])
corrplot(cor(Z[3:4]), method = "number",diag = FALSE,type = "lower")
         
Z <- data.frame(K[5])
corrplot(cor(Z[3:4]), method = "number",diag = FALSE,type = "lower")

Z <- data.frame(K[6])
corrplot(cor(Z[3:4]), method = "number",diag = FALSE,type = "lower")

sm <- group_by(Data, pro_code) %>% summarize( n = n())
summary(sm$n)


#########regression########

####Components of Variance####

#Null Model
null <- lmer(Data$math ~ 1 + (1 | Data$pro_code), data=Data, REML = FALSE)
summary(null)

#Intra-corr
intr <- VarCorr(null); intr
s2u <- as.numeric(intr)
s2e <- sigma(null)^2
s2u/(s2u + s2e) # icc

#Grand mean
schools <- group_by(Data, pro_code) %>% summarize(sn = n(), sm = mean(math))
summarize(schools, mw = weighted.mean(sm, sn), mu = mean(sm))
schools <- mutate(schools, sw = 1/(s2u + s2e/sn))
summarize(schools, mle =  weighted.mean(sm, sw))

####Random intercepts and slope####

#Center Time on overall mean
Data <- mutate(Data,
                      time_vc = time - mean(time))
#Estimate for each school
EC <- group_by(Data, pro_code) %>% do( reg = lm(math ~ time_vc, data = .) )
ols <- data.frame(id = EC[[1]], t(sapply(EC[[2]],coef)))
names(ols) <- c("school", "sa", "sb")

Data <- left_join(Data, ols, by = "school") %>%  mutate(fv = sa + sb * time_vc)
ggplot(Data, aes(time_vc, fv, group = pro_code)) + geom_line() + ggtitle("School Regressions")

#Random Intercept
ri <- lmer(math ~ time_vc + (1 | pro_code), data = Data, REML = FALSE); ri

Data <- mutate(Data, fv1 = predict(ri))
b <- fixef(ri)
re <- ranef(ri)$pro_code[,1]
check <- b[1] + re[Data$pro_code] + b[2] * Data$time
cbind(Data$fv1[1:5], check[1:5])
ggplot(Data, aes(time_vc, fv1, group = factor(pro_code))) + geom_line() +
    ggtitle("Random Intercept")
ols <- mutate(ols, sab = b[1] + re)
ggplot(ols, aes(sa, sab)) + geom_point() + 
     geom_abline(intercept=0, slope=1) + ggtitle("School Intercepts")

##Random slope
rs <- lmer(math ~ time_vc + (time_vc | pro_code), data=Data, REML=FALSE);rs

Data <- mutate(Data, fv2 = predict(rs))
re <- ranef(rs)$pro_code
b <- fixef(rs)
map <- Data$pro_code
check <- (b[1] + re[map, 1]) + (b[2] + re[map ,2]) * Data$pro_code
cbind(Data$fv2[1:5], check[1:5])
ggplot(Data, aes(time_vc, fv2, group=pro_code)) + geom_line() +
     ggtitle("Random Slopes")
ols <- mutate(ols, sa2 = b[1] + re[,1], sb2 = b[2] + re[,2])
ggplot(ols, aes(sa, sa2)) + geom_point() + ggtitle("Intercepts")
ggplot(ols, aes(sb, sb2)) + geom_point() + ggtitle("Slopes")

#how it effect 
school2 <- filter(Data, pro_code ==2) %>%
     mutate(avg = b[1] + b[2] * time_vc)
ggplot(school2, aes(time_vc, math)) + geom_point() +
     geom_line(aes(time_vc, fv,  color="Within")) + 
     geom_line(aes(time_vc, fv2, color="EB")) +
     geom_line(aes(time_vc, avg, color="Average")) +
    guides(color=guide_legend(title="Line"))

#####A level 2 predictor#####
size <- (group_by(Data, pro_code) %>% slice(1))$school
school1 <- data.frame(meansize = mean(size), sdsize = sd(size))
Data <- mutate(Data, size_vc = school - school1$meansize)  
#REG
cli <- lmer(math ~ time_vc*size_vc + (1 + time_vc | pro_code), 
               data = Data, REML = FALSE); cli
#
Data <- mutate(Data, yhat3 = predict(cli))
ggplot(group_by(Data, pro_code), aes(time_vc, yhat3, group=pro_code)) + 
     geom_line() + ggtitle("Random Coefficient Model with SES")
#
b <- fixef(cli)
sr <- attr(VarCorr(cli)$pro_code, "stddev")
af <-  b[3] * school1$sdsize
bf <-  b[4] * school$sdsize
x <- seq(-7.8, 6.1, 0.2)

d <- data.frame(time_vc = x,
                   f1 = (b[1] + af + sr[1]) + (b[2] + bf - sr[2]) * x,
                       f2 = (b[1] + af - sr[1]) + (b[2] + bf + sr[2]) * x,
                          f3 = (b[1] - af + sr[1]) + (b[2] - bf - sr[2]) * x,
                  f4 = (b[1] - af - sr[1]) + (b[2] - bf + sr[2]) * x)
x = d$time_vc[1] - 0.5

ggplot(d, aes(time_vc, f1)) + xlim(-10, 6) + ylab("math score") +
     geom_line(col = "steelblue3") + geom_line(aes(time_vc,f2),col = "steelblue3") +
     geom_line(aes(time_vc,f3),col = "palevioletred", linetype=2, ) +
     geom_line(aes(time_vc,f4),col = "palevioletred", linetype=2)  +
     geom_text(aes(x, f1[1]),label="+ses +re", hjust=1,  col = "steelblue3") +
     geom_text(aes(x, f1[2]),label="-ses +re", hjust=1, col = "palevioletred") +
     geom_text(aes(x, f2[1]),label="+ses +re", hjust=1, col = "steelblue3") +
     geom_text(aes(x, f2[2]),label="-ses +re", hjust=1, col = "palevioletred") 
