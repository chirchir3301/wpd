#loading the data

wpd <- read.csv("WPD.csv")
View(wpd)


#calling libraries

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
#checking & cleaning/wrangling dataset
is.na(wpd)

#summary
head(wpd)
summary(wpd)


#scatter plot of the data

ggplot (wpd, mapping = aes(x = NUMBER.OF.FARMERS, y =  NUMBER.OF.WATER.PANS))+
  geom_point(wpd, mapping = aes(x = NUMBER.OF.FARMERS, y =  NUMBER.OF.WATER.PANS))+
  geom_point(size = 2, color = "slateblue", alpha = 1)+
  labs(x = "NUMBER OF FARMERS", y = "NUMBER OF WATER PANS", title = "ANALYSIS OF PEARSON'S PRODUCT MOMENT OF CORRELATION")+
  geom_abline( mapping = NULL,
               data = NULL,
               show.legend = NULL,
               conf.int = TRUE)

#finding the coefficients
coef(lm(NUMBER.OF.FARMERS ~  NUMBER.OF.WATER.PANS, data = wpd))

lm(formula = NUMBER.OF.FARMERS ~  NUMBER.OF.WATER.PANS, data = wpd)

           # (Intercept) NUMBER.OF.WATER.PANS 
           # 3.1641917 
           # 0.4463435
 


  # computing Pearson"s coefficient of correlation/ parametric correlation

pcf <- cor.test(wpd$NUMBER.OF.FARMERS, wpd$NUMBER.OF.WATER.PANS, 
                method = "pearson")

print(pcf)


#PCF values
# Pearson's product-moment correlation
# 
# data:  wpd$NUMBER.OF.FARMERS and wpd$NUMBER.OF.WATER.PANS

# t = 6.6522, df = 7, p-value = 0.0002899

# alternative hypothesis: true correlation is not equal to 0

# 95 percent confidence interval:

#  0.6923068 0.9852953

# sample estimates:



            #       cor 
            # 0.9292032 














