## Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90. What is the mean of Solar.R in this subset?
* a<-hw1_data.2
* b<-a[a$Ozone<31,]
* c<-b[b$Temp>90,]
* good <-complete.cases(c$Solar.R)
* fin2 <-c[good,]
* mean(fin2$Solar.R)

## What is the mean of "Temp" when "Month" is equal to 6?

* x<-a[a$Month==6,]
* mean(y$Temp)

## What is ozone max value when "Month" is equal to 5?

* ses <-a[a$Month==5,]
* good <- complete.cases(ses$Ozone)
* aywa <- ses[good,]
* max(aywa$Ozone)