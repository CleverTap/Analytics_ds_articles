###Please install the below library, if not already installed
###Importing the libraries in R Env
library(ggplot2)

###Importing the dataset for univariate analysis
dat <- read.csv(".../dat.csv")

###Histogram with 5 intervals
ggplot(dat, aes(x = x)) + geom_histogram(breaks = seq(35,85, by = 10),color = "red", fill = "yellow", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + ggtitle("Histogram of x") + ylab("Frequency") + xlab("")

###Number of observations between 65 and 75
sum(dat$x > 65 & dat$x <= 75)/nrow(dat)

###Histogram with 20 intervals
ggplot(dat, aes(x = x)) + geom_histogram(bins = 20,color = "red", fill = "yellow", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + ggtitle("Histogram of x") + ylab("Frequency") + xlab("")

###Histogram with 20 intervals & higher range for y-axis
ggplot(dat, aes(x = x)) + geom_histogram(aes(y =..density..),bins = 20,color = "red", fill = "yellow", alpha = 0.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + ggtitle("Histogram of x") + ylab("Frequency") + xlab("")

###########################################################################################

###Importing the dataset for bivariate analysis
dat1 <- read.csv(".../dat1.csv")
year <- dat1$Year; infant_death <- dat1$Infant.Deaths; cheese_consumption <- dat1$Cheese.Consumption

###Comparing trend for infant death and cheese consumption
par(mar=c(5,4,4,5)+.1)
plot(year, infant_death,type="l",col="red", main = "Infant Death and Cheese Consumption")
par(new=TRUE)
plot(year, cheese_consumption,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("cheese_consumption",side=4,line=3)

###Comparing trend for infant death and cheese consumption with higher range for y-axis
par(mar=c(5,4,4,5)+.1)
plot(year, infant_death,type="l",col="red", ylim = c(0, 10), main = "Infant Death and Cheese Consumption")
par(new=TRUE)
plot(year, cheese_consumption,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("cheese_consumption",side=4,line=3)

###Correlation 
cor(infant_death, cheese_consumption)

###########################################################################################

###Importing the dataset for correlation.. large dataset
dat2 <- read.csv(".../dat2.csv")

##index created to select the samples
c <- 1:nrow(dat2)

###row selection for sample 1
set.seed(123); ind1 <- sample(c,1e5 )

###row selection for sample 2
set.seed(567); ind2 <- sample(c[-ind1], 1e5)

###row selection for sample 3
set.seed(743); ind3 <- sample(c[-c(ind1, ind2)], 1e5)

###row selection for sample 4
set.seed(900); ind4 <- sample(c[-c(ind1, ind2, ind3)], 1e5)

###Correlation for entire dataset
cor(dat2$a, dat2$b)

###Correlation for sample 1
cor(dat2$a[ind1], dat2$b[ind1])

###Correlation for sample 2
cor(dat2$a[ind2], dat2$b[ind2])

###Correlation for sample 3
cor(dat2$a[ind3], dat2$b[ind3])

###Correlation for sample 4
cor(dat2$a[ind4], dat2$b[ind4])

