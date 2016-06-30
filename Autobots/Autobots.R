library(car)
library(Johnson)
library(ggplot2)

set.seed(123)
#Generating a weibull distribution sample
dat <- data.frame("x" = rweibull(1000,2,5))

###Histogram of x
ggplot(dat, aes(x = x)) + geom_histogram(aes(y =..density..),bins = 10,color = "#0517B2", fill = "#FF6B00", alpha = 0.5) +
  geom_density(col = "black") + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + ggtitle("Histogram of x") + ylab("") + xlab("")

###Running Box-Cox Transformation on x
dat$boxcox <- bcPower(dat$x,powerTransform(dat$x)$lambda)

###Histogram of transformed x using box-cox transformation
ggplot(dat, aes(x = boxcox)) + geom_histogram(aes(y =..density..),bins = 10,color = "#0517B2", fill = "#FF6B00", alpha = 0.5) +
  geom_density(col = "black") + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + ggtitle("Histogram of Transformed x using Box-Cox") + ylab("") + xlab("")

###Running Johnson Transformation on x
dat$johnson <- RE.Johnson(dat$x)$transformed

###Histogram of transformed x using johnson transformation
ggplot(dat, aes(x = johnson)) + geom_histogram(aes(y =..density..),bins = 10,color = "#0517B2", fill = "#FF6B00", alpha = 0.5) +
  geom_density(col = "black") + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + ggtitle("Histogram of Transformed x using Johnson") + ylab("") + xlab("")
