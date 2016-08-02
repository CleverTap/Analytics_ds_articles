
###Please install the below library, if not already installed

###Importing the libraries in R Env
library(L1pack)
library(ggplot2)


###############################Case (a)################################################
dat <- data.frame("x" = c(10.0, 8.5, 15.0, 20.0, 6.0, 21.0, 3.0, 38.0, 11.0, 42.0),
                  "y" = c(13, 14, 19, 20, 10, 16, 7, 24, 10, 28))

###fitting a linear regression line using L2
fit <- lm(dat$y ~ dat$x); summary(fit)

###fitting a linear regression line using L1
fit1 <- l1fit( dat$x,dat$y); fit1

###Comparing the regression line visually
ggplot(dat, aes(x = x, y = y)) + geom_point(col = "red") + 
  geom_text(aes(label = paste(x,y, sep=",")), vjust = 0, nudge_y = 0.5, check_overlap = T) +
  geom_abline(slope = fit1$coefficients[2],intercept = fit1$coefficients[1]) +
  geom_smooth(method = "lm", col = "cyan2", se = F) +  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ggtitle(paste("(a) L1: y =", round(fit1$coefficients[1], 3), "+", round(fit1$coefficients[2],3), 
                "* x; L2: y =", round(fit$coefficients[1],3), "+", round(fit$coefficients[2], 3), "* x")) + 
  ylab("y")

################################Case (b)##################################################

dat$y1 <- dat$y

###Introducing an outlier
dat$y1[5] <- 60

###fitting a linear regression line using L2
fit <- lm(dat$y1 ~ dat$x); summary(fit)

###fitting a linear regression line using L2
fit1 <- l1fit( dat$x,dat$y1); fit1

###Comparing the regression line visually
ggplot(dat, aes(x = x, y = y1)) + geom_point(col = "red") + 
  geom_text(aes(label = paste(x,y, sep=",")), vjust = 0, nudge_y = 0.5, check_overlap = T) +
  geom_abline(slope = fit1$coefficients[2],intercept = fit1$coefficients[1]) +
  geom_smooth(method = "lm", col = "cyan2", se = F) +  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ggtitle(paste("(a) L1: y =", round(fit1$coefficients[1], 3), "+", round(fit1$coefficients[2],3), 
                "* x; L2: y =", round(fit$coefficients[1],3), "+", round(fit$coefficients[2], 3), "* x")) + 
  ylab("y")
#################################Case (c)###############################################

dat$y2 <- dat$y

###Introducing an outlier
dat$y2[10] <- 70

###fitting a linear regression line using L2
fit <- lm(dat$y2 ~ dat$x); summary(fit)

###fitting a linear regression line using L2
fit1 <- l1fit( dat$x,dat$y2); fit1

###Comparing the regression line visually
ggplot(dat, aes(x = x, y = y2)) + geom_point(col = "red") + 
  geom_text(aes(label = paste(x,y, sep=",")), vjust = 0, nudge_y = 0.5, check_overlap = T) +
  geom_abline(slope = fit1$coefficients[2],intercept = fit1$coefficients[1]) +
  geom_smooth(method = "lm", col = "cyan2", se = F) +  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ggtitle(paste("(a) L1: y =", round(fit1$coefficients[1], 3), "+", round(fit1$coefficients[2],3), 
                "* x; L2: y =", round(fit$coefficients[1],3), "+", round(fit$coefficients[2], 3), "* x")) + 
  ylab("y")
#################################Case (d)##############################################

dat$y3 <- dat$y

###Introducing an outlier
dat$y3[10] <- 15

###fitting a linear regression line using L2
fit <- lm(dat$y3 ~ dat$x); summary(fit)

###fitting a linear regression line using L2
fit1 <- l1fit( dat$x,dat$y3); fit1

###Comparing the regression line visually
ggplot(dat, aes(x = x, y = y3)) + geom_point(col = "red") + 
  geom_text(aes(label = paste(x,y, sep=",")), vjust = 0, nudge_y = 0.5, check_overlap = T) +
  geom_abline(slope = fit1$coefficients[2],intercept = fit1$coefficients[1]) +
  geom_smooth(method = "lm", col = "cyan2", se = F) +  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ggtitle(paste("(a) L1: y =", round(fit1$coefficients[1], 3), "+", round(fit1$coefficients[2],3), 
                "* x; L2: y =", round(fit$coefficients[1],3), "+", round(fit$coefficients[2], 3), "* x")) + 
  ylab("y")
#########################################################################################



