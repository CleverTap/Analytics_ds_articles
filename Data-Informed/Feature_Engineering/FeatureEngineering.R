
###Please install the below libraries, if not already installed
###Importing the libraries in R Env
library(NbClust)
library(rattle)
library(dplyr)
library(ggbiplot)
library(car)
library(rpart)
library(caret)
library(psych)

##########################################################################################

#####Trick 1 - Standardizing

data(wine, package="rattle")

summary(wine)

###setting the seed and running the NbClust function to identify the right cluster size
##for the unstandardized data
set.seed(1234)
nc <- NbClust(wine[,-1], min.nc=2, max.nc=15, method="kmeans", index = "all")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]), 
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")


####standardizing the wine dataset
wine1 <- data.frame(scale(wine[,-1]))
head(wine1)
summary(wine1)

###setting the seed and running the NbClust function to identify the right cluster size
###for the standardized data
set.seed(1234)
nc <- NbClust(wine1, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), 
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

###running the k-means with the best cluster size
set.seed(1234)
fit <- kmeans(wine1, 3, nstart=25)  

###Size of the clusters
fit$size

####Confusion Matrix comparing the type and cluster
ct <- table(wine$Type, fit$cluster)
ct

##########################################################################################

####Trick 2 - Binning

bin <- read.csv(".../binning.csv")
bin$Interact <- as.factor(bin$Interact)

head(bin)

###Creating a new variable Age_Group to bin Age
bin$Age_Group <- NA

summary(bin$Age)

####Assigning values to Age_Group based on percentiles
bin$Age_Group[bin$Age < 21] <- "< 21"
bin$Age_Group[bin$Age >= 21 & bin$Age < 42] <- ">= 21 & < 42"
bin$Age_Group[bin$Age >= 42] <- ">= 42"

bin$Age_Group <- factor(bin$Age_Group)

###Creating a logistic regression model for Interact with Age and OS as independent variables
mod <- glm(Interact ~ Age + OS, data = bin, family = "binomial")

###Creating a logistic regression model for Interact with Age, Age_Group and OS as independent variables
mod1 <- glm(Interact ~ Age + Age_Group + OS, data = bin, family = "binomial")

###comparing the models
summary(mod)
summary(mod1)

##########################################################################################

####Trick 3: Reducing Levels in Categorical Variables

###Importing dataset
dat <- read.csv(".../ReduceLevels.csv")

###Arranging data in descending order of AppLaunches 
dat <- dat[order(dat$App.Launched, decreasing = T),]

dat

###Calculating the percentage share of App Launches city wise
dat$Percentage <- round(prop.table(matrix(dat$App.Launched, ncol = 1),margin = 2) * 100,2)

###Cumulative app launches
dat$Cumulative <- cumsum(dat$Percent)

###Creating a new variable which assigns each city to a state
dat$States <- c("NewYork", "California", "California", "California", "Texas", "Texas", "Pennsylvania",
                "Arizona", "Arizona", "Texas", "Texas", "Florida", "California", "Pennsylvania",
                "Arizona", "Florida", "Texas", "California", "Florida", "California") 

###Checking the distribution of states
table(dat$States)

dat$States <- factor(dat$States)

##########################################################################################

####Trick 4: Transforming Non-Normal to Normal Distribution

###generating normal distribution
set.seed(123)
g <- data.frame("x" = rnorm(1e6))

###generating right skewed distribution
set.seed(123)
g1 <- data.frame("x" = rbeta(1e6,1,500))

###generating left skewed distribution
set.seed(123)
g2 <- data.frame("x" = (rbeta(1e6,5,1)))

ggplot(g, aes(x = x)) + geom_histogram(aes(y =..density..),color = "red", fill = "yellow") +
  geom_density(col = "black") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + ggtitle("Normal") 


ggplot(g1, aes(x = x)) + geom_histogram(aes(y =..density..),color = "red", fill = "beige") +
  geom_density(col = "black") + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + ggtitle("Non-Normal")


ggplot(g2, aes(x = x)) + geom_histogram(aes(y =..density..),color = "red", fill = "lightgreen") +
  geom_density(col = "black") +theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + ggtitle("Non-Normal")

###Importing data
dat <- read.csv(".../boxcox.csv")
head(dat)
summary(dat)

###histogram of y
ggplot(dat, aes(x = y)) + geom_histogram(aes(y =..density..),bins = 8,color = "red", fill = "green", alpha = 0.5) +
  geom_density(col = "black") + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + ggtitle("Histogram of y") + ylab("")

###scatter plot of y and x
ggplot(dat, aes(x = x, y= y)) + geom_point(col = "red") + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + ggtitle("Relationship of y with x")

### Box cox transformation
b <- powerTransform(dat$y)
z <- bcPower(dat$y,b$lambda)

dat$z <- z

###Histogram of z = transformed y
ggplot(dat, aes(x = z)) + geom_histogram(aes(y =..density..),bins = 8,color = "red", fill = "green", alpha = 0.5) +
  geom_density(col = "black") + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + ggtitle("Histogram of z") + ylab("")

###scatter plot of z and x
ggplot(dat, aes(x = x, y= z)) + geom_point(col = "red") + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + ggtitle("Relationship of z with x")

###Linear Regression model comparison with non-normal and tranformed normal variable using box-cox
m <- lm(y ~ x, data = dat); summary(m)
m1 <- lm(z ~ x, data = dat); summary(m1)

##########################################################################################

#### Trick 5: Missing Data

dat <- read.csv(".../missing.csv")

head(dat)
summary(dat)

###Imputing Gender by Decision tree
ind <- which(is.na(dat$Gender))

dt <- rpart(Gender ~ Visits + Transactions + OS, data = dat[-ind,], control = rpart.control(minsplit=2, cp=0.000001))


pred <- predict(dt, newdata = dat[ind,], type = "class")
data.train.impute <- dat
data.train.impute$Gender[ind] <- pred

###Imputing Transactions by linear regression
ind.tr <- which(is.na(dat$Transactions))
trans.lm <- lm(Transactions ~ Visits + OS + Gender, data = data.train.impute[-ind.tr,])

pred <- predict(trans.lm, newdata = dat[ind.tr,])
data.train.impute$Transactions[ind.tr] <- pred

###Building model with imputed values for missing data
imp <- lm(Revenue ~ Transactions + OS + Gender, data = data.train.impute)

###Building model with ignored values for missing data
data.train.del <- dat[!(is.na(dat$Gender) | is.na(dat$Transactions)),]
del <- lm(Revenue ~ Transactions + OS + Gender , data = data.train.del)

###Comparing models
summary(del)
summary(imp)

##########################################################################################

### Trick 6: Dummy Variables

###Creating dummy variables
bin1 <- cbind(bin, predict(dummyVars( ~ Age_Group, data = bin, fullRank = T), bin))

bin1$`Age_Group.>= 21 & < 42` <- factor(bin1$`Age_Group.>= 21 & < 42`)
bin1$`Age_Group.>= 42` <- factor(bin1$`Age_Group.>= 42`)

###Building logistic regression model with dummy variables
mod2 <- glm(Interact ~ Age + `Age_Group.>= 21 & < 42`+ OS, data = bin1, family = "binomial")
summary(mod2)

table(bin$Interact,bin$Age_Group)

###Importing data
rev <- read.csv(".../revenue.csv")
head(rev)
summary(rev)

###Creating dummy variables
rev1 <- cbind(rev, predict(dummyVars( ~ OS + AgeGroup, data = rev, fullRank = T), rev))

###Creating linear regression model without dummy variables
mod <- lm(Revenue ~ Visits + OS + AgeGroup, data = rev )

###Creating linear regression model with dummy variables
mod1 <- lm(Revenue ~ Visits + Gender  + OS.iOS + AgeGroup.B, data = rev1 )

###Comparing the models
summary(mod)
summary(mod1)

##########################################################################################

### Trick 7 : Interaction

### Logistic Regression model with no interaction term
mod <-  glm(Interact ~  Age + OS, data = bin, family = "binomial")

### Model with interaction term i.e. Age x OS
mod1 <- glm(Interact ~  Age + OS + Age * OS, data = bin, family = "binomial")

### Comparing the models
summary(mod); summary(mod1)

##########################################################################################

### Trick 8: Dimensionality Reduction

physics <- c(70, 40, 80.5, 72.1, 55.1, 60, 85.5, 56)
maths <- c(69, 55.8, 74.2, 70, 63, 59.5, 75, 62)
d <- data.frame("Physics" = physics, "Maths" = maths)
d <- cbind(d, rep(0,nrow(d)))
names(d)[3] <- "zero"

ggplot(d, aes(x = Physics, y = Maths)) + geom_point(col = "red") +theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + ggtitle("Relationship of Physics and Maths marks")


ggplot(d, aes(x = Physics, y = zero)) + geom_point(col = "red", size=3) +theme_bw() + ylim(c(0,0.1)) + 
  xlim(c(30,90))+ geom_smooth() +theme(axis.line.y = element_blank(), 
                                       axis.title.y = element_blank(),
                                       axis.text.y = element_blank(),
                                       axis.ticks.y = element_blank(), 
                                       panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank())

ggplot(d, aes(x = Maths, y = zero)) + geom_point(col = "red", size=3) +theme_bw() + ylim(c(0,0.1)) + 
  xlim(c(30,90))+   geom_smooth() +theme(axis.line.y = element_blank(), 
                                         axis.title.y = element_blank(),
                                         axis.text.y = element_blank(),
                                         axis.ticks.y = element_blank(), 
                                         panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank())


### running PCA on data by using correlation matrix. Correlation matrix is ideal
### when data is on the same scale. 
fit <- princomp(d[,-3], cor=T)

### Analyzing the variance explained by the Principal components
summary(fit)

### Proportion of variance explained by PC1 can be assessed by the eigenvalues of the
### correlation matrix of the data
eig <- eigen(cor(d[,-3]))

### eigen vectors are like the coefficients for the linear transformation
eig

### variation explained by PC1
eig$values[1]/sum(eig$values)

### transformed dataset. The scores are the transformed values of the variables on the 
## new axes
d1 <- data.frame(fit$scores)

d1 <- cbind(d1, rep(0,nrow(d1)))
names(d1) <- c("PC1", "PC2", "zero")

ggplot(d1, aes(x = PC1, y = zero)) + geom_point(col = "red", size=3) +theme_bw() + ylim(c(0,0.1)) + 
  xlim(c(-2.5,2.5))+ geom_smooth() +theme(axis.line.y = element_blank(), 
                                          axis.title.y = element_blank(),
                                          axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(), 
                                          panel.grid.major = element_blank(),
                                          panel.grid.minor = element_blank())

ggplot(d1, aes(x = PC2, y = zero)) + geom_point(col = "red", size=3) +theme_bw() + ylim(c(0,0.1)) + 
  xlim(c(-2.5,2.5))+   geom_smooth() +theme(axis.line.y = element_blank(), 
                                            axis.title.y = element_blank(),
                                            axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank(), 
                                            panel.grid.major = element_blank(),
                                            panel.grid.minor = element_blank())


data(wine, package="rattle")
summary(wine)

### running PCA on wine dataset
wine.pca <- princomp(wine[,-1],  cor = T)

### Analyzing the variance explained by the Principal components
summary(wine.pca)

### Creating a biplot
ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         groups = wine$Type, labels = 1:nrow(wine), labels.size = 5, varname.size = 4) +
  scale_color_discrete(name = '') + theme_bw() +
  theme(legend.direction = 'horizontal', legend.position = 'top',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

##########################################################################################
