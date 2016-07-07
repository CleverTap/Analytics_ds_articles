
###Please install the below libraries, if not already installed
###Importing the libraries in R Env
library(NbClust)
library(rattle)


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

### Code to reproduce further examples will be released alongwith the publication
### of the next part