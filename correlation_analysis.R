# Read the dataset
cigarette_consumption <- read.csv('~/Downloads/CigaretteConsumption.csv')
attach(cigarette_consumption)

#Scatter Plot matirix
pairs(cigarette_consumption[,2:8],col="dodgerblue4",pch=100)

#Regression Model -Full Model
reg.model<-lm(Sales~Age+HS+Income+Black+Female+Price)
summary(reg.model)


# Four PLots like Normal QQPlo,.......
par(mfrow=c(2,2))
plot(reg.model)

#Testing for multiple parameters simultaneously:
reduced=lm(Sales~Age+Income+Black+Price)
summary(reduced)
f= ((SSE(Reduced model) - SSE(Full Model))/(p-q) )/(SSE(FULL Model)/(n-p-1))
Where
p is the number of variables in the full model
q is the number variables in the reduced model


f=((34960-34926)/(6-4))/(34926/(51-6-1))
p-value=pf(f,2,(51-6-1))



# Confidence Interval for the slopes
confint(reg.model,level=0.95)

#Correlation Matrix
cor(cigarette_consumption[,2:8])


### Partial Correlation matrix: Install and load the package called "corpcor"
library('corpcor')
cor2pcor(cor(cigarette_consumption[,2:8]))

# Install and load the package called "car"
library("car") 

#Added Variable Plots
avPlots(reg.model, id.n=2, id.cex=1) 

#Residuals vs Regressors
residualPlots(reg.model) 

# Deletion Diagnostics : 
influence.measures(reg.model)
influenceIndexPlot(reg.model, id.n=3) # Index Plots of the influence measures
influencePlot(reg.model, id.n=3) # A user friendly representation of the above
