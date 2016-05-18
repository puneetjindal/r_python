
# Case 1:If the “group” variable is of character type (Machines A,B,C), 

anovaABC <- read.csv("~/Desktop/SA1 examples/anovaABC.csv")
View(anovaABC)
attach(anovaABC)
anova(lm(Output~Machine, data=anovaABC))


"
Null hypothesis we have taken is that mean speed of machines are same

Analysis of Variance Table

Response: Output
Df Sum Sq Mean Sq F value   Pr(>F)   
Machine    2    250 125.000     7.5 0.007707 **
Residuals 12    200  16.667                    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# interpretation is if we reject the null hypothesis that machines are equal in their average speeds 
#and in fact their average speeds are different 
#then probablity of commiting a Type 1 error is less than .7 %
"
r= 3
n= len(Output) = 15

" test statistic to compute if any two means are different
1. construct the t statistic
2. calculate the p value associated with the t statistic ptukey(q,r,n-r)
3. reject the null hypothesis that 2 means are equal if p value <alpha
"

#case 2: If the “group” variable is of Numeric type 
anova123 <- read.csv("~/Desktop/SA1 examples/anova123.csv")
View(anova123)
anova(lm(Output ~ factor(Machine),data=anova123))

"
Analysis of Variance Table

Response: Output
Df Sum Sq Mean Sq F value   Pr(>F)   
factor(Machine)  2    250 125.000     7.5 0.007707 **
Residuals       12    200  16.667                    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
"

