hw <- data.frame(Height.cm=c(164, 167, 168, 169, 169, 170, 170, 170, 171, 172, 172, 173, 173, 175, 176, 178),
                 Weight.kg=c( 54,  57,  58,  60,  61,  60,  61,  62,  62,  64,  62,  62,  64,  56,  66,  70)) 

is.height.outlier <- abs(scale(hw$Height.cm)) > 2
is.weight.outlier <- abs(scale(hw$Weight.kg)) > 2
pch   <- (is.height.outlier | is.weight.outlier) * 16
plot(hw, pch=pch) 
scale()