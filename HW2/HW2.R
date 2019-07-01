# Homework 2 using R

options(digits = 4)

# imported the Excel data file and called the data.frame "data"
attach(data)
save(data,file="HW2_data.RData")
head(data)
#   Month               Cnsmr Manuf HiTec  Hlth Other `Mkt-RF`   SMB   HML    RF
# 1 2014-01-01 00:00:00 -5.94 -4.22 -1.79  1.65 -4.44    -3.32  0.86 -2.09     0
# 2 2014-02-01 00:00:00  4.9   5.11  4.71  6.47  3.45     4.65  0.32 -0.4      0
# 3 2014-03-01 00:00:00  0.74  1.42 -0.66 -2.56  2.3      0.43 -1.89  5.08     0
# 4 2014-04-01 00:00:00  0.02  3.01 -0.89 -0.3  -1.6     -0.19 -4.25  1.14     0
# 5 2014-05-01 00:00:00  1.82  0.92  3.56  1.88  1.75     2.06 -1.85 -0.27     0
# 6 2014-06-01 00:00:00  1.53  3     2.68  2.86  2.12     2.61  3.07 -0.74     0

##############################################################
#                                                            #
#                QUESTION 1                                  #
#                                                            #
##############################################################

# Subtract risk-free rate from returns to get excess returns
xret <- (data[,2]-data[,10])
for (i in 3:6) {xret <- cbind(xret,data[,i] - data[,10])}

colnames(xret)<-c("Con", "Mfg", "Tech", "Hlth", "Other")
head(xret)
#     Con   Mfg  Tech  Hlth Other
# 1 -5.94 -4.22 -1.79  1.65 -4.44
# 2  4.90  5.11  4.71  6.47  3.45
# 3  0.74  1.42 -0.66 -2.56  2.30
# 4  0.02  3.01 -0.89 -0.30 -1.60
# 5  1.82  0.92  3.56  1.88  1.75
# 6  1.53  3.00  2.68  2.86  2.12

# calculate column means, standard deviations, and Sharpe Ratios
mean.xret <- colMeans(xret)

mean.xret
sd.xret <- array(rep(0,5))
for(i in 1:5) {sd.xret[i] <- sd(xret[,i])}
SR <-mean.xret/sd.xret *sqrt(12)
stats.xret <- as.matrix(cbind(mean.xret,sd.xret,SR))

stats.xret
#       mean.xret sd.xret     SR
# Con      0.6298   3.262 0.6689
# Mfg      0.2367   3.514 0.2333
# Tech     0.9440   3.739 0.8745
# Hlth     0.8055   4.075 0.6848
# Other    0.6642   3.846 0.5983

t(stats.xret)
#              Con    Mfg   Tech   Hlth  Other
# mean.xret 0.6298 0.2367 0.9440 0.8055 0.6642
# sd.xret   3.2617 3.5141 3.7394 4.0749 3.8456
# SR        0.6689 0.2333 0.8745 0.6848 0.5983


##############################################################
#                                                            #
#                QUESTION 2                                  #
#                                                            #
##############################################################

# Calculate Annualized Covariance Matrix
options(digits=5)
cov.mat <- cov(xret) * 12

cov.mat
#          Con    Mfg   Tech   Hlth  Other
# Con   127.67 100.58 114.19 116.84 116.65
# Mfg   100.58 148.19 107.88 107.22 128.02
# Tech  114.19 107.88 167.80 124.26 127.09
# Hlth  116.84 107.22 124.26 199.26 131.50
# Other 116.65 128.02 127.09 131.50 177.46

##############################################################
#                                                            #
#                QUESTION 3                                  #
#                                                            #
##############################################################
options(digits=4) 
betas <- matrix(rep(0,15),nrow=5)
rownames(betas) <- c("Con", "Mfg", "Tech", "Hlth", "Other")
colnames(betas) <- c("b.mkt", "b.smb", "b.hml")

for(i in 1:5) {betas[i,] <- coef(lm(xret[,i] ~ data[[7]] + data[[8]] + data[[9]]))[2:4]}
betas
#        b.mkt    b.smb   b.hml
# Con   0.9096 -0.13602 -0.1831
# Mfg   0.9291  0.10602  0.3458
# Tech  1.0420 -0.16622 -0.3451
# Hlth  0.9500  0.14251 -0.5077
# Other 1.0880  0.07297  0.4420

##############################################################
#                                                            #
#                QUESTION 4                                  #
#                                                            #
##############################################################
options(digits = 5)
Cov.factors <- cov(data[,7:9])*12
Cov.factors

Struct.Cov.matrix <- betas %*% Cov.factors %*% t(betas)
diag(Struct.Cov.matrix) <- diag(cov.mat)

Struct.Cov.matrix
#
#          Con    Mfg   Tech   Hlth  Other
# Con   127.67 101.32 124.48 120.89 117.29
# Mfg   101.32 148.19 113.24 109.91 141.37
# Tech  124.48 113.24 167.80 143.69 130.78
# Hlth  120.89 109.91 143.69 199.26 125.44
# Other 117.29 141.37 130.78 125.44 177.46

options(digits = 3)
cov.diff <- (Struct.Cov.matrix - cov.mat)/cov.mat
cov.diff[upper.tri(cov.diff,diag=TRUE)]<- NA

as.table(cov.diff[-1,-5])
#
#           Con      Mfg     Tech     Hlth
# Mfg    0.00735                           
# Tech   0.09012  0.04968                  
# Hlth   0.03465  0.02502  0.15638         
# Other  0.00555  0.10429  0.02906 -0.04603

##############################################################
#                                                            #
#                QUESTION 5                                  #
#                                                            #
##############################################################
options(digits = 3)
e <- matrix(rep(1,5),ncol=1)
mvp.smpl <- solve(cov.mat) %*% e/as.numeric(t(e) %*% solve(cov.mat) %*% e)
mvp.struct <- solve(Struct.Cov.matrix) %*% e/as.numeric(t(e) %*% solve(Struct.Cov.matrix) %*% e)

tble <- cbind(mvp.smpl,mvp.struct,mvp.struct)
colnames(tble) <- c('Sample','Structural','Difference')
tble
#
#       Sample Structural Difference
# Con    0.571     0.7093      0.138
# Mfg    0.373     0.5080      0.135
# Tech   0.105    -0.0408     -0.146
# Hlth   0.050     0.0380     -0.012
# Other -0.099    -0.2145     -0.115

options(digits = 5)
Vol.mvp.smpl <- sqrt(t(mvp.smpl) %*% cov.mat %*% mvp.smpl)
Vol.mvp.struct <- sqrt(t(mvp.struct) %*% Struct.Cov.matrix %*% mvp.struct)
vol <- c(Vol.mvp.smpl,Vol.mvp.struct)
names(vol)<-c('sample','structural')
vol
#
#  sample structural 
#  10.803     10.788 