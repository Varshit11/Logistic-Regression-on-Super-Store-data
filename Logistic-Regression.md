Logistic-Regression
================
Varshit Dubey (CoE Pune)
30 September 2018

Setup
=====

### Load packages

``` r
library(ggplot2)
library(dplyr)
library(statsr)
library(ggthemes)
library(corrgram)
library(corrplot)
library(caTools)
library(sp)
library(raster)
library(usdm)
library(lmtest)
library(MASS)
library(dplyr)
library(plyr)
library(InformationValue)
library(caret)
```

------------------------------------------------------------------------

### Load the data set

``` r
# load the data set 
library(readxl)
returns1 <- read_excel("returns1.xlsx")
orders <- read_excel("orders.xlsx")
```

    ## Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
    ## shim, : Coercing text to numeric in L3499 / R3499C12: '05408'

    ## Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
    ## shim, : Coercing text to numeric in L3535 / R3535C12: '05408'

    ## Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
    ## shim, : Coercing text to numeric in L4159 / R4159C12: '05408'

    ## Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
    ## shim, : Coercing text to numeric in L4160 / R4160C12: '05408'

    ## Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
    ## shim, : Coercing text to numeric in L4161 / R4161C12: '05408'

    ## Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
    ## shim, : Coercing text to numeric in L4463 / R4463C12: '05408'

    ## Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
    ## shim, : Coercing text to numeric in L6758 / R6758C12: '05408'

    ## Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
    ## shim, : Coercing text to numeric in L6759 / R6759C12: '05408'

    ## Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
    ## shim, : Coercing text to numeric in L6760 / R6760C12: '05408'

    ## Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
    ## shim, : Coercing text to numeric in L6761 / R6761C12: '05408'

    ## Warning in read_fun(path = path, sheet_i = sheet, limits = limits, shim =
    ## shim, : Coercing text to numeric in L9644 / R9644C12: '05408'

``` r
#View(data)
str(returns1)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    296 obs. of  9 variables:
    ##  $ Returned  : chr  "Yes" "Yes" "Yes" "Yes" ...
    ##  $ Order ID  : chr  "CA-2014-153822" "CA-2014-129707" "CA-2011-152345" "CA-2012-156440" ...
    ##  $ Order     : chr  "Product" "Product" "Product" "Product" ...
    ##  $ Returns   : chr  "Damaged" "Damaged" "Damaged" "Damaged" ...
    ##  $ X__1      : chr  NA NA NA NA ...
    ##  $ X__2      : chr  "-2.5248000000000022" NA NA NA ...
    ##  $ X__3      : chr  NA NA NA NA ...
    ##  $ X__4      : chr  NA NA NA NA ...
    ##  $ Returns__1: chr  "Damaged" "Damaged" "Damaged" "Damaged" ...

``` r
returns2 <- returns1[,-c(4:8)]
data1 <- join(orders, returns2, type = "left", by = "Order ID")
#?join
data1$Returned <- ifelse(is.na(data1$Returned),0,1)
sum(is.na(data1$Returned))
```

    ## [1] 0

``` r
any(is.na(data1$Returned))
```

    ## [1] FALSE

------------------------------------------------------------------------

Part 1: Data
------------

The data set is comprised of 9994 observations of sales based on 21 variables.

------------------------------------------------------------------------

Part 2: Research question
-------------------------

------------------------------------------------------------------------

Now before making the model I am removing some statistically insignificant variables like Product name, Country etc, also I am making a new variable named "days\_diff" which is the difference in the shipping date and Order Date and I think that days\_diff will have a role to play in the model.

``` r
#data1$days_diff <- as.Date(data1$`Ship Date`, format="%Y/%m/%d")-
 #                 as.Date(data1$`Order Date`, format="%Y/%m/%d")


# removing some variables..
data2 <- data1[,-c(2,3,4,6,7,9,10,11,14,17,23,24)]


str(data2)
```

    ## 'data.frame':    9994 obs. of  12 variables:
    ##  $ Row ID      : num  7981 740 741 742 1760 ...
    ##  $ Ship Mode   : chr  "Standard Class" "Standard Class" "Standard Class" "Standard Class" ...
    ##  $ Segment     : chr  "Consumer" "Home Office" "Home Office" "Home Office" ...
    ##  $ Postal Code : num  77095 60540 60540 60540 19143 ...
    ##  $ Region      : chr  "Central" "Central" "Central" "Central" ...
    ##  $ Category    : chr  "Office Supplies" "Office Supplies" "Office Supplies" "Office Supplies" ...
    ##  $ Sub-Category: chr  "Paper" "Labels" "Storage" "Binders" ...
    ##  $ Sales       : num  16.45 11.78 272.74 3.54 19.54 ...
    ##  $ Quantity    : num  2 3 3 2 3 3 3 9 2 2 ...
    ##  $ Discount    : num  0.2 0.2 0.2 0.8 0.2 0 0 0 0 0 ...
    ##  $ Profit      : num  5.55 4.27 -64.77 -5.49 4.88 ...
    ##  $ Returned    : num  0 0 0 0 0 0 0 0 0 0 ...

``` r
#data2$days_diff <- as.numeric(data2$days_diff)

set.seed(123)
train=createDataPartition(data2$Returned, p=0.7,list=FALSE)

Training=data2[train,]
Testing=data2[-train,]


sum(Training$Returned)
```

    ## [1] 575

``` r
sum(Testing$Returned)
```

    ## [1] 225

``` r
all <- glm(Returned ~.,data = Training, family = binomial(link = "logit"))
summary(all)
```

    ## 
    ## Call:
    ## glm(formula = Returned ~ ., family = binomial(link = "logit"), 
    ##     data = Training)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.9103  -0.4696  -0.3217  -0.2759   2.8010  
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)               -2.679e+00  4.479e-01  -5.981 2.22e-09 ***
    ## `Row ID`                  -1.427e-05  1.560e-05  -0.915  0.36024    
    ## `Ship Mode`Same Day        1.224e-01  1.941e-01   0.631  0.52833    
    ## `Ship Mode`Second Class   -4.736e-01  1.514e-01  -3.127  0.00176 ** 
    ## `Ship Mode`Standard Class -2.622e-01  1.183e-01  -2.216  0.02666 *  
    ## SegmentCorporate           1.038e-01  9.971e-02   1.041  0.29787    
    ## SegmentHome Office        -2.706e-01  1.311e-01  -2.064  0.03901 *  
    ## `Postal Code`              3.995e-06  5.158e-06   0.775  0.43856    
    ## RegionEast                 4.084e-01  2.884e-01   1.416  0.15675    
    ## RegionSouth                2.250e-01  2.454e-01   0.917  0.35925    
    ## RegionWest                 1.325e+00  2.021e-01   6.557 5.51e-11 ***
    ## CategoryOffice Supplies   -1.551e-01  3.757e-01  -0.413  0.67960    
    ## CategoryTechnology        -3.865e-01  2.883e-01  -1.341  0.18002    
    ## `Sub-Category`Appliances  -6.438e-02  3.544e-01  -0.182  0.85585    
    ## `Sub-Category`Art         -4.410e-01  3.410e-01  -1.293  0.19593    
    ## `Sub-Category`Binders     -4.884e-02  3.215e-01  -0.152  0.87928    
    ## `Sub-Category`Bookcases   -5.692e-01  3.909e-01  -1.456  0.14542    
    ## `Sub-Category`Chairs      -1.835e-01  2.863e-01  -0.641  0.52152    
    ## `Sub-Category`Copiers     -3.570e-01  6.547e-01  -0.545  0.58556    
    ## `Sub-Category`Envelopes   -7.108e-01  4.520e-01  -1.573  0.11578    
    ## `Sub-Category`Fasteners   -6.193e-01  4.707e-01  -1.316  0.18828    
    ## `Sub-Category`Furnishings -5.055e-01  2.843e-01  -1.778  0.07534 .  
    ## `Sub-Category`Labels      -4.245e-01  3.926e-01  -1.081  0.27956    
    ## `Sub-Category`Machines     7.951e-01  3.879e-01   2.050  0.04039 *  
    ## `Sub-Category`Paper       -1.021e-01  3.130e-01  -0.326  0.74434    
    ## `Sub-Category`Phones       1.800e-01  2.141e-01   0.840  0.40065    
    ## `Sub-Category`Storage     -3.387e-01  3.318e-01  -1.021  0.30740    
    ## `Sub-Category`Supplies            NA         NA      NA       NA    
    ## `Sub-Category`Tables              NA         NA      NA       NA    
    ## Sales                     -5.155e-05  9.744e-05  -0.529  0.59676    
    ## Quantity                  -3.861e-03  2.063e-02  -0.187  0.85152    
    ## Discount                  -1.937e-01  3.109e-01  -0.623  0.53330    
    ## Profit                    -3.615e-07  2.324e-04  -0.002  0.99876    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3974.9  on 6995  degrees of freedom
    ## Residual deviance: 3708.0  on 6965  degrees of freedom
    ## AIC: 3770
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
a <- stepAIC(all, direction = "both")
```

    ## Start:  AIC=3769.96
    ## Returned ~ `Row ID` + `Ship Mode` + Segment + `Postal Code` + 
    ##     Region + Category + `Sub-Category` + Sales + Quantity + Discount + 
    ##     Profit
    ## 
    ## 
    ## Step:  AIC=3769.96
    ## Returned ~ `Row ID` + `Ship Mode` + Segment + `Postal Code` + 
    ##     Region + `Sub-Category` + Sales + Quantity + Discount + Profit
    ## 
    ##                  Df Deviance    AIC
    ## - `Sub-Category` 16   3726.5 3756.5
    ## - Profit          1   3708.0 3768.0
    ## - Quantity        1   3708.0 3768.0
    ## - Sales           1   3708.3 3768.3
    ## - Discount        1   3708.3 3768.3
    ## - `Postal Code`   1   3708.6 3768.6
    ## - `Row ID`        1   3708.8 3768.8
    ## <none>                3708.0 3770.0
    ## - Segment         2   3715.4 3773.4
    ## - `Ship Mode`     3   3722.3 3778.3
    ## - Region          3   3775.3 3831.3
    ## 
    ## Step:  AIC=3756.5
    ## Returned ~ `Row ID` + `Ship Mode` + Segment + `Postal Code` + 
    ##     Region + Sales + Quantity + Discount + Profit
    ## 
    ##                  Df Deviance    AIC
    ## - Sales           1   3726.6 3754.6
    ## - Quantity        1   3726.6 3754.6
    ## - Profit          1   3726.6 3754.6
    ## - `Postal Code`   1   3726.8 3754.8
    ## - Discount        1   3726.9 3754.9
    ## - `Row ID`        1   3727.2 3755.2
    ## <none>                3726.5 3756.5
    ## + Category        2   3725.8 3759.8
    ## - Segment         2   3734.5 3760.5
    ## - `Ship Mode`     3   3740.7 3764.7
    ## + `Sub-Category` 16   3708.0 3770.0
    ## - Region          3   3803.3 3827.3
    ## 
    ## Step:  AIC=3754.61
    ## Returned ~ `Row ID` + `Ship Mode` + Segment + `Postal Code` + 
    ##     Region + Quantity + Discount + Profit
    ## 
    ##                  Df Deviance    AIC
    ## - Profit          1   3726.7 3752.7
    ## - Quantity        1   3726.7 3752.7
    ## - `Postal Code`   1   3726.9 3752.9
    ## - Discount        1   3727.1 3753.1
    ## - `Row ID`        1   3727.3 3753.3
    ## <none>                3726.6 3754.6
    ## + Sales           1   3726.5 3756.5
    ## + Category        2   3725.9 3757.9
    ## - Segment         2   3734.6 3758.6
    ## - `Ship Mode`     3   3740.8 3762.8
    ## + `Sub-Category` 16   3708.3 3768.3
    ## - Region          3   3803.6 3825.6
    ## 
    ## Step:  AIC=3752.68
    ## Returned ~ `Row ID` + `Ship Mode` + Segment + `Postal Code` + 
    ##     Region + Quantity + Discount
    ## 
    ##                  Df Deviance    AIC
    ## - Quantity        1   3726.8 3750.8
    ## - `Postal Code`   1   3726.9 3750.9
    ## - Discount        1   3727.2 3751.2
    ## - `Row ID`        1   3727.4 3751.4
    ## <none>                3726.7 3752.7
    ## + Profit          1   3726.6 3754.6
    ## + Sales           1   3726.6 3754.6
    ## + Category        2   3726.0 3756.0
    ## - Segment         2   3734.7 3756.7
    ## - `Ship Mode`     3   3740.8 3760.8
    ## + `Sub-Category` 16   3708.3 3766.3
    ## - Region          3   3803.7 3823.7
    ## 
    ## Step:  AIC=3750.79
    ## Returned ~ `Row ID` + `Ship Mode` + Segment + `Postal Code` + 
    ##     Region + Discount
    ## 
    ##                  Df Deviance    AIC
    ## - `Postal Code`   1   3727.0 3749.0
    ## - Discount        1   3727.3 3749.3
    ## - `Row ID`        1   3727.5 3749.5
    ## <none>                3726.8 3750.8
    ## + Quantity        1   3726.7 3752.7
    ## + Profit          1   3726.7 3752.7
    ## + Sales           1   3726.8 3752.8
    ## + Category        2   3726.1 3754.1
    ## - Segment         2   3734.8 3754.8
    ## - `Ship Mode`     3   3741.0 3759.0
    ## + `Sub-Category` 16   3708.4 3764.4
    ## - Region          3   3803.8 3821.8
    ## 
    ## Step:  AIC=3749.05
    ## Returned ~ `Row ID` + `Ship Mode` + Segment + Region + Discount
    ## 
    ##                  Df Deviance    AIC
    ## - `Row ID`        1   3727.7 3747.7
    ## - Discount        1   3727.8 3747.8
    ## <none>                3727.0 3749.0
    ## + `Postal Code`   1   3726.8 3750.8
    ## + Quantity        1   3726.9 3750.9
    ## + Profit          1   3727.0 3751.0
    ## + Sales           1   3727.0 3751.0
    ## + Category        2   3726.3 3752.3
    ## - Segment         2   3735.1 3753.1
    ## - `Ship Mode`     3   3741.4 3757.4
    ## + `Sub-Category` 16   3709.0 3763.0
    ## - Region          3   3948.3 3964.3
    ## 
    ## Step:  AIC=3747.75
    ## Returned ~ `Ship Mode` + Segment + Region + Discount
    ## 
    ##                  Df Deviance    AIC
    ## - Discount        1   3728.5 3746.5
    ## <none>                3727.7 3747.7
    ## + `Row ID`        1   3727.0 3749.0
    ## + `Postal Code`   1   3727.5 3749.5
    ## + Quantity        1   3727.6 3749.6
    ## + Profit          1   3727.7 3749.7
    ## + Sales           1   3727.7 3749.7
    ## + Category        2   3727.0 3751.0
    ## - Segment         2   3735.7 3751.7
    ## - `Ship Mode`     3   3742.0 3756.0
    ## + `Sub-Category` 16   3709.8 3761.8
    ## - Region          3   3949.0 3963.0
    ## 
    ## Step:  AIC=3746.48
    ## Returned ~ `Ship Mode` + Segment + Region
    ## 
    ##                  Df Deviance    AIC
    ## <none>                3728.5 3746.5
    ## + Discount        1   3727.7 3747.7
    ## + `Row ID`        1   3727.8 3747.8
    ## + `Postal Code`   1   3728.1 3748.1
    ## + Profit          1   3728.3 3748.3
    ## + Quantity        1   3728.4 3748.4
    ## + Sales           1   3728.5 3748.5
    ## + Category        2   3727.8 3749.8
    ## - Segment         2   3736.5 3750.5
    ## - `Ship Mode`     3   3743.1 3755.1
    ## + `Sub-Category` 16   3710.0 3760.0
    ## - Region          3   3952.4 3964.4

``` r
summary(a)
```

    ## 
    ## Call:
    ## glm(formula = Returned ~ `Ship Mode` + Segment + Region, family = binomial(link = "logit"), 
    ##     data = Training)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.7227  -0.5080  -0.3239  -0.2885   2.7077  
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                -2.8888     0.1627 -17.753  < 2e-16 ***
    ## `Ship Mode`Same Day         0.1178     0.1931   0.610  0.54187    
    ## `Ship Mode`Second Class    -0.4744     0.1507  -3.148  0.00164 ** 
    ## `Ship Mode`Standard Class  -0.2698     0.1176  -2.293  0.02183 *  
    ## SegmentCorporate            0.1092     0.0992   1.101  0.27090    
    ## SegmentHome Office         -0.2766     0.1307  -2.116  0.03433 *  
    ## RegionEast                  0.2371     0.1597   1.485  0.13763    
    ## RegionSouth                 0.1095     0.1892   0.579  0.56275    
    ## RegionWest                  1.4526     0.1379  10.533  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3974.9  on 6995  degrees of freedom
    ## Residual deviance: 3728.5  on 6987  degrees of freedom
    ## AIC: 3746.5
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
predicted <- predict(a, newdata = Testing, type="response")

# this will show what should be the optimum cutoff for 0 and 1..
optCutOff <- optimalCutoff(Testing$Returned, predicted)[1]
misClassError(Testing$Returned, predicted, threshold = optCutOff)
```

    ## [1] 0.0801

``` r
plotROC(actuals=Training$Returned,predictedScores=as.numeric(fitted(a)))
```

![](Logistic-Regression_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
Pred2=ifelse(predicted<optCutOff,0,1)


caret::confusionMatrix(table(Testing$Returned,Pred2,dnn=list('actual','predicted')))
```

    ## Confusion Matrix and Statistics
    ## 
    ##       predicted
    ## actual    0    1
    ##      0 2745   28
    ##      1  212   13
    ##                                           
    ##                Accuracy : 0.9199          
    ##                  95% CI : (0.9097, 0.9294)
    ##     No Information Rate : 0.9863          
    ##     P-Value [Acc > NIR] : 1               
    ##                                           
    ##                   Kappa : 0.0764          
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 0.92831         
    ##             Specificity : 0.31707         
    ##          Pos Pred Value : 0.98990         
    ##          Neg Pred Value : 0.05778         
    ##              Prevalence : 0.98632         
    ##          Detection Rate : 0.91561         
    ##    Detection Prevalence : 0.92495         
    ##       Balanced Accuracy : 0.62269         
    ##                                           
    ##        'Positive' Class : 0               
    ##
