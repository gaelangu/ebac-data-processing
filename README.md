### Introduction

We will be preparing the salary dataset, extracted from the 1994 US Census, for a logistic regression.

We will determine whether a person makes over 50k a year; class column will be the dependent variable.

### Data Import

``` r
train = read.csv('salary-train.csv')
test = read.csv('salary-test.csv')
summary(train)
```

    ##       age                    workclass         fnlwgt       
    ##  Min.   :17.00    Private         :22696   Min.   :  12285  
    ##  1st Qu.:28.00    Self-emp-not-inc: 2541   1st Qu.: 117827  
    ##  Median :37.00    Local-gov       : 2093   Median : 178356  
    ##  Mean   :38.58    ?               : 1836   Mean   : 189778  
    ##  3rd Qu.:48.00    State-gov       : 1298   3rd Qu.: 237051  
    ##  Max.   :90.00    Self-emp-inc    : 1116   Max.   :1484705  
    ##                  (Other)          :  981                    
    ##          education                       marital     
    ##   HS-grad     :10501    Divorced             : 4443  
    ##   Some-college: 7291    Married-AF-spouse    :   23  
    ##   Bachelors   : 5355    Married-civ-spouse   :14976  
    ##   Masters     : 1723    Married-spouse-absent:  418  
    ##   Assoc-voc   : 1382    Never-married        :10683  
    ##   11th        : 1175    Separated            : 1025  
    ##  (Other)      : 5134    Widowed              :  993  
    ##             occupation            relationship  
    ##   Prof-specialty :4140    Husband       :13193  
    ##   Craft-repair   :4099    Not-in-family : 8305  
    ##   Exec-managerial:4066    Other-relative:  981  
    ##   Adm-clerical   :3770    Own-child     : 5068  
    ##   Sales          :3650    Unmarried     : 3446  
    ##   Other-service  :3295    Wife          : 1568  
    ##  (Other)         :9541                          
    ##                   race            sex         capital.gain  
    ##   Amer-Indian-Eskimo:  311    Female:10771   Min.   :    0  
    ##   Asian-Pac-Islander: 1039    Male  :21790   1st Qu.:    0  
    ##   Black             : 3124                   Median :    0  
    ##   Other             :  271                   Mean   : 1078  
    ##   White             :27816                   3rd Qu.:    0  
    ##                                              Max.   :99999  
    ##                                                             
    ##   capital.loss    hours.per.week         native.country     class      
    ##  Min.   :   0.0   Min.   : 1.00    United-States:29170    <=50K:24720  
    ##  1st Qu.:   0.0   1st Qu.:40.00    Mexico       :  643    >50K : 7841  
    ##  Median :   0.0   Median :40.00    ?            :  583                 
    ##  Mean   :  87.3   Mean   :40.44    Philippines  :  198                 
    ##  3rd Qu.:   0.0   3rd Qu.:45.00    Germany      :  137                 
    ##  Max.   :4356.0   Max.   :99.00    Canada       :  121                 
    ##                                   (Other)       : 1709

``` r
summary(test)
```

    ##       age                    workclass         fnlwgt       
    ##  Min.   :17.00    Private         :11210   Min.   :  13492  
    ##  1st Qu.:28.00    Self-emp-not-inc: 1321   1st Qu.: 116736  
    ##  Median :37.00    Local-gov       : 1043   Median : 177831  
    ##  Mean   :38.77    ?               :  963   Mean   : 189436  
    ##  3rd Qu.:48.00    State-gov       :  683   3rd Qu.: 238384  
    ##  Max.   :90.00    Self-emp-inc    :  579   Max.   :1490400  
    ##                  (Other)          :  482                    
    ##          education                      marital    
    ##   HS-grad     :5283    Divorced             :2190  
    ##   Some-college:3587    Married-AF-spouse    :  14  
    ##   Bachelors   :2670    Married-civ-spouse   :7403  
    ##   Masters     : 934    Married-spouse-absent: 210  
    ##   Assoc-voc   : 679    Never-married        :5434  
    ##   11th        : 637    Separated            : 505  
    ##  (Other)      :2491    Widowed              : 525  
    ##             occupation            relationship 
    ##   Prof-specialty :2032    Husband       :6523  
    ##   Exec-managerial:2020    Not-in-family :4278  
    ##   Craft-repair   :2013    Other-relative: 525  
    ##   Sales          :1854    Own-child     :2513  
    ##   Adm-clerical   :1841    Unmarried     :1679  
    ##   Other-service  :1628    Wife          : 763  
    ##  (Other)         :4893                         
    ##                   race            sex         capital.gain  
    ##   Amer-Indian-Eskimo:  159    Female: 5421   Min.   :    0  
    ##   Asian-Pac-Islander:  480    Male  :10860   1st Qu.:    0  
    ##   Black             : 1561                   Median :    0  
    ##   Other             :  135                   Mean   : 1082  
    ##   White             :13946                   3rd Qu.:    0  
    ##                                              Max.   :99999  
    ##                                                             
    ##   capital.loss    hours.per.week         native.country      class      
    ##  Min.   :   0.0   Min.   : 1.00    United-States:14662    <=50K.:12435  
    ##  1st Qu.:   0.0   1st Qu.:40.00    Mexico       :  308    >50K. : 3846  
    ##  Median :   0.0   Median :40.00    ?            :  274                  
    ##  Mean   :  87.9   Mean   :40.39    Philippines  :   97                  
    ##  3rd Qu.:   0.0   3rd Qu.:45.00    Puerto-Rico  :   70                  
    ##  Max.   :3770.0   Max.   :99.00    Germany      :   69                  
    ##                                   (Other)       :  801

### Setting Entries with Question Marks as NA Values

``` r
# train set
train$workclass = as.factor(gsub('?', NA, train$workclass, fixed = T))
train$native.country = as.factor(gsub('?', NA, train$native.country, fixed = T))
train$occupation = as.factor(gsub('?', NA, train$occupation, fixed = T))

# test set
test$workclass = as.factor(gsub('?', NA, test$workclass, fixed = T))
test$native.country = as.factor(gsub('?', NA, test$native.country, fixed = T))
test$occupation = as.factor(gsub('?', NA, test$occupation, fixed = T))
```

### Removing Incomplete Cases

``` r
train = train[complete.cases(train), ]
test = test[complete.cases(test), ]
str(train)
```

    ## 'data.frame':    30162 obs. of  14 variables:
    ##  $ age           : int  39 50 38 53 28 37 49 52 31 42 ...
    ##  $ workclass     : Factor w/ 8 levels " Federal-gov",..: 7 6 4 4 4 4 4 6 4 4 ...
    ##  $ fnlwgt        : int  77516 83311 215646 234721 338409 284582 160187 209642 45781 159449 ...
    ##  $ education     : Factor w/ 16 levels " 10th"," 11th",..: 10 10 12 2 10 13 7 12 13 10 ...
    ##  $ marital       : Factor w/ 7 levels " Divorced"," Married-AF-spouse",..: 5 3 1 3 3 3 4 3 5 3 ...
    ##  $ occupation    : Factor w/ 14 levels " Adm-clerical",..: 1 4 6 6 10 4 8 4 10 4 ...
    ##  $ relationship  : Factor w/ 6 levels " Husband"," Not-in-family",..: 2 1 2 1 6 6 2 1 2 1 ...
    ##  $ race          : Factor w/ 5 levels " Amer-Indian-Eskimo",..: 5 5 5 3 3 5 3 5 5 5 ...
    ##  $ sex           : Factor w/ 2 levels " Female"," Male": 2 2 2 2 1 1 1 2 1 2 ...
    ##  $ capital.gain  : int  2174 0 0 0 0 0 0 0 14084 5178 ...
    ##  $ capital.loss  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ hours.per.week: int  40 13 40 40 40 40 16 45 50 40 ...
    ##  $ native.country: Factor w/ 41 levels " Cambodia"," Canada",..: 39 39 39 39 5 39 23 39 39 39 ...
    ##  $ class         : Factor w/ 2 levels " <=50K"," >50K": 1 1 1 1 1 1 1 2 2 2 ...

``` r
str(test)
```

    ## 'data.frame':    15060 obs. of  14 variables:
    ##  $ age           : int  25 38 28 44 34 63 24 55 65 36 ...
    ##  $ workclass     : Factor w/ 8 levels " Federal-gov",..: 4 4 2 4 4 6 4 4 4 1 ...
    ##  $ fnlwgt        : int  226802 89814 336951 160323 198693 104626 369667 104996 184454 212465 ...
    ##  $ education     : Factor w/ 16 levels " 10th"," 11th",..: 2 12 8 16 1 15 16 6 12 10 ...
    ##  $ marital       : Factor w/ 7 levels " Divorced"," Married-AF-spouse",..: 5 3 3 3 5 3 5 3 3 3 ...
    ##  $ occupation    : Factor w/ 14 levels " Adm-clerical",..: 7 5 11 7 8 10 8 3 7 1 ...
    ##  $ relationship  : Factor w/ 6 levels " Husband"," Not-in-family",..: 4 1 1 1 2 1 5 1 1 1 ...
    ##  $ race          : Factor w/ 5 levels " Amer-Indian-Eskimo",..: 3 5 5 3 5 5 5 5 5 5 ...
    ##  $ sex           : Factor w/ 2 levels " Female"," Male": 2 2 2 2 2 2 1 2 2 2 ...
    ##  $ capital.gain  : int  0 0 0 7688 0 3103 0 0 6418 0 ...
    ##  $ capital.loss  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ hours.per.week: int  40 50 40 40 30 32 40 10 40 40 ...
    ##  $ native.country: Factor w/ 40 levels " Cambodia"," Canada",..: 38 38 38 38 38 38 38 38 38 38 ...
    ##  $ class         : Factor w/ 2 levels " <=50K."," >50K.": 1 1 2 2 1 2 1 1 2 1 ...

### Logistic Classifier

``` r
fit = suppressWarnings(glm(formula = class ~ .,
          family = binomial,
          data = train))
```

### Prediction and Confusion Matrix

``` r
# Predicting the Test set results
prob_pred = predict(fit, type = 'response', newdata = test[-14])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

### Confusion Matrix
cm = table(test[, 14], y_pred)
cm
```

    ##          y_pred
    ##               0     1
    ##    <=50K. 10530   830
    ##    >50K.   1465  2235

Model has a 84.8% accuracy rate.
