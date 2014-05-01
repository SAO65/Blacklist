LA BLACK LIST DELLA CONSOB. CRISI FINANZIARIE E OBBLIGHI INFORMATIVI PER LE SOCIETÀ QUOTATE
========================================================
A. Danovi, S. Olgiati, A. Morlin Visconti 
--------------------------------------------------------


```r
library(MASS)
library(lattice)
library(RCurl)
```

```
## Loading required package: bitops
```



```r
raw.data = read.csv("/Users/stefanoolgiati/Desktop/BlackList/blacklist.csv")
names(raw.data)
```

```
## [1] "ENTITY"    "IN"        "OUT"       "CAUSE_OUT" "BIN"       "SECTOR"   
## [7] "EMP"       "REV"       "CAP"
```

```r
dim(raw.data)
```

```
## [1] 95  9
```



```r
clean.data <- na.omit(raw.data)
dim(clean.data)
```

```
## [1] 18  9
```



```r
scaled.data <- scale(clean.data[, -1:-6])
scaled.data
```

```
##        EMP     REV      CAP
## 2  -0.6749 -0.2691 -0.95745
## 6  -0.5819 -0.4826  0.07660
## 9  -0.5154  1.6815  2.48937
## 11 -0.6085 -0.7407 -0.95745
## 14  1.8768  0.6839  0.42128
## 15 -0.5819 -0.1699  1.45532
## 27 -0.3161 -0.7407 -0.61277
## 30  2.6742  2.6097 -0.09574
## 32  0.6142  0.3761  0.42128
## 34  1.3452  1.3688  1.11064
## 36  0.1491 -0.5173 -0.61277
## 38 -0.5819 -0.6166  1.11064
## 48 -0.7786 -0.8648 -0.78511
## 50 -0.7374 -0.8002  0.07660
## 54 -0.5154 -0.7903 -0.54383
## 67 -0.3918  0.3761 -1.02639
## 70  0.1796 -0.4180 -0.61277
## 77 -0.5553 -0.6861 -0.95745
## attr(,"scaled:center")
##    EMP    REV    CAP 
## 587.83 174.22  37.78 
## attr(,"scaled:scale")
##    EMP    REV    CAP 
## 752.43 201.47  29.01
```

```r
class(scaled.data)
```

```
## [1] "matrix"
```

```r
scaled.data.frame <- as.data.frame(scaled.data)
class(scaled.data.frame)
```

```
## [1] "data.frame"
```

```r
scaled.data.frame
```

```
##        EMP     REV      CAP
## 2  -0.6749 -0.2691 -0.95745
## 6  -0.5819 -0.4826  0.07660
## 9  -0.5154  1.6815  2.48937
## 11 -0.6085 -0.7407 -0.95745
## 14  1.8768  0.6839  0.42128
## 15 -0.5819 -0.1699  1.45532
## 27 -0.3161 -0.7407 -0.61277
## 30  2.6742  2.6097 -0.09574
## 32  0.6142  0.3761  0.42128
## 34  1.3452  1.3688  1.11064
## 36  0.1491 -0.5173 -0.61277
## 38 -0.5819 -0.6166  1.11064
## 48 -0.7786 -0.8648 -0.78511
## 50 -0.7374 -0.8002  0.07660
## 54 -0.5154 -0.7903 -0.54383
## 67 -0.3918  0.3761 -1.02639
## 70  0.1796 -0.4180 -0.61277
## 77 -0.5553 -0.6861 -0.95745
```

```r
input.data <- data.frame(BIN = na.omit(clean.data$BIN), scaled.data.frame)
input.data
```

```
##    BIN     EMP     REV      CAP
## 2    1 -0.6749 -0.2691 -0.95745
## 6    1 -0.5819 -0.4826  0.07660
## 9    1 -0.5154  1.6815  2.48937
## 11   0 -0.6085 -0.7407 -0.95745
## 14   0  1.8768  0.6839  0.42128
## 15   1 -0.5819 -0.1699  1.45532
## 27   0 -0.3161 -0.7407 -0.61277
## 30   0  2.6742  2.6097 -0.09574
## 32   1  0.6142  0.3761  0.42128
## 34   1  1.3452  1.3688  1.11064
## 36   1  0.1491 -0.5173 -0.61277
## 38   1 -0.5819 -0.6166  1.11064
## 48   1 -0.7786 -0.8648 -0.78511
## 50   1 -0.7374 -0.8002  0.07660
## 54   1 -0.5154 -0.7903 -0.54383
## 67   0 -0.3918  0.3761 -1.02639
## 70   0  0.1796 -0.4180 -0.61277
## 77   0 -0.5553 -0.6861 -0.95745
```



```r
attach(input.data)
cor(input.data[, -1])
```

```
##        EMP    REV    CAP
## EMP 1.0000 0.7544 0.1445
## REV 0.7544 1.0000 0.4851
## CAP 0.1445 0.4851 1.0000
```

```r
par(mfrow = (c(1, 3)))
qqnorm(CAP, main = "Capitalizzazione", xlab = NA)
qqline(CAP, col = 2)
qqnorm(REV, main = "Ricavi", ylab = NA)
qqline(REV, col = 2)
qqnorm(EMP, main = "Organico", xlab = NA, ylab = NA)
qqline(EMP, col = 2)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 



```r
attach(clean.data)
```

```
## The following objects are masked from input.data:
## 
##     BIN, CAP, EMP, REV
```

```r
lda.fit <- lda(BIN ~ EMP + REV + CAP, data = clean.data)
lda.fit
```

```
## Call:
## lda(BIN ~ EMP + REV + CAP, data = clean.data)
## 
## Prior probabilities of groups:
##      0      1 
## 0.3889 0.6111 
## 
## Group means:
##     EMP   REV   CAP
## 0 895.1 205.4 21.86
## 1 392.3 154.4 47.91
## 
## Coefficients of linear discriminants:
##            LD1
## EMP -0.0006589
## REV -0.0022781
## CAP  0.0411477
```

```r
plot(lda.fit, dimen = 1, type = "both")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 



```r
lda.pred <- predict(lda.fit, clean.data)
names(lda.pred)
```

```
## [1] "class"     "posterior" "x"
```

```r
lda.class <- lda.pred$class
table(lda.class, BIN)
```

```
##          BIN
## lda.class 0 1
##         0 6 2
##         1 1 9
```

```r
mean(lda.class == BIN)
```

```
## [1] 0.8333
```

