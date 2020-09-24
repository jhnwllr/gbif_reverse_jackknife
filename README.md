
## reverse jackknifing for outlier detection 

This project is a scala/Spark port of Arthur Chapman's reverse jackknifing approach to finding bioclimatic outliers within occurrence data. It is intended for **internal usage** within GBIF.

Currently it uses bioclim data from [19 bioclimatic surfaces](https://www.worldclim.org/data/bioclim.html) at a 0.1 degree resolution. 




## run this project 

## data preparations 





1. Build Project
```
sbt package
```

2. Copy file packaged jar onto 
```
scp -r /cygdrive/c/Users/ftw712/Desktop/gbif_reverse_jackknife/target/scala-2.11/gbif_reverse_jackknife_2.11-0.1.jar jwaller@c5gateway-vh.gbif.org:/home/jwaller/
```

3. Run
```
spark2-submit --num-executors 40 --executor-cores 5 --driver-memory 8g --driver-cores 4 --executor-memory 16g gbif_reverse_jackknife_2.11-0.1.jar
```




## plotted examples

![](https://raw.githubusercontent.com/jhnwllr/gbif_reverse_jackknife/master/plots/raster_plots/8978926.jpg)

bioclim 


This implementation is a translation from R to scala of `biogeo::rjack`. [source](https://github.com/cran/biogeo/blob/master/R/rjack.R). It also similar to what is available in  DivaGIS.


```R

rjack <-
function (d) 
{
    xx <- d
    d <- unique(d)
    rng <- diff(range(d))
    mx <- mean(d)
    n <- length(d)
    n1 <- n - 1
    t1 <- (0.95 * sqrt(n)) + 0.2
    x <- sort(d)
    y <- rep(0, n1)
    for (i in 1:n1) {
        x1 <- x[i + 1]
        if (x[i] < mx) {
            y[i] <- (x1 - x[i]) * (mx - x[i])
        }
        else {
            y[i] <- (x1 - x[i]) * (x1 - mx)
        }
    }
    my <- mean(y)
    z <- y/(sqrt(sum((y - my)^2)/n1))
    out <- rep(0, length(xx))
    if (any(z > t1)) {
        f <- which(z > t1)
        v <- x[f]
        if (v < median(x)) {
            xa <- (xx <= v) * 1
            out <- out + xa
        }
        if (v > median(x)) {
            xb <- (xx >= v) * 1
            out <- out + xb
        }
    }
    else {
        out <- out
    }
    f <- which(out == 1)
}

```

## References 

Chapman, A.D. (2005) Principles and Methods of Data Cleaning - Primary Species and Species- Occurrence Data, version 1.0. Report for the Global Biodiversity Information Facility, Copenhagen.








