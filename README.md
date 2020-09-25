
## Reverse jackknifing for outlier detection 

This project is a scala/Spark port of Arthur Chapman's **reverse jackknifing** approach to finding bioclimatic outliers within occurrence data. It is intended for **internal usage** within GBIF.

Currently it uses bioclim data from [19 bioclimatic surfaces](https://www.worldclim.org/data/bioclim.html) at a 0.1 degree resolution. 

## run this project 

Run [bioclim_extract.r]( https://github.com/jhnwllr/gbif_reverse_jackknife/blob/master/R/bioclim_extract.r) to create `bioclim_0.1_extract.tsv` 

Copy the `bioclim_0.1_extract.tsv` onto the server and put into hdfs. 
```
scp -r /cygdrive/c/Users/ftw712/Desktop/gbif_reverse_jackknife/data/bioclim_0.1_extract.tsv jwaller@c5gateway-vh.gbif.org:/home/jwaller/
hdfs dfs -put bioclim_0.1_extract.tsv bioclim_0.1_extract.tsv
hdfs dfs -ls
```

1. Build project
```
sbt package
```

2. Copy file packaged jar onto 
```
scp -r /cygdrive/c/Users/ftw712/Desktop/gbif_reverse_jackknife/target/scala-2.11/gbif_reverse_jackknife_2.11-0.1.jar jwaller@c5gateway-vh.gbif.org:/home/jwaller/
```

3. Run with spark2-submit
```
spark2-submit --num-executors 40 --executor-cores 5 --driver-memory 8g --driver-cores 4 --executor-memory 16g gbif_reverse_jackknife_2.11-0.1.jar
```

## Plotted example

Here I plot an example of the results of a primate species with one **outlier** in Oregon State.

[The point(s)](https://www.gbif.org/occurrence/1145339223) occur(s) at the Oregon Regional Primate Research Center. 

Cumulative frequency plots are to the right. Bioclimatic surface values are on the x-axis and frequency on the y-axis. Black-colored points very much to the right or left or right are outliers. I also write the number of other surfaces that flagged this point as an outlier as a little number next to the point. 

Because of multiple-comparisons, a point should not be considered a "true outlier" unless it an outlier in **> 5 surfaces**. 

![](https://raw.githubusercontent.com/jhnwllr/gbif_reverse_jackknife/master/plots/raster_plots/8978926.jpg)


This implementation is a translation from R to scala of `biogeo::rjack` [source](https://github.com/cran/biogeo/blob/master/R/rjack.R). It also similar to what is available in  DivaGIS.

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








