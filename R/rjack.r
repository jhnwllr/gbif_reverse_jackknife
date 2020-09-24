# library(biogeo)

rjack = function (d) {

xx = d
d = unique(d)
# rng = diff(range(d))
mx = mean(d)
n = length(d)
n1 = n - 1
t1 = (0.95 * sqrt(n)) + 0.2

x = sort(d)
y = rep(0, n1)

for (i in 1:n1) {
x1 = x[i + 1] # lagged 
if (x[i] < mx) { # if less than mean 
y[i] = (x1 - x[i]) * (mx - x[i])
}
else {
y[i] = (x1 - x[i]) * (x1 - mx)
}
}

my = mean(y)
z = y/(sqrt(sum((y - my)^2)/n1))
out = rep(0, length(xx))

if (any(z > t1)) {
f = which(z > t1)
v = x[f]
print(v)
if (v < median(x)) {
xa = (xx <= v) * 1
out = out + xa
}
if (v > median(x)) {
xb = (xx >= v) * 1
out = out + xb
}

} else {
out = out
}

f = which(out == 1)
}

set.seed(1)
x = c(rnorm(10,mean=10,sd=2),1)
a = rjack(x)
x[a]

library(dplyr)

d = x %>% 
data.frame(d = .) %>% 
mutate(xx = d) %>% 
mutate(mx = mean(d)) %>% 
mutate(n = length(d)) %>% 
mutate(n1 = n - 1) %>% 
mutate(t1 = (0.95 * sqrt(n)) + 0.2) %>%
mutate(x = sort(d)) %>% 
mutate(x1 = lead(x,1)) %>% 
mutate(y = ifelse(
x < mx,
(x1 - x) * (mx - x),
(x1 - x) * (x1 - mx)
)) %>%
mutate(my = mean(y,na.rm=TRUE)) %>%
mutate(z = (y/(sqrt(sum((y - my)^2,na.rm=TRUE)/n1)))) 

d 

v = d %>%
filter(z > t1) %>%
pull(x)

v
outliers = d %>%
mutate(greater = v > median(x)) %>%
mutate(less = v < median(x)) %>%
filter( (greater & xx >= v) | (less & xx <= v) ) %>%
glimpse()

outliers


# mutate(out = ifelse(x < median(x) | x > median(x),"filter"))


# if(v < median(x)) {
# xa = (xx <= v) * 1
# out = out + xa
# }
# if(v > median(x)) {
# xb = (xx >= v) * 1
# out = out + xb
# }

 
# mutate(z_t1_filter = z > t1) %>% 
 

# %>% 
# glimpse()
# select(x,t1,z)
# mutate(z > t1)

# mutate(f = which(z > t1))


# v = x[f]

