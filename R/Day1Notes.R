

x <- letters[1:3]
is.vector(x)

names(x) <- LETTERS[1:3]
is.vector(x)

vec1 <- c(TRUE, FALSE)
class(vec1*1L)


x <- c(1, 1, NA, NA)
y <- c(1, NA, 1, NA)


is.na(x)
is.na(y)

Reduce(function(a, b) , x, y, accumulate = FALSE)


out1 <- Map(is.na, list(x, y))
Reduce("&", out1)


x <- c(F, F, T, F, F, F, F, T, F, T)
rle(x)
cumsum(x) + !x[1]

cumsum(lag(x))


df <- mtcars

x <- list(
  1:4, 
  c(letters[1:3]), 
  runif(5)
)


funlist <- list(length, mean, median, sd)

x <- c(1:10)

lapply(funlist, function(f) f(x))


library(purrr)

map_dbl(x, length)
Map(length, x)
?map_dbl

wts <- seq(0, 1, length.out = 10)
x <- 1:10


Map(function(x, y) mean(x)*y, x, wts )



df <- mtcars


map_dbl(c(-10, 0, 10, 100), rnorm(, 10))

vec <- c(-10, 0, 10, 100)

Map(rnorm, list(10, vec))

map_dbl(veclist, rnorm(10, ))

Map(rnorm, 10, veclist)
Map(function(x) length(unique(x)), iris)

map_df(iris, function(x) length(unique(x)))
map_int(iris, function(x) length(unique(x)))
map_int(iris, ~length(unique(.)))

map_dbl(mtcars, mean)
map(vec, function(f) rnorm(10, f))
map(vec, rnorm, n = 10)

Map(rnorm, 10, vec)



library(data.table)
mt_dt <- as.data.table(mtcars)


mt_dt[, lapply(.SD, uniqueN)]


df <- data.frame(a = 1L, b = 1.5, y = Sys.time(), z = ordered(1))

sapply(df, class)
lapply(df, class)
sapply(df[1:3], class)
sapply(df[1:2], class)
sapply(df[], class)


map_chr(df[1:4], ~class(.)[1])

mt_dt[, lapply(.SD, mean)]

mt_dt[(0)]
mt_dt[, c(0)]


df <- data.frame(x = 1:26, y = letters)

col_means(df)

dt <- as.data.table(df)

dt[, lapply(.SD, mean)]

vapply(df, is.numeric, logical(1L))
lapply(df, is.numeric)
sapply(df, is.numeric)



by_cyl <- split(mtcars, mtcars$cyl)
library(purrr)
models <- map(by_cyl, ~lm(mpg ~ wt, data = .))
str(models)

#Summary for each model
Map(summary, models)
model_summaries <- map(models, summary)
model_summaries$`4`$r.squared

map(model_summaries, function(f) f[["r.squared"]])


Map(function(f) f[["r.squared"]], model_summaries)

lapply(model_summaries, function(f) f[["r.squared"]])
map(model_summaries, function(f) f[["coefficients"]])

extracts <- c("r.squared", "coefficients")
ll <- lapply(extracts, function(f) Map("[[", model_summaries, f))

summary(1234567, digits = 7)
summary(1234567)

summary(c(1234567, 1234568))




map_df(model_summaries, broom::tidy) #much tidier
map(model_summaries, broom::tidy)

map(models, broom::tidy)

?broom::tidy.lm
#map_df(model_summaries, ggplot2::fortify) #much tidier

#vs

models %>%
  map(summary) %>%
  map("r.squared") #magic

library(purrr)
urls <- c(
  "http://google.com",
  "https://en.wikipedia.org",
  "asdfasdasdkfjlda"
)

contents <- map(urls, readLines)
contents <- urls %>% map(safely(readLines))
str(contents)

cnts <- transpose(contents)
ok <- map_lgl(cnts$error, is.null)

urls[!ok]
a <- cnts$result[ok]


my_mean <- c(5, 10, -3)
my_sd   <- c(1, 2, 3)
n <- c(3, 2, 7)

set.seed(1)
map2(my_mean, my_sd, rnorm, n = 10)
set.seed(1)
Map(rnorm, 10, my_mean, my_sd)

set.seed(1)
Map(rnorm, n, my_mean, my_sd)

funs <- list(runif, rnorm, rpois)
param <- list(
  list(min = -1, max = 1),
  list(sd = 5),
  list(lambda = 10)
)

invoke_map(funs, param, n = 5)

Map(funs, param, n = 5)

Map(do.call, funs, param, n = 5)


df <- data.frame(x = 1:10)
attr(df, "altname") <- "myDF"


df
attributes(df)

df2 <- df[1:5, , drop = FALSE]
df2
attributes(df2)

myfun <- function(x) {
  x + 1
}

myfctrs <- factor(3:1, labels = letters[1:3])
attributes(myfctrs)

attr(myfctrs, "levels")
labels(myfctrs)
myfctrs

as.integer(myfctrs)

attr(myfun, "myattr") <- "myFun"

myfun


my_date <- Sys.Date()


attr(my_date)
typeof(my_date)
str(my_date)
my_date_uc <- unclass(my_date)

lubridate::as_date(my_date_uc, origin = "1970-01-01")


my_time <- Sys.time()
typeof(my_time)
str(my_time)
my_time_uc <- unclass(my_time)

data.table::as.ITime(my_time_uc, origin = "1970-01-01")

source("./data/day-1/rv.R")

x <- c(-1, 0, 1, 2, 3)  
p <- c(0.2, 0.1, 0.3, 0.1, 0.3)  


structure(x, prob = p)

rv <- function(x, probs = NULL) {  
  if (is.null(probs)) {  
    probs <- rep(1, length(x)) / length(x)  
  }  
  structure(x, probs = probs, class = "rv")  
}  


rv(x)


x <- rv(1:6)
str(x)
x

print.rv <- function(x, ...) {
  cat("THIS IS MY METHOD :)\n")
}

print.rv(x)

print.factor(mtcars) # you can do this, but probably should not


