# RWorkshop2016
2016 R Developer Workshop NYC


|Date and time|Location|
|:-------------:|:--------:|
|Mon, Sep 12, 9:00 AM<br>-<br>Tue, Sep 13, 5:00 PM|1601 Broadway<br>New York City<br>NY 10019|


-----
##General notes

http://bit.ly/advr16-1  
Wifi: NYECCC@!601

Four standard types of vectors  
    - logical  
    - integer 
    - numeric  
    - character  
    
Two less common  
    - complex  
    - raw  

Check via `r typeof()` or `r class()` or `r mode()`; should never use `r mode()` for reasons that will be explained later.  
Should also not use `r storage.mode()`; both `r mode()` and `r storage.mode()` are provided for S compatibility.

Vectors have:  
    1. Class  
    2. Length  
    3. Attributes  

`r is.vector()`: be very careful, RTFM. Does not actually check if only a vector, but rather if is a vector AND has no
attributes. 

Vector coercion:  
    1. Chr  
    2. Numeric  
    3. Integer  
    4. Logi  
    
mean() of logical is particularly useful as shortcut to get proportion of TRUE  

NA trickiness: don't compare NA using standard equality operators, but rather is.na().  

Consider: `r c(-1, 0, 1)/0`  

Types of missingness; consider:  
    1. `r typeof(NA)` #NA logical  
    2. `r typeof(NA_real_)` #NA double  
    3. `r typeof(NA_integer_)` #NA integer  
    4. `r typeof(NA_character_)` #NA chr  
    
###Data Structures

List vs. vector vs. matrix vs. data.frame  
Consider: `r str(list(TRUE, 1)) #Each element of a list can be different  
Consider: `r c(1, c(2, 3, c(4, 5)))` #Flat vector  
Consider: `r str(list(1, list(2, 3, list(4, 5))))` #Recursive list (nested list)  

Lists can be heterogeneous. Collectively, we call Lists and Atomic vectors "vectors"  
Matrices or arrays are built from atomic vectors with special attributes on top.  
Data frames are built from Lists. A list of vectors where each vector can be its own type.  

`r str()` for lists and more compact printing: see options for `r ?str()`, namely max.level and list.len

###Subsetting

What can you put inside `r []`?  
    1. Logical; consider `r NA` vs. r` NA_integer_`
    2. Positive integer (not strictly subsetting, because you can actually get something longer via recycling)  
    3. Negative integer (removing elements)  
    4. Zero integer (`r 0L`) where you return 0. Not really practically useful, sometimes in testing  
    5. Chr (names), lookup-style for example  
    6. Nothing, which just returns everything. Could be useful for e.g. printing data.tables. Or subsetting n-dim.  
        - Consider: `r mtcars[1:5, ]` or `r mtcars[, c("vs", "am")]`  
        - Consider: `r x[] <- y` vs. `r x <- y` (later)  
        

###Memory usage
x <- rnorm(1e6)  
pryr::object.size(x) #8MB  

x1 <- list(x, x, x)  
pryr::object.size(x1) #Still 8MB  

x1[[1]][[1]] <- NA  
pryr::object.size(x1) #16MB; will copy the first list due to copy-on-modify semantics  

COnsider whether a name "has" an object, and not whether an object doesn't have a name (GC considerations).  
This is a fairly subtle distinction-- the direction of the "arrow" is important from a mem usage perspective.  

Note that `r object.size()` will not be sufficient for this investigation-- would need to use pryr.  


###RStudio shortcuts
Ctrl + Up --> reverse history search  
In the development version of RStudio, Ctrl + Enter (Ctrl + R) will run the entire argument chain, instead of the
literal current line.   
Alt + select will activate multiple cursor (neat)  

Never restore workspace, never save workspace (never save rdata)  
Ctrl + shift + F10 to restart R without restarting RStudio.  
Using rm(list=ls()) does not actually reset the workspace-- but not the loaded packages.  
USE CTRL + SHIFT + F10 INSTEAD  

Alt + Shift + K gives a list of all the shortcuts.  


###Functions and scoping

Formal arguments vs. arguments; consider:  
myfun <- f(x) { x + 1}  
x is the formal argument (formals)  

myfun(5)  
5 is the argument (of the function myfun())  

Infix functions are the ordinary functions, and these go in-between, e.g. `r 2 * 3` where `r *` is infix  
Prefix functions take Infix and put backticks around, e.g. `r `*`(2, 3)`.  
    - Which means that each infix must have two args around it  
    - Likewise, a prefix must have at least two args  


####Function scoping
Remember, if a function cannot find an argument value, it will look for the argument (variable) in the environment
in which it was defined. This has implications for functions defined in-script vs. loaded from a package

####Function best practices
Always create an example  
Consider:  
`r x <- c(1, 1, NA, NA)`
`r y <- c(1, NA, 1, NA)`

Boolean algebra is probably the right way to go. For example:  
`r xy <- Map(is.na, list(x, y))  
Reduce("&", xy)`  

###Writing "good" functions

    1. Correct (does what it should do)-- obviously correct  
    2. Generalizable (general purpose tool)  
    3. Maintainable  
    4. Good errors (i.e. worst case is incorrect and returns incorrect values, silently)  
    5. Performance; fast enough (vectorized if possible). See profvis package  
    6. Testable/decomposable  
    7. Type stable  
    
Must balance tradeoffs:  
Helpful vs. Strict  
Verbose vs. Clever  
Specialized vs. General  
Isolated vs. Contextual (how easy to understand in isolation?)  

Either give the right answer, or a useful error message.  

####Error handling basics

stopifnot(), which is easy for you to write, although not always very helpful error message for users  
Better: `r if(length(x) != length(y)) stop("length of x must equal length of y")`  

Look at e.g. assert, assertthat, assertive, etc. packages to help generate useful error messages.  

####Helpful vs. Strict (Vestigal R helpfulness (ironically))

    - Factor coercion  
    - Dimensionality dropping  
    - Partial name matching with $  

tibbles or data.tables are probably a better option, or at least safer/stricter.  

####Verbse vs. Clever

Sometimes it is better to be verbose vs. an extremely succinct function that will likely be difficult to understand
later. 

x <- c(F, F, T, F, F, F, F, T, F, T) #input  
y <- c(1, 1, 2, 2, 2, 2, 2, 3, 3, 4) #desired output  
cumsum(x) + !x[1] #Probably too succinct, although very clever  

Other tips:
x is the same as x == TRUE  
!x is the same as x == FALSE  
Be careful using `r which()`; logical algebra almost always clearer  

####Isolated vs. Contextual

Isolated code == Easier to understand (need less context to understand what is going on)  
baz <- foo(bar, qux) vs. df2 <- arrange(df, qux). Can you read the single line and understand/infer what is going on?  

Consider:  
df[, vars] #This is type-unstable because of the drop arg not being explicitly set as FALSE  
The danger of subset, where scoping + NSE can create unexpected results depending on globalenv  
What happens if you create df[x = "a"], where you expect a chr but get fctr  

A type-stable function gives the same type of output regardless of input 
A type-unstable function may or may not...  

When writing functions, always return the same thing.  

###NSE

Consider:  
`r big_x <- function(df, threshold) {
dplyr::filter(df, x > 1)
} `  

There are 2 ways this can fail: the first is if x is not in the data.frame, but is in the globalenv.  
The second applies to the same as threshold.  

Currently, you have to handle this scenario manually. In a future version, this will be explicit in the function to 
make this easier.  

Or you could write such functions using only base functions, i.e. "[[".  

Any functions you write with dplyr or ggplot2, or pretty much any functions using NSE, you need to be careful.  

If `r options()` are modified in the current environment, this can also throw things off-- so you must handle this.  
Other things: `r na.action`, time zone, system language, text encoding.  

Only ever use `r options()` to control side-effects of a function. The value of an option should never affect the return 
value of a function.  

Be strict (consistent) on outputs, perhaps be a bit less strict on inputs.  


###Functional Programming

Functional programming tends to emphasize actions, not the steps.  
Consider:  
`r fix_missing <- function(x) {
x[x == -99] <- NA
x
}`  

`r df[] <- map(df, fix_missing)` #[] keeps as data.frame, modifying in-place(?)  

Can set options for partial matching:  
`r options(
warnPartialMatchArgs = TRUE, 
warnPartialMatchDollar = TRUE,
warnPartialMatchAttr = TRUE
)`  

Function factories, e.g. f <- function(i) { function(x) x + i}  
Function operators = take functions as input and return a function  
Functionals = Take function as argument, return a vector/some value/structure  

Consider:  
`r library(purr)  
set.seed(1014)  
l <- rerun(20, runif(sample(1:10, 1)))
str(l)
l`  

If you are going to use a for loop, always allocate the vector ahead of time  
Always define what you are going to loop over, ideally with something like `r seq_along()`.  
    - The reason you do not want to use 1:length():  
        - Consider:  
        - `r x <- runif(0)`  
        - `r seq_along(x)`  
        - `r 1:length(x)`  
        - Length zero input will generate a subscript out of bounds using 1:length since you are asking for start of 1 when
          length is in fact zero.  


A .Primitive indicates implementation in C  
Remember that a name has an object.  An object does not have a name.  

Functions can be arguments-- so you can pass functions as names.  
Using purrr:  
`r map_int(iris, function(x) length(unique(x)))`  
`r map_int(iris, ~length(unique(.)))`  

####Dangers of sapply()

Consider:  
`r df <- data.frame(a = 1L, b = 1.5, y = Sys.time(), z = ordered(1))`
`r str(sapply(df[1:4], class))`  
`r str(sapply(df[1:2], class))`  
`r str(sapply(df[3:4], class))`  

Versus:  
`r str(map_chr(df[1:2], class))`  
`r str(map_chr(df[3:4], class))`  
`r map_chr(df[1:4], ~class(.)[1])`  
`r map_chr(df[1:4], ~class(.))` #error  

Insert `r browser()` into function to debug  

If something takes a data.frame, what happens about input of matrix? What should it return?  
    - One way is to enforce input for non-df, via e.g. `r as.data.frame()`  
    - Another way is to fail (early)  

`r library(purr) library(magrittr)`
col_means <- function(df) {  
stopifnot(is.data.frame(df))  
df %>% keep (is.numeric) %>% map(mean) %>% as.data.frame() 

This function is always going to return dbl; should you return a data.frame or a vector?  
In general, if your function accepts, or the key option your function takes is a df, you should return a df.  
    - Even though a double vector is not wrong, and is conceptually simpler.  
    
###map()
`r by_cyl <- split(mycars, mtcars$cyl)`  

####Fitting a model to each data frame

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
models %>%  
  map(summary) %>%  
  map("r.squared") #magic  
  
###Dealing with failure

`r input <- list(1:10, sqrt(4), 5, "n")`  
`r map(input, log)`  
`r str(map(input, safely(log)))`  

Consider:  
`r urls <- c("https://google.com", "https://wikipedia.org")`

###OOP

Getting, setting values of attributes  

    - names  
    - dim  
    - class  
    
A matrix is not a class. Consider:  
`r x <- matrix(1:20)`  
`r class(x)`  
`r attr(x, "class")`  

####Generic functions

The telltale sign is if you look at the code and it has UseMethod and no code, really.  

The goal of generic functions is to let others extend.  `r mean()` just calls "UseMethod("mean")" where different 
functions are called depending on the object class that is passed in. This is the basis of S3, and is the simplest 
object system you can really have.  

As your functions get more complicated, you may need to return more complicated things. S3 lets you control how objects 
are printed. The print() generic function is something that is really useful to customize, if you do nothing else, and can 
be really useful for e.g. debugging.  

tibbles have different methods from data.frames. One specific example is the print() method for tibbles.  

Print methods are actually quite complicated, but often worth the effort.  

####Constructur functions

Useful for programming to ensure return consistency.  

Can add new methods to your own S3 packages, and provide new generics to let others extend.  

####Complex return values

How is date represented? Date time? What do the values mean (unclass(x).  
How is a linear model represented?  

time <- as.POSIXct(date)  
attr(time, "tzone") <- "UTC"  

Models are lists.  

You can either take a vector and add attributes, or you can take a list.   
Goal: model random variables in R  

`r source("./data/day-1/rv.R")`
Ways to represent the following:  
`r x <- c(-1, 0, 1, 2, 3)`  
`r p <- c(0.2, 0.1, 0.3, 0.1, 0.3)`   

    1. list(x = x, p = p)  
    2. data.frame(x, p)  
    3. cbind(x, p)  
    4. rbind(x, p)  
    5. structure(x, prob = p) #this is probably the easiest  
    6. structure(p, value = x)  
    
Would probably not want to use a matrix (not 3, 4)  
A data.frame is probably a bit overkill  
Probably want to go with structure(x, prob = p)  

All you need to do to create an S3 object is:  
`r structure(x, prob = p, class = "rv")  

But this ease lets us do silly things:  
mod <- lm(log(mpg) ~ log(disp), data = mtcars)  
class(mod)  
mod  

class(mod) <- "data.frame" #Don't do this! But you can...  

The first thing you should do is create a constructor function. Again, there is the strict vs. helpful tension.  

rv <- function(x, probs = NULL) {  
if (is.null(probs)) {  
probs <- rep(1, length(x)) / length(x)  
}  
structure(x, probs = probs, class = "rv")  
}  

You should always create a constructor function.  
The constructor function should also check its inputs.  
Floating point math considerations are important-- exactly equal to 1 needs to be defined, i.e. what is "exactly?"  
Balance strictness and helpfulness.  

Once you have a constructor function, make sure it actually checks inputs and returns.  

####Methods

Once you have a working constructor, move on to methods.  
Most common methods have to do with outputs.  
Use `r methods()` to find methods.  E.g. `r methods("mean")  

In R, methods belong to generic functions.  
`r methods(class = "data.frame")`  

Can also use the method name, e.g. `r \\`[.factor\\``.  More reliable is to use getS3method()  

Methods belong to functions, not classes!  
This is quite important, especially if you have used other languages. Most languages use object.method.  
R does not.  

Why?  

Most OO systems take a data structure, where an object can use functions.  
In R, this matrix is transposed, i.e. functions have methods for objects.  
    - Where object is referring to data structures in most cases.  
    
This is called "generic function style OO" and is used in Julia as well.  

The first thing you wan to do is to find the generics. For something like print:  
`r print`  

If you want to write your own print method:

rv(1:6)  
str(x)  
If you want to define a print method:  
print.rv <- function(x, ...) {
cat("THIS IS MY METHOD :)\\\n")
}  
print(x)  
x  

mean.rv <- function(x, ...) {  
sum(x * probs(x)))  
}  

Often simply about putting some extra custom behavior around the object in your package.  

Again, S3 is the simplest thing that could work, and handles the majority of cases in R.  

####S4

Much more formal and rigorous. Not worthwhile for most programming in R, but finds use in BioConductor.  

####R6

Works much more like the OO you would find in standard OO languages. Probably about 8 different styles you can 
choose from in R. Should probably stick with R3.  

ggplot2 actually implements its own OO system that is used only in ggplot2. Probably not the best decision, but 
was done over a decade ago. An object oriented system that only works in one package.  









  




































