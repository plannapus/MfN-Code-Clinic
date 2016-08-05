#Basics on writing functions

> add1 <- function(x){
+ x+1
+ }
> add1(10)
[1] 11
> addN <- function(x, n){x+n}
> addN(10, 5)
[1] 15
> addN <- function(x, ...){sum(x, ...)}
> addN(10, 2, 3, 4, 5)
[1] 24
> addN(10, 2, 3, 4, NA, 5)
[1] NA
> addN(10, 2, 3, 4, NA, 5, na.rm=TRUE)
[1] 24
> addN <- function(x, ...)sum(x, ...)

#Looking for a function definition:
#See also: http://stackoverflow.com/questions/19226816/how-can-i-view-the-source-code-for-a-function for a complete guide

> sum
function (..., na.rm = FALSE)  .Primitive("sum")
> mean
function (x, ...) 
UseMethod("mean")
<bytecode: 0x7fb6f95853c8>
<environment: namespace:base>
> methods(mean)
[1] mean.Date     mean.default  mean.difftime mean.POSIXct  mean.POSIXlt 
see '?methods' for accessing help and source code
> mean.default
function (x, trim = 0, na.rm = FALSE, ...) 
{
    if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
        warning("argument is not numeric or logical: returning NA")
        return(NA_real_)
    }
    if (na.rm) 
        x <- x[!is.na(x)]
    if (!is.numeric(trim) || length(trim) != 1L) 
        stop("'trim' must be numeric of length one")
    n <- length(x)
    if (trim > 0 && n) {
        if (is.complex(x)) 
            stop("trimmed means are not defined for complex data")
        if (anyNA(x)) 
            return(NA_real_)
        if (trim >= 0.5) 
            return(stats::median(x, na.rm = FALSE))
        lo <- floor(n * trim) + 1
        hi <- n + 1 - lo
        x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]
    }
    .Internal(mean(x))
}
<bytecode: 0x7fb6f95add50>
<environment: namespace:base>
> library(scales)
> alpha
function (colour, alpha = NA) 
{
    col <- grDevices::col2rgb(colour, TRUE)/255
    if (length(colour) != length(alpha)) {
        if (length(colour) > 1 && length(alpha) > 1) {
            stop("Only one of colour and alpha can be vectorised")
        }
        if (length(colour) > 1) {
            alpha <- rep(alpha, length.out = length(colour))
        }
        else if (length(alpha) > 1) {
            col <- col[, rep(1, length(alpha)), drop = FALSE]
        }
    }
    alpha[is.na(alpha)] <- col[4, ][is.na(alpha)]
    new_col <- grDevices::rgb(col[1, ], col[2, ], col[3, ], alpha)
    new_col[is.na(colour)] <- NA
    new_col
}
<environment: namespace:scales>

#Creating a new function based on an existing function (aka "The Kobayashi-Maru", dixit Olszewski)

> alpha2 <- function (colour, alpha = NA, blue) 
+ {
+     col <- grDevices::col2rgb(colour, TRUE)/255
+     if (length(colour) != length(alpha)) {
+         if (length(colour) > 1 && length(alpha) > 1) {
+             stop("Only one of colour and alpha can be vectorised")
+         }
+         if (length(colour) > 1) {
+             alpha <- rep(alpha, length.out = length(colour))
+         }
+         else if (length(alpha) > 1) {
+             col <- col[, rep(1, length(alpha)), drop = FALSE]
+         }
+     }
+     alpha[is.na(alpha)] <- col[4, ][is.na(alpha)]
+     new_col <- grDevices::rgb(col[1, ], col[2, ], blue, alpha)
+     new_col[is.na(colour)] <- NA
+     new_col
+ }
> alpha("red", 0.3)
[1] "#FF00004D"
> alpha2("red", 0.3, blue=0.5)
[1] "#FF00804D"
> plot(1:3, col=c("red", alpha("red",0.5), alpha2("red", 0.5, 0.5)), pch=19, cex=3)
> alpha2 <- function (colour, alpha = NA, blue = 0.5) #Adding a default value for an argument
+ {
+     col <- grDevices::col2rgb(colour, TRUE)/255
+     if (length(colour) != length(alpha)) {
+         if (length(colour) > 1 && length(alpha) > 1) {
+             stop("Only one of colour and alpha can be vectorised")
+         }
+         if (length(colour) > 1) {
+             alpha <- rep(alpha, length.out = length(colour))
+         }
+         else if (length(alpha) > 1) {
+             col <- col[, rep(1, length(alpha)), drop = FALSE]
+         }
+     }
+     alpha[is.na(alpha)] <- col[4, ][is.na(alpha)]
+     new_col <- grDevices::rgb(col[1, ], col[2, ], blue, alpha)
+     new_col[is.na(colour)] <- NA
+     new_col
+ }
> plot(1:3, col=c("red", alpha("red",0.5), alpha2("red", 0.5)), pch=19, cex=3)
> plot(1:3, col=c("red", alpha("red",0.5), alpha2("red", 0.5, 0.8)), pch=19, cex=3)

#Tapply, lapply etc.
> tapply(1:10, cut(1:10, c(0,5,10)), mean)
 (0,5] (5,10] 
     3      8 
> tapply(1:10, cut(1:10, c(0,5,10)), sum)
 (0,5] (5,10] 
    15     40 
> cut(1:10, c(0,5,10))
 [1] (0,5]  (0,5]  (0,5]  (0,5]  (0,5]  (5,10] (5,10] (5,10] (5,10] (5,10]
Levels: (0,5] (5,10]
> tapply(c(1:10,NA), cut(c(1:10,NA), c(0,5,10)), function(x){sum(x+3)})
 (0,5] (5,10] 
    30     55 
> a=1:10
> b=5:1
> tapply(c(1:10,NA), cut(c(1:10,NA), c(0,5,10)), function(x){cor(x,b)})
 (0,5] (5,10] 
    -1     -1 

> outer(matrix(1:4,nrow=2),matrix(5:8, nrow=2) , "*")
, , 1, 1

     [,1] [,2]
[1,]    5   15
[2,]   10   20

, , 2, 1

     [,1] [,2]
[1,]    6   18
[2,]   12   24

, , 1, 2

     [,1] [,2]
[1,]    7   21
[2,]   14   28

, , 2, 2

     [,1] [,2]
[1,]    8   24
[2,]   16   32

> sapply(1:10, function(x)sapply(1:4, function(y) x*y))
     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
[1,]    1    2    3    4    5    6    7    8    9    10
[2,]    2    4    6    8   10   12   14   16   18    20
[3,]    3    6    9   12   15   18   21   24   27    30
[4,]    4    8   12   16   20   24   28   32   36    40
> lapply(1:10, function(x)sapply(1:4, function(y) x*y))
[[1]]
[1] 1 2 3 4

[[2]]
[1] 2 4 6 8

[[3]]
[1]  3  6  9 12

[[4]]
[1]  4  8 12 16

[[5]]
[1]  5 10 15 20

[[6]]
[1]  6 12 18 24

[[7]]
[1]  7 14 21 28

[[8]]
[1]  8 16 24 32

[[9]]
[1]  9 18 27 36

[[10]]
[1] 10 20 30 40

> lapply(1:10, function(x)lapply(1:4, function(y) x*y))
[[1]]
[[1]][[1]]
[1] 1

[[1]][[2]]
[1] 2

[[1]][[3]]
[1] 3

[[1]][[4]]
[1] 4


[[2]]
[[2]][[1]]
[1] 2

[[2]][[2]]
[1] 4

[[2]][[3]]
[1] 6

[[2]][[4]]
[1] 8


[[3]]
[[3]][[1]]
[1] 3

[[3]][[2]]
[1] 6

[[3]][[3]]
[1] 9

[[3]][[4]]
[1] 12


[[4]]
[[4]][[1]]
[1] 4

[[4]][[2]]
[1] 8

[[4]][[3]]
[1] 12

[[4]][[4]]
[1] 16


[[5]]
[[5]][[1]]
[1] 5

[[5]][[2]]
[1] 10

[[5]][[3]]
[1] 15

[[5]][[4]]
[1] 20


[[6]]
[[6]][[1]]
[1] 6

[[6]][[2]]
[1] 12

[[6]][[3]]
[1] 18

[[6]][[4]]
[1] 24


[[7]]
[[7]][[1]]
[1] 7

[[7]][[2]]
[1] 14

[[7]][[3]]
[1] 21

[[7]][[4]]
[1] 28


[[8]]
[[8]][[1]]
[1] 8

[[8]][[2]]
[1] 16

[[8]][[3]]
[1] 24

[[8]][[4]]
[1] 32


[[9]]
[[9]][[1]]
[1] 9

[[9]][[2]]
[1] 18

[[9]][[3]]
[1] 27

[[9]][[4]]
[1] 36


[[10]]
[[10]][[1]]
[1] 10

[[10]][[2]]
[1] 20

[[10]][[3]]
[1] 30

[[10]][[4]]
[1] 40


> array(1:4, c(2,2))
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> apply(array(1:4, c(2,2)), 1, sum)
[1] 4 6
> apply(array(1:4, c(2,2)), 2, sum)
[1] 3 7

> apply(array(1:8, c(2,2,2)), 3, sum)
[1] 10 26
> apply(array(1:8, c(2,2,2)), c(1,2), sum)
     [,1] [,2]
[1,]    6   10
[2,]    8   12

> data(mtcars)
> head(mtcars)
                   mpg cyl disp  hp drat    wt  qsec vs am gear carb
Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
> split(mtcars, mtcars$gear)
$`3`
                     mpg cyl  disp  hp drat    wt  qsec vs am gear carb
Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2

$`4`
                mpg cyl  disp  hp drat    wt  qsec vs am gear carb
Mazda RX4      21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
Mazda RX4 Wag  21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
Datsun 710     22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
Merc 240D      24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
Merc 230       22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
Merc 280       19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
Merc 280C      17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
Fiat 128       32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
Honda Civic    30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
Toyota Corolla 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
Fiat X1-9      27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
Volvo 142E     21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2

$`5`
                mpg cyl  disp  hp drat    wt qsec vs am gear carb
Porsche 914-2  26.0   4 120.3  91 4.43 2.140 16.7  0  1    5    2
Lotus Europa   30.4   4  95.1 113 3.77 1.513 16.9  1  1    5    2
Ford Pantera L 15.8   8 351.0 264 4.22 3.170 14.5  0  1    5    4
Ferrari Dino   19.7   6 145.0 175 3.62 2.770 15.5  0  1    5    6
Maserati Bora  15.0   8 301.0 335 3.54 3.570 14.6  0  1    5    8

> lapply(split(mtcars, mtcars$gear), function(x) cor(x$mpg, x$disp))
$`3`
[1] -0.7249926

$`4`
[1] -0.9011593

$`5`
[1] -0.8806232


> lapply(split(mtcars, mtcars$gear), function(x) split(x, x$carb))
$`3`
$`3`$`1`
                mpg cyl  disp  hp drat    wt  qsec vs am gear carb
Hornet 4 Drive 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
Valiant        18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
Toyota Corona  21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1

$`3`$`2`
                   mpg cyl disp  hp drat    wt  qsec vs am gear carb
Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
Dodge Challenger  15.5   8  318 150 2.76 3.520 16.87  0  0    3    2
AMC Javelin       15.2   8  304 150 3.15 3.435 17.30  0  0    3    2
Pontiac Firebird  19.2   8  400 175 3.08 3.845 17.05  0  0    3    2

$`3`$`3`
             mpg cyl  disp  hp drat   wt qsec vs am gear carb
Merc 450SE  16.4   8 275.8 180 3.07 4.07 17.4  0  0    3    3
Merc 450SL  17.3   8 275.8 180 3.07 3.73 17.6  0  0    3    3
Merc 450SLC 15.2   8 275.8 180 3.07 3.78 18.0  0  0    3    3

$`3`$`4`
                     mpg cyl disp  hp drat    wt  qsec vs am gear carb
Duster 360          14.3   8  360 245 3.21 3.570 15.84  0  0    3    4
Cadillac Fleetwood  10.4   8  472 205 2.93 5.250 17.98  0  0    3    4
Lincoln Continental 10.4   8  460 215 3.00 5.424 17.82  0  0    3    4
Chrysler Imperial   14.7   8  440 230 3.23 5.345 17.42  0  0    3    4
Camaro Z28          13.3   8  350 245 3.73 3.840 15.41  0  0    3    4


$`4`
$`4`$`1`
                mpg cyl  disp hp drat    wt  qsec vs am gear carb
Datsun 710     22.8   4 108.0 93 3.85 2.320 18.61  1  1    4    1
Fiat 128       32.4   4  78.7 66 4.08 2.200 19.47  1  1    4    1
Toyota Corolla 33.9   4  71.1 65 4.22 1.835 19.90  1  1    4    1
Fiat X1-9      27.3   4  79.0 66 4.08 1.935 18.90  1  1    4    1

$`4`$`2`
             mpg cyl  disp  hp drat    wt  qsec vs am gear carb
Merc 240D   24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
Merc 230    22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
Honda Civic 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
Volvo 142E  21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2

$`4`$`4`
               mpg cyl  disp  hp drat    wt  qsec vs am gear carb
Mazda RX4     21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
Mazda RX4 Wag 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
Merc 280      19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
Merc 280C     17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4


$`5`
$`5`$`2`
               mpg cyl  disp  hp drat    wt qsec vs am gear carb
Porsche 914-2 26.0   4 120.3  91 4.43 2.140 16.7  0  1    5    2
Lotus Europa  30.4   4  95.1 113 3.77 1.513 16.9  1  1    5    2

$`5`$`4`
                mpg cyl disp  hp drat   wt qsec vs am gear carb
Ford Pantera L 15.8   8  351 264 4.22 3.17 14.5  0  1    5    4

$`5`$`6`
              mpg cyl disp  hp drat   wt qsec vs am gear carb
Ferrari Dino 19.7   6  145 175 3.62 2.77 15.5  0  1    5    6

$`5`$`8`
              mpg cyl disp  hp drat   wt qsec vs am gear carb
Maserati Bora  15   8  301 335 3.54 3.57 14.6  0  1    5    8


> L <-lapply(split(mtcars, mtcars$gear), function(x) split(x, x$carb))
> M <-list(); for(i in seq_along(L)) M <- c(M, L[[i]])
> M
$`1`
                mpg cyl  disp  hp drat    wt  qsec vs am gear carb
Hornet 4 Drive 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
Valiant        18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
Toyota Corona  21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1

$`2`
                   mpg cyl disp  hp drat    wt  qsec vs am gear carb
Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
Dodge Challenger  15.5   8  318 150 2.76 3.520 16.87  0  0    3    2
AMC Javelin       15.2   8  304 150 3.15 3.435 17.30  0  0    3    2
Pontiac Firebird  19.2   8  400 175 3.08 3.845 17.05  0  0    3    2

$`3`
             mpg cyl  disp  hp drat   wt qsec vs am gear carb
Merc 450SE  16.4   8 275.8 180 3.07 4.07 17.4  0  0    3    3
Merc 450SL  17.3   8 275.8 180 3.07 3.73 17.6  0  0    3    3
Merc 450SLC 15.2   8 275.8 180 3.07 3.78 18.0  0  0    3    3

$`4`
                     mpg cyl disp  hp drat    wt  qsec vs am gear carb
Duster 360          14.3   8  360 245 3.21 3.570 15.84  0  0    3    4
Cadillac Fleetwood  10.4   8  472 205 2.93 5.250 17.98  0  0    3    4
Lincoln Continental 10.4   8  460 215 3.00 5.424 17.82  0  0    3    4
Chrysler Imperial   14.7   8  440 230 3.23 5.345 17.42  0  0    3    4
Camaro Z28          13.3   8  350 245 3.73 3.840 15.41  0  0    3    4

$`1`
                mpg cyl  disp hp drat    wt  qsec vs am gear carb
Datsun 710     22.8   4 108.0 93 3.85 2.320 18.61  1  1    4    1
Fiat 128       32.4   4  78.7 66 4.08 2.200 19.47  1  1    4    1
Toyota Corolla 33.9   4  71.1 65 4.22 1.835 19.90  1  1    4    1
Fiat X1-9      27.3   4  79.0 66 4.08 1.935 18.90  1  1    4    1

$`2`
             mpg cyl  disp  hp drat    wt  qsec vs am gear carb
Merc 240D   24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
Merc 230    22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
Honda Civic 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
Volvo 142E  21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2

$`4`
               mpg cyl  disp  hp drat    wt  qsec vs am gear carb
Mazda RX4     21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
Mazda RX4 Wag 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
Merc 280      19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
Merc 280C     17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4

$`2`
               mpg cyl  disp  hp drat    wt qsec vs am gear carb
Porsche 914-2 26.0   4 120.3  91 4.43 2.140 16.7  0  1    5    2
Lotus Europa  30.4   4  95.1 113 3.77 1.513 16.9  1  1    5    2

$`4`
                mpg cyl disp  hp drat   wt qsec vs am gear carb
Ford Pantera L 15.8   8  351 264 4.22 3.17 14.5  0  1    5    4

$`6`
              mpg cyl disp  hp drat   wt qsec vs am gear carb
Ferrari Dino 19.7   6  145 175 3.62 2.77 15.5  0  1    5    6

$`8`
              mpg cyl disp  hp drat   wt qsec vs am gear carb
Maserati Bora  15   8  301 335 3.54 3.57 14.6  0  1    5    8

> lapply(M, function(x)cor(x$mpg, x$disp))
$`1`
[1] -0.3128988

$`2`
[1] 0.9556646

$`3`
[1] NA

$`4`
[1] -0.6492474

$`1`
[1] -0.8963193

$`2`
[1] -0.7940624

$`4`
[1] -0.92976

$`2`
[1] -1

$`4`
[1] NA

$`6`
[1] NA

$`8`
[1] NA

Warning message:
In cor(x$mpg, x$disp) : the standard deviation is zero

#####Edit 5th of August:
#Or more straight-forward:
> lapply(split(mtcars, interaction(mtcars$carb, mtcars$gear)),function(x)cor(x$disp,x$mpg))
$`1.3`
[1] -0.3128988

$`2.3`
[1] 0.9556646

$`3.3`
[1] NA

$`4.3`
[1] -0.6492474

$`6.3`
[1] NA

$`8.3`
[1] NA

$`1.4`
[1] -0.8963193

$`2.4`
[1] -0.7940624

$`3.4`
[1] NA

$`4.4`
[1] -0.92976

$`6.4`
[1] NA

$`8.4`
[1] NA

$`1.5`
[1] NA

$`2.5`
[1] -1

$`3.5`
[1] NA

$`4.5`
[1] NA

$`6.5`
[1] NA

$`8.5`
[1] NA
