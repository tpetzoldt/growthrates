# Extract or Replace Parts of a 'multiple_fits' Object

Operators to access parts of 'multiple_fits' objects

## Usage

``` r
# S4 method for class 'multiple_fits,ANY,missing'
x[i, j, ..., drop = TRUE]

# S4 method for class 'multiple_fits,ANY,missing'
x[[i, j, ...]]
```

## Arguments

- x:

  object of class multiple_fits

- i:

  numeric or character index

- j:

  NULL (for compatibility with other uses of `[` or `[[`)

- ...:

  optional arguments passed to `[`

- drop:

  If `TRUE` the result is coerced to the lowest possible dimension

## Examples

``` r

data(bactgrowth)
L <- all_splines(value ~ time | strain + conc +replicate, data=bactgrowth)

coef(L[[1]])
#>          y0       mumax 
#> 0.008092251 0.298829003 

plot(L[["R:0:2"]])


# par(mfrow=c(2, 2))
plot(L[1:4])




```
