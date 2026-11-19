# Simple Formula Interface

This simple formula interface handles formulae of the form
`dependent ~ independent | group1 + group2 + ...`.

## Usage

``` r
parse_formula(grouping)
```

## Arguments

- grouping:

  a model formula specifying dependent, independent and grouping
  variables in the form:
  `dependent ~ independent | group1 + group2 + ...`.

## Value

a list with the elements `valuevar`, `timevar`, and `groups`

## Details

This function is used by
[`multisplit`](http://tpetzoldt.github.io/growthrates/reference/multisplit.md)
and normally not called by the user.

## See also

[`multisplit`](http://tpetzoldt.github.io/growthrates/reference/multisplit.md),
[`split`](https://rdrr.io/r/base/split.html)

## Examples

``` r

parse_formula(y ~ x | a+b+c)
#> $valuevar
#> [1] "y"
#> 
#> $timevar
#> [1] "x"
#> 
#> $groups
#> [1] "a" "b" "c"
#> 
```
