# Simple Formula Interface for Grouped Nonlinear Functions

This simple formula interface handles formulae of the form
`dependent ~ FUN(independent, parms) | group1 + group2 + ...`.

## Usage

``` r
parse_formula_nonlin(formula)
```

## Arguments

- formula:

  a model formula specifying dependent and independent variables,
  nonlinear model and grouping variables in the form:
  `dependent ~ FUN(independent, parms) | group1 + group2 + ...`. FUN can
  be a name of an existing growth model (e.g. `grow_logistic`) or a
  valid user-defined function (see
  [`growthmodel`](http://tpetzoldt.github.io/growthrates/reference/growthmodel-constructor.md)).

## Value

a list with the elements `FUN`, `valuevar`, `timevar`, and `groups`

## Details

This function is used by
[`all_growthmodels`](http://tpetzoldt.github.io/growthrates/reference/all_growthmodels.md)
and normally not called for the user.

## See also

[`multisplit`](http://tpetzoldt.github.io/growthrates/reference/multisplit.md),
[`split`](https://rdrr.io/r/base/split.html),
[`parse_formula`](http://tpetzoldt.github.io/growthrates/reference/parse_formula.md)

## Examples

``` r

ret <- parse_formula_nonlin(y ~ f(x, parms) | a + b + c)
```
