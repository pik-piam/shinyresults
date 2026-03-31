# extractVariableGroups

Groups variable names by groups based on the \|+\| separators given in
the variable names

## Usage

``` r
extractVariableGroups(x)
```

## Arguments

- x:

  a vector of variable names

## Value

a data frame with variables and corresponding groups as columns.

## See also

[`plotstyle.add`](https://rdrr.io/pkg/mip/man/plotstyle.add.html)

## Author

Jan Philipp Dietrich

## Examples

``` r
x <- c("a|+|1|+|aa","a|+|2|abc","a|+|1|+|bb","a|+|1|+|cc","a|+|3|+|aa","a|+|3|+|bb","b|2")
shinyresults:::extractVariableGroups(x)
#>       variable  group
#>         <char> <char>
#>  1: a|+|1|+|aa      a
#>  2: a|+|1|+|aa  a|+|1
#>  3:  a|+|2|abc      a
#>  4: a|+|1|+|bb      a
#>  5: a|+|1|+|bb  a|+|1
#>  6: a|+|1|+|cc      a
#>  7: a|+|1|+|cc  a|+|1
#>  8: a|+|3|+|aa      a
#>  9: a|+|3|+|aa  a|+|3
#> 10: a|+|3|+|bb      a
#> 11: a|+|3|+|bb  a|+|3
```
