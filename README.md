
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CardiacDP

<!-- badges: start -->
<!-- badges: end -->

CardiacDP can automatically read and collate heart rate data, and then
employing the autocorrelation function (ACF) with a genetic algorithm
framework to locate periodic sub-sequences within each sequence. From
the candidate heart rates of these sub-sequences, the final results are
either evaluated based on the autocorrelation value or a tracking index
(TI).

## Test coverage

    CardiacDP Coverage: 88.36%
    R/collatedata.R: 87.91%
    R/computeHR.R: 88.44%

## Package structure

<img src="pkg_str.png" width="100%" />

## Installation

``` r
install.packages("CardiacDP")

# Or the source package from GitHub:
# Vector of package names
packages <- c("data.table", "doParallel", "dplyr", "foreach", "ggplot2", "purrr", "RColorBrewer", "stringr")

lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
})

# Install the source package
install.packages("CardiacDP_0.1.0.tar.gz", repos = NULL, type = "source")
```

## Example
#### see [User guidelines](./User%20guidelines.pdf) for detailed function descriptions

</br>

This is an example shows the complete analysis pipeline:

``` r
library(CardiacDP)
# use collatedata function to generate single collated data table
collatedata(file_path = "../20210518A.zip")
#> [[1]]
#> [1] "Zipped file name: 20210518A"
#> 
#> [[2]]
#> [1] "Number of files: 6"
#> 
#> [[3]]
#>               folders pages
#> 1:     20210518A-0001    90
#> 2: 20210518A-0001 (2)    90
#> 3: 20210518A-0001 (3)    90
#> 4: 20210518A-0001 (4)    90
#> 5: 20210518A-0001 (5)    90
#> 6: 20210518A-0001 (6)    61
#> 
#> [[4]]
#> [1] "Total duration: 170 mins"
#> 
#> [[5]]
#> [1] "Number of channels: 8"
#> 
#> [[6]]
#> [1] "Names of channels: Channel A, Channel B, Channel C, Channel D, Channel E, Channel F, Channel G, Channel H"
#> 
#> [1] "Reading data: 17%"
#> [1] "Reading data: 33%"
#> [1] "Reading data: 50%"
#> [1] "Reading data: 67%"
#> [1] "Reading data: 83%"
#> [1] "Reading data: 100%"
#> [1] "Finalizing..."
#> Classes 'data.table' and 'data.frame':   7667044 obs. of  9 variables:
#>  $ Time     : num  0 0.00133 0.00267 0.004 0.00533 ...
#>  $ Channel A: num  -0.002198 -0.00058 -0.001129 -0.000061 -0.001648 ...
#>  $ Channel B: num  0.00369 0.00314 0.00424 0.00314 0.00314 ...
#>  $ Channel C: num  0.00424 0.00479 0.00424 0.00372 0.00479 ...
#>  $ Channel D: num  -0.000488 -0.000488 -0.001007 -0.001007 -0.001007 ...
#>  $ Channel E: num  -0.0266 -0.0277 -0.0282 -0.0277 -0.0304 ...
#>  $ Channel F: num  -0.023 -0.0246 -0.0246 -0.023 -0.0241 ...
#>  $ Channel G: num  0.00906 0.00906 0.01337 0.00748 0.008 ...
#>  $ Channel H: num  -0.00534 -0.00375 -0.00641 -0.00693 -0.00693 ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
#> [1] "Collated data table saved as 20210518A.csv"

# use the default parameters to analyse a test file
output <- computeHR("../20210518A.csv")
#> [1] "Calculating heart rate: Channel A..."
#> [1] "Generating output..."
#> [1] "Calculating heart rate: Channel B..."
#> [1] "Generating output..."
#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable

#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable

#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable
#> [1] "Calculating heart rate: Channel C..."
#> [1] "Generating output..."
#> [1] "Calculating heart rate: Channel D..."
#> [1] "Generating output..."
#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable
#> [1] "Calculating heart rate: Channel E..."
#> [1] "Generating output..."
#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable
#> [1] "Calculating heart rate: Channel F..."
#> [1] "Generating output..."
#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable

#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable

#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable

#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable

#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable

#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable

#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable

#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable

#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable

#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable

#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable
#> [1] "Calculating heart rate: Channel G..."
#> [1] "Generating output..."
#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable

#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable

#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable
#> [1] "Calculating heart rate: Channel H..."
#> [1] "Generating output..."
#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable

#> Warning in summary.lm(lm(hr ~ ix, prev)): essentially perfect fit: summary may
#> be unreliable
```

The user can then access the output by the channel name. finalsubseq is
a list showing the positions and durations of the final sub-sequences
determined for each sequence. They are presented as data tables (s =
start index of the sub-sequence; e = end index of the sub-sequence; p =
which of the initial population the sub-sequence is derived from; and f
= duration of the sub-sequence), the rows of which represent separate
final sub-sequences. The example below shows the first five sequences
(e.g. \[\[4\]\] indicates at the 4th minute there are three final
sub-sequences).

``` r
# positions (in indices) and durations of the final sub-sequences
# NOTE: the results are not included to ease the reading, please refer to Appendix S4

output[["finalsubseq"]][["Channel A"]]
```

The corresponding candidate heart rates per sub-sequences can be found
in candidateHR (ACF = autocorrelation value; lag = time lag; and hr =
heart rate). Taking the 4th minute again as an example, there are two
candidate heart rates for the first sub-sequence (as shown in
\[\[4\]\]\[\[1\]\]), and only one candidate heart rate for the other two
sub-sequences (as shown in \[\[4\]\]\[\[2\]\]and \[\[4\]\]\[\[3\]\]).

``` r
# candidate heart rates of the final sub-sequences
# NOTE: the results are not included to ease the reading, please refer to Appendix S4

output[["candidateHR"]][["Channel A"]]
```

The final results after evaluating the candidate heart rates, checking
for resolution and weighing for durations can eventually be obtained
from results_ACF and results_TI. These results referred to evaluating
the candidate heart rates by autocorrelation values (i.e. the “ACF + GA”
approach) and the tracking index (i.e. the “ACF + GA +TI” approach)
respectively. Each of them consists of 1) the details of the
sub-sequences (subseqHR); 2) the weighted heart rate per sequence
(weightedHR); and 3) a plot of weighted heart rate against time (plot).

``` r
## results obtained from evaluating the candidate heart rates by autocorrelation values
output[["results_ACF"]][["Channel A"]]
#> $subseqHR
#>       ix win    s    e  p    f       ACF lag       hr        res
#>   1:   1   1    1 6430 10 6429 0.5748986 180 35.71411 0.00933338
#>   2:   2   1    1 4248  1 4247 0.5236789 170 37.81494 0.00933338
#>   3:   3   1    1 5728  2 5727 0.5001360 164 39.19841 0.00933338
#>   4:   4   1 3215 3856  6  641 0.5241728 156 41.20859 0.00933338
#>   5:   4   2 3572 6430  8 2858 0.5034119 186 34.56204 0.00933338
#>  ---                                                            
#> 228: 166   1   NA   NA NA   NA        NA  NA       NA 0.00933338
#> 229: 167   1   NA   NA NA   NA        NA  NA       NA 0.00933338
#> 230: 168   1   NA   NA NA   NA        NA  NA       NA 0.00933338
#> 231: 169   1   NA   NA NA   NA        NA  NA       NA 0.00933338
#> 232: 170   1   NA   NA NA   NA        NA  NA       NA 0.00933338
#> 
#> $weighedHR
#>       ix      wACF      whr
#>   1:   1 0.5748986 35.71411
#>   2:   2 0.5236789 37.81494
#>   3:   3 0.5001360 39.19841
#>   4:   4 0.5066364 35.59436
#>   5:   5 0.5078256 36.38321
#>  ---                       
#> 166: 166        NA       NA
#> 167: 167        NA       NA
#> 168: 168        NA       NA
#> 169: 169        NA       NA
#> 170: 170        NA       NA
#> 
#> $plot
#> Warning: Removed 14 rows containing missing values (`geom_point()`).
```

<img src="man/figures/README-final results-1.png" width="100%" />

``` r

# results obtained from evaluating the candidate heart rates by the tracking index
output[["results_TI"]][["Channel A"]]
#> $subseqHR
#>       ix win    s    e  p    f       ACF lag       hr        res
#>   1:   1   1    1 6430 10 6429 0.5748986 180 35.71411 0.00933338
#>   2:   2   1    1 4248  1 4247 0.5236789 170 37.81494 0.00933338
#>   3:   3   1    1 5728  2 5727 0.5001360 164 39.19841 0.00933338
#>   4:   4   1 3215 3856  6  641 0.5241728 156 41.20859 0.00933338
#>   5:   4   2 3572 6430  8 2858 0.5034119 186 34.56204 0.00933338
#>  ---                                                            
#> 228: 166   1   NA   NA NA   NA        NA  NA       NA 0.00933338
#> 229: 167   1   NA   NA NA   NA        NA  NA       NA 0.00933338
#> 230: 168   1   NA   NA NA   NA        NA  NA       NA 0.00933338
#> 231: 169   1   NA   NA NA   NA        NA  NA       NA 0.00933338
#> 232: 170   1   NA   NA NA   NA        NA  NA       NA 0.00933338
#> 
#> $weighedHR
#>       ix      wACF      whr
#>   1:   1 0.5748986 35.71411
#>   2:   2 0.5236789 37.81494
#>   3:   3 0.5001360 39.19841
#>   4:   4 0.5066364 35.59436
#>   5:   5 0.5078256 36.38321
#>  ---                       
#> 166: 166        NA       NA
#> 167: 167        NA       NA
#> 168: 168        NA       NA
#> 169: 169        NA       NA
#> 170: 170        NA       NA
#> 
#> $plot
#> Warning: Removed 16 rows containing missing values (`geom_point()`).
```

<img src="man/figures/README-final results-2.png" width="100%" />

## Interpret results

| Variable    | Content                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|-------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| finalsubseq | A list of positions and durations of the final periodic sub-sequences                                                                                                                                                                                                                                                                                                                                                                       |
| candidateHR | A list of candidate heart rates extracted from ACF for each sub-sequence                                                                                                                                                                                                                                                                                                                                                                    |
| results_ACF | Results obtained from evaluating the candidate heart rates of each sub-sequence based on autocorrelation values (i.e. following the “ACF + GA” approach as described in the main manuscript). Consisted of three items: 1) subseqHR: a list of sub-sequences and the corresponding heart rates and durations; 2) weightedHR: a list of final heart rates per sequence after weighing; and 3) plot: a plot of final heart rates against time |
| results_TI  | Results obtained from evaluating the candidate heart rates of each sub-sequence using a tracking index (i.e. following the “ACF + GA + TI” approach as described in the main manuscript). Consisted of three items: 1) subseqHR: a list of sub-sequences and the corresponding heart rates and durations; 2) weightedHR: a list of final heart rates per sequence after weighing; and 3) plot: a plot of final heart rates against time     |
