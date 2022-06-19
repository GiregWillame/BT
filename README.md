# BT : (Adaptive) Boosting Tree for Tweedie distributed response variable.

## Aknowledgements

The author thanks Detralytics' Scientific Directors Julien Trufin and Michel Denuit for the close collaboration.

The idea behind (A)BT is akin to the Gradient Boosting Methods. 
This package is therefore inspired by the `gbm3` one, originally written by Greg Ridgeway <greg.ridgeway@gmail.com> (for more details, see https://github.com/gbm-developers/gbm3).
Some of the developed codes are then pretty similar.

## References

This package is based on the original idea proposed by Detralytics' Scientific Directors Donatien Hainaut, Julien Trufin and Michel Denuit, elaborated in their books. For more details, see [Volume 1 - "GLMs and Extensions"](https://link.springer.com/book/10.1007/978-3-030-25820-7), [Volume 2 - "Tree-based Methods and Extensions"](https://link.springer.com/book/10.1007/978-3-030-57556-4) and [Volume 3 - "Neural Networks and Extensions"](https://link.springer.com/book/10.1007/978-3-030-25827-6).

Moreover, a **DetraNote** has been written on this subject, using the `BT` algorithm.

## Package builder and maintainer

This package has been written and is currently maintained by Gireg Willame <g.willame@detralytics.eu>.
All remarks/suggestions/improvements are warmly welcome.

## Details and installation

Non-production releases (bug fixes, mostly) will be released via the GitHub
release workflow. To install from GitHub, first install `devtools` from CRAN:

```r
install.packages("devtools")
```

Then install the `BT` package from GitHub:

```r
library("devtools")
install_github("GiregWillame/BT")
```
