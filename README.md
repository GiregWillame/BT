# BT : (Adaptive) Boosting Tree for Tweedie distributed response variable.

## Aknowledgements

The author thanks Detralytics' Scientific Directors Julien Trufin and Michel Denuit for their theoretical support.

The idea behind (A)BT is akin to the one developed in the `gbm3` package, originally written by Greg Ridgeway <greg.ridgeway@gmail.com> (for more details, see https://github.com/gbm-developers/gbm3).
This package is inspired by the latter and some of the developed codes are then pretty similar.

## References

This package is based on the original idea proposed by Detralytics' Scientific Directors Donatien Hainaut, Julien Trufin and Michel Denuit, elaborated in their books. For more details, see [volume1](https://link.springer.com/book/10.1007/978-3-030-25820-7), [volume2](https://link.springer.com/book/10.1007/978-3-030-57556-4) and [volume3](https://link.springer.com/book/10.1007/978-3-030-25827-6).

Moreover, a **DetraNote** has been written on this subject, using the `BT` algorithm.

## Package builder and maintainer

This package has been written and is currently maintained by Gireg Willame.
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
