# StatonMisc

This is an R package containing shortcut utility functions commonly used by B. Staton in his R code. Many other packages written by B. Staton will import this package. It can be installed from R as follows:

```R
devtools::install_packages("bstaton1/StatonMisc")
```

Note that you must have the `devtools` [package ](<https://cran.r-project.org/web/packages/devtools/index.html>) installed first. 

Example functions include:

* `ext_device()` which creates new graphics window using the operating system-specific graphics function. For example, if the user is using a PC, the effect is the same as calling `windows()` or if on a Mac, the effect is the same as calling `quartz()`

* `cv2sig()` and `sig2cv()`, which are opposite transformations of one another and are used to convert back and forth between the standard deviation and the coefficient of variation of a lognormal random variable

* `doy2date()` and `date2doy()`, which are also opposites. They convert numbers back and forth from 1-365 (or 366 if a leap year) to MM/DD/YYYY format, while properly acknowledging leap years

* `beta_mm()`, which is used to calculate the shape parameters of a beta distribution when passed a vector of assumed i.i.d. beta random variables using moment matching.

* `logit()` and `expit()`, which perform the logit and inverse-logit transformations, respectively

* `%!in%`, which is the opposite of `%in%`. Allows users to specify

  ``` R
  x %!in% y
  ```
  Rather than
  
  ``` R
  !(x %in% y)
  ```
  When determining which elements of the vector `x` are **not** found in the vector `y`.

All functions have associated help files. Though it may seem like a silly package, it is the first one created by B. Staton and it was used as a training exercise.

Bug fixes or suggested additions can be submitted via either issues or pull requests.

