Version 0.70.1
--------------
  * Bug fix: Very rarely bootstrapping results in singular datasets.

Version 0.70.0
--------------
  * Update documentation
  * Bug fix: 1 sided p-value for Vuong's.
  * Now pass a matrix of parameters when estimating xmin (fixes #68). Used for 
  lognormal distribution.

Version 0.60.3
--------------
  * Incorrect p-value for bootstrap_p (fixes #64). Thanks to @lsaravia for reporting.

Version 0.60.0
--------------
  * New distance argument for estimate_xmin and bootstrap functions. No longer limited
  to the KS distance measure.
  * Better error handling in the bootstrap function. Fix for https://github.com/csgillespie/poweRlaw/issues/60. Thanks to @lsaravia for reporting.
  * Added in the Weibull distribution. Fix for https://github.com/csgillespie/poweRlaw/issues/57.

Version 0.50.1 (not on CRAN)
----------------------------
 * Bug fix: Export get_n in bootstrap_p. Fix for https://github.com/csgillespie/poweRlaw/issues/53. Thanks to @AnaCG for reporting.

Version 0.50.0
----------------------------
 * Added get_ntail function which returns the number of points greater than or equal to xmin.
 * Added get_n function which returns the sample size.
 * Bug fix: bootstrap_p was incorrect for CTN models (https://github.com/csgillespie/poweRlaw/issues/52). Thanks to @lsaravia for reporting and diagnosing the problem.

Version 0.40.0 (not on CRAN)
----------------------------
 * Add ByteCompile flag. Testing suggests that bootstrapping is now around twice as fast.
 * Added seed and package_version to the output of bootstrap and bootstrap_p. 
 * Added poisson random number generator.

Version 0.30.2
--------------
 * Bug fix: Plotting the data cdf failed when data values were larger than `xmax` 
 (https://github.com/csgillespie/poweRlaw/issues/40). Thanks to @LaurentFranckx

Version 0.30.1
--------------
 * Bug fix: pdf and cdf functions should now handle values of q less than xmin in a 
  sensible way (Thanks to Pierce Brookss)

Version 0.30.0
--------------
 * New package title to satisfy CRAN
 * A new xmax argument has been added to the bootstrap and estimate_xmin functions. 
  This argument limits the search space when calculating the KS statistic. 
 * The all_values argument has been removed from dist_cdf. A new function dist_all_cdf has been created.
 * Added random number generators for log normal and exponential functions (https://github.com/csgillespie/poweRlaw/issues/32)
 * Added seed argument to bootstrap and bootstrap_p functions
 * Added warning message to handle estimation in tail regions (https://github.com/csgillespie/poweRlaw/issues/31)
 * Bug fix: Bootstrap edge cases (https://github.com/csgillespie/poweRlaw/issues/29) @jkeirstead

Version 0.20.5
--------------
 * Further changes to the tolerance in the test suite comparison (Solaris-sparc failed to build)

Version 0.20.4
--------------
 * Added tolerance to test suite comparison (Solaris-sparc failed to build)
 * Removed tufte vignette styles

Version 0.20.3
--------------
 * Test suite now included in the package
 * Improved numerical stability when working out discrete exp and log normal pdfs (https://github.com/csgillespie/poweRlaw/issues/21) 
 * Merged data_max and xmins argument in estimate_xmin function 
 * Added example on copying distribution objects (https://github.com/csgillespie/poweRlaw/issues/9)
 * Added new vignette on comparing distributions (https://github.com/csgillespie/poweRlaw/issues/8)
 * Bug fix: When estimating xmin is not possible (e.g. not enough data), estimate_xmin now returns NA rather than an error (https://github.com/csgillespie/poweRlaw/issues/25)
 * Bug fix: When setting parameters in distributions, no longer a strict class comparison
 * Bug fix: Error when the length of xmins is 1 in estimate_xmin (https://github.com/csgillespie/poweRlaw/issues/20).
Thanks to @linzhp
 * Bug fix: Generating random numbers for the discrete power-law distribution wasn't quite right for small x values.  (https://github.com/csgillespie/poweRlaw/issues/24). Thanks to @wrhaas.

Version 0.20.2
--------------
 * Discrete power-law mle now uses L-BFGS-B optimiser by default
 * Vignette source now included within the package
 * Bug fix in lines methods
 * Bug fix: use data_max argument in estimate_xmin (https://github.com/csgillespie/poweRlaw/issues/13) Thanks to @pgoldberg.

Version 0.20.1
--------------
 * Updated documentation
 * Added swiss_prot data set
 * Renamed NativeAmerican to native_american 
 * Renamed USAmerican to us_american 
 * Changed license to GPL-2 | GPL-3

Version 0.20.0
--------------
 * Added discrete exponential function
 * Added compare_distribution functions
 * dist_pdf now have a log argument
 * Updated documentation
 * Bug fixes

Version 0.17.0
--------------
 * Added discrete log normal, log normal and poisson distributions.
 * Generic plot functions added for bootstrap output.
 * Test suite.
 * New examples vignette.
 * Bug fixes

Version 0.16.1
--------------
 * bootstrap_xmin now implements the procedure described in Clauset
 * bootstrap_p estimates the p-value

Version 0.16.0
--------------
 * Added dist_data_cdf_function
 * Can now plot the entire data line and add distribution lines starting at xmin
 * Added vignette
 * Improved documentation
 * Deprecated pl_data data class
 
Version 0.15.2
--------------
 * Generating discrete random numbers took up too much memory. 
   Reduced the threshold for switching to the CTN PL distribution.

Version 0.15.1
--------------
 * Bug fix in the lines and points function

Version 0.15.0
--------------
 * Adding support for continuous power-laws

Version 0.14.4
--------------
  * No visible changes - preparing for future R versions.
  * Added discrete_xmax parameter to the discrete random number generator. 
  This parameter controls where we change from using a (true) discrete 
  random number generator to a CTN approximation.

Version 0.14.3
--------------
  * xmin now set to minimum value of x
  
Version 0.14.2 
--------------
 *  Plots, lines and points functions now return the data using
    invisible
 * Moved to parSapplyLB
 * Added a bootstrap_moby data set.

Version 0.14.1
--------------
 *  Bug fix when calculating the bootstrapping p-value

Version 0.14
------------
 * Added explicit garbage collection call to the bootstrap routine to avoid memory issues.

Version 0.13
------------
 * Created the estimate_pars method - an mle estimate of the parameters.

Version 0.12
------------
 * Fixed bug when calculating the KS statistics
 * Updated docs

Version 0.11
----------------
 * Fixed bug in random number generator
 * Updated docs

Version 0.1
----------------
 * Initial release