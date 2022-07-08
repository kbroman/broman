Revision history for the R/broman package
-----------------------------------------

## Version 0.80, 2022-07-08

- Include <limits.h> in a C file, for upcoming change to R.


## Version 0.76, 2021-10-11

- Removed all pushbullet functions. They stopped supporting iOS
  which makes it useless for me.

- Added warning to dec2hex() and add to documentation for that and
  hex2dec(), about maximum allowable values (2^31 - 1).

- Revised bromanversion() to handle a case like "0.74".


## Version 0.72-4, 2021-02-04

- Changed default max_jiggle in dotplot() to 0.45 (rather than NULL).
  Similarly changed default for maxvalue in jiggle() to 0.45.

- Fixed typo in help file for excel_fig().


## Version 0.71-6, 2020-11-18

- Changed the default "jiggle" method in jiggle() and dotplot() to
  be "random", as it seems to work better than the deterministic
  approach ("fixed").

- Fixed problem in ciplot() to avoid warnings about some graphics
  parameters.

- grayplot() now can take mgp graphics argument.

- Added argument maxvalue to jiggle(), and max_jiggle to dotplot(), to
  better control the horizontal jiggling of points when there is a lot
  of data. (Fixes Issue #8)

- Make it so you can adjust the vertical lines in dotplot() by including
  vlines.lwd and/or vlines.col. (Fixes Issue #7)

- Make it so you can use v_over_h in dotplot()

- Fixed potential problem in documentation, since plot() has moved
  from the graphics package to base.


## Version 0.70-4, 2020-05-21

- Added an option to include grid lines in triplot(), and a separate
  function trigrid() that does the work and may be called
  separately.

- Added twocolorpal() for a color palette that goes from one color
  (say blue) to another (say red) through a third (say white). That
  is, blue to white to red.

- In vec2string(), add conjunction argument, to allow a different
  combining word than "and".

- In ciplot(), allow argument main in ... to be passed to
  grayplot() and so on to plot().


## Version 0.69-5, 2019-04-11

- Added function ciplot() for plotting a set of confidence intervals.

- Added operators %nin%, %win%, and %wnin%.

  - a %nin% b   is like !(a %in% b)
  - a %win% b   is like a[a %in% b]
  - a %wnin% b  is like a[a %nin% b]

- Added function spell_out() for spelling out numbers provided they are
  smallish integers. Goes with the datasets numbers and Numbers.

- Added function revgray() that's like revrainbow() but white/black.

- In dotplot(), make "Group" the default axis label whether rotated or
  not. (Was when rotate=TRUE, but not when rotate=FALSE.)

- Use Markdown throughout documentation


## Version 0.68-2, 2018-07-25

- Added function tritext(), to go with triplot(), tripoints(),
  trilines(), and triarrows(), but to add text annotations.

- Revised openfile() so that it works on Linux.


## Version 0.67-4, 2017-12-08

- Added function grayplot_na(). It's like grayplot, but includes boxes
  near the margins to show points that are missing x or y or both.

- Added function vec2string() for turning a vector into a string with
  the items separated by commas and an "and". This is for use in
  R Markdown documents, to list a series of items.

- Added function get_precision() for determining the precision of a
  number (the number of digits past the decimal point). I want to use
  this to compare two data sets with different amounts of rounding, to
  pick the more precise values.

- Added function align_vectors() for aligning two vectors using their
  names attributes.

- In grayplot(), have default be pch=21 and bg="lightblue" (that is,
  points that are lightblue with a black circle around them).


## Version 0.66-2, 2017-10-24

- Add bgcolor argument to triplot(), to have the background of the
  rectangle be gray, by default.

- Also change default background in grayplot() and theme_karl() from
  "gray80" to "gray90" (which is much less dark of a gray).

- Fix bug in dotplot() in case that y has missing values (omit them).


## Version 0.65-4, 2017-05-07

- In note() for RPushbullet, force 'body' to be non-empty, because we
  get an error otherwise.  In done(), use body=NULL to avoid similar
  errors.


## Version 0.65-3, 2017-05-04

- Added the function crayons() which is shorthand for
  brocolors("crayons"), but also can take a subset of color names, and
  uses grep to find partial matches. So you can use
  crayons("purple mountain") rather than
  brocolors("crayons")["Purple Mountain's Majesty"]


## Version 0.65-2, 2017-02-21

- Fix problem with pushbullet_devices(); output of
  RPushbullet::pbGetDevices()$devices is now a data frame; code
  assumes it's a list.


## Version 0.65-1, 2016-11-30

- Add 'mar' argument to excel_fig. (plot margins; passed to par())


## Version 0.63-3, 2015-06-20

- Add "web" colors option (from https://clrs.cc) and an alternative set
  of Collaborative Cross colors to brocolors()

- Add checks for ^C (user interrupt) in compare_rows().

- Add data numbers and Numbers (numbers 1-20 spelled out in English;
  numbers in lower-case and Numbers capitalized)


## Version 0.62-1, 2015-01-05

- Remove attachfile() and loadfile() as I don't use them and they
  cause CRAN headaches.


## Version 0.61-3, 2015-11-21

- Add function maxabs() which just does max(abs(x)).


## Version 0.61-2, 2015-11-15

- Add function dotplot() which is a simplified version of grayplot()
  for categorical x.

- Add function jiggle() for horizontal jiggling, either "fixed"
  (deterministic, similar to the beeswarm package) or "random"; used
  in dotplot()

- In grayplot(), have vlines and hlines follow from xlim and ylim,
  respectively, if they're provided.


## Version 0.60-1, 2015-09-24

- Add function fac2num() for converting a factor to numeric.


## Version 0.59-5, 2015-08-04

- Add function excel_fig() for making a figure that shows the contents
  of a matrix, as it might appear in Excel.

- Add function pushbullet_devices() for getting list of active
  pushbullet devices.


## Version 0.58-2, 2015-07-16

- Add function compare_rows(), for comparing rows in a numeric matrix,
  either by proportion mismatches or RMS difference.

- Add function add_commas(), for adding commas in large numbers,
  like 91310 -> "91,310"


## Version 0.57-5, 2015-07-06

- Add function stop_sending_errors(), the reverse of errors2pushbullet().


## Version 0.57-3, 2015-06-25

- Allow use of col.lab in grayplot(), for coloring the axis titles.


## Version 0.57-1, 2015-06-18

- Add function switchv(), a vectorized version of switch().


## Version 0.56-1, 2015-06-04

- Add function openfile() for opening a file using system().


## Version 0.55-3, 2015-04-27

- In theme_karl(), add black rectangle around the facet rectangles, too.


## Version 0.55-2, 2015-04-22

- Add function theme_karl() (aka karl_theme()), with a ggplot2 theme
  that removes the tick boxes and adds a black border.


## Version 0.54-3, 2015-03-05

- Delete functions get0() and get.() since there's going to be a get0
  function in base R 3.2.0.


## Version 0.54-2, 2015-02-12

- Add note() function, which is just a wrapper for
  RPushbullet::pbPost.

- Revised make() to work with any directory (not just an R package).
  Also added a "target" argument.


## Version 0.53-1, 2015-02-09

- Add exit() function, which is just the same as q("no").


## Version 0.50-1, 2014-12-01

- Revise errors2pushbullet(), argument deviceind replaced by
  recipients.


## Version 0.49-6, 2014-11-03

- Added argument v_over_h to grayplot(), controlling whether
  horizontal gridlines are on top of the vertical ones or vice versa.

- Revisions to grayplot for better treatment of case of missing y.
  To avoid gridlines, use vlines=NA and/or hlines=NA.


## Version 0.49-3, 2014-10-24

- Added function attrnames() which is just names(attributes(object))


## Version 0.49, 2014-09-03

- Add function errors2pushbullet() which sets options(error) to use
  the RPushbullet package to send a push notification.


## Version 0.48, 2014-07-16

- Add crayon colors to brocolors(), and plot_crayons function to view
  them. Change bg and bgpng for near-black background.

- Add colwalpha function.

- grayplot: change defaults for xat, yat, vlines, hlines


## Version 0.47, 2014-03-30

- Added a simple utility function kbdate() for getting the date in
  a different format.

- Added some unit tests using testthat.


## Version 0.46, 2014-03-13

- Add function make() for running make within a package directory, for
  use with devtools().


## Version 0.45, 2014-03-09

- Add function brocolors() returning vectors of colors I use in
  figures.

- Convert package to use roxygen2 for documentation.


## Version 0.44, 2013-10-29

- Revise tripoints() and related functions to invisibly return the
  (x,y) coordinates of the points, as plotted.


## Version 0.43, 2013-08-02

- Added qqline2(), which is like qqline() but for two datasets.


## Version 0.42, 2013-06-08

- Revise grayplot for better treatment of x- and y-axis titles,
  including the use of mgp/mgp.x/mgp.y.


## Version 0.41, 2013-05-13

- Added function histlines for making a histogram with lines().


## Version 0.40, 2013-05-03

- Added function xlimlabel for determining appropriate xlim to add
  character string labels to a plot

- Added functions strwidth2xlim and strwidth2lines for calculating
  margin lengths or x-axis limits that will fit a given set of text.


## Version 0.39, 2012-10-17

- In Description file, added "Depends: R (>= 2.15.0)", since get0
  uses paste0.  I could use paste(..., sep=""), but...


## Version 0.38, 2012-10-04

- Changed order of plotting of vertical and horizontal lines in
  grayplot().  Also added ability to control the margin parameters
  (spacing of axis labels and such).


## Version 0.37, 2012-07-19

- Improved convert2hex so it handles numbers > 255; added alias
  dec2hex.  Added related function hex2dec.


## Version 0.36, 2012-05-14

- Added function cf() for comparing vectors/matrices/lists,
  including proper handling of NAs

- Added functions get0() and get.() which combine get and paste.

- Added grayplot() for plots with gray backgrounds.

- Added manyboxplot() for making boxplot-like figures with *many* groups

- Added paste.() function, which is just paste(..., sep=".")

- Added function revrainbow(), which is just rev(rainbow(start=0, end=2/3, ...))

- Added paste00(), paste..(), paste0.() and paste.0()


## Version 0.35, 2012-04-11

- runningmean() can now also calculate a running SD.

- Revised bromanversion() to use packageVersion().

- Simplified myround(), using sprintf().


## Version 0.34, 2011-11-07

- Added NAMESPACE (for R 2.14.0)


## Version 0.33, 2011-04-23

- Revised normalize() to handle NAs.

- Added function winsorize() for winsorizing a numeric vector.

- Added function bromanversion() to print installed version number.


## Version 0.32, 2010-12-14

- Added function objectsizes(), for calculating the sizes of the
  objects in one's workspace.


## Version 0.31, 2010-11-22

- Added functions triplot(), tripoints(), trilines(), triarrow() for
  plotting Holmans triangle (an equilateral triangle used to depict
  trinomial distributions).

- Added function venn() for drawing a to-scale Venn diagram.

- Added function arrowlocator() for using locator() to indicate the
  endpoints of an arrow and then draw it.


## Version 0.30, 2010-11-17

- Fixed a bug in permtest (the pval argument didn't work).


## Version 0.29, 2010-08-06

- Deleted the nqrank() function, as I've now placed it within
  R/qtl.


## Version 0.28, 2010-03-25

- Slight change to nqrank, so that -Inf values are replaced by
  10 *less* than the minimum of any finite values (rather than 10
  *more* than that minimum).


## Version 0.27, 2009-11-06

- Revised runningmean and runningratio so that the statistics can be
  calculated at arbitrary locations.  Also revised them so that the
  input do not need to have sorted positions.


## Version 0.26, 2009-10-26

- Slight change to nqrank, to preserve the mean and SD.

- Fixed a few links in the help files.

- Revised h() to conform to a change in R.


## Version 0.25, 2009-08-11

- Slight change to nqrank, so that ties are not jittered by default,
  but may be (if one uses the argument jitter=TRUE).


## Version 0.24, 2009-04-28

- Updated license to GPL-3


## Version 0.23, 2008-11-20

- Revised perm.test and paired.perm.test so that they can return the
  actual permutation results (rather than just the p-value), though
  the default is still to just return the p-value.

- Fixed a bug in myround().


## Version 0.22, 2008-09-19

- Revised attachfile and attachwork so that they return (invisibly)
  TRUE/FALSE according to whether the file was found.

- Also added an argument 'verbose'; if TRUE and the file can't be
  loaded, a warning is given.


## Version 0.21, 2008-02-29

- Added a function normalize() for doing quantile normalization.


## Version 0.20, 2007-01-18

- Modified fisher() so that the p-value is Prop'n >= rather
  than just >.

- Added a similar function, chisq() for doing a chi-square test
  with p-value calculated by simulation.  In 0.20-2, suppressed
  warnings produced by chisq.test.


## Version 0.19, 2007-10-09

- Added a function runningratio, for calculating
  sum(numerator)/sum(denominator) in sliding window.

- Fixed some errors in the help files.


## Version 0.18, 2007-09-20

- Fixed a bug in loadwork().


## Version 0.17, 2006-01-16

- Added a function h() for getting access to html help files while
  running R via ESS within emacs.


## Version 0.16, 2006-12-19

- Added a function runningmean().

- Fixed a bug in paired.perm.test().


## Version 0.15, 2006-08-14

- Added a function myround().


## Version 0.14, 2006-06-28

- Added a function mypairs(), similar to pairs() but producing only
  the upper triangle.

- Added a convert2hex() function.


## Version 0.13, 2004-12-08

- Added attachfile() and loadfile(), similar to loadwork() and
  attachwork().


## Version 0.12, 2004-08-06

- Added little utility functions loadwork() and attachwork()
  for loading or attaching .RData files of the form
  "dir*/.RData", when * is an integer, and especially when
  dir = "Work".


## Version 0.11, 2002-04-29

- Added perm.test and paired.perm.test for getting p-values for a
  two-sample t-statistic by permutation.

- There was an obvious bug in the function fisher(), which is now
  fixed.


## Version 0.10, 2001-11-21

- This is a package to contain miscellaneous R functions that I find
  useful.

- Currently, it contains functions to get SEs of sample quantiles,
  a sampling version of fisher.test, a re-parse of the output of
  qr(), simulation from a multivariate normal distribution, and
  numerical integration by Simpson's rule.
