LW1949
======

**LW1949** is a package of functions for the [R programming language](http://www.r-project.org/).  It provides an automated approach to 
Litchfield and Wilcoxon's (1949) evaluation of dose-effect experiments. **LW1949** was first introduced by Adams et al. (*in preparation*).
A brief demonstration of **LW1949** is given in this [web app](https://jvadams.shinyapps.io/LW1949demo/LW1949Interactive.Rmd).
An example of how to use the functions in **LW1949** is given in this [vignette](https://github.com/JVAdams/LW1949/blob/master/vignettes/Intro.Rmd).

- - -

You should be able to access the functions by installing them directly from within R.

	library("devtools")
	devtools::install_github("JVAdams/LW1949", build_vignettes=TRUE)
	library(LW1949)

If you don't already have `Rtools` and `devtools`, you will need to download and install (as administrator, if using a PC) `Rtools` from 
[CRAN](http://cran.r-project.org/bin/windows/Rtools/) then run the following lines of code before submitting the code above:

	find_rtools()
	install.packages("devtools")

An alternative approach for Windows users is to download this 
[zip file](https://github.com/JVAdams/LW1949/raw/master/LW1949.zip)
and install the package from the R menu:
- Packages
- Install package(s) from local zip files...
	
- - -

_Thanks to Hilary Parker whose blog post 
[Writing an R package from scratch](http://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/)
encouraged me to create my first R package._

- - -

_U.S. Geological Survey_ (USGS) Computer Program **LW1949** version 0.0.0.9006. 
Written by Jean V. Adams, [USGS - Great Lakes Science Center](http://www.glsc.usgs.gov/), Ann Arbor, Michigan, USA. 
Written in programming language R (R Core Team, 2015, www.R-project.org), version 3.1.3 (2015-03-09). 
Run on a PC with Intel(R) Core(TM) I7-4600m CPU, 2.90 GHz processor, 16.0 GB RAM, and Microsoft Windows 7 Enterprise operating system 2009 Service Pack 1. 
Source code is available from Jean V. Adams on [GitHub](https://github.com/JVAdams/LW1949), _jvadams (at) usgs (dot) gov_.

_Disclaimer:_ Although this program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the United States Government 
as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, 
and no responsibility is assumed by the USGS in connection therewith.
