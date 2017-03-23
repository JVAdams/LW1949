LW1949
======

**LW1949** is a package of functions for the [R programming language](http://www.r-project.org/).  It provides an automated approach to 
Litchfield and Wilcoxon's (1949) evaluation of dose-effect experiments. **LW1949** was first introduced by Adams et al. (*in preparation*).
A brief demonstration of **LW1949** is given in this [web app](https://jvadams.shinyapps.io/LW1949demo).
An example of how to use the functions in **LW1949** is given in this [vignette](https://rawgit.com/JVAdams/LW1949/master/vignettes/Intro.html).

- - -

To get the current released version from CRAN:

    install.packages("LW1949")

To get the current development version from GitHub:

    install.packages("devtools")
    devtools::install_github("JVAdams/LW1949", build_vignettes=TRUE)

 or:

    source("https://raw.githubusercontent.com/MangoTheCat/remotes/master/install-github.R")$value("mangothecat/remotes")
    remotes::install_github("JVAdams/LW1949")

Then load the package:

    library(LW1949)

- - -

_Thanks to Hilary Parker whose blog post 
[Writing an R package from scratch](http://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/)
encouraged me to create my first R package._

- - -

_U.S. Geological Survey_ (USGS) Computer Program **LW1949** version 1.1.0.9000. 
Written by Jean V. Adams, [USGS - Great Lakes Science Center](http://www.glsc.usgs.gov/), Ann Arbor, Michigan, USA. 
Written in programming language R (R Core Team, 2017, www.R-project.org), version 3.3.3  (2017-03-06). 
Run on a PC with Intel(R) Core(TM) I7-4600m CPU, 2.90 GHz processor, 16.0 GB RAM, and Microsoft Windows 7 Enterprise operating system 2009 Service Pack 1. 
Source code is available from Jean V. Adams on [GitHub](https://github.com/JVAdams/LW1949), _jvadams (at) usgs (dot) gov_.

_Disclaimer:_ This software has been approved for release by the U.S. Geological Survey (USGS). Although the software has been subjected to rigorous review, the USGS reserves the right to update the software as needed pursuant to further analysis and review. No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. Furthermore, the software is released on condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from its authorized or unauthorized use.
