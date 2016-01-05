#Upper range limit establishment after wildfire of an obligate-seeding montane forest tree fails to keep pace with 20th century warming

*By John W. Morgan, James Vincent & James S. Camac*

*Respository maintained by J S Camac*

###Overview

**Question**: How species respond to climate change at local scales will depend on how edaphic and biological characteristics interact with species physiological limits and traits such as dispersal. Obligate-seeders, those species that depend on fire for recruitment, have few and episodic opportunities to track a changing climate envelope.  In such cases, long-distance seed dispersal will be necessary to take advantage of rare recruitment opportunities. We examine recruitment patterns and seedling growth below, at and above the timberline of an obligate-seeding Australian montane forest tree (Eucalyptus delegatensis) after stand-replacing fire, and place these changes in the context of regional warming.

**Location**: Montane forests, south-eastern Australia.

**Methods**: We use two methods to detect whether Eucalyptus delegatensis can establish and persist above the timberline after stand-replacing wildfire. First, we examine establishment patterns by using belt transects at six sites to quantify how changes in post-fire recruit density with increasing distance above the timberline seven years post-fire. Second, to determine whether dispersal or physiological constraints determine post-fire establishment patterns, we transplanting seedlings and saplings into bare ground above (100 m elevation), at and below (50 m elevation) timberline 18-months after fire. We monitored seedling growth and survival for one growing season.

**Results**: There was minimal upslope migration of the species after fire; all saplings were observed near seed-bearing timberline trees. Transplanted seedlings and saplings, however, survived equally well when planted above existing timberlines, relative to saplings at or below the timberline. Seedling growth rates also did not differ across these locations. 

**Conclusions**: These findings suggest that upslope growing conditions are unlikely to limit initial range expansion of trees after fire. Instead, it is more likely that traits governing seed dispersal modulate responses to environmental gradients, and global change more generally.


###Rebuilding repository

We are committed to reproducible science. As such, this repository contains all the data and code necessary to fully reproduce our results. To facilitate this reproducibility the entire workflow has been written in [remake](https://github.com/richfitz/remake). Below outlines the instructions on how to clone this repository and build the entire analysis and figures.

First copy the repository to your local computer. Then open R in this directory.
Once this is done we must install `remake` dependencies that are not on CRAN.
To do this install [devtools](https://github.com/hadley/devtools) if you haven't already by running the following in R:
```
install.packages("devtools")
```
Now install [storr](https://github.com/richfitz/storr) a `remake` dependency not on CRAN.
```
devtools::install_github("richfitz/storr", dependencies=TRUE)
```
Now we can install `remake` (also not on CRAN)
```
devtools::install_github("richfitz/remake", dependencies=TRUE)
```

This project also depends on several packages. Now that `remake` is installed we can install them all by simply running:

```
remake::install_missing_packages()
```

This project uses [rstan](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started), a package that provides a probabilistic programming language for Bayesian inference. Because this project runs 10 Bayesian models, we are using stan's inbuilt chain parallelisation to reduce computing time. As such, we require that you have `rstan` 2.8.0 or greater. If you have an older version of `rstan` you can update it by running:
```
install.packages("rstan", dependencies = TRUE)
```

Now we have everything we need to reprocess the raw data, run the models and produce the figures. We can do all of this using a single command in R.

```
remake::make() # This should only take a couple of minutes
```

If you only wish to extract the processed (i.e. errors removed) datasets just run:

```
remake::make("export_processed_data")
