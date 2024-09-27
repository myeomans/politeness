
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Politeness

[![](https://www.r-pkg.org/badges/version/politeness?color=blue)](https://cran.r-project.org/package=politeness)
[![](http://cranlogs.r-pkg.org/badges/grand-total/politeness?color=green)](https://cran.r-project.org/package=politeness)
[![](http://cranlogs.r-pkg.org/badges/last-month/politeness?color=green)](https://cran.r-project.org/package=politeness)

Politeness is a universal dimension of langauge (Lakoff, 1973; Brown &
Levinson, 1987). In practically all communication, a speaker can choose
to be more or less polite to their audience. In this package, we provide
tools for researchers to measure the markers and effects of politeness
in natural language.

## Installation

You can install politeness directly, like so:

``` r
 install.packages("politeness")
```

Many of the politeness features containted in this package use
dependency parsing. We rely on the popular python library
[SpaCy](https://spacy.io/), which is simple to install, although the
procedure can vary for different machines. Our software depends on a
convenient wrapper function, [spacyr](https://spacyr.quanteda.io/), that
also includes several intallation tools. If you do not have SpaCy
installed, a reduced set of features is provided (i.e. those that do not
require dependency tags) but this only recommended for initial tests,
rather than final analyses.

Please confirm that your machine can run SpaCy and spacyr first! This
step can be difficult for some machines, and we defer to the existing
documentation for that software as a guide.

<!---
If you want to try out this package without having to configure SpaCy on
your own machine, I have written a tutorial that will let you analyse
the data on
[Colab](https://colab.research.google.com/drive/1EmVhqlPLUIlFjYw73nzydtfT1PQ8QU2_?usp=sharing).
This will install everything you need automatically (though it does take
~ 20 minutes for everything to run). However, for people unfamiliar with
python, it can be much easier to use colab to start, than to configure
their own computer.
-->

## Citation

If you find this package useful, please cite us, using the following
reference from our R Journal publication.

Yeomans, M., Kantor, A. & Tingley, D. (2018). Detecting Politeness in
Natural Language. The R Journal, 10(2), 489-502.

Note that this publication was written using a very early version
(0.2.4) of the package. For the most up-to-date description of the
functionality, please see the vignette in this repository.

## Example

In the package we have included a dataset, `phone_offers`. It was
collected from an experiment in which participants were told to write
offers asking to buy a smartphone from a seller on Craigslist. We
randomly manipulated their instructions so that their task was to write
in a warm or tough style. The dataset includes the text of their
messages, as well as the condition assignment.

``` r
data("phone_offers")

politeness::politeness(phone_offers$message)

# install.packages("spacyr")
#spacyr::spacy_initialize(python_executable = "PYTHON_PATH")
#politeness::politeness(phone_offers$message, parser="spacy")

politeness::politenessPlot(politeness::politeness(phone_offers$message),
                           split=phone_offers$condition,
                           split_levels = c("Warm","Tough"),
                           split_name = "Condition")
```
