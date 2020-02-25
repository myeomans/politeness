
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Politeness

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

Please confirm that your machine can run SpaCy and spacyr first\! This
step can be difficult for some machines, and we defer to the existing
documentation for that software as a guide. Users may also want to
consider [RStudio Cloud](https://www.rstudio.cloud/), which can support
all of this software without local installation issues.

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
