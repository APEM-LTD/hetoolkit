
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hetoolkit

<!-- badges: start -->

<!-- badges: end -->

## Overview

The `hetoolkit` package comprises a collection of 20 functions for
assembling, processing, visualising and modelling hydro-ecological data.
These are:

  - `import_nrfa` for importing flow data from the National River Flow
    Archive (NRFA);
  - `import_hde` for importing flow data from the Environment Agency
    (EA) Hydrology Data Explorer (HDE);
  - `import_flowfiles` for importing flow data from local files;
  - `import_flow` for importing flow data from a mix of the above
    sources;
  - `import_inv` for importing macroinvertebrate sampling data from the
    EA Ecology and Fish Data Explorer;
  - `import_env` for importing environmental base data from the EA
    Ecology and Fish Data Explorer;
  - `import_rhs` for importing River Habitat Survey (RHS) data from the
    EA’s Open Data portal;
  - `predict_indices` for calculating expected scores for
    macroinvertebrate indices using the RICT model (FBA 2020);
  - `calc_flowstats` and `calc_rfrstats` for calculating summary
    statistics describing historical flow conditions;
  - `join_he` for joining the above datasets;
  - `plot_heatmap` for visualising and summarising gaps in time series
    data;
  - `plot_hev` and `shiny_hev` for producing time series plots of
    biology and flow data;
  - `plot_sitepca` for summarising environmental characteristics of
    biological sampling sites;
  - `plot_rngflows` for Visualising the range of flow conditions
    experienced historically at a site;
  - `model_cv` and `model_logocv` for performing cross-validation on
    linear mixed-effects models and hierarchical generalized additive
    models;
  - `diag_lmer` for generating a variety of diagnostic plots for a
    mixed-effects regression (lmer) model; and
  - `plot_predictions` for visualising the time series predictions from
    a hydro-ecological model.

## Installation

To install the latest release of `hetoolkit` use the following code:

``` r
remotes::install_github("APEM-LTD/hetoolkit")
```

## Development

The `hetoolkit` package was developed by APEM LTD on behalf on the
Environment Agency. Version 1.0.0 was released in May 2021. For further
information please contact Mike Dunbar at the Environment Agency.

## Examples

Further information on the functions contained within the package,
including examples, can be found in the `HE Toolkit - Vingette` at
<https://apem-ltd.github.io/hetoolkit/>.

## Reporting Bugs

Please aid future development of the package by reporting bugs via the
Issues page on github.

## References

FBA, 2020. River Invertebrate Classification Tool (RICT2) User Guide
V1.5 (2020) \[Online\] Available at:
<https://www.fba.org.uk/FBA/Public/Discover-and-Learn/Projects/User%20Guides.aspx>

## Licence

The R-Code included within the Hydro-Ecology Toolkit package is licensed
under GPL-V3 (see: <https://www.gnu.org/licenses/gpl-3.0.en.html>).

The Hydro-Ecology Toolkit package contains public sector information
licensed under the Open Government Licence v3.0. Licence information is
available at:
<https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/>

## Disclaimer

This software has been approved for release by the Environment Agency
(EA). Although the software has been subjected to review and testing,
the EA reserves the right to update the software as needed pursuant to
further analysis and review. No warranty, expressed or implied, is made
by the EA as to the functionality of the software and related material
nor shall the fact of release constitute any such warranty. Furthermore,
the software is released on condition that the EA shall be held liable
for any damages resulting from its authorized or unauthorized use.
