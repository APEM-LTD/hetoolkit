
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hetoolkit

<!-- badges: start -->
<!-- badges: end -->

## Overview

The `hetoolkit` package comprises a collection of 21 functions for
assembling, processing, visualising and modelling hydro-ecological data.
These are:

-   `import_nrfa` for importing flow data from the National River Flow
    Archive (NRFA);
-   `import_hde` for importing flow data from the Environment Agency
    (EA) Hydrology Data Explorer (HDE);
-   `import_flowfiles` for importing flow data from local files;
-   `import_flow` for importing flow data from a mix of the above
    sources;
-   `impute_flow` for infilling missing records in daily flow time
    series for one or more sites (gauging stations) using either an
    interpolation or an equipercentile method.
-   `import_inv` for importing macroinvertebrate sampling data from the
    EA Ecology and Fish Data Explorer;
-   `import_env` for importing environmental base data from the EA
    Ecology and Fish Data Explorer;
-   `import_rhs` for importing River Habitat Survey (RHS) data from the
    EA’s Open Data portal;
-   `predict_indices` for calculating expected scores for
    macroinvertebrate indices using the RICT model (FBA 2020);
-   `calc_flowstats` and `calc_rfrstats` for calculating summary
    statistics describing historical flow conditions;
-   `join_he` for joining the above datasets;
-   `plot_heatmap` for visualising and summarising gaps in time series
    data;
-   `plot_hev` and `shiny_hev` for producing time series plots of
    biology and flow data;
-   `plot_sitepca` for summarising environmental characteristics of
    biological sampling sites;
-   `plot_rngflows` for Visualising the range of flow conditions
    experienced historically at a site;
-   `model_cv` and `model_logocv` for performing cross-validation on
    linear mixed-effects models and hierarchical generalized additive
    models;
-   `diag_lmer` for generating a variety of diagnostic plots for a
    mixed-effects regression (lmer) model; and
-   `plot_predictions` for visualising the time series predictions from
    a hydro-ecological model/

The deprecated versions of three functions are retained for
back-compatibility. These are:

-   `calc_flowstats_old`, `predict_indices_old` and `join_he_old`

## WORKFLOW

The different functions link together as shown in the flwo chart below:

![image info](./FlowChart_v01.png)

## Installation

To install the latest release of `hetoolkit` use the following code:

``` r
install.packages("devtools")
library(devtools)
install_github("APEM-LTD/hetoolkit_updated")
library(hetoolkitUpdated)
```

As an alternative, the ‘remotes’ package can be used:

``` r
install.packages("remotes")
library(remotes)
remotes::install_github("APEM-LTD/hetoolkit_updated")
library(hetoolkitUpdated)
```

## Development

The `hetoolkit` package was developed by APEM LTD on behalf on the
Environment Agency. Version 1.0.0 was released in May 2021. For further
information please contact Mike Dunbar at the Environment Agency.

## Change history

<table>
<colgroup>
<col style="width: 24%" />
<col style="width: 30%" />
<col style="width: 45%" />
</colgroup>
<thead>
<tr class="header">
<th>Version</th>
<th>Date</th>
<th>Details</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>1.0.0</td>
<td>May-2021</td>
<td>New release</td>
</tr>
<tr class="even">
<td>2.0.0</td>
<td>TBC</td>
<td>Added new function impute_flows<br />
Major updates to calc_flowstats, predict_indices and join_he</td>
</tr>
</tbody>
</table>

## Examples

Further information on the functions contained within the package,
including examples, can be found in the `HE Toolkit - Vingette` at
<https://apem-ltd.github.io/hetoolkit_updated/>.

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
the software is released on condition that the EA shall not be held
liable for any damages resulting from its authorized or unauthorized
use.
