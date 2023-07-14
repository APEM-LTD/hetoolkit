
<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

# hetoolkit 2.1.1

## Changes to Functions

-   `predict_indices`: Minor update to account for changes in
    `rict::rict_predict`.

# hetoolkit 2.1.0

## New Functions

-   `import_wq`: for importing water quality data from the EA Water
    Quality Archive database.

## Changes to functions

-   `import_rhs`: deprecated rhs_dir argument, use source instead.
    Function now returns an object, even when no surveys are specified
    for filtering.

## Bug fixes

-   `calc_flowstats`: Corrections to calculations for long-term
    statistics, including adding mean, min and max flows.
-   `import_inv`: Fix to resolve error if importing a formatted rds
    file.  
-   `import_env`: Removed tictoc call.
-   `import_rhs`: Removed tictoc call.
-   `import_hde` updated following updates to the HDE.

# hetoolkit 2.0.0

## New Functions

-   `impute_flow`: for infilling missing records in daily flow time
    series for one or more sites (gauging stations) using either an
    interpolation or an equipercentile method.

## Changes to functions

-   `import_hde`: Temporary .csv files downloaded are now deleted
    afterwards.
-   `import_inv`: Now includes an option to download macroinvertebrate
    data from Ecology and Fish Data Explorer in .parquet format, which
    is faster than .csv and has data types pre-formatted. This is the
    new default, replacing the old .csv file download. New argument
    `source` added, with options to automatically download data from EDE
    in .parquet or .csv format, or read in a previously saved .csv, .rds
    or .parquet file. The old `biol_dir` argument for specifying a local
    file is deprecated, but retained for backwards compatibility.
-   `predict_indices`: updated to utilise the `rict_predict` function
    from the `rict` package (<https://github.com/aquaMetrics/rict>),
    thereby eliminating the risk of divergence between the `hetoolkit`
    and `rict` packages. The former function has been re-named to
    `predict_indices_old` for backwards compatibility.
-   `calc_flowstats`: Completely re-written to provide much greater
    flexibility for characterising antecedent flows. Rather than use
    fixed, 6-month winter and summer periods, the new function uses a
    user-defined moving window, and calculates a wider range of flow
    statistics (include statistics to quantify the timing, frequency and
    severity of high flow, low flow and dry events, as well as the
    timing and magnitude of maximum and minimum flows). If the required
    minimum number of records is not met, then NAs are returned. The
    long-term flow statistics are now all annual statistics (separate
    winter and summer statistics have been dropped). The former function
    has been re-named to `calc_flowstats_old` for backwards
    compatibility.
-   `join_he`: Completely re-written to provide much greater flexibility
    when joining flow and biology data. It is now possible to: (i) join
    biology data to flow statistics (as well as join flow statistics to
    biology samples), (ii) join flow statistics that have been
    calculated by `calc_flowstats` for any moving window (not just fixed
    6-month winter and summer periods), and (ii) join flows for any
    lagged time period (not just the summer period of the previous year
    and the year before last). The former function has been re-named to
    `join_he_old` for backwards compatibility.
-   `model_cv` and `model_logocv`: modified to support model class
    “lmerTest” (from lmerTest::lmer), in addition to “lmerMod” (from
    lme4::lmer) and “gam” (from mgcv::gam).

## Bug fixes

-   `plot_rngflows`: Fixed issue to prevent error when z variable is
    unspecified.
-   `plot_sitepcs`: fixed issue to show labels when `plotly = TRUE`.
-   `predict_indices`: Removed deprecated function.
-   `impute_flows`: Check added to correct warning message appearance
    when donor sites specified.
-   `calc_flowstats`: Updated to deal with non-consecutive flow samples

## General

-   Vignette updated to include new functions and reflect other changes.
-   Created new support pages with guidance on installing and using the
    hetoolkit.  
-   Updated and rationalised the list of external package dependencies
    to speed up installation.  
-   Various improvements and updates to function documentation and
    worked examples.