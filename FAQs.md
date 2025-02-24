FAQs
================

<!-- FAQs.md is generated from FAQs.Rmd. Please edit that file -->

</br>

### 1. What ecological components can the `hetoolkit` be used to assess?

In principle the toolkit can be used to help analyse data on any
ecological receptor of interest. Depending on the receptor, manual
importing and processing of your ecological data may be required before
you progress onto using the toolkit’s various functions. This is one of
the advantages of the toolkit being in the R environment: personalised
code and manual workarounds can be interwoven with the toolkit’s
functions to best suit your analysis.

</br>

### 2. What ecological data can be downloaded by the `hetoolkit`?

The toolkit currently only provides functions to import data from
macroinvertebrate samples, including indices (import_inv()),
taxon-specific abundances (import_inv_taxa()) and contextual
environmental information (import_env()). However, macrophyte, diatom
and fish data can be manually downloaded from the EA’s Ecology & Fish
Data Explorer and then imported into R for subsequent processing and
analysis. There is a bulk downloads page
(<https://environment.data.gov.uk/ecology/explorer/downloads/>) which
makes it possible to import data ‘en masse’ into R and then filter to
your sites or area of interest. All of this, including the downloading
of data, can be done in R (see
<https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/download.file>).

</br>

### 3. What hydrological datasets can be downloaded and which are the most appropriate to use?

Observed (gauged) river flow data can be downloaded from the National
River Flow Archive and the EA’s Hydrology Data Explorer using the
import_flow set of functions. These data can be appropriate to use if
flow variation at a gauging station is representative of that at a
paired ecology site, and if the focus of the analysis is on responses to
historic flows, rather than flow alteration. Otherwise modelled flow
data should be used, which will require model outputs to be manally
imported into R.

</br>

### 4. Which is best to use – surface water or groundwater hydrological models/datasets, or both?

In parts of the country covered by well-calibrated, EA-approved
groundwater models, these are the best source of modelled flow data.
Elsewhere tools such as QUBE or rainfall-runoff/lumped catchment models
may be used. If you are uncertain about the best available source of
flow time series data, you should consult a local EA hydrologist.

</br>

### 5. Can the `hetoolkit` be used to download water quality data?

Yes, the import_wq() function allows you to download data for
determinands of interest from the EA’s Water Quality Archive.

</br>

### 6. Can I import my own datasets?

Yes, you can import your own data and then use the `hetoolkit` for
processing and analysis. You may first need to format your data to
ensure it is compatible.

</br>

### 7. Can I just use the first stages of model development to download and prepare datasets?

Yes, you can use any or all of the functions in the toolkit as you see
fit. You may choose to use the toolkit for relatively simple actions
such as running RICT predictions, importing and plotting flow data or
creating HEV plots. The toolkit is not exclusively for the development
of HE models.

</br>

### 8. Is the use of the `hetoolkit` a ‘must’ for water company assessments?

It is not obligatory to use the toolkit for all water company HE
assessments, although some applications (e.g. local flow constraint
proposals) demand a modelling approach with a comparable or greater
level of statistical robustness to that offered by the workflow of the
toolkit. Ultimately the toolkit makes it easier to collate, process and
analyse HE data, and it is therefore in the interest of water companies
and their consultants to use it.

</br>

### 9. What range of ecological modelling tools or assessment approaches can be used?

The toolkit primarily facilitates the building of mixed regression
models using ‘pooled’ monitoring data. These can take the form of
generalised linear models or generalised additive models. The latter,
which may be abbreviated as GAMs, GAMMs or HGAMs, are more versatile as,
among other things, they can capture both linear and nonlinear
ecological responses. The case study in this website’s article library
‘Macroinvertebrates in upland rivers’ illustrates how to undertake
generalised additive modelling.

other modelling approaches may be complementary or in specific cases
more suitable. These include hydraulic-habitat modelling and
multivariate analyses of community composition. There is a case study on
the latter (‘Diatoms’) in this website’s article library. Even for
approaches not using mixed regression models the toolkit may still be
useful for data collation and processing.

</br>

### 10. Is the `hetoolkit` designed only for the assessment of invertebrate impacts?

No, the toolkit is designed for use with any ecological receptor of
interest. The toolkit’s functionality is more conducive to the analysis
of invertebrate data because invertebrates are the best flow
bioindicators: they form diverse communities comprising taxa with
distinct and well-established flow preferences. Most HE analyses and
models therefore tend to use invertebrate data. However, other aspects
of the ecology may need to be considered depending on the study.

</br>

### 11. Are there any case studies for macrophytes, diatoms or fish assessments using the `hetoolkit`?

There are toolkit case studies on macrophyte and diatom responses to
flow in this website’s article library. We are planning work to explore
how fish data can be best represented in HE models.

</br>

### 12. Can the same flow time series data be used for analyses of different ecological receptors?

Yes, the same flow data can be used to build multiple models based on
different ecological receptors, although the selection of optimal flow
windows may differ (see below). Depending on the scope of the study it
can make sense to build a large HE dataset that includes all relevant
flow and environmental data plus multiple ecological receptors, each of
which is then used for a different model.

</br>

### 13. What period of antecedent flow should flow statistics represent?

The optimal antecedent flow period over which to calculate statistics
will vary from model to model, and should be informed by statistical
approaches and/or expert judgement. Statistical approaches include
sensitivity analysis and moving window analysis (for which there is a
case study in this website’s article library), while expert judgement
can draw on the literature and knowledge of species life cycles and
phenology. The toolkit provides the flexibility to calculate flow
statistics for a wide range of time periods, at various intervals and
lags.

</br>

### 14. How many sites should an HE model incorporate?

There is no hard-and-fast rule, but all other things being equal, the
more sites the better. However, ensuring that sites provide good
coverage of the environmental gradients of interest (e.g. flow
alteration) is more important than the absolute size of the dataset. For
instance, a dataset of 25 sites exposed to varying levels of abstraction
pressure is likely to be preferable to a dataset of 50 sites that are
all minimally impacted or all heavily impacted. Even with a large number
of sites there is no guarantee of producing a useful model that can
predict ecological responses to flow or flow alteration with a high
degree of confidence.

</br>

### 15. Is there a minimum number of samples per site required?

Again there is no hard-and-fast rule, and again all other things being
equal the more samples per site the better. Typically for an
invertebrate-based model a minimum of six samples straddling at least
three years is preferable, but this may depend on data availability and
the advantage of a pooled modelling approach is that the relationship
derived at any one site can ‘borrow strength’ from the wider dataset.

</br>

### 16. What geographical scale can/should an HE model cover – site, waterbody, whole catchment, regional or national?

A site-specific model should only be developed if long-term (15-20+
years of continuous annual monitoring) flow and ecology data are
available. Realistically, models built at a waterbody scale are unlikely
to be able to draw on sufficient monitoring data or capture an adequate
range of flow pressures. Whole-catchment models are likely to be
preferable in this regard, although model extents do not necessarily
need to be defined by watersheds. Regional models, particularly those
specific to a particular pre-defined river type, offer the greatest
promise, capturing a wide range of conditions (or calibration range)
whilst also still allowing local knowledge to play an important part in
model development. At a national scale flow-ecology relationships may be
masked by very broad environmental gradients, and HE models are largely
untested at this scale.

</br>

### 17. What environmental variables should be included in a model?

Any environmental variable that could, based on the sites in the model,
confound the derived relationship between flow and ecology should be
included. Commonly these include channel modification, which can be
represented by RHS data, and water quality, which can be represented by
historic monitoring data or outputs from water quality models. Other
relevant environmental data include land use variables (e.g. % arable
land cover in catchment), substrate information (e.g. % fine sediment
recorded during macroinvertebrate sample collection) and suspected
presence of invasive species. Where these data are not used site
screening will need to be more stringent, to exclude sites where
confounding pressures are prevalent.

</br>

### 18. For invertebrates must studies use LIFE or can other indices, community or species data be used?

It is not obligatory to use LIFE as your ecological receptor, and
stronger relationships with flow and flow alteration have been derived
using WHPT ASPT in some models. WHPT ASPT also has the advantage of
providing a direct measure of changes in WFD classification. However,
community-averaged indices, such as LIFE and WHPT ASPT, are relatively
insensitive to shifts in abundance, and their responses may mask or lag
behind species-level impacts. An index-based HE model may therefore not
be sufficiently precautionary, at least in isolation, for certain
applications, such as identifying a threshold level of abstraction for
the purposes of proposing a local flow constraint. Analyses of community
compositional responses can therefore be valuable, and the toolkit’s
import_inv_taxa can be used for this purpose. If you require help
processing invertebrate abundance data, including harmonising records at
different taxonomic levels, you can contact
<hydroecologyteam@environment-agency.gov.uk>.

</br>

### 19. Can the `hetoolkit` be used to build a hydroecological model for a temporary river?

Yes: although the case studies on this website provide examples from
perennial rivers, many of the toolkit’s functions can still be used to
process and analyse data for studies on temporary rivers. However, it is
important to remember that temporary rivers‒ those that naturally cease
to flow at one or more points along their length for at least part of
the year‒ exhibit characteristically different flow-ecology
relationships. On perennial watercourses flow magnitude exerts a
fundamental influence on river ecology, with low and high flow events
particularly significant. On temporary rivers flow duration replaces
magnitude as the critical aspect of the flow regime, so flow metrics
(e.g. Q95) and biomonitoring tools (e.g. LIFE scores) developed on
perennial rivers will not be transferable to these systems.

These differences are important to bear in mind when undertaking a
temporary river HE assessment, and will likely necessitate a bespoke
modelling approach. The definition of reference conditions and good
ecological status and the development of biomonitoring indices (with the
exception of the Plant Flow Index, PFI) for temporary rivers are
currently ongoing, and are unlikely to inform the Environment Agency’s
regulatory position for several years to come. In the interim, studies
should make use of experimental approaches and expert interpretation,
informed by the now-substantial body of scientific literature on
temporary rivers. Please contact
<hydroecologyteam@environment-agency.gov.uk> for further advice.

</br>

### 20. Can the functions in the `hetoolkit` be expanded beyond what is currently on Github?

We anticipate that the toolkit will continue to evolve as its uptake
increases. If you have any particular suggestions for functions you
would like to see added please contact
<hydroecologyteam@environment-agency.gov.uk>.

</br>

### 21. What model/R code evidence will the EA require to review HE models?

This will depend on the area for application: where the toolkit is used
as part of a standard WINEP Water Resources Investigation a clear and
concise overview of the key stages of data compilation and processing
and model specification will typically suffice. For higher-risk
applications such as local flow constraint work, comprehensive and
suitably annotated R code should be shared, along with all relevant
output including model diagnostics.

</br>

### 22. Who do I contact if there are issues with running any parts of the `hetoolkit`?

Please contact the EA’s National Hydroecology Team
(<hydroecologyteam@environment-agency.gov.uk>) and/or APEM
(<hetoolkit@apemltd.co.uk>). If you find a bug you can also log an issue
(and, if possible, a reproducible example) at
<https://github.com/APEM-LTD/hetoolkit/issues>

</br>

### 23. Who do I contact for general advice and support for the `hetoolkit`?

Please contact the EA’s National Hydroecology Team
(<hydroecologyteam@environment-agency.gov.uk>)

</br>

### 24. Can we send in our own case studies to add to the articles library – if so what format and where to send?

You are welcome to share your toolkit case studies, whether they detail
the full development of an HE model or simply use of certain aspects of
the toolkit’s functionality. If possible please send them in R Markdown
format to <hydroecologyteam@environment-agency.gov.uk>.
