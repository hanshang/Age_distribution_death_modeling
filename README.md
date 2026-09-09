# Forecasting Japanese subnational life-table death counts

Replication code for *Forecasting multiple density-valued time series* by Han Lin Shang.

The code forecasts age-specific life-table death counts for Japan — nationally and for all 47
prefectures, both sexes — by mapping the death-count densities into an unconstrained space,
fitting a functional time series model there, and mapping back. Two transformations and eleven
forecasting methods are compared on point and interval forecast accuracy over horizons
*h* = 1, …, 20.

## 1. Method in brief

Life-table death counts *d<sub>x</sub>* form a density: they are non-negative and sum to the radix
(10<sup>5</sup> here). Linear forecasting methods do not respect either constraint, so the counts
are transformed first.

**CDF transformation.** Form the cumulative distribution `F = cumsum(dx / 10^5)`, drop the last
ordinate (*F*(110) = 1 by construction, and `logit(1) = Inf`), and apply `logit()`. Forecast in
logit space, then `invlogit()`, re-append the 1, and `diff()` back to a density.

**CLR transformation.** Treat each year's *d<sub>x</sub>* vector as a composition and apply the
centred log-ratio transform, clr(*x*) = log *x* − mean(log *x*). Forecast in clr space and map
back with `clrInv()`. Zeros must be replaced before taking logarithms.

Within each transformation, the same five multi-population models plus three "gap" models are
fitted:

| Label | Model |
|---|---|
| UFTS | Univariate functional time series — one `ftsm` per prefecture per sex |
| MFTS | Multivariate FTS — female and male curves stacked into one joint `ftsm` |
| MLFTS | Multilevel FTS — a common component plus sex-specific residual components |
| HDFPCA | High-dimensional functional PCA, two-stage (`hdftsa::hdfpca`) |
| FANOVA+FFM | Two-way functional ANOVA plus a functional factor model on the residuals |
| Gender gap | Forecast the female curve and the male − female gap, reconstruct the male curve |
| Region gap | Forecast the national curve and the prefecture − national gap |
| Double gap | National → region gap → gender gap, applied in sequence |

Principal component scores are extrapolated with `ets()` or `auto.arima()`. The number of retained
components is chosen either by an eigenvalue-ratio rule (`select_K`, reported as **EVR**) or fixed
at **K = 6**. UFTS, MFTS and MLFTS appear in the results tables under both settings; HDFPCA, FANOVA
and the three gap models appear once each — giving the eleven columns of every table.

**Evaluation design.** With 52 years of data (1973–2024):

```
|<------- 11 years ------->|<------ 21 years ------>|<----- 20 years ----->|
        initial training            validation               test
          1973-1983                 1984-2004              2005-2024
```

- **Point accuracy** is computed on the test period with an expanding window: KLD, JSD,
  Wasserstein *L*<sub>1</sub> and Wasserstein *L*<sub>2</sub> against the held-out
  *d<sub>x</sub>*. Only KLD reaches the published tables.
- **Interval accuracy** first uses the validation period to tune a scalar multiplier
  `tune_para` such that the pointwise band
  forecast ± `tune_para` × sd(validation residuals) attains nominal coverage; the tuned bands are
  then scored on the test period with **ECP** (empirical coverage probability), **CPD**
  (|ECP − nominal|) and **MIS** (mean interval / Winkler score), at nominal 80% and 95%.

This is where the recurring index arithmetic comes from: `21 - horizon`, `22 - horizon`,
`n_year - 21 + ij` and `n_year - 42 + ij` appear throughout and all encode the split above.

---

## 2. Data

| | |
|---|---|
| Source | Japanese Mortality Database (JMD), IPSS — <https://www.ipss.go.jp/p-toukei/JMD/> |
| Accessed via | `HMDHFDplus::readJMDweb()` |
| Credentials | **None required.** (Unlike HMD, JMD is open.) |
| Series | `fltper_1x1`, `mltper_1x1`; only the `qx` column is used |
| Units | Prefecture codes `"01"`–`"47"`; `"00"` = national |
| Coverage | Ages 0–110, years 1973–2024 (Okinawa has no data before 1973, so all prefectures are truncated to that window) |
| Derivation | Life-table death counts are reconstructed by iterating a radix of 10<sup>5</sup> through `qx` |
| Accessed on | **[fill in the download date]** |

`read_data.R` performs the download (94 sequential HTTP requests) and builds every object the
model scripts consume. Because JMD is revised and extended annually, **record the access date** —
re-running against a later vintage will not reproduce the published numbers.

---

## 3. Requirements

**R ≥ 4.1** (the native pipe `|>` is used in `win_heatmap.R`).

CRAN packages: `ftsa`, `LaplacesDemon`, `flexmix`, `psych`, `easyCODA`, `DescTools`, `xtable`,
`transport`, `compositions`, `HMDHFDplus`, `zCompositions`, `forecast`, `MCS`, `dplyr`, `ggplot2`
(≥ 3.4), `tidyr` (≥ 1.3).

Not on CRAN or requiring a note:

- **`hdftsa`** — provides `hdfpca()` and the `Two_way_mean()` / `Two_way_mean_residuals()` FANOVA
  helpers.
- **`MCS`** — has been archived on CRAN in the past; check it installs before relying on it.

`auxiliary/load_packages.R` currently loads several packages that are never used (`doMC`,
`MortalityLaws`, `RColorBrewer`, `rlist`, `demography`, `vars`, `Compositional`, `tidyverse`) and
omits two that are (`forecast`, `MCS`). `doMC` is Unix-only and is the sole thing preventing the
code from running on Windows; nothing in the repository uses it.

**Record your session.** Add the output of `sessionInfo()`, or an `renv.lock`, to the repository —
`auto.arima()` and `ets()` results are sensitive to the `forecast` package version.

---

## 4. Repository layout

```
.
├── read_data.R                     download JMD, build all data objects (RUN FIRST)
├── CDF_clr_win_heatmap.R                   summary heat map across all tables
│
├── auxiliary/                      shared function library
│   ├── load_packages.R
│   ├── auxiliary_point.R           point-forecast models and evaluation
│   ├── auxiliary_interval.R        interval-forecast models and evaluation
│   ├── hdfpca_fun.R                HDFPCA wrapper and a warning-free forecast method
│   └── interval_score.R            (duplicate of a function in auxiliary_interval.R; unused)
│
├── CDF/                            CDF (logit-cumulative) transformation
│   ├── point_forecast/
│   │   ├── Multi-population_modeling/   CDF_{UFTS,MFTS,MLFTS,HDFPCA,FANOVA}.R
│   │   ├── Gap_modeling/                gender_gap*, region_gap, double_gap
│   │   └── summary_point_{arima,ets}.R  boxplot figure + accuracy table
│   ├── interval_forecast/
│   │   ├── Multi-population_modeling/   {UFTS,MFTS,MLFTS,HDFPCA,FANOVA}/
│   │   ├── Gap_modelling/               *_interval.R
│   │   └── summary_interval_{arima,ets}_{ECP,CPD,score}.R
│   └── CDF_clr_MCS.R                   model confidence sets (CDF and CLR together)
│
└── CLR/                            CLR (compositional) transformation — mirrors CDF/
    ├── point_forecasts/
    └── interval_forecasts/
```

Every `.R` file carries a header block stating its purpose, what must already be in the workspace,
what it produces, and which output it feeds.

## 5. Run order

There is no driver script. Scripts communicate through the global environment rather than through
files, so **order matters and is not expressed anywhere in the code**. Run in this order, in one
continuous session:

```r
# --- setup ---------------------------------------------------------------
source("auxiliary/load_packages.R")
source("read_data.R")                 # ~94 HTTP requests; slow
source("auxiliary/auxiliary_point.R")
source("auxiliary/hdfpca_fun.R")
source("auxiliary/auxiliary_interval.R")

# --- CDF point forecasts -------------------------------------------------
source("CDF/point_forecast/Multi-population_modeling/CDF_UFTS.R")   # also defines
                                        #   point_fore_national_cdf(), which the CLR tree needs
source("CDF/point_forecast/Multi-population_modeling/CDF_MFTS.R")
source("CDF/point_forecast/Multi-population_modeling/CDF_MLFTS.R")
source("CDF/point_forecast/Multi-population_modeling/CDF_HDFPCA.R")
source("CDF/point_forecast/Multi-population_modeling/CDF_FANOVA.R")
source("CDF/point_forecast/Gap_modeling/gender_gap_fun.R")
source("CDF/point_forecast/Gap_modeling/gender_gap.R")
source("CDF/point_forecast/Gap_modeling/region_gap.R")
source("CDF/point_forecast/Gap_modeling/double_gap.R")
source("CDF/point_forecast/summary_point_arima.R")
source("CDF/point_forecast/summary_point_ets.R")

# --- CDF interval forecasts ----------------------------------------------
#   NOTE: within MFTS and HDFPCA the *_ets and *_arima files are not
#   interchangeable in order — see Known issues.
source("CDF/interval_forecast/Multi-population_modeling/UFTS/CDF_UFTS_interval_ets.R")
source("CDF/interval_forecast/Multi-population_modeling/UFTS/CDF_UFTS_interval_arima.R")
source("CDF/interval_forecast/Multi-population_modeling/MFTS/CDF_MFTS_interval_ets.R")
source("CDF/interval_forecast/Multi-population_modeling/MFTS/CDF_MFTS_interval_arima.R")
source("CDF/interval_forecast/Multi-population_modeling/MLFTS/CDF_MLFTS_interval_ets.R")
source("CDF/interval_forecast/Multi-population_modeling/MLFTS/CDF_MLFTS_interval_arima.R")
source("CDF/interval_forecast/Multi-population_modeling/HDFPCA/CDF_HDFPCA_interval_arima.R")
source("CDF/interval_forecast/Multi-population_modeling/HDFPCA/CDF_HDFPCA_interval_ets.R")
source("CDF/interval_forecast/Multi-population_modeling/FANOVA/CDF_FANOVA_interval.R")
source("CDF/interval_forecast/Gap_modelling/gender_gap_interval.R")
source("CDF/interval_forecast/Gap_modelling/region_gap_interval.R")
source("CDF/interval_forecast/Gap_modelling/double_gap_interval.R")
for (f in Sys.glob("CDF/interval_forecast/summary_interval_*.R")) source(f)

# --- CLR: the same sequence under CLR/point_forecasts and CLR/interval_forecasts

# --- cross-transformation results ----------------------------------------
source("CDF_clr_win_heatmap.R")
source("CDF_clr_MCS.R")               
```
