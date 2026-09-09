# Forecasting Japanese subnational life-table death counts

Replication code for *Forecasting multiple density-valued time series* by Han Lin Shang.

The code forecasts age-specific life-table death counts for Japan — nationally and for all 47
prefectures, both sexes — by mapping the death-count densities into an unconstrained space,
fitting a functional time series model there, and mapping back. Two transformations and eleven
forecasting methods are compared on point and interval forecast accuracy over horizons
*h* = 1, …, 20.

## 1. Repository layout

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
