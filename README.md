# Adding Up Paper: Replication Code

This project includes code used in the manuscript "Locally Satisfying the Adding Up Condition in Meta-Regression Models: A Simple Solution to a Persistent Regulatory Challenge". The input data are openly available in Zenodo at https://doi.org/10.5281/zenodo.17966084.

Each model is estimated twice:

- Midpoint models denoted by `ln_q_mid`. Scripts without a suffix. Outputs go to `Results/.../`.
- Baseline models denoted by `ln_q0`. Scripts with the `_nomid` suffix. Outputs go to `Results/.../no midpoint/`.

The `_nomid` scripts match their midpoint counterparts line for line except for the quality variable and the output paths.

---

## Requirements

**R packages:** `tidyverse`, `patchwork`, `brms`, `rstan`, `readxl`, `xlsx` (requires Java/`rJava`), `bayestestR`, `bayesplot`, `arrow`, `RSQLite`, `ggrepel`, `coda`

**Paths:** Every script calls `setwd()` with a hard-coded path. Before running, update this path to the local data storage location.

---

## Run Order

1. `descriptive_stats.R`: Cleans the metadata, plots WTP and WTP/dq against dq, summarizes the metadata variables, summarizes BenSPLASH water quality inputs at the stream-segment and grid-cell levels, and writes constant benefit transfer inputs.
   - Outputs: `Results/output tables/wtp_dq_plots.png`, `wtp_dq_plots_trend.png`, `lnmwtp_dq_plots.png`, `meta_stats.csv`, `bensplash wq stats.csv`, `bt_inputs.csv`
   - Publication Figures/Tables: Figure 1 (`wtp_dq_plots.png`); Table 1 (`bensplash wq stats.csv` plus the q0/dq rows of `meta_stats.csv`); Table B1 (`meta_stats.csv`)

2. `mrm2_mrm2s_estimation.R`: Fits MRM2 and MRM2-S with and without study random effects for midpoint models, under Gaussian and Student-t errors (8 models). The paper uses the Student-t random-effects fits (`.sid.fit.st`).
   - Outputs: `Results/mrm output/*.fit.{g,st}.rds`; `Results/output tables/regression_output.xlsx`, `bayes_compare.csv`, `log10_bayes_compare.csv`
   - Publication Figures/Tables: Table 3, MRM2 and MRM2-S columns (`mrm2.sid.fit.st` and `mrm2s.sid.fit.st` sheets)

3. `mrm2_mrm2s_nomid_estimation.R`: Fits MRM2 and MRM2-S with and without study random effects for baseline models, under Gaussian and Student-t errors (8 models). The paper uses the Student-t random-effects fits (`.sid.fit.st`).
   - Outputs: `Results/mrm output/no midpoint/*.fit.{g,st}.rds`; `Results/output tables/no midpoint/regression_output.xlsx`, `bayes_compare.csv`, `log10_bayes_compare.csv`
   - Publication Figures/Tables: Table 3, MRM2 and MRM2-S columns (`mrm2.sid.fit.st` and `mrm2s.sid.fit.st` sheets)

4. `mrm2ds_estimation.r`: Fits MRM2-DSx (Student-t, study random effects) for midpoint models with thresholds x = 3–15, 20, 25, 30, 35.
   - Outputs: `Results/mrm output/mrm2ds*.sid.fit.st.rds`; new sheets in `regression_output.xlsx`; `bayes_compare_ds.csv`, `log10_bayes_compare_ds.csv`
   - Publication Figures/Tables: Table 3, MRM2-DS6 columns (`mrm2ds6.sid.fit.st` sheet)

5. `mrm2ds_nomid_estimation.r`: Fits MRM2-DSx (Student-t, study random effects) for baseline models with thresholds x = 3–15, 20, 25, 30, 35.
   - Outputs: `Results/mrm output/no midpoint/mrm2ds*.sid.fit.st.rds`; new sheets in `regression_output.xlsx`; `bayes_compare_ds.csv`, `log10_bayes_compare_ds.csv`
   - Publication Figures/Tables: Table 3, MRM2-DS6 columns (`mrm2ds6.sid.fit.st` sheet)

6. `bayes_factors_all.R`: Loads 16 models (MRM2, DS3–DS15, DS20, MRM2-S) in both midpoint and baseline versions and computes a 32 × 32 matrix of pairwise Bayes factors.
   - Outputs: `Results/output tables/bayes_compare_all.csv`, `log10_bayes_compare_all.csv`
   - Publication Figures/Tables: Table 2, log10 Bayes factors for selected models, all three panels; Table C1, full log10 Bayes factors

7. `plot_posteriors.R`: Plots posterior distributions with 95% intervals for the `ln_q_mid` and `ln_q0` coefficients in MRM2, MRM2-DS6, and MRM2-S.
   - Outputs: `Results/output tables/qmid_q0_posteriors.png`
   - Publication Figures/Tables: Figure 2

8. `test_adding_up.R`: Predicts median per-household WTP for pre-specified inputs to compute the adding up error for midpoint models.
   - Outputs: `Results/output tables/adding_up_compare_ds.csv` (all), `adding_up_compare_ds_bias.csv` (percent-error rows only), `adding_up_compare_ds_subset.csv`
   - Publication Figures/Tables: Table 4 (`_subset.csv`, midpoint and baseline); Table C2 (`_bias.csv`, midpoint)

9. `test_adding_up_nomid.R`: Predicts median per-household WTP for pre-specified inputs to compute the adding up error for baseline models.
   - Outputs: `Results/output tables/no midpoint/adding_up_compare_ds.csv` (all), `adding_up_compare_ds_bias.csv` (percent-error rows only), `adding_up_compare_ds_subset.csv`
   - Publication Figures/Tables: Table 4 (`_subset.csv`, midpoint and baseline); Table C2 (`_bias.csv`, baseline)

10. `predict_low_dwqi_v2.r`: Out-of-sample prediction test for midpoint and baseline models on observations with dq ≥ 10 (n = 129).
    - Notes: The model-fitting block is commented out with a note to "ONLY RUN ONCE". Uncomment it on a first run so that `Results/mrm output/train wqi 10/*.rds` exist.
    - Outputs: `Results/output tables/predictions_summary_trainwqi10.csv`, `predictions_violin_trainwqi10.pqt`, `rmse_trainwqi10.csv`
    - Publication Figures/Tables: Table 5 (`rmse_trainwqi10.csv`)

11. `mean_vs_median.R`: Compares mean, trimmed mean (drop > 99.9th and > 99th percentiles), and median as summaries of predicted per-household WTP.
    - Outputs: `Results/output tables/mean_median_total_wtp.csv`
    - Publication Figures/Tables: Table C3 (supports manuscript footnote 22)

12. `national_bt.R`: Builds a grid-cell × year panel (63,766 cells × 2025–2049) from the BenSPLASH outputs for midpoint models.
    - Outputs: `Results/benefits transfer/bt_grid_year.pqt`, `bt_grid.pqt`, `national_summary.csv`, `bt_summary.csv`, `partial_bt_grid_year.pqt`
    - Publication Figures/Tables: Table 6 (model columns of `bt_summary.csv`, rows `grid_year mwtp`, `grid_year hhwtp`, `national awtp`); Table D1 (BenSPLASH columns of `bt_summary.csv`: `mrm2.median`, `mrm2.pct1`/`pct2`, `mrm2s.low`/`high`)

13. `national_bt_nomid.R`: Builds a grid-cell × year panel (63,766 cells × 2025–2049) from the BenSPLASH outputs for baseline models.
    - Outputs: `Results/benefits transfer/no midpoint/bt_grid_year.pqt`, `bt_grid.pqt`, `national_summary.csv`, `bt_summary.csv`
    - Publication Figures/Tables: Table 6 (model columns of `bt_summary.csv`, rows `grid_year mwtp`, `grid_year hhwtp`, `national awtp`); Table D1 (BenSPLASH columns of `bt_summary.csv`: `mrm2.median`, `mrm2.pct1`/`pct2`, `mrm2s.low`/`high`)
