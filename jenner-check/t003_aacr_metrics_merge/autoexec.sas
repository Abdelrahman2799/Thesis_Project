/* cap input rows for the captured run */
options obs=100;

/* ------------------------------------------------------------------
   Mock alpha-amylase datasets.

   Upstream these come from imported xlsx (aas_long_prepared_with_times
   and aas_metrics_log; paths stripped for portability). Two small
   synthetic tables are supplied with the column shape the author's
   covariate merge and AACR-ratio derivation read: a long table keyed
   by (ID, _Imputation_) carrying Group/Gender and the log analytes,
   and a per-subject metrics table with the summary log measures.
   Values are invented.
   ------------------------------------------------------------------ */
data aas_long_prepared;
  input ID _Imputation_ Group Gender $ lnAmy lnCort;
  datalines;
1 1 0 m 2.10 1.30
1 1 0 m 2.40 1.55
2 1 1 f 2.55 1.42
2 1 1 f 2.80 1.61
3 1 1 m 1.95 1.20
3 1 1 m 2.20 1.38
4 1 0 f 2.35 1.50
4 1 0 f 2.60 1.66
;
run;

data metrics;
  input ID _Imputation_
        lnAmy_s2 lnCort_s2 lnAmy_peak46 lnCort_peak46
        auc_lnAmy auc_lnCort slope_lnAmy slope_lnCort;
  datalines;
1 1  2.10 1.30 2.45 1.58  12.1 8.0  0.11 0.07
2 1  2.55 1.42 2.85 1.65  13.4 8.6  0.14 0.08
3 1  1.95 1.20 2.25 1.40  11.2 7.3  0.09 0.05
4 1  2.35 1.50 2.62 1.70  12.8 8.9  0.12 0.09
;
run;
