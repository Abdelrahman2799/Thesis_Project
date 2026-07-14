/* ============================================================
   Covariate merge + AACR ratio derivation (from alpha-amylase.sas)

   Two pieces of the author's alpha-amylase pipeline:
   (1) the per-subject covariate merge that carries Group/Gender onto
       the metrics table, guarded by a hard abort if either covariate
       is missing, then a proc sql row-count integrity check;
   (2) the log alpha-amylase/cortisol ratio (lnAACR) derived at the
       baseline, peak, delta, AUC and recovery-slope summaries.
   Reproduced unmodified against the mock data in autoexec.sas.
   ============================================================ */

proc sort data=aas_long_prepared;
    by ID _Imputation_;
run;

data id_covariates;
    set aas_long_prepared;
    by ID _Imputation_;
    if first._Imputation_;
    keep ID _Imputation_ Group Gender;
run;

proc sort data=metrics;
    by ID _Imputation_;
run;

data metrics_with_cov;
    merge metrics(in=a)
          id_covariates(in=b);
    by ID _Imputation_;
    if a;

    /* Hard check */
    if missing(Group) then do;
        put "ERROR: Missing Group for ID=" ID " Imputation=" _Imputation_;
        abort;
    end;

    if missing(Gender) then do;
        put "ERROR: Missing Gender for ID=" ID " Imputation=" _Imputation_;
        abort;
    end;
run;

/* Check no loss of rows */
proc sql;
    select count(*) as n_metrics,
           (select count(*) from metrics_with_cov) as n_with_cov
    from metrics;
quit;

/* Check distributions */
proc freq data=metrics_with_cov;
    tables Group Gender;
run;

/* Add lnAACR summaries */
data metrics_with_cov;
    set metrics_with_cov;

    /* baseline ratio (S2) */
    lnAACR_s2 = lnAmy_s2 - lnCort_s2;

    /* peak ratio (S4-S6) */
    lnAACR_peak46 = lnAmy_peak46 - lnCort_peak46;

    /* delta (log ratio of ratios) */
    lnAACR_delta = lnAACR_peak46 - lnAACR_s2;

    /* AUC ratio (log scale) */
    auc_lnAACR = auc_lnAmy - auc_lnCort;

    /* recovery slope ratio */
    slope_lnAACR = slope_lnAmy - slope_lnCort;
run;

proc print data=metrics_with_cov;
    var ID Group Gender lnAACR_s2 lnAACR_peak46 lnAACR_delta auc_lnAACR slope_lnAACR;
    title "AACR summary metrics with covariates";
run;
