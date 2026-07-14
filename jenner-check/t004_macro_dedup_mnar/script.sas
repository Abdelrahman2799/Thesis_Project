/* ============================================================
   %dedup_mnar macro (from alpha-amylase.sas)

   The author's utility macro that collapses a pooled MNAR results
   table to unique rows over its four key columns. The macro definition
   is reproduced verbatim. A tiny mock results table with seeded
   duplicate keys is included inline so this script is self-contained
   (it runs on its own, e.g. via the /v1/quick example in the PR),
   then the macro is called on it and the de-duplicated result printed.
   ============================================================ */

/* Small inline mock MNAR results table (invented values), with
   deliberately duplicated key rows for the nodupkey sort to collapse. */
data ALL_MNAR_CORT;
  length Outcome $20 MNAR_pattern $12 Parameter $40;
  input Outcome $ MNAR_pattern $ Delta_index Parameter $ Estimate;
  datalines;
lnCort stress 1 Intercept 0.51
lnCort stress 1 Intercept 0.51
lnCort stress 1 Stress_vs_rest 0.22
lnCort stress 2 Stress_vs_rest 0.24
lnCort high 1 Group_high_vs_lowmed 0.13
lnCort high 1 Group_high_vs_lowmed 0.13
lnCort high 1 Female_vs_male -0.08
lnCort highstress 1 Interaction_GroupxStress 0.05
;
run;

%macro dedup_mnar(ds=, out=);
proc sort data=&ds out=&out nodupkey;
  by Outcome MNAR_pattern Delta_index Parameter;
run;
%mend;

%dedup_mnar(ds=ALL_MNAR_CORT, out=ALL_MNAR_CORT_DEDUP);

proc print data=ALL_MNAR_CORT_DEDUP;
    title "De-duplicated MNAR results (lnCort)";
run;
