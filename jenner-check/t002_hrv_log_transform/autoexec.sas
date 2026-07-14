/* cap input rows for the captured run */
options obs=100;

/* ------------------------------------------------------------------
   Mock HRV "clean base" wide table.

   Upstream this is HRV_Data__Wide_Format_.csv after dropping the lg10:
   columns. This block supplies a small synthetic table carrying the raw
   RSA0_msec_T1-T6 and Average_SCL_uS_T1-T2 columns the author's log
   transformation reads, plus placeholder ln* targets it overwrites.
   Values are invented; a few missings are seeded to exercise the
   ne . guard and the nmiss QC.
   ------------------------------------------------------------------ */
data hrv_clean_base;
  input Subject_ID
        RSA0_msec_T1-RSA0_msec_T6
        Average_SCL_uS_T1 Average_SCL_uS_T2
        lnRSA0_msec_T1-lnRSA0_msec_T6
        lnAverage_SCL_uS_T1 lnAverage_SCL_uS_T2;
  datalines;
1  120 118 .   115 116 119   2.1 2.3   . . . . . .   . .
2  130 128 126 124 .   127   2.5 2.7   . . . . . .   . .
3  110 108 106 104 105 .     1.9 .     . . . . . .   . .
4  125 123 121 119 120 122   2.4 2.6   . . . . . .   . .
5  115 113 111 109 110 112   2.0 2.2   . . . . . .   . .
;
run;
