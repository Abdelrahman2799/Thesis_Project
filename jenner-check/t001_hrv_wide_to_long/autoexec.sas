/* cap input rows for the captured run */
options obs=100;

/* ------------------------------------------------------------------
   Mock HRV wide-format dataset.

   The thesis pipeline imports HRV_Data__Wide_Format_.csv (paths were
   stripped from the repo for portability), then reshapes it from wide
   (T1..T6 per measure) to long. This block supplies a tiny, synthetic
   wide table with the same column shape the reshape reads, so the
   author's own array/reshape DATA step below runs unmodified.
   Values are invented; only the structure matters.
   ------------------------------------------------------------------ */
data hrv_wide;
  input Subject_ID Gender $ Group Anxiety_mother
        lnRMSSD_msec_T1-lnRMSSD_msec_T6
        lnSDNN_msec_T1-lnSDNN_msec_T6
        lnHF_ms_T1-lnHF_ms_T6
        lnLF_ms_T1-lnLF_ms_T6
        lnLFHF_T1-lnLFHF_T6
        lnRSA0_msec_T1-lnRSA0_msec_T6
        PEP_msec_T1-PEP_msec_T6
        lnAverage_SCL_uS_T1 lnAverage_SCL_uS_T2;
  datalines;
1 m 0 12  3.1 3.2 3.0 3.3 3.1 3.2  3.5 3.6 3.4 3.7 3.5 3.6  6.1 6.0 6.2 6.3 6.1 6.0  6.8 6.7 6.9 7.0 6.8 6.7  0.6 0.7 0.5 0.8 0.6 0.7  4.1 4.2 4.0 4.3 4.1 4.2  110 108 106 104 105 107  1.2 1.3
2 f 1 20  3.4 3.5 3.3 3.6 3.4 3.5  3.8 3.9 3.7 4.0 3.8 3.9  6.4 6.3 6.5 6.6 6.4 6.3  7.1 7.0 7.2 7.3 7.1 7.0  0.9 1.0 0.8 1.1 0.9 1.0  4.4 4.5 4.3 4.6 4.4 4.5  118 116 114 112 113 115  1.5 1.6
3 m 1 25  2.9 3.0 2.8 3.1 2.9 3.0  3.3 3.4 3.2 3.5 3.3 3.4  5.9 5.8 6.0 6.1 5.9 5.8  6.6 6.5 6.7 6.8 6.6 6.5  0.4 0.5 0.3 0.6 0.4 0.5  3.9 4.0 3.8 4.1 3.9 4.0  102 100  98  96  97  99  1.0 1.1
4 f 0 15  3.2 3.3 3.1 3.4 3.2 3.3  3.6 3.7 3.5 3.8 3.6 3.7  6.2 6.1 6.3 6.4 6.2 6.1  6.9 6.8 7.0 7.1 6.9 6.8  0.7 0.8 0.6 0.9 0.7 0.8  4.2 4.3 4.1 4.4 4.2 4.3  112 110 108 106 107 109  1.3 1.4
5 m 0 18  3.0 3.1 2.9 3.2 3.0 3.1  3.4 3.5 3.3 3.6 3.4 3.5  6.0 5.9 6.1 6.2 6.0 5.9  6.7 6.6 6.8 6.9 6.7 6.6  0.5 0.6 0.4 0.7 0.5 0.6  4.0 4.1 3.9 4.2 4.0 4.1  106 104 102 100 101 103  1.1 1.2
6 f 1 22  3.3 3.4 3.2 3.5 3.3 3.4  3.7 3.8 3.6 3.9 3.7 3.8  6.3 6.2 6.4 6.5 6.3 6.2  7.0 6.9 7.1 7.2 7.0 6.9  0.8 0.9 0.7 1.0 0.8 0.9  4.3 4.4 4.2 4.5 4.3 4.4  116 114 112 110 111 113  1.4 1.5
;
run;
