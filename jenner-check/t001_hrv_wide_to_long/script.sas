/* ============================================================
   HRV wide -> long reshape (from Thesis_Project/HRV_DL.sas)

   The author's array-based reshape: six timepoints per physiological
   measure collapse into one row per (Subject_ID, Time), a REST_VS_STRESS
   condition flag is derived from Time, and SCL is kept only at T1-T2.
   Reproduced unmodified; the wide table it reads is the mock data in
   autoexec.sas.
   ============================================================ */

data hrv;
    set hrv_wide;

    /* Arrays for transformed variables */
    array lnRMSSD_a[6] lnRMSSD_msec_T1-lnRMSSD_msec_T6;
    array lnSDNN_a[6]  lnSDNN_msec_T1-lnSDNN_msec_T6;
    array lnHF_a[6]    lnHF_ms_T1-lnHF_ms_T6;
    array lnLF_a[6]    lnLF_ms_T1-lnLF_ms_T6;
    array lnLFHF_a[6]  lnLFHF_T1-lnLFHF_T6;
    array lnRSA_a[6]   lnRSA0_msec_T1-lnRSA0_msec_T6;
    array PEP_a[6]     PEP_msec_T1-PEP_msec_T6;

    /* SCL only available at T1-T2 */
    array lnSCL_a[2] lnAverage_SCL_uS_T1-lnAverage_SCL_uS_T2;

    do Time = 1 to 6;

        /* Transformed physiological variables */
        lnRMSSD_msec = lnRMSSD_a[Time];
        lnSDNN_msec  = lnSDNN_a[Time];
        lnHF_ms     = lnHF_a[Time];
        lnLF_ms     = lnLF_a[Time];
        lnLFHF       = lnLFHF_a[Time];
        lnRSA0_msec  = lnRSA_a[Time];
        PEP_msec     = PEP_a[Time];

        if Time in (1,2) then lnAverage_SCL_uS = lnSCL_a[Time];
        else lnAverage_SCL_uS = .;

        /* Condition variable */
        REST_VS_STRESS = (Time in (2,3,4,5));
        label REST_VS_STRESS = "Condition: 1=Stress, 0=Rest";

        output;
    end;

    /* Keep ONLY what you need */
    keep
        Subject_ID
		Time
        Gender
        Group
        Anxiety_mother
        REST_VS_STRESS
        lnRMSSD_msec
        lnSDNN_msec
        lnHF_ms
        lnLF_ms
        lnLFHF
        lnRSA0_msec
        lnAverage_SCL_uS
        PEP_msec;
run;


proc freq data=hrv;
    tables Time;
run;

proc means data=hrv n nmiss;
run;

/*QC before modeling*/
proc means data=hrv n nmiss min mean max;
  class Time;
  var lnRMSSD_msec lnSDNN_msec lnHF_ms lnLF_ms lnLFHF lnRSA0_msec lnAverage_SCL_uS PEP_msec;
  title "QC: Ranges by Time (long format)";
run;
