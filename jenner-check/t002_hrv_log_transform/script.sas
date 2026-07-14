/* ============================================================
   Corrected RSA / SCL log transformations (from HRV_DL.sas)

   The author replaces the incoming lnRSA and lnSCL columns with a
   shifted-log transform: log(RSA + 1) and log(SCL + 0.01), applied
   element-wise across the T1-T6 (RSA) and T1-T2 (SCL) arrays and
   guarded so missing inputs stay missing. Reproduced unmodified,
   followed by the author's missingness QC (proc means n nmiss).
   ============================================================ */

/* Replace lnRSA and lnSCL with correct transformations */
data hrv_transformed_wide;
    set hrv_clean_base;

    /* Correct RSA and SCL transformations */
    array rsa[6] RSA0_msec_T1-RSA0_msec_T6;
    array scl[2] Average_SCL_uS_T1 Average_SCL_uS_T2;
    array ln_rsa[6] lnRSA0_msec_T1-lnRSA0_msec_T6;
    array ln_scl[2] lnAverage_SCL_uS_T1-lnAverage_SCL_uS_T2;

    do i = 1 to dim(rsa);
        if rsa[i] ne . then ln_rsa[i] = log(rsa[i] + 1);
    end;

    do i = 1 to dim(scl);
        if scl[i] ne . then ln_scl[i] = log(scl[i] + 0.01);
    end;

    /* PEP kept on raw scale */

    drop i;
run;

/*--------------------------------------------------------------
Quick QC check before imputation
--------------------------------------------------------------*/
proc means data=hrv_transformed_wide n nmiss min mean std max;
    var lnRSA0_msec_T1-lnRSA0_msec_T6 lnAverage_SCL_uS_T1-lnAverage_SCL_uS_T2;
    title "QC: Check corrected log transformations (RSA, SCL)";
run;

proc sort data=hrv_transformed_wide; by Subject_ID; run;
proc print data=hrv_transformed_wide(obs=20); run;
