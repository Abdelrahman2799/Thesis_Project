/* ============================================================
   HRV and SCL Analysis

   This script performs:
   - Direct likelihood LMM analyses for the HRV/SCL dataset.

   Paths are omitted for portability.
   ============================================================ */


proc import datafile=".../hrv_data.xlsx"
    out=work.hrv
    dbms=xlsx
    replace;
run;
proc contents data=hrv; run;


/*==============================================================
			1 — Data Import and preprocessing
==============================================================*/
data hrv;
set hrv;
Time = Subsequent_Observations;
run;

proc import datafile=".../HRV_Data__Wide_Format_.csv"
    out=hrv_wide
    dbms=csv
    replace;
    guessingrows=max;
run;


/*Drop all log10 variables (not needed since we have already ln ones)*/
data hrv_clean_base;
    set hrv_wide;
    drop lg10:;
run;

/*Check the data content and variables distribution*/
proc contents data=hrv_clean_base; run;

proc means data=hrv_clean_base n min mean max; 
var  RSA0_msec_T1-RSA0_msec_T6 Average_SCL_uS_T1-Average_SCL_uS_T2
		SDNN_msec_T1-SDNN_msec_T6 RMSSD_msec_T1-RMSSD_msec_T6
		
		PEP_msec_T1-PEP_msec_T6;
run;

proc means data=hrv_clean_base n min mean max; 
var  HF_ms_T1-HF_ms_T6 LF_ms_T1-LF_ms_T6 LFHF_T1-LFHF_T6;
run;

/* Replace lnRSA and lnSCL with correct transformations and keep only relevant physiological variables */

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
3. Quick QC check before imputation
--------------------------------------------------------------*/
proc means data=hrv_transformed_wide n nmiss min mean std max;  /*check missingness at each time point*/
    var lnRSA0_msec_T1-lnRSA0_msec_T6 lnAverage_SCL_uS_T1-lnAverage_SCL_uS_T2
		lnSDNN_msec_T1-lnSDNN_msec_T6 lnRMSSD_msec_T1-lnRMSSD_msec_T6
		PEP_msec_T1-PEP_msec_T6;
    title "QC: Check corrected log transformations (RSA, SCL) + other log transformations + PEP";
run;
proc sort data=hrv_transformed_wide; by Subject_ID; run;
proc print data=hrv_transformed_wide(obs=20); run;

proc export data=hrv_transformed_wide
  outfile='.../hrv_transformed_wide.xlsx'
  dbms=xlsx
  replace;
  sheet="HRV_transformed_wide";
run;


proc import datafile=".../hrv_transformed_wide.xlsx"
    out=hrv_wide
    dbms=xlsx
    replace;
    sheet="HRV_transformed_wide";
run;

proc contents data=hrv_wide; 
run;


/* Convert only T5 frequency-domain variables to numeric */
data hrv_wide;
    set hrv_wide;

    /* Convert RAW frequency-domain variables at T5 */
    HF_ms_T5_num    = input(HF_ms_T5, best32.);
    LF_ms_T5_num    = input(LF_ms_T5, best32.);
    LFHF_T5_num     = input(LFHF_T5, best32.);

    /* Convert LOG-transformed frequency-domain variables at T5 */
    lnHF_ms_T5_num  = input(lnHF_ms_T5, best32.);
    lnLF_ms_T5_num  = input(lnLF_ms_T5, best32.);
    lnLFHF_T5_num   = input(lnLFHF_T5, best32.);

    drop HF_ms_T5 LF_ms_T5 LFHF_T5
         lnHF_ms_T5 lnLF_ms_T5 lnLFHF_T5;

    rename
        HF_ms_T5_num   = HF_ms_T5
        LF_ms_T5_num   = LF_ms_T5
        LFHF_T5_num    = LFHF_T5
        lnHF_ms_T5_num = lnHF_ms_T5
        lnLF_ms_T5_num = lnLF_ms_T5
        lnLFHF_T5_num  = lnLFHF_T5;
run;

proc contents data=hrv_wide; 
run;


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

    /* SCL only available at T1–T2 */
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


proc export data=hrv
  outfile='.../hrv_RsaSCLtransformed_long.xlsx'
  dbms=xlsx
  replace;
  sheet="HRV_long";
run;


proc import datafile=".../hrv_RsaSCLtransformed_long.xlsx"
    out=hrv
    dbms=xlsx
    replace;
    sheet="HRV_long";
run;



/*QC before modeling*/

proc means data=hrv n nmiss min mean max;
  class Time;
  var lnRMSSD_msec lnSDNN_msec lnHF_ms lnLF_ms lnLFHF lnRSA0_msec lnAverage_SCL_uS PEP_msec;
  title "QC: Ranges by Time (long format)";
run;

/*Confirmation*/
/* Count nonmissing per Time for each variable to verify structural missingness */
proc means data=hrv n nmiss;
  class Time;
  var lnRMSSD_msec lnSDNN_msec lnHF_ms lnLF_ms lnLFHF lnRSA0_msec lnAverage_SCL_uS PEP_msec;
  title "Nonmissing (N) and Missing (NMISS) by Time";
run;

proc means data=hrv n nmiss min mean max;
  var lnRMSSD_msec lnSDNN_msec lnHF_ms lnLF_ms lnLFHF lnRSA0_msec lnAverage_SCL_uS PEP_msec;
  title "QC: Distribution";
run;




/*======================================================
            LMM Model fitting under MAR
========================================================*/

/*Model selection*/
proc glm data=hrv;
    class Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

    model lnRMSSD_msec =
        /* Main effects */
        Group
        REST_VS_STRESS
        Gender

        /* Two-way interactions */
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender

        / solution;

    output out=rmssd_olsresid
           r = r_lnRMSSD;
run;
quit;


proc sgplot data=rmssd_olsresid;
    scatter x=Time y=r_lnRMSSD / transparency=0.4;
    loess x=Time y=r_lnRMSSD;
    title "OLS Residuals vs Time for lnRMSSD";
run;

proc sgplot data=rmssd_olsresid;
    vbox r_lnRMSSD / category=REST_VS_STRESS;
    title "lnRMSSD Residual Distribution by Condition (Rest vs Stress)";
run;

proc sgplot data=rmssd_olsresid;
    vbox r_lnRMSSD / category=Group;
    title "lnRMSSD Residual Distribution by PMA Group";
run;

proc sgplot data=rmssd_olsresid;
    series x=Time y=r_lnRMSSD / group=Subject_ID transparency=0.8;
    title "lnRMSSD Residual Profiles by Subject";
run;

data resvar;
    set rmssd_olsresid;
    r2 = r_lnRMSSD * r_lnRMSSD;
run;

proc sgplot data=resvar;

    scatter x=Time y=r2 / transparency=0.5;
    loess x=Time y=r2;
    title "lnRMSSD Squared Residuals vs Time (Variance Diagnostics)";
run;


/*Start with random intercept and random slope for condition, and check Residuals Covariance*/


/*Explore residuals to see:
? Is correlation strong between nearby time points?
? Does correlation decay with time?
? Is correlation stronger under stress than rest?
? Which covariance structure is most appropriate?*/

/*Try in-place adding-removing condition slope*/

/*After seeing the plots, the candidates for the covariance structure are: AR, Simple, CS,let's try*/

/*1) AR(1)*/
proc mixed data=rmssd_olsresid;
    class Subject_ID Time;
    model r_lnRMSSD = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

proc mixed data=rmssd_olsresid;
    class Subject_ID Time;
    model r_lnRMSSD = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;


/*3) CS*/
proc mixed data=rmssd_olsresid;
    class Subject_ID Time;
    model r_lnRMSSD = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=CS;
    ods output FitStatistics=CS_AIC;
run;

proc mixed data=rmssd_olsresid;
    class Subject_ID Time;
    model r_lnRMSSD = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=CS;
    ods output FitStatistics=CS_AIC;
run;

/*4) Simple*/
proc mixed data=rmssd_olsresid;
    class Subject_ID Time;
    model r_lnRMSSD = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;

proc mixed data=rmssd_olsresid;
    class Subject_ID Time;
    model r_lnRMSSD = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;
/*AR(1)*/


/*Reduce the r.effect structure*/
proc mixed data=hrv nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnRMSSD_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

run;

proc mixed data=hrv nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnRMSSD_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

run;

/*The final variance component structure: Random intercept + AR(1)*/

/*"Random slope" for rest_vs_stress is near zero, both REML likelihood and AIC support the random intercept model*/


/*==============================
	Mean structure reduction
  ==============================*/

proc mixed data=hrv nobound method=ml;
    class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnRMSSD_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;




proc mixed data=hrv nobound method=ml;
    class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnRMSSD_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        /* REST_VS_STRESS*Gender removed */
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_noRG;
run;


/*Reduce more*/
proc mixed data=hrv nobound method=ml;
    class 
        Subject_ID Time
        Group          (ref='0')
        REST_VS_STRESS (ref='0')
        Gender         (ref='m');

    model lnRMSSD_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
/*        Group*Gender*/
        /* REST_VS_STRESS*Gender removed */
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_noRG;
run;

proc mixed data=hrv nobound method=reml;
        class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnRMSSD_msec =
        Group
        REST_VS_STRESS
        Group*REST_VS_STRESS
        Gender
        / solution cl ddfm=kr;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = RMSSD_SolutionF;
run;


proc mixed data=hrv nobound method=reml;
        class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnRMSSD_msec =
        Group
        REST_VS_STRESS
/*        Group*REST_VS_STRESS*/
        Gender
        / solution cl ddfm=kr;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = RMSSD_SolutionF;
run;



/*Model Assumptions*/

proc glimmix data=hrv nobound method=rspl;
  class Subject_ID Time Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

  model lnRMSSD_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        / solution ddfm=kr dist=normal link=identity;

  random intercept / subject=Subject_ID;

/*   AR(1) within subject over Time */
  random _residual_ / subject=Subject_ID type=ar(1);

  output out=RMSSD_Diag
         pred=Pred
         resid=Resid
         student=StuResid;
run;


proc sgplot data=RMSSD_Diag;
  scatter x=Pred y=Resid;
  refline 0 / axis=y;
run;

proc univariate data=RMSSD_Diag normal;
  var Resid;
  qqplot Resid / normal(mu=est sigma=est);
run;

proc sgplot data=RMSSD_Diag;
  histogram Resid;
  density Resid;
run;




/*######
   SDNN
  ######*/

/*Model selection*/
proc glm data=hrv;

    class Subject_ID 
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnSDNN_msec =
        /* Main effects */
        Group
        REST_VS_STRESS
        Gender

        /* Two-way interactions */
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender

        / solution;

    output out=sdnn_olsresid
           r = r_lnSDNN;
run;
quit;

/*Preliminary r.effects */
/*Explore Residuals to: 
? Does variance increase with condition or time?
? Does variance differ by group?
*/


proc sgplot data=sdnn_olsresid;
    scatter x=Time y=r_lnSDNN / transparency=0.4;
    loess x=Time y=r_lnSDNN;
    title "OLS Residuals vs Time for lnSDNN";
run;

proc sgplot data=sdnn_olsresid;
    vbox r_lnSDNN / category=REST_VS_STRESS;
    title "lnSDNN Residual Distribution by Condition (Rest vs Stress)";
run;

proc sgplot data=sdnn_olsresid;
    vbox r_lnSDNN / category=Group;
    title "lnSDNN Residual Distribution by PMA Group";
run;

proc sgplot data=sdnn_olsresid;
    series x=Time y=r_lnSDNN / group=Subject_ID transparency=0.8;
    title "lnSDNN Residual Profiles by Subject";
run;

data resvar;
    set sdnn_olsresid;
    r2 = r_lnSDNN * r_lnSDNN;
run;

proc sgplot data=resvar;
    scatter x=Time y=r2 / transparency=0.5;
    loess x=Time y=r2;
    title "lnSDNN Squared Residuals vs Time (Variance Diagnostics)";
run;

/*Check Residuals Covariance*/
/*After seeing the plots, the candidates for the covariance structure are: AR, Simple, CS, let's try*/

/*1) AR(1)*/
proc mixed data=sdnn_olsresid;
    class Subject_ID Time;
    model r_lnSDNN = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;
proc mixed data=sdnn_olsresid;
    class Subject_ID Time;
    model r_lnSDNN = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;


/*2) CS*/
proc mixed data=sdnn_olsresid;
    class Subject_ID Time;
    model r_lnSDNN = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=CS;
    ods output FitStatistics=CS_AIC;
run;
proc mixed data=sdnn_olsresid;
    class Subject_ID Time;
    model r_lnSDNN = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=CS;
    ods output FitStatistics=CS_AIC;
run;


/*3) Simple*/
proc mixed data=sdnn_olsresid;
    class Subject_ID Time;
    model r_lnSDNN = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=simple_AIC;
run;
proc mixed data=sdnn_olsresid;
    class Subject_ID Time;
    model r_lnSDNN = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=simple_AIC;
run;

/*Reduce r.effects structure*/
proc mixed data=hrv nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnSDNN_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

run;

proc mixed data=hrv nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnSDNN_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

run;


/*Random intercept with AR(1)*/

/*==============================
	Mean structure reduction
  ==============================*/

proc mixed data=hrv nobound method=ml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnSDNN_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;


proc mixed data=hrv nobound method=ml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnSDNN_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
/*        REST_VS_STRESS*Gender*/
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;



proc mixed data=hrv nobound method=ml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnSDNN_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
/*        Group*Gender*/
/*        REST_VS_STRESS*Gender*/
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;

/*Do not forget to compare the last model with the first one*/

/*Fit the final model*/
proc mixed data=hrv nobound method=reml;
    class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnSDNN_msec =
        Group
        REST_VS_STRESS
        Group*REST_VS_STRESS
        Gender
        / solution cl ddfm=kr;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = SDNN_SolutionF;
run;



/*Final model -> Main effects model*/
proc mixed data=hrv nobound method=reml;
    class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnSDNN_msec =
        Group
        REST_VS_STRESS
        Gender
        / solution cl ddfm=kr;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = SDNN_SolutionF;
run;



/*Model Assumptions*/

proc glimmix data=hrv nobound method=rspl;
  class Subject_ID Time Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

  model lnSDNN_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        / solution ddfm=kr dist=normal link=identity;

  random intercept / subject=Subject_ID;

/*   AR(1) within subject over Time */
  random _residual_ / subject=Subject_ID type=ar(1);

  output out=SDNN_Diag
         pred=Pred
         resid=Resid
         student=StuResid;
run;


proc sgplot data=SDNN_Diag;
  scatter x=Pred y=Resid;
  refline 0 / axis=y;
run;

proc univariate data=SDNN_Diag normal;
  var Resid;
  qqplot Resid / normal(mu=est sigma=est);
run;

proc sgplot data=SDNN_Diag;
  histogram Resid;
  density Resid;
run;




/*#########
    lnRSA 
  #########*/

proc glm data=hrv;
    class Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

    model lnRSA0_msec =
        /* Main effects */
        Group
        REST_VS_STRESS
        Gender

        /* Two-way interactions */
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender

        / solution;

    output out=rsa_olsresid
           r = r_lnRSA;
run;
quit;

/*Preliminary r.effects */
/*Explore Residuals to: 
? Does variance increase with condition or time?
? Does variance differ by group?
*/


proc sgplot data=rsa_olsresid;
    scatter x=Time y=r_lnRSA / transparency=0.4;
    loess x=Time y=r_lnRSA;
    title "OLS Residuals vs Time for lnRSA";
run;

proc sgplot data=rsa_olsresid;
    vbox r_lnRSA / category=REST_VS_STRESS;
    title "lnRSA Residual Distribution by Condition (Rest vs Stress)";
run;

proc sgplot data=rsa_olsresid;
    vbox r_lnRSA / category=Group;
    title "lnRSA Residual Distribution by PMA Group";
run;

proc sgplot data=rsa_olsresid;
    series x=Time y=r_lnRSA / group=Subject_ID transparency=0.8;
    title "lnRSA Residual Profiles by Subject";
run;



/*Check Residuals Covariance*/



/*After seeing the plots, the candidates for the covariance structure are: AR, Simple, CS, let's try*/

/*1) AR(1)*/

proc mixed data=rsa_olsresid;
    class Subject_ID Time;
    model r_lnRSA = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

proc mixed data=rsa_olsresid;
    class Subject_ID Time;
    model r_lnRSA = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

/*2) CS*/
proc mixed data=rsa_olsresid;
    class Subject_ID Time;
    model r_lnRSA = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=CS;
    ods output FitStatistics=CS_AIC;
run;

proc mixed data=rsa_olsresid;
    class Subject_ID Time;
    model r_lnRSA = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=CS;
    ods output FitStatistics=CS_AIC;
run;



/*Simple*/
proc mixed data=rsa_olsresid;
    class Subject_ID Time;
    model r_lnRSA = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;
proc mixed data=rsa_olsresid;
    class Subject_ID Time;
    model r_lnRSA = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;

/*AR(1)*/

/*Reduce the r.effects structure*/
proc mixed data=hrv nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnRSA0_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

run;

proc mixed data=hrv nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnRSA0_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

run;

/*Random intercept with AR(1)*/


/*==============================
	Mean structure reduction
  ==============================*/

proc mixed data=hrv method=ml;
    class Subject_ID Time
		  Group(ref='0')
		  REST_VS_STRESS(ref='0') 
		  Gender(ref='m');

    model lnRSA0_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;




proc mixed data=hrv method=ml;
    class Subject_ID Time
		  Group(ref='0')
		  REST_VS_STRESS(ref='0') 
		  Gender(ref='m');

    model lnRSA0_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
/*         REST_VS_STRESS*Gender*/
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_noRG;
run;


/*Reduce more*/
proc mixed data=hrv method=ml;
    class Subject_ID Time
		  Group(ref='0')
		  REST_VS_STRESS(ref='0') 
		  Gender(ref='m');

    model lnRSA0_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
/*        Group*Gender*/
        /* REST_VS_STRESS*Gender */
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_noRG;
run;



/*Fit the Final model (with groupxcondition)*/
proc mixed data=hrv nobound method=reml;
    class Subject_ID Time
		  Group(ref='0')
		  REST_VS_STRESS(ref='0') 
		  Gender(ref='m');

    model lnRSA0_msec =
        Group
        REST_VS_STRESS
        Group*REST_VS_STRESS
        Gender
        / solution cl ddfm=kr;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = lnRSA_SolutionF;
run;



proc mixed data=hrv nobound method=reml;
    class Subject_ID Time
		  Group(ref='0')
		  REST_VS_STRESS(ref='0') 
		  Gender(ref='m');

    model lnRSA0_msec =
        Group
        REST_VS_STRESS
/*        Group*REST_VS_STRESS*/
        Gender
        / solution cl ddfm=kr;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = lnRSA_SolutionF;
run;





/*Model assumptions*/

proc glimmix data=hrv nobound method=rspl;
  class Subject_ID Time Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

  model lnRSA0_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        / solution ddfm=kr dist=normal link=identity;

  random intercept / subject=Subject_ID;

/*   AR(1) within subject over Time */
  random _residual_ / subject=Subject_ID type=ar(1);

  output out=RSA_Diag
         pred=Pred
         resid=Resid
         student=StuResid;
run;


proc sgplot data=RSA_Diag;
  scatter x=Pred y=Resid;
  refline 0 / axis=y;
run;

proc univariate data=RSA_Diag normal;
  var Resid;
  qqplot Resid / normal(mu=est sigma=est);
run;

proc sgplot data=RSA_Diag;
  histogram Resid;
  density Resid;
run;










/*#############
	   PEP
  #############*/

proc glm data=hrv;
    class Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

    model PEP_msec =
        /* Main effects */
        Group
        REST_VS_STRESS
        Gender

        /* Two-way interactions */
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender

        / solution;

    output out=pep_olsresid
           r = r_pep;
run;
quit;

/*11.4 preliminary r.effects */
/*Explore Residuals to: 
? Does variance increase with condition or time?
? Does variance differ by group?
*/
proc print data=pep_olsresid(obs=10);run;

proc sgplot data=pep_olsresid;
    scatter x=Time y=r_pep / transparency=0.4;
    loess x=Time y=r_pep;
    title "OLS Residuals vs Time for PEP";
run;

proc sgplot data=pep_olsresid;
    vbox r_pep / category=REST_VS_STRESS;
    title "PEP Residual Distribution by Condition (Rest vs Stress)";
run;

proc sgplot data=pep_olsresid;
    vbox r_pep / category=Group;
    title "PEP Residual Distribution by PMA Group";
run;

proc sgplot data=pep_olsresid;
    series x=Time y=r_pep / group=Subject_ID transparency=0.8;
    title "PEP Residual Profiles by Subject";
run;

data resvar;
    set pep_olsresid;
    r2 = r_pep * r_pep;
run;

proc sgplot data=resvar;
    scatter x=Time y=r2 / transparency=0.5;
    loess x=Time y=r2;
    title "PEP Squared Residuals vs Time (Variance Diagnostics)";
run;

/*Check Residuals Covariance*/
/*After seeing the plots, the candidates for the covariance structure are: AR, Simple, CS, let's try*/

/*1) AR(1)*/
proc mixed data=pep_olsresid;
    class Subject_ID Time;
    model r_pep = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

proc mixed data=pep_olsresid;
    class Subject_ID Time;
    model r_pep = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

/*2) CS*/
proc mixed data=pep_olsresid;
    class Subject_ID Time;
    model r_pep = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=cs;
    ods output FitStatistics=CS_AIC;
run;
proc mixed data=pep_olsresid;
    class Subject_ID Time;
    model r_pep = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=cs;
    ods output FitStatistics=CS_AIC;
run;

/*3) Simple*/
proc mixed data=pep_olsresid;
    class Subject_ID Time;
    model r_pep = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;

proc mixed data=pep_olsresid;
    class Subject_ID Time;
    model r_pep = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;

/*Simple*/

/*Reduce the r.effects structure*/
proc mixed data=hrv nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model PEP_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

run;

proc mixed data=hrv nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model PEP_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

run;

/*Random intercept with simple*/


/*==============================
	Mean structure reduction
  ==============================*/

proc mixed data=hrv method=ml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model PEP_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;


/*remove Condition x Gender*/
proc mixed data=hrv method=ml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model PEP_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
/*        REST_VS_STRESS*Gender*/
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;



/*Remove Gender x Group*/
proc mixed data=hrv method=ml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model PEP_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
/*        Group*Gender*/
/*        REST_VS_STRESS*Gender*/
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;


/*Fit the final model under REML: 
PEP_msec ~ Group * REST_VS_STRESS + Gender ....  Random intercept + Simple*/

proc mixed data=hrv nobound method=reml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model PEP_msec =
        Group
        REST_VS_STRESS
        Group*REST_VS_STRESS
        Gender
        / solution cl ddfm=kr;
    random intercept  / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output SolutionF = PEP_SolutionF;
run;


/*Main effects model*/

proc mixed data=hrv nobound method=reml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model PEP_msec =
        Group
        REST_VS_STRESS
/*        Group*REST_VS_STRESS*/
        Gender
        / solution cl ddfm=kr;
    random intercept  / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output SolutionF = PEP_SolutionF;
run;


/*Model assumptions*/

proc glimmix data=hrv nobound method=rspl;
  class Subject_ID Time Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

  model PEP_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        / solution ddfm=kr dist=normal link=identity;

  random intercept / subject=Subject_ID;

   /*AR(1) within subject over Time*/ 
  random _residual_ / subject=Subject_ID type=simple;

  output out=PEP_Diag
         pred=Pred
         resid=Resid
         student=StuResid;
run;


proc sgplot data=PEP_Diag;
  scatter x=Pred y=Resid;
  refline 0 / axis=y;
run;

proc univariate data=PEP_Diag normal;
  var Resid;
  qqplot Resid / normal(mu=est sigma=est);
run;

proc sgplot data=PEP_Diag;
  histogram Resid;
  density Resid;
run;




/*##############
	   lnHF
  ##############*/

proc glm data=hrv;
    class Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

    model lnHF_ms =
        /* Main effects */
        Group
        REST_VS_STRESS
        Gender

        /* Two-way interactions */
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender

        / solution;

    output out=lnHF_olsresid
           r = r_lnHF;
run;
quit;

/*11.4 preliminary r.effects */
/*Explore Residuals to: 
? Does variance increase with condition or time?
? Does variance differ by group?
*/
proc print data=lnHF_olsresid(obs=10);run;

proc sgplot data=lnHF_olsresid;
    scatter x=Time y=r_lnHF / transparency=0.4;
    loess x=Time y=r_lnHF;
    title "OLS Residuals vs Time for lnHF";
run;

proc sgplot data=lnHF_olsresid;
	    vbox r_lnHF / category=REST_VS_STRESS;
    title "lnHF Residual Distribution by Condition (Rest vs Stress)";
run;

proc sgplot data=lnHF_olsresid;
    vbox r_lnHF / category=Group;
    title "lnHF Residual Distribution by PMA Group";
run;

proc sgplot data=lnHF_olsresid;
	series x=Time y=r_lnHF / group=Subject_ID transparency=0.8;
    title "lnHF Residual Profiles by Subject";
run;

data resvar;
    set lnHF_olsresid;
    r2 = r_lnHF * r_lnHF;
run;

proc sgplot data=resvar;

    scatter x=Time y=r2 / transparency=0.5;
    loess x=Time y=r2;
    title "lnHF Squared Residuals vs Time (Variance Diagnostics)";
run;

/*Check Residuals Covariance*/
/*After seeing the plots, the candidates for the covariance structure are: AR, Simple, CS, let's try*/

/*1) AR(1)*/
proc mixed data=lnHF_olsresid;
    class Subject_ID Time;
    model r_lnHF = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

proc mixed data=lnHF_olsresid;
    class Subject_ID Time;
    model r_lnHF = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

/*2) CS*/
proc mixed data=lnHF_olsresid;
    class Subject_ID Time;
    model r_lnHF = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=cs;
    ods output FitStatistics=CS_AIC;
run;
proc mixed data=lnHF_olsresid;
    class Subject_ID Time;
    model r_lnHF = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=cs;
    ods output FitStatistics=CS_AIC;
run;

/*3) Simple*/
proc mixed data=lnHF_olsresid;
    class Subject_ID Time;
    model r_lnHF = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;

proc mixed data=lnHF_olsresid;
    class Subject_ID Time;
    model r_lnHF = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;

/*Simple*/

/*Reduce the r.effects structure*/
proc mixed data=hrv nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnHF_ms =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

run;

proc mixed data=hrv nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnHF_ms =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

run;

/*Random intercept with Simple*/

/*==============================
	Mean structure reduction
  ==============================*/

proc mixed data=hrv nobound method=ml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnHF_ms =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution ddfm=kr;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;



/*drop Group x Gender*/

proc mixed data=hrv nobound method=ml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnHF_ms =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
/*        Group*Gender*/
        REST_VS_STRESS*Gender
        / solution ddfm=kr;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;


/*Drop Gender x Condition*/
proc mixed data=hrv nobound method=ml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnHF_ms =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
/*        Group*Gender*/
/*        REST_VS_STRESS*Gender*/
        / solution ddfm=kr;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;



/*Final model: Group*REXT_VS_STRESS + Gender*/

proc mixed data=hrv nobound method=reml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnHF_ms =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        / solution cl ddfm=kr;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;


/*Final2: Main effects model*/
proc mixed data=hrv nobound method=reml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnHF_ms =
        Group
        REST_VS_STRESS
        Gender
/*        Group*REST_VS_STRESS*/
        / solution cl ddfm=kr;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;


/*Model assumptions*/
proc glimmix data=hrv nobound method=rspl;
  class Subject_ID Time Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

  model lnHF_ms =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        / solution ddfm=kr dist=normal link=identity;

  random intercept / subject=Subject_ID;


  random _residual_ / subject=Subject_ID type=simple;

  output out=HF_Diag
         pred=Pred
         resid=Resid
         student=StuResid;
run;


proc sgplot data=HF_Diag;
  scatter x=Pred y=Resid;
  refline 0 / axis=y;
run;

proc univariate data=HF_Diag normal;
  var Resid;
  qqplot Resid / normal(mu=est sigma=est);
run;

proc sgplot data=HF_Diag;
  histogram Resid;
  density Resid;
run;




/*##################
		lnLF
  ##################*/

proc glm data=hrv;

    class Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

    model lnLF_ms =
        /* Main effects */
        Group
        REST_VS_STRESS
        Gender

        /* Two-way interactions */
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender

        / solution;

    output out=lnLF_olsresid
           r = r_lnLF;
run;
quit;

/*Preliminary r.effects */
/*Explore Residuals to: 
? Does variance increase with condition or time?
? Does variance differ by group?
*/
proc print data=lnLF_olsresid(obs=10);run;

proc sgplot data=lnLF_olsresid;
    scatter x=Time y=r_lnLF / transparency=0.4;
    loess x=Time y=r_lnLF;
    title "OLS Residuals vs Time for lnLF";
run;

proc sgplot data=lnLF_olsresid;
    vbox r_lnLF / category=REST_VS_STRESS;
    title "lnLF Residual Distribution by Condition (Rest vs Stress)";
run;

proc sgplot data=lnLF_olsresid;
    vbox r_lnLF / category=Group;
    title "lnLF Residual Distribution by PMA Group";
run;

proc sgplot data=lnLF_olsresid;
    series x=Time y=r_lnLF / group=Subject_ID transparency=0.8;
    title "lnLF Residual Profiles by Subject";
run;

data resvar;
    set lnLF_olsresid;
    r2 = r_lnLF * r_lnLF;
run;

proc sgplot data=resvar;
    scatter x=Time y=r2 / transparency=0.5;
    loess x=Time y=r2;
    title "lnLF Squared Residuals vs Time (Variance Diagnostics)";
run;

/*Check Residuals Covariance*/
/*After seeing the plots, the candidates for the covariance structure are: AR, Simple, CS, let's try*/

/*1) AR(1)*/
proc mixed data=lnLF_olsresid;
    class Subject_ID Time;
    model r_lnLF = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

proc mixed data=lnLF_olsresid;
    class Subject_ID Time;
    model r_lnLF = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;


/*2) CS*/
proc mixed data=lnLF_olsresid;
    class Subject_ID Time;
    model r_lnLF = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=cs;
    ods output FitStatistics=CS_AIC;
run;

proc mixed data=lnLF_olsresid;
    class Subject_ID Time;
    model r_lnLF = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=cs;
    ods output FitStatistics=CS_AIC;
run;


/*3) Simple*/
proc mixed data=lnLF_olsresid;
    class Subject_ID Time;
    model r_lnLF = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;


proc mixed data=lnLF_olsresid;
    class Subject_ID Time;
    model r_lnLF = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;

/*AR(1) wins*/


/*Reduce r.eff structure*/

proc mixed data=hrv nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnLF_ms =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

run;

proc mixed data=hrv nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnLF_ms =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

run;

/*Random intercept with AR(1)*/

/*==============================
	Mean structure reduction
  ==============================*/

proc mixed data=hrv nobound method=ml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnLF_ms =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;


/*Drop Gender*REST_VS_STRESS*/

proc mixed data=hrv nobound method=ml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnLF_ms =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
/*        REST_VS_STRESS*Gender*/
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;


/*Drop Group*Gender*/
proc mixed data=hrv nobound method=ml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnLF_ms =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
/*        Group*Gender*/
/*        REST_VS_STRESS*Gender*/
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;


/*Final model: Group*REST_VS_STRESS + Gender*/
proc mixed data=hrv nobound method=reml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnLF_ms =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        / solution cl ddfm=kr;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;



/*Final Model: Main Results*/
proc mixed data=hrv nobound method=reml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnLF_ms =
        Group
        REST_VS_STRESS
        Gender
        / solution cl ddfm=kr;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;


/*Model assumptions*/
proc glimmix data=hrv nobound method=rspl;
  class Subject_ID Time Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

  model lnLF_ms =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        / solution ddfm=kr dist=normal link=identity;

  random intercept / subject=Subject_ID;


  random _residual_ / subject=Subject_ID type=ar(1);

  output out=LF_Diag
         pred=Pred
         resid=Resid
         student=StuResid;
run;


proc sgplot data=LF_Diag;
  scatter x=Pred y=Resid;
  refline 0 / axis=y;
run;

proc univariate data=LF_Diag normal;
  var Resid;
  qqplot Resid / normal(mu=est sigma=est);
run;

proc sgplot data=LF_Diag;
  histogram Resid;
  density Resid;
run;



/*###################
		lnLFHF
  ###################*/

proc glm data=hrv;

    class Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

    model lnLFHF =
        /* Main effects */
        Group
        REST_VS_STRESS
        Gender

        /* Two-way interactions */
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender

        / solution;

    output out=lnLFHF_olsresid
           r = r_lnLFHF;
run;
quit;

/*11.4 preliminary r.effects */
/*Explore Residuals to: 
? Does variance increase with condition or time?
? Does variance differ by group?
*/
proc print data=lnLFHF_olsresid(obs=10);run;

proc sgplot data=lnLFHF_olsresid;
    scatter x=Time y=r_lnLFHF / transparency=0.4;
    loess x=Time y=r_lnLFHF;
    title "OLS Residuals vs Time for lnLFHF";
run;

proc sgplot data=lnLFHF_olsresid;
    vbox r_lnLFHF / category=REST_VS_STRESS;
    title "lnLFHF Residual Distribution by Condition (Rest vs Stress)";
run;

proc sgplot data=lnLFHF_olsresid;
    vbox r_lnLFHF / category=Group;
    title "lnLFHF Residual Distribution by PMA Group";
run;

proc sgplot data=lnLFHF_olsresid;
    series x=Time y=r_lnLFHF / group=Subject_ID transparency=0.8;
    title "lnLFHF Residual Profiles by Subject";
run;

data resvar;
    set lnLFHF_olsresid;
    r2 = r_lnLFHF * r_lnLFHF;
run;

proc sgplot data=resvar;
    scatter x=Time y=r2 / transparency=0.5;
    loess x=Time y=r2;
    title "lnLFHF Squared Residuals vs Time (Variance Diagnostics)";
run;

/*Check Residuals Covariance*/
/*After seeing the plots, the candidates for the covariance structure are: AR, Simple, CS, let's try*/


/*1) AR(1)*/
proc mixed data=lnLFHF_olsresid;
    class Subject_ID Time;
    model r_lnLFHF = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

proc mixed data=lnLFHF_olsresid;
    class Subject_ID Time;
    model r_lnLFHF = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

/*2) CS*/
proc mixed data=lnLFHF_olsresid;
    class Subject_ID Time;
    model r_lnLFHF = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=cs;
    ods output FitStatistics=CS_AIC;
run;

proc mixed data=lnLFHF_olsresid;
    class Subject_ID Time;
    model r_lnLFHF = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=cs;
    ods output FitStatistics=CS_AIC;
run;

/*3) Simple*/
proc mixed data=lnLFHF_olsresid;
    class Subject_ID Time;
    model r_lnLFHF = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;

proc mixed data=lnLFHF_olsresid;
    class Subject_ID Time;
    model r_lnLFHF = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;

/*Simple wins*/

/*Reduce r.eff structure*/
proc mixed data=hrv nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnLFHF =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

run;

proc mixed data=hrv nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnLFHF =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

run;
/*Random intercept with simple*/




/*==============================
	Mean structure reduction
  ==============================*/

proc mixed data=hrv nobound method=ml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnLFHF =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        REST_VS_STRESS*Gender
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;

/*Drop Group*Gender 307.9*/


proc mixed data=hrv nobound method=ml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnLFHF =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
/*        Group*Gender*/
        REST_VS_STRESS*Gender
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;



/*Try: drop Gender x condition*/
proc mixed data=hrv nobound method=ml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnLFHF =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
/*        Group*Gender*/
/*        REST_VS_STRESS*Gender*/
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;


/*Removing it makes it worse, keep it and fit the final model*/

proc mixed data=hrv nobound method=reml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnLFHF =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        REST_VS_STRESS*Gender
        / solution cl ddfm=kr;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;


/*Remove Group*REST_VS_STRESS*/

proc mixed data=hrv nobound method=reml;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnLFHF =
        Group
        REST_VS_STRESS
        Gender
/*        Group*REST_VS_STRESS*/
        REST_VS_STRESS*Gender
        / solution cl ddfm=kr;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;


/*Model assumptions*/

proc glimmix data=hrv nobound method=rspl;
  class Subject_ID Time Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

  model lnLFHF =
        Group
        REST_VS_STRESS
        Gender
/*        Group*REST_VS_STRESS*/
		Gender*REST_VS_STRESS

        / solution ddfm=kr dist=normal link=identity;

  random intercept / subject=Subject_ID;

   /*AR(1) within subject over Time*/ 
  random _residual_ / subject=Subject_ID type=simple;

  output out=lnLFHF_Diag
         pred=Pred
         resid=Resid
         student=StuResid;
run;


proc sgplot data=lnLFHF_Diag;
  scatter x=Pred y=Resid;
  refline 0 / axis=y;
run;

proc univariate data=lnLFHF_Diag normal;
  var Resid;
  qqplot Resid / normal(mu=est sigma=est);
run;

proc sgplot data=lnLFHF_Diag;
  histogram Resid;
  density Resid;
run;




/*###########
	 lnSCL
  ###########*/

/*The questions that can be answered for lnSCL:
1- Is there group effect? */


proc glm data=hrv;
    class Group(ref='0') Gender(ref='m');

    model lnAverage_SCL_uS =
        /* Main effects */
        Group
        Gender

        /* Two-way interactions */
        Group*Gender

        / solution;

    output out=lnSCL_olsresid
           r = r_lnSCL;
run;
quit;

/*11.4 preliminary r.effects */
/*Explore Residuals to: 
? Does variance differ by group?
*/
proc print data=lnSCL_olsresid(obs=10);run;

proc sgplot data=lnSCL_olsresid;
    scatter x=Time y=r_lnSCL / transparency=0.4;
    loess x=Time y=r_lnSCL;
	xaxis min=1 max=2;
    title "OLS Residuals vs Time for lnSCL";
run;

proc sgplot data=lnSCL_olsresid;
    vbox r_lnSCL / category=Group;
    title "lnSCL Residual Distribution by PMA Group";
run;

proc sgplot data=lnSCL_olsresid;
    series x=Time y=r_lnSCL / group=Subject_ID transparency=0.8;
	xaxis min=1 max=2;
    title "lnSCL Residual Profiles by Subject";
run;

data resvar;
    set lnSCL_olsresid;
    r2 = r_lnSCL * r_lnSCL;
run;

proc sgplot data=resvar;
    scatter x=Time y=r2 / transparency=0.5;
    loess x=Time y=r2;
	xaxis min=1 max=2;
    title "lnSCL Squared Residuals vs Time (Variance Diagnostics)";
run;

/*11.5 Check Residuals Covariance*/


/*Simple*/
proc mixed data=lnSCL_olsresid;
    class Subject_ID Time;
    model r_lnSCL = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;


/*Choose mean structure*/

proc mixed data=hrv nobound method=ML;
    class Subject_ID Group(ref='0') Gender(ref='m');
    model lnAverage_SCL_uS = Group*Gender / solution;
    random intercept / subject=Subject_ID;
run;

proc mixed data=hrv nobound method=ML;
    class Subject_ID Group(ref='0') Gender(ref='m');
    model lnAverage_SCL_uS = Group Gender / solution;
    random intercept / subject=Subject_ID;
run;



/*Final model: lnSCL ~ Group + Gender*/
proc mixed data=hrv nobound method=REML;
    class Subject_ID
          Group (ref='0')
          Gender (ref='m');

    model lnAverage_SCL_uS = Group Gender / solution cl ddfm=kr;

    random intercept / subject=Subject_ID;

    ods output SolutionF = lnSCL_SolutionF;
run;




/*Model assumptions*/
proc glimmix data=hrv nobound method=rspl;
  class Subject_ID Time Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

  model lnAverage_SCL_uS =
        Group
        Gender

        / solution ddfm=kr dist=normal link=identity;

  random intercept / subject=Subject_ID;

   /*AR(1) within subject over Time*/ 
/*  random _residual_ / subject=Subject_ID type=ar(1);*/

  output out=lnSCL_Diag
         pred=Pred
         resid=Resid
         student=StuResid;
run;


proc sgplot data=lnSCL_Diag;
  scatter x=Pred y=Resid;
  refline 0 / axis=y;
run;

proc univariate data=lnSCL_Diag normal;
  var Resid;
  qqplot Resid / normal(mu=est sigma=est);
run;

proc sgplot data=lnSCL_Diag;
  histogram Resid;
  density Resid;
run;




