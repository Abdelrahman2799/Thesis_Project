/* ============================================================
   HRV and SCL Analysis
   Multiple Imputation and MNAR Sensitivity Analyses

   This script performs:
   - Exploration of missingness patterns
   - Multiple imputation under MAR
   - Model fitting across imputed datasets
   - MNAR sensitivity analyses

   Paths are omitted for portability.
   ============================================================ */





/*==============================================================
			1 — Data Import and preprocessing
==============================================================*/

proc import datafile="..../HRV_Data__Wide_Format_.csv"
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
var  
run;


/* Replace lnRSA and lnSCL with correct transformations and keep only relevant physiological variables */

data hrv_pre_impute;
    set hrv_clean_base;

    /* Replace RSA and SCL transformations */
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

    /* PEP: keep raw (no transform) */
    /* Drop HF/LF/LFHF at timepoints 2 and 5: For MI analyses, these timepoints were dropped since they could add noise to the imputation model*/
    drop HF_ms_T2 LF_ms_T2 LFHF_T2 HF_ms_T5 LF_ms_T5 LFHF_T5;

    drop i;
run;

/* Quick QC check before imputation */
proc means data=hrv_pre_impute n min mean max;
    var RMSSD_msec_T1 RMSSD_msec_T6 SDNN_msec_T1 SDNN_msec_T6 
        HF_ms_T1 HF_ms_T6 LF_ms_T1 LF_ms_T6 LFHF_T1 LFHF_T6
        RSA0_msec_T1 RSA0_msec_T6 Average_SCL_uS_T1 Average_SCL_uS_T2 PEP_msec_T1 PEP_msec_T6;
    title "QC: Check variable ranges before imputation";
run;

proc means data=hrv_pre_impute n nmiss min mean std max;  /*check missingness at each time point*/
    var lnRSA0_msec_T1-lnRSA0_msec_T6 lnAverage_SCL_uS_T1-lnAverage_SCL_uS_T2
		lnSDNN_msec_T1-lnSDNN_msec_T6 lnRMSSD_msec_T1-lnRMSSD_msec_T6
		lnHF_ms_T1 lnHF_ms_T3 lnHF_ms_T4 lnHF_ms_T6 lnLF_ms_T1 lnLF_ms_T3 lnLF_ms_T4 lnLF_ms_T6 
		lnLFHF_T1 lnLFHF_T3 lnLFHF_T4 lnLFHF_T6
		PEP_msec_T1-PEP_msec_T6;
    title "QC: Check corrected log transformations (RSA, SCL) + other log transformations + PEP";
run;
proc sort data=hrv_pre_impute; by Subject_ID; run;
proc print data=hrv_pre_impute(obs=20); run;

proc export data=hrv_pre_impute
  outfile='.../HRV_pre_impute_wide.xlsx'
  dbms=xlsx
  replace;
  sheet="HRV_pre_impute_wide";
run;



/*==============================================================
	2 — Multiple Imputation under MAR (Impute on Log Scale)
==============================================================*/
/* =========================
   FCS REGPMM on ln-scale
   (incl. PEP, lnSDNN5, lnRSA5, lnRMSSD5 with tailored block)
   ========================= */
proc sort data=hrv_pre_impute; by Subject_ID; run;

ods excel file=".../MI_Results_FCS_LN.xlsx"
          options(sheet_name="MI_FCS" embedded_titles='yes');

proc mi data=hrv_pre_impute
        out=hrv_imputed_ln
        seed=12345
        nimpute=10;    
  class Gender Group;

  /* Global PMM */
  fcs regpmm(/ k=5);

  /* --- Tailored fixes --- */
  /* lnRMSSD/SDNN/RSA T5: shrink predictors to high-coverage only */
   fcs regpmm( lnRMSSD_msec_T5 = Gender Group Anxiety_mother lnRMSSD_msec_T4 lnRMSSD_msec_T6);
   fcs regpmm( lnSDNN_msec_T5 = Gender Group Anxiety_mother lnSDNN_msec_T4 lnRMSSD_msec_T6);
   fcs regpmm( lnRSA0_msec_T5 = Gender Group Anxiety_mother lnRSA0_msec_T4 lnRSA0_msec_T6);


  /*PEP*/
  fcs regpmm( PEP_msec_T1 = Gender Group Anxiety_mother lnRMSSD_msec_T1 lnSDNN_msec_T1 );
  fcs regpmm( PEP_msec_T2 = Gender Group Anxiety_mother lnRMSSD_msec_T2 lnSDNN_msec_T2 PEP_msec_T1 PEP_msec_T2);
  fcs regpmm( PEP_msec_T3 = Gender Group Anxiety_mother lnRMSSD_msec_T3 lnSDNN_msec_T3 PEP_msec_T2 );
  fcs regpmm( PEP_msec_T4 = Gender Group Anxiety_mother lnRMSSD_msec_T4 lnSDNN_msec_T4 PEP_msec_T3 );
  fcs regpmm( PEP_msec_T5 = Gender Group Anxiety_mother lnRMSSD_msec_T5 lnSDNN_msec_T5 PEP_msec_T4 );
  fcs regpmm( PEP_msec_T6 = Gender Group Anxiety_mother lnRMSSD_msec_T6 lnSDNN_msec_T6 PEP_msec_T5 );

  /* Variables in imputation */
  var 
    Gender Group
    Anxiety_mother

    lnRMSSD_msec_T1-lnRMSSD_msec_T6
    lnSDNN_msec_T1-lnSDNN_msec_T6

    lnHF_ms_T1 lnHF_ms_T3 lnHF_ms_T4 lnHF_ms_T6
    lnLF_ms_T1 lnLF_ms_T3 lnLF_ms_T4 lnLF_ms_T6
    /* lnLFHF_* excluded; derive later */

    lnRSA0_msec_T1-lnRSA0_msec_T6
    lnAverage_SCL_uS_T1-lnAverage_SCL_uS_T2

    PEP_msec_T1-PEP_msec_T6
  ;
run;

/* Recreate lnLFHF after MI: ln(LF/HF)=lnLF-lnHF */
data hrv_imputed_ln;
  set hrv_imputed_ln;
  array lnLF[4]  lnLF_ms_T1 lnLF_ms_T3 lnLF_ms_T4 lnLF_ms_T6;
  array lnHF[4]  lnHF_ms_T1 lnHF_ms_T3 lnHF_ms_T4 lnHF_ms_T6;
  array lnLFHF[4] lnLFHF_T1 lnLFHF_T3 lnLFHF_T4 lnLFHF_T6;
  do i=1 to 4;
    if nmiss(lnLF[i], lnHF[i])=0 then lnLFHF[i] = lnLF[i] - lnHF[i];
    else lnLFHF[i] = .;
  end; drop i;
run;

title "QC: Post-imputation ranges (per imputation)";
ods excel close;
title;


/*Post-reshaping diagnostics and plausibility checks*/
/*QC post imputation ranges per imputation*/
proc means data=hrv_imputed_ln n min mean max;
  class _Imputation_;
  var lnRMSSD_msec_T1 lnRMSSD_msec_T2 lnRMSSD_msec_T3 lnRMSSD_msec_T4 lnRMSSD_msec_T5 lnRMSSD_msec_T6
	  lnSDNN_msec_T1 lnSDNN_msec_T2 lnSDNN_msec_T3 lnSDNN_msec_T4 lnSDNN_msec_T5 lnSDNN_msec_T6 
      lnHF_ms_T1 lnHF_ms_T3 lnHF_ms_T4 lnHF_ms_T6 
lnLF_ms_T1 lnLF_ms_T3 lnLF_ms_T4 lnLF_ms_T6 
lnLFHF_T1 lnLFHF_T3 lnLFHF_T4 lnLFHF_T6 

lnRSA0_msec_T1 lnRSA0_msec_T2 lnRSA0_msec_T3 lnRSA0_msec_T4 lnRSA0_msec_T5 lnRSA0_msec_T6 
lnAverage_SCL_uS_T1 lnAverage_SCL_uS_T2 
PEP_msec_T1 PEP_msec_T2 PEP_msec_T3 PEP_msec_T4 PEP_msec_T5 PEP_msec_T6;
run;


proc contents data=hrv_imputed_ln; run;


proc export data=hrv_imputed_ln
  outfile='.../hrv_imputed_Fcs.xlsx'
  dbms=xlsx
  replace;
  sheet="FCS_imputed";
run;


proc import datafile='.../hrv_imputed_Fcs.xlsx'
dbms=xlsx                                   
out=work.imputed_data                     
replace;                                    
run;

/*Import original data and check distributions to verify that imputation model produced plausible values*/
proc import datafile='.../hrv_data.xlsx'
dbms=xlsx                                   
out=work.hrv_original                     
replace;                                    
run;

proc means data=hrv_original n nmiss mean std min max skew kurt; run;




/* Summary for PEP distribution (Imputation 1 only) */
proc means data=imputed_data n mean std min max skew kurt;
  where _Imputation_ = 1;
  var PEP_msec_T1-PEP_msec_T6;
  title "Descriptive statistics for PEP (Imputation 1)";
run;

/*Prepare the imputed data for reshaping to long format*/

proc sort data=imputed_data; by Subject_ID _imputation_; run;
proc print data=imputed_data(obs=20); run;



/* Reshape imputed wide data to long format*/
proc sort data=imputed_data;
  by _Imputation_ Subject_ID;
run;

data hrv_long_imputed;
  set imputed_data;
  by _Imputation_ Subject_ID;

  array lnRMSSD_a[6]  lnRMSSD_msec_T1-lnRMSSD_msec_T6;
  array lnSDNN_a[6]   lnSDNN_msec_T1-lnSDNN_msec_T6;
  array lnRSA_a[6]    lnRSA0_msec_T1-lnRSA0_msec_T6;
  array PEP_a[6]      PEP_msec_T1-PEP_msec_T6;

  array lnSCL_a[2]    lnAverage_SCL_uS_T1-lnAverage_SCL_uS_T2;


  retain lnHF_ms_T1 lnHF_ms_T3 lnHF_ms_T4 lnHF_ms_T6;
  retain lnLF_ms_T1 lnLF_ms_T3 lnLF_ms_T4 lnLF_ms_T6;
  retain lnLFHF_T1  lnLFHF_T3  lnLFHF_T4  lnLFHF_T6;

  do Time = 1 to 6;

    /* Mean structure vars */
    lnRMSSD = lnRMSSD_a[Time];
    lnSDNN  = lnSDNN_a[Time];

    /* Frequency-domain (T1,3,4,6 only) */
    select (Time);
      when (1) do; lnHF=lnHF_ms_T1; lnLF=lnLF_ms_T1; lnLFHF=lnLFHF_T1; end;
      when (2) do; lnHF=.; lnLF=.; lnLFHF=.; end;   /* missing-by-design */
      when (3) do; lnHF=lnHF_ms_T3; lnLF=lnLF_ms_T3; lnLFHF=lnLFHF_T3; end;
      when (4) do; lnHF=lnHF_ms_T4; lnLF=lnLF_ms_T4; lnLFHF=lnLFHF_T4; end;
      when (5) do; lnHF=.; lnLF=.; lnLFHF=.; end;   /* missing-by-design */
      when (6) do; lnHF=lnHF_ms_T6; lnLF=lnLF_ms_T6; lnLFHF=lnLFHF_T6; end;
      otherwise do; lnHF=.; lnLF=.; lnLFHF=.; end;
    end;

    /* RSA (ln) & SCL (ln) */
    lnRSA = lnRSA_a[Time];
    if Time in (1,2) then lnSCL = lnSCL_a[Time]; else lnSCL = .;

    /* PEP raw */
    PEP_msec = PEP_a[Time];

    /* ===== Condition variables ===== */
    /* Binary: 1=Stress (Time 2–5), 0=Rest (Time 1 & 6) */
    REST_VS_STRESS = (Time in (2,3,4,5));
    label REST_VS_STRESS = "Condition: 1=Stress (T2–T5), 0=Rest (T1,T6)";
    format REST_VS_STRESS condition01_.;

    output;
  end;

  keep _Imputation_ Subject_ID Gender Group Anxiety_mother Time
       REST_VS_STRESS PHASE
       lnRMSSD lnSDNN lnHF lnLF lnLFHF lnRSA lnSCL PEP_msec;
run;

/*Post-reshaping diagnostics and plausibility checks*/
proc freq data=hrv_long_imputed;
  tables _Imputation_*Time / nopercent norow nocol;
  tables Time*REST_VS_STRESS / nopercent norow nocol;
  tables Time*PHASE / nopercent norow nocol;
run;

proc means data=hrv_long_imputed n min mean max;
  class Time;
  var lnRMSSD lnSDNN lnHF lnLF lnLFHF lnRSA lnSCL PEP_msec;
  title "QC: Ranges by Time (long format)";
run;

/*Confirmation*/
/* Count nonmissing per Time for each variable to verify structural missingness */
proc means data=hrv_long_imputed n nmiss;
  class Time;
  var lnRMSSD lnSDNN lnHF lnLF lnLFHF lnRSA lnSCL PEP_msec;
  title "Nonmissing (N) and Missing (NMISS) by Time";
run;

/* Or a crisper table: one row per variable x time */
proc sql;
  create table nm_by_time as
  select Time,
         sum(lnRMSSD is not missing) as N_lnRMSSD,
         sum(lnSDNN  is not missing) as N_lnSDNN,
         sum(lnHF    is not missing) as N_lnHF,
         sum(lnLF    is not missing) as N_lnLF,
         sum(lnLFHF  is not missing) as N_lnLFHF,
         sum(lnRSA   is not missing) as N_lnRSA,
         sum(lnSCL   is not missing) as N_lnSCL,
         sum(PEP_msec is not missing) as N_PEP
  from hrv_long_imputed
  group by Time
  order by Time;
quit;

proc print data=nm_by_time; run;

/*Exporting*/
proc export data=hrv_long_imputed
  outfile='.../HRV/hrv_fcs_imputed_Long.xlsx'
  dbms=xlsx
  replace;
  sheet="Fcs_imputed_long";
run;



proc import datafile='.../hrv_fcs_imputed_Long.xlsx'
dbms=xlsx
out=work.hrv_long
replace;
run;
proc print data=hrv_long(obs=20);run;



/*========================================================================
 					 3-  Correlation Exploration.
		Mean, variance and individual profiles exploration are done in R
 ==========================================================================*/

/*Semi-variogram to decide for the 3 variance components. Remove the systematic trend, use residuals to explore correlation. */

%macro semivariogram(ds      = hrv_long,
                     idvar   = Subject_ID,
                     timevar = Time,
                     yvar    = PEP_msec,
                     impvar  = _Imputation_,
                     use_imp = 1);

/* 1. Subset one imputation */
data _oneimp;
  set &ds;
  where &impvar = &use_imp;
run;

/* 2. Fit simple average trend: outcome ~ time, get residuals */
proc glm data=_oneimp;
  model &yvar = &timevar;
  output out=_resid r=residual;
run; quit;

/* 3. Variogram pairs (all pairs of residuals with time lag) */
proc variogram data=_resid outpair=_pairs;
  coordinates xc=&timevar yc=&idvar;
  compute robust novariogram;
  var residual;
run;

/* 4. Semi-variances within same subject: estimate ?(u) */
data _variogram;
  set _pairs;
  if y1 = y2  /* within-subject pairs */
  vario = (v1 - v2)**2 / 2;  /* semi-variance */
run;

/* 5. Semi-variances between subjects to estimate total variance */
data _variance;
  set _pairs;
  if y1 < y2;                             /* different subjects */
  vario = (v1 - v2)**2 / 2;
run;

proc means data=_variance noprint;
  var vario;
  output out=_totalvar mean=totvar;
run;

/* store total variance in a macro variable for plotting */
data _null_;
  set _totalvar;
  call symputx("totvar", totvar);
run;

/* 6. LOESS smoothing of the semi-variogram */
proc loess data=_variogram;
  ods output scoreresults=_vario_loess;
  model vario = distance;
  score data=_variogram;
run;

proc sort data=_vario_loess; by distance; run;

/* 7. Plot: raw semi-variances + LOESS curve + total variance line */
goptions reset=all;

symbol1 c=red   v=dot  h=0.2 mode=include;   /* raw points */
symbol2 c=black i=join w=2   mode=include;   /* smoothed line */

axis1 label=(h=2 'Time lag') value=(h=1.5)
      minor=none;
axis2 label=(h=2 a=90 'v(u)') value=(h=1.5)
      minor=none;

title h=3 "Semi-variogram (&yvar, Imputation=&use_imp)";
/*axis1 order=(1 to 2 by 1);*/
axis2 order=(100 to 700 by 100);

proc gplot data=_vario_loess;
  plot vario*distance=1
       p_vario*distance=2 /
       overlay haxis=axis1 vaxis=axis2
       vref=&totvar lvref=3;
run; quit;


%mend semivariogram;

%macro allimp;
  %do m = 1 %to 10;
    %semivariogram(yvar=PEP_msec, use_imp=&m);
  %end;
%mend;

%allimp;






/*======================================================
       4- MAR: Model fitting across imputed datasets
========================================================*/

/*=======
   RMSSD
  =======*/

/*Prelimnary mean structure*/
proc sort data=hrv_long;  
    by _Imputation_ Subject_ID Time;
run;

proc glm data=hrv_long;
    by _Imputation_;

    class Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

    model lnRMSSD =
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

/*  OLS residuals are used at this stage for diagnostic purposes only.
Even if the design matrix is singular, SAS computes residuals using a generalized inverse.
Because parameter estimates are not interpreted here, matrix singularity does not affect the validity of the residual-based diagnostics.
*/



/*Preliminary r.effects: 

Does variance increase with condition or time?
Does variance differ by group?*/

data rmssd_resid;
    set rmssd_olsresid;
run;


proc sgplot data=rmssd_olsresid;
	by _imputation_;
    scatter x=Time y=r_lnRMSSD / transparency=0.4;
    loess x=Time y=r_lnRMSSD;
    title "OLS Residuals vs Time for lnRMSSD";
run;

proc sgplot data=rmssd_olsresid;
	by _imputation_;
    vbox r_lnRMSSD / category=REST_VS_STRESS;
    title "lnRMSSD Residual Distribution by Condition (Rest vs Stress)";
run;

proc sgplot data=rmssd_olsresid;
    by _imputation_;
    vbox r_lnRMSSD / category=Group;
    title "lnRMSSD Residual Distribution by PMA Group";
run;

proc sgplot data=rmssd_olsresid;
	by _imputation_;
    series x=Time y=r_lnRMSSD / group=Subject_ID transparency=0.8;
    title "lnRMSSD Residual Profiles by Subject";
run;

data resvar;
    set rmssd_olsresid;
	by _imputation_;
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
    by _Imputation_;
    class Subject_ID Time;
    model r_lnRMSSD = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

proc mixed data=rmssd_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnRMSSD = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;


/*3) CS*/
proc mixed data=rmssd_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnRMSSD = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=CS;
    ods output FitStatistics=CS_AIC;
run;

proc mixed data=rmssd_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnRMSSD = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=CS;
    ods output FitStatistics=CS_AIC;
run;

/*4) Simple*/
proc mixed data=rmssd_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnRMSSD = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;

proc mixed data=rmssd_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnRMSSD = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;
/*AR(1)*/


/*Reduce the r.effect structure*/
proc mixed data=hrv_long nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnRMSSD =
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

proc mixed data=hrv_long nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnRMSSD =
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



/*==============================
	Mean structure reduction
  ==============================*/

proc mixed data=hrv_long nobound method=ml;
    where _Imputation_ = 2;
    class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnRMSSD =
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




proc mixed data=hrv_long nobound method=ml;
    where _Imputation_ = 2;
    class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnRMSSD =
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
proc mixed data=hrv_long nobound method=ml;
    where _Imputation_ = 2;
    class 
        Subject_ID Time
        Group          (ref='0')
        REST_VS_STRESS (ref='0')
        Gender         (ref='m');

    model lnRMSSD =
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

/*Do not forget to compare the last model with the first one*/



/*proc mixed data=hrv_long nobound method=reml outpm=RMSSD_PM;*/
/*    by _Imputation_;*/
/*    class Subject_ID Time*/
/*          Group (ref='0')*/
/*          REST_VS_STRESS (ref='0') */
/*          Gender (ref='m');*/
/**/
/*    model lnRMSSD =*/
/*        Group*/
/*        REST_VS_STRESS*/
/*        Gender*/
/*        / solution ddfm=kr;*/
/**/
/*    random intercept / subject=Subject_ID;*/
/*    repeated Time / subject=Subject_ID type=AR(1);*/
/*run;*/










/*Fit the final model*/
/* Final mixed model for lnRMSSD across 10 imputations */

/* 0. Sort data by imputation */
proc sort data=hrv_long;
    by _Imputation_;
run;

/* 1. Final mixed model: lnRMSSD ~ Group * REST_VS_STRESS + Gender
      Random intercept + AR(1) */

proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
        class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnRMSSD =
        Group
        REST_VS_STRESS
        Group*REST_VS_STRESS
        Gender
        / solution ddfm=kr;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = RMSSD_SolutionF;
run;

/* Keep only the estimable rows (no reference levels) */
data RMSSD_Parms;
    set RMSSD_SolutionF;
    where StdErr ne .;  * drop rows like Group=1 with StdErr=. ;
run;

proc print data=RMSSD_Parms(obs=20);
run;



data RMSSD_Parms2;
    set RMSSD_Parms;
    length Parameter $40;

    /* create a single label for each parameter */
    if Effect = 'Intercept' then Parameter = 'Intercept';
    else if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
    else if Effect = 'REST_VS_STRESS' then Parameter = 'Stress_vs_rest';
    else if Effect = 'Group*REST_VS_STRESS' then Parameter = 'Interaction_GroupxStress';
    else if Effect = 'Gender' then Parameter = 'Female_vs_male';

    keep _Imputation_ Parameter Estimate StdErr;
run;
proc print data=RMSSD_Parms2(obs=20);
run;

proc sort data=RMSSD_Parms2;
    by Parameter _Imputation_;
run;

proc mianalyze data=RMSSD_Parms2;
    by Parameter;          * one pooled result per parameter label;
    modeleffects Estimate; * variable that holds the coefficient;
    stderr StdErr;         * variable that holds the SE;
run;




/*Model assumptions: */

proc sort data=hrv_long(where=(_Imputation_=1)) out=hrv_imp1;
  by Subject_ID Time;
run;

proc glimmix data=hrv_imp1 nobound method=rspl;
  class Subject_ID Time Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

  model lnRMSSD =
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




/*Interaction is not significant, fit an additive model to be able to correctly interpret the overall main effects*/

proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
    class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnRMSSD =
        Group
        REST_VS_STRESS
/*        Group*REST_VS_STRESS*/
        Gender
        / solution ddfm=kr;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = RMSSD_SolutionF;
run;

/* Keep only the estimable rows (no reference levels) */
data RMSSD_Parms_main;
    set RMSSD_SolutionF;
    where StdErr ne .;  * drop rows like Group=1 with StdErr=. ;
run;

proc print data=RMSSD_Parms_main(obs=20);
run;



data RMSSD_Parms_main;
    set RMSSD_Parms_main;
    length Parameter $40;

    /* create a single label for each parameter */
    if Effect = 'Intercept' then Parameter = 'Intercept';
    else if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
    else if Effect = 'REST_VS_STRESS' then Parameter = 'Stress_vs_rest';
/*    else if Effect = 'Group*REST_VS_STRESS' then Parameter = 'Interaction_GroupxStress';*/
    else if Effect = 'Gender' then Parameter = 'Female_vs_male';

    keep _Imputation_ Parameter Estimate StdErr;
run;
proc print data=RMSSD_Parms_main(obs=20);
run;

proc sort data=RMSSD_Parms_main;
    by Parameter _Imputation_;
run;

proc mianalyze data=RMSSD_Parms_main;
    by Parameter;          * one pooled result per parameter label;
    modeleffects Estimate; * variable that holds the coefficient;
    stderr StdErr;         * variable that holds the SE;
run;






/*=======
   SDNN
  =======*/

/*Prelimnary mean structure*/

proc sort data=hrv_long;  
    by _Imputation_ Subject_ID Time;
run;

proc glm data=hrv_long;
    by _Imputation_;

    class Subject_ID 
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnSDNN =
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

/*preliminary r.effects */
/*Explore Residuals to: 
? Does variance increase with condition or time?
? Does variance differ by group?
*/


proc sgplot data=sdnn_olsresid;
	by _imputation_;
    scatter x=Time y=r_lnSDNN / transparency=0.4;
    loess x=Time y=r_lnSDNN;
    title "OLS Residuals vs Time for lnSDNN";
run;

proc sgplot data=sdnn_olsresid;
	by _imputation_;
    vbox r_lnSDNN / category=REST_VS_STRESS;
    title "lnSDNN Residual Distribution by Condition (Rest vs Stress)";
run;

proc sgplot data=sdnn_olsresid;
    by _imputation_;
    vbox r_lnSDNN / category=Group;
    title "lnSDNN Residual Distribution by PMA Group";
run;

proc sgplot data=sdnn_olsresid;
	by _imputation_;
    series x=Time y=r_lnSDNN / group=Subject_ID transparency=0.8;
    title "lnSDNN Residual Profiles by Subject";
run;

data resvar;
    set sdnn_olsresid;
	by _imputation_;
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
    by _Imputation_;
    class Subject_ID Time;
    model r_lnSDNN = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;
proc mixed data=sdnn_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnSDNN = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;


/*2) CS*/
proc mixed data=sdnn_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnSDNN = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=CS;
    ods output FitStatistics=CS_AIC;
run;
proc mixed data=sdnn_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnSDNN = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=CS;
    ods output FitStatistics=CS_AIC;
run;


/*3) Simple*/
proc mixed data=sdnn_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnSDNN = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=simple_AIC;
run;
proc mixed data=sdnn_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnSDNN = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=simple_AIC;
run;

/*Reduce r.effects structure*/
proc mixed data=hrv_long nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnSDNN =
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

proc mixed data=hrv_long nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnSDNN =
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

proc mixed data=hrv_long nobound method=ml;
    where _Imputation_ = 8;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnSDNN =
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




proc mixed data=hrv_long nobound method=ml;
    where _Imputation_ = 8;
 	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnSDNN =
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
proc mixed data=hrv_long nobound method=ml;
    where _Imputation_ = 8;
    class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnSDNN =
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


/*Do not forget to compare the last model with the first one*/




/*Fit the final model*/
/* Final mixed model for lnSDNN across 10 imputations */

/* 0. Sort data by imputation */
proc sort data=hrv_long;
    by _Imputation_;
run;

/* 1. Final mixed model: lnSDNN ~ Group * REST_VS_STRESS + Gender
      Random intercept + AR(1) */
proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
    class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnSDNN =
        Group
        REST_VS_STRESS
        Group*REST_VS_STRESS
        Gender
        / solution ddfm=kr;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = SDNN_SolutionF;
run;

/* Keep only the estimable rows (no reference levels) */
data SDNN_Parms;
    set SDNN_SolutionF;
    where StdErr ne .;  * drop rows like Group=1 with StdErr=. ;
run;

proc print data=SDNN_Parms(obs=20);
run;


data SDNN_Parms2;
    set SDNN_Parms;
    length Parameter $40;

    /* create a single label for each parameter */
    if Effect = 'Intercept' then Parameter = 'Intercept';
    else if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
    else if Effect = 'REST_VS_STRESS' then Parameter = 'Stress_vs_rest';
    else if Effect = 'Group*REST_VS_STRESS' then Parameter = 'Interaction_GroupxStress';
    else if Effect = 'Gender' then Parameter = 'Female_vs_male';

    keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=SDNN_Parms2;
    by Parameter _Imputation_;
run;

proc print data=SDNN_Parms2(obs=20);
run;

proc sort data=SDNN_Parms2;
    by Parameter _Imputation_;
run;

proc mianalyze data=SDNN_Parms2;
    by Parameter;          * one pooled result per parameter label;
    modeleffects Estimate; * variable that holds the coefficient;
    stderr StdErr;         * variable that holds the SE;
run;




proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
    class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnSDNN =
        Group
        REST_VS_STRESS
        Gender
        / solution ddfm=kr;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = SDNN_SolutionF_main;
run;
data SDNN_Parms_main;
    set SDNN_SolutionF_main;
    where StdErr ne .;  * drop rows like Group=1 with StdErr=. ;
run;

data SDNN_Parms_main;
    set SDNN_Parms_main;
    length Parameter $40;

    /* create a single label for each parameter */
    if Effect = 'Intercept' then Parameter = 'Intercept';
    else if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
    else if Effect = 'REST_VS_STRESS' then Parameter = 'Stress_vs_rest';
    else if Effect = 'Gender' then Parameter = 'Female_vs_male';

    keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=SDNN_Parms_main;
    by Parameter _Imputation_;
run;

proc print data=SDNN_Parms_main(obs=20); run;

proc mianalyze data=SDNN_Parms_main;
    by Parameter;          * one pooled result per parameter label;
    modeleffects Estimate; * variable that holds the coefficient;
    stderr StdErr;         * variable that holds the SE;
run;





proc sort data=hrv_long(where=(_Imputation_=1)) out=hrv_imp1;
  by Subject_ID Time;
run;

proc glimmix data=hrv_imp1 nobound method=rspl;
  class Subject_ID Time Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

  model lnSDNN =
        Group
        REST_VS_STRESS
        Gender
/*        Group*REST_VS_STRESS*/
        / solution ddfm=kr dist=normal link=identity;

  random intercept / subject=Subject_ID;

   /*AR(1) within subject over Time*/ 
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













/*================
		RSA
  ================*/

/*Prelimnary mean structure*/


proc sort data=hrv_long;  
    by _Imputation_ Subject_ID Time;
run;

proc glm data=hrv_long;
    by _Imputation_;

    class Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

    model lnRSA =
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
	by _imputation_;
    scatter x=Time y=r_lnRSA / transparency=0.4;
    loess x=Time y=r_lnRSA;
    title "OLS Residuals vs Time for lnRSA";
run;

proc sgplot data=rsa_olsresid;
	by _imputation_;
    vbox r_lnRSA / category=REST_VS_STRESS;
    title "lnRSA Residual Distribution by Condition (Rest vs Stress)";
run;

proc sgplot data=rsa_olsresid;
    by _imputation_;
    vbox r_lnRSA / category=Group;
    title "lnRSA Residual Distribution by PMA Group";
run;

proc sgplot data=rsa_olsresid;
	by _imputation_;
    series x=Time y=r_lnRSA / group=Subject_ID transparency=0.8;
    title "lnRSA Residual Profiles by Subject";
run;

data resvar;
    set rsa_olsresid;
	by _imputation_;
    r2 = r_lnRSA * r_lnRSA;
run;

proc sgplot data=resvar;

    scatter x=Time y=r2 / transparency=0.5;
    loess x=Time y=r2;
    title "lnRSA Squared Residuals vs Time (Variance Diagnostics)";
run;

/*Check Residuals Covariance*/



/*After seeing the plots, the candidates for the covariance structure are: AR, Simple, CS, let's try*/

/*1) AR(1)*/

proc mixed data=rsa_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnRSA = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

proc mixed data=rsa_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnRSA = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

/*2) CS*/
proc mixed data=rsa_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnRSA = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=CS;
    ods output FitStatistics=CS_AIC;
run;

proc mixed data=rsa_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnRSA = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=CS;
    ods output FitStatistics=CS_AIC;
run;



/*Simple*/
proc mixed data=rsa_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnRSA = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;
proc mixed data=rsa_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnRSA = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;

/*AR(1)*/

/*Reduce the r.effects structure*/
proc mixed data=hrv_long nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnRSA =
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

proc mixed data=hrv_long nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnRSA =
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

proc mixed data=hrv_long method=ml;
    where _Imputation_ = 10;
    class Subject_ID Time
		  Group(ref='0')
		  REST_VS_STRESS(ref='0') 
		  Gender(ref='m');

    model lnRSA =
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




proc mixed data=hrv_long method=ml;
    where _Imputation_ = 10;
    class Subject_ID Time
		  Group(ref='0')
		  REST_VS_STRESS(ref='0') 
		  Gender(ref='m');

    model lnRSA =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
/*        Group*Gender*/
         REST_VS_STRESS*Gender
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_noRG;
run;


/*Reduce more*/
proc mixed data=hrv_long method=ml;
    where _Imputation_ = 10;
    class Subject_ID Time
		  Group(ref='0')
		  REST_VS_STRESS(ref='0') 
		  Gender(ref='m');

    model lnRSA =
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






/*Fit the final model*/
/* Final mixed model for lnSDNN across 10 imputations */

/* 0. Sort data by imputation */
proc sort data=hrv_long;
    by _Imputation_;
run;

/* 1. Final mixed model: lnSDNN ~ Group * REST_VS_STRESS + Gender
      Random intercept + AR(1) */

proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
    class Subject_ID Time
		  Group(ref='0')
		  REST_VS_STRESS(ref='0') 
		  Gender(ref='m');

    model lnRSA =
        Group
        REST_VS_STRESS
        Group*REST_VS_STRESS
        Gender
        / solution ddfm=kr;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = lnRSA_SolutionF;
run;

/* Keep only the estimable rows (no reference levels) */
data RSA_Parms;
    set lnRSA_SolutionF;
    where StdErr ne .;  * drop rows like Group=1 with StdErr=. ;
run;

proc print data=RSA_Parms(obs=20);
run;


data RSA_Parms2;
    set RSA_Parms;
    length Parameter $40;

    /* create a single label for each parameter */
    if Effect = 'Intercept' then Parameter = 'Intercept';
    else if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
    else if Effect = 'REST_VS_STRESS' then Parameter = 'Stress_vs_rest';
    else if Effect = 'Group*REST_VS_STRESS' then Parameter = 'Interaction_GroupxStress';
    else if Effect = 'Gender' then Parameter = 'Female_vs_male';

    keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=RSA_Parms2;
    by Parameter _Imputation_;
run;

proc print data=RSA_Parms2(obs=20);
run;

proc sort data=RSA_Parms2;
    by Parameter _Imputation_;
run;

proc mianalyze data=RSA_Parms2;
    by Parameter;          * one pooled result per parameter label;
    modeleffects Estimate; * variable that holds the coefficient;
    stderr StdErr;         * variable that holds the SE;
run;






/*Main effects model*/
proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
    class Subject_ID Time
		  Group(ref='0')
		  REST_VS_STRESS(ref='0') 
		  Gender(ref='m');

    model lnRSA =
        Group
        REST_VS_STRESS
/*        Group*REST_VS_STRESS*/
        Gender
        / solution ddfm=kr;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = lnRSA_SolutionF;
run;

/* Keep only the estimable rows (no reference levels) */
data RSA_Parms;
    set lnRSA_SolutionF;
    where StdErr ne .;  * drop rows like Group=1 with StdErr=. ;
run;

proc print data=RSA_Parms(obs=20);
run;


data RSA_Parms2;
    set RSA_Parms;
    length Parameter $40;

    /* create a single label for each parameter */
    if Effect = 'Intercept' then Parameter = 'Intercept';
    else if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
    else if Effect = 'REST_VS_STRESS' then Parameter = 'Stress_vs_rest';
/*    else if Effect = 'Group*REST_VS_STRESS' then Parameter = 'Interaction_GroupxStress';*/
    else if Effect = 'Gender' then Parameter = 'Female_vs_male';

    keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=RSA_Parms2;
    by Parameter _Imputation_;
run;

proc print data=RSA_Parms2(obs=20);
run;

proc sort data=RSA_Parms2;
    by Parameter _Imputation_;
run;

proc mianalyze data=RSA_Parms2;
    by Parameter;          * one pooled result per parameter label;
    modeleffects Estimate; * variable that holds the coefficient;
    stderr StdErr;         * variable that holds the SE;
run;



/* Model assumptions*/
proc sort data=hrv_long(where=(_Imputation_=1)) out=hrv_imp1;
  by Subject_ID Time;
run;

proc glimmix data=hrv_imp1 nobound method=rspl;
  class Subject_ID Time Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

  model lnRSA =
        Group
        REST_VS_STRESS
        Gender
/*        Group*REST_VS_STRESS*/
        / solution ddfm=kr dist=normal link=identity;

  random intercept / subject=Subject_ID;

   /*AR(1) within subject over Time*/ 
  random _residual_ / subject=Subject_ID type=ar(1);

  output out=lnRSA_Diag
         pred=Pred
         resid=Resid
         student=StuResid;
run;


proc sgplot data=lnRSA_Diag;
  scatter x=Pred y=Resid;
  refline 0 / axis=y;
run;

proc univariate data=lnRSA_Diag normal;
  var Resid;
  qqplot Resid / normal(mu=est sigma=est);
run;

proc sgplot data=lnRSA_Diag;
  histogram Resid;
  density Resid;
run;



/*================
		PEP
  ================*/

/*Prelimnary mean structure*/


proc sort data=hrv_long;  
    by _Imputation_ Subject_ID Time;
run;

proc glm data=hrv_long;
    by _Imputation_;

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

/*Preliminary r.effects */
/*Explore Residuals to: 
? Does variance increase with condition or time?
? Does variance differ by group?
*/
proc print data=pep_olsresid(obs=10);run;

proc sgplot data=pep_olsresid;
	by _imputation_;
    scatter x=Time y=r_pep / transparency=0.4;
    loess x=Time y=r_pep;
    title "OLS Residuals vs Time for PEP";
run;

proc sgplot data=pep_olsresid;
	by _imputation_;
    vbox r_pep / category=REST_VS_STRESS;
    title "PEP Residual Distribution by Condition (Rest vs Stress)";
run;

proc sgplot data=pep_olsresid;
    by _imputation_;
    vbox r_pep / category=Group;
    title "PEP Residual Distribution by PMA Group";
run;

proc sgplot data=pep_olsresid;
	by _imputation_;
    series x=Time y=r_pep / group=Subject_ID transparency=0.8;
    title "PEP Residual Profiles by Subject";
run;

data resvar;
    set pep_olsresid;
	by _imputation_;
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
    by _Imputation_;
    class Subject_ID Time;
    model r_pep = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

proc mixed data=pep_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_pep = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

/*2) CS*/
proc mixed data=pep_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_pep = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=cs;
    ods output FitStatistics=CS_AIC;
run;
proc mixed data=pep_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_pep = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=cs;
    ods output FitStatistics=CS_AIC;
run;

/*3) Simple*/
proc mixed data=pep_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_pep = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;

proc mixed data=pep_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_pep = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;

/*Simple*/

/*Reduce the r.effects structure*/
proc mixed data=hrv_long nobound method=reml;
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

proc mixed data=hrv_long nobound method=reml;
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

proc mixed data=hrv_long method=ml;
    where _Imputation_ = 10;
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




proc mixed data=hrv_long method=ml;
    where _Imputation_ = 10;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model PEP_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
        /* REST_VS_STRESS*Gender removed */
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output FitStatistics=Fit_noRG;
run;


/*Reduce more*/
proc mixed data=hrv_long method=ml;
    where _Imputation_ = 10;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model PEP_msec =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
/*        Group*Gender*/
        /* REST_VS_STRESS*Gender removed */
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output FitStatistics=Fit_noRG;
run;






/*Fit the final model*/
/* Final mixed model for lnSDNN across 10 imputations */

/* 0. Sort data by imputation */
proc sort data=hrv_long;
    by _Imputation_;
run;


/* 1. Final mixed model: PEP_msec ~ Group * REST_VS_STRESS + Gender
      Random intercept + Simple */

proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model PEP_msec =
        Group
        REST_VS_STRESS
        Group*REST_VS_STRESS
        Gender
        / solution ddfm=kr;
    random intercept  / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output SolutionF = PEP_SolutionF;
run;

/* Keep only the estimable rows (no reference levels) */
data PEP_Parms;
    set PEP_SolutionF;
    where StdErr ne .;  * drop rows like Group=1 with StdErr=. ;
run;

proc print data=PEP_Parms(obs=20);
run;


data PEP_Parms2;
    set PEP_Parms;
    length Parameter $40;

    /* create a single label for each parameter */
    if Effect = 'Intercept' then Parameter = 'Intercept';
    else if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
    else if Effect = 'REST_VS_STRESS' then Parameter = 'Stress_vs_rest';
    else if Effect = 'Group*REST_VS_STRESS' then Parameter = 'Interaction_GroupxStress';
    else if Effect = 'Gender' then Parameter = 'Female_vs_male';

    keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=PEP_Parms2;
    by Parameter _Imputation_;
run;

proc print data=PEP_Parms2(obs=20);
run;

proc sort data=PEP_Parms2;
    by Parameter _Imputation_;
run;

proc mianalyze data=PEP_Parms2;
    by Parameter;          * one pooled result per parameter label;
    modeleffects Estimate; * variable that holds the coefficient;
    stderr StdErr;         * variable that holds the SE;
run;


/*Main Effects*/

proc sort data=hrv_long;
    by _Imputation_;
run;


/* 1. Final mixed model: PEP_msec ~ Group * REST_VS_STRESS + Gender
      Random intercept + Simple */


proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model PEP_msec =
        Group
        REST_VS_STRESS
/*        Group*REST_VS_STRESS*/
        Gender
        / solution ddfm=kr;
    random intercept  / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;

    ods output SolutionF = PEP_SolutionF;
run;

/* Keep only the estimable rows (no reference levels) */
data PEP_Parms;
    set PEP_SolutionF;
    where StdErr ne .;  * drop rows like Group=1 with StdErr=. ;
run;

proc print data=PEP_Parms(obs=20);
run;


data PEP_Parms2;
    set PEP_Parms;
    length Parameter $40;

    /* create a single label for each parameter */
    if Effect = 'Intercept' then Parameter = 'Intercept';
    else if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
    else if Effect = 'REST_VS_STRESS' then Parameter = 'Stress_vs_rest';
/*    else if Effect = 'Group*REST_VS_STRESS' then Parameter = 'Interaction_GroupxStress';*/
    else if Effect = 'Gender' then Parameter = 'Female_vs_male';

    keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=PEP_Parms2;
    by Parameter _Imputation_;
run;

proc print data=PEP_Parms2(obs=20);
run;

proc sort data=PEP_Parms2;
    by Parameter _Imputation_;
run;

proc mianalyze data=PEP_Parms2;
    by Parameter;          * one pooled result per parameter label;
    modeleffects Estimate; * variable that holds the coefficient;
    stderr StdErr;         * variable that holds the SE;
run;



/*Model assumptions*/
/* Model assumptions*/
proc sort data=hrv_long(where=(_Imputation_=1)) out=hrv_imp1;
  by Subject_ID Time;
run;

proc glimmix data=hrv_imp1 nobound method=rspl;
  class Subject_ID Time Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

  model PEP_msec =
        Group
        REST_VS_STRESS
        Gender
/*        Group*REST_VS_STRESS*/
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





/*========================
	       lnHF
 ========================*/


/*Prelimnary mean structure*/

proc sort data=hrv_long;  
    by _Imputation_ Subject_ID Time;
run;

proc glm data=hrv_long;
    by _Imputation_;

    class Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

    model lnHF =
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

/*Preliminary r.effects */
/*Explore Residuals to: 
? Does variance increase with condition or time?
? Does variance differ by group?
*/
proc print data=lnHF_olsresid(obs=10);run;

proc sgplot data=lnHF_olsresid;
	by _imputation_;
    scatter x=Time y=r_lnHF / transparency=0.4;
    loess x=Time y=r_lnHF;
    title "OLS Residuals vs Time for lnHF";
run;

proc sgplot data=lnHF_olsresid;
	by _imputation_;
    vbox r_lnHF / category=REST_VS_STRESS;
    title "lnHF Residual Distribution by Condition (Rest vs Stress)";
run;

proc sgplot data=lnHF_olsresid;
    by _imputation_;
    vbox r_lnHF / category=Group;
    title "lnHF Residual Distribution by PMA Group";
run;

proc sgplot data=lnHF_olsresid;
	by _imputation_;
    series x=Time y=r_lnHF / group=Subject_ID transparency=0.8;
    title "lnHF Residual Profiles by Subject";
run;

data resvar;
    set lnHF_olsresid;
	by _imputation_;
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
    by _Imputation_;
    class Subject_ID Time;
    model r_lnHF = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

proc mixed data=lnHF_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnHF = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

/*2) CS*/
proc mixed data=lnHF_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnHF = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=cs;
    ods output FitStatistics=CS_AIC;
run;

proc mixed data=lnHF_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnHF = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=cs;
    ods output FitStatistics=CS_AIC;
run;


/*3) Simple*/
proc mixed data=lnHF_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnHF = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;
proc mixed data=lnHF_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnHF = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;

/*AR(1) wins*/

/*Reduce the r.effects*/
proc mixed data=hrv_long nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnHF =
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

proc mixed data=hrv_long nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnHF =
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

proc mixed data=hrv_long nobound method=ml;
    where _Imputation_ = 2;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnHF =
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




proc mixed data=hrv_long nobound method=ml;
    where _Imputation_ = 2;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnHF =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
/*        Group*Gender*/
        REST_VS_STRESS*Gender
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_noRG;
run;


/*Reduce more (just to see)*/
proc mixed data=hrv_long nobound method=ml;
    where _Imputation_ = 8;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnHF =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
/*        Group*Gender*/
/*        REST_VS_STRESS*Gender*/
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_noRG;
run;


/*Removing condition*Gender does not improve the model*/




/* 1. Final mixed model: lnHF ~ Group * REST_VS_STRESS + Gender*REST_VS_STRESS
      Random intercept + AR(1) */


proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnHF =
        Group
        REST_VS_STRESS
        Gender
		Group*REST_VS_STRESS
        REST_VS_STRESS*Gender
        / solution ddfm=kr; 
    random intercept  / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = lnHF_SolutionF;
run;

/* Keep only the estimable rows (no reference levels) */
data lnHF_SolutionF;
    set lnHF_SolutionF;
    where StdErr ne .;  * drop rows like Group=1 with StdErr=. ;
run;

proc print data=lnHF_SolutionF(obs=20);
run;


data lnHF_SolutionF2;
    set lnHF_SolutionF;
    length Parameter $40;

    /* create a single label for each parameter */
    if Effect = 'Intercept' then Parameter = 'Intercept';
    else if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
    else if Effect = 'REST_VS_STRESS' then Parameter = 'Stress_vs_rest';
    else if Effect = 'Group*REST_VS_STRESS' then Parameter = 'Interaction_GroupxStress';
	else if Effect = 'REST_VS_STRES*Gender' then Parameter = 'Interaction_StressxGender';
    else if Effect = 'Gender' then Parameter = 'Female_vs_male';

    keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=lnHF_SolutionF2;
    by Parameter _Imputation_;
run;

proc print data=lnHF_SolutionF2(firstobs=20 obs=40);
run;

proc sort data=lnHF_SolutionF2;
    by Parameter _Imputation_;
run;

proc mianalyze data=lnHF_SolutionF2;
    by Parameter;          * one pooled result per parameter label;
    modeleffects Estimate; * variable that holds the coefficient;
    stderr StdErr;         * variable that holds the SE;
run;



/*Removing stress*Group*/

proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnHF =
        Group
        REST_VS_STRESS
        Gender
/*		Group*REST_VS_STRESS*/
        REST_VS_STRESS*Gender
        / solution ddfm=kr;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = lnHF_SolutionF1;
run;


data lnHF_SolutionF1;
    set lnHF_SolutionF1;
    where StdErr ne .;  * drop rows like Group=1 with StdErr=. ;
run;

proc print data=lnHF_SolutionF1(obs=20);
run;


data lnHF_SolutionF21;
    set lnHF_SolutionF1;
    length Parameter $40;

    /* create a single label for each parameter */
    if Effect = 'Intercept' then Parameter = 'Intercept';
    else if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
    else if Effect = 'REST_VS_STRESS' then Parameter = 'Stress_vs_rest';
/*    else if Effect = 'Group*REST_VS_STRESS' then Parameter = 'Interaction_GroupxStress';*/
	else if Effect = 'REST_VS_STRES*Gender' then Parameter = 'Interaction_StressxGender';
    else if Effect = 'Gender' then Parameter = 'Female_vs_male';

    keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=lnHF_SolutionF21;
    by Parameter _Imputation_;
run;

proc print data=lnHF_SolutionF21(firstobs=20 obs=40);
run;

proc sort data=lnHF_SolutionF21;
    by Parameter _Imputation_;
run;

proc mianalyze data=lnHF_SolutionF21;
    by Parameter;          * one pooled result per parameter label;
    modeleffects Estimate; * variable that holds the coefficient;
    stderr StdErr;         * variable that holds the SE;
run;





/*Removing stress*gender*/

proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnHF =
        Group
        REST_VS_STRESS
        Gender
/*		Group*REST_VS_STRESS*/
/*        REST_VS_STRESS*Gender*/
        / solution ddfm=kr;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = lnHF_SolutionF1;
run;


data lnHF_SolutionF1;
    set lnHF_SolutionF1;
    where StdErr ne .;  * drop rows like Group=1 with StdErr=. ;
run;

proc print data=lnHF_SolutionF1(obs=20);
run;


data lnHF_SolutionF21;
    set lnHF_SolutionF1;
    length Parameter $40;

    /* create a single label for each parameter */
    if Effect = 'Intercept' then Parameter = 'Intercept';
    else if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
    else if Effect = 'REST_VS_STRESS' then Parameter = 'Stress_vs_rest';
/*    else if Effect = 'Group*REST_VS_STRESS' then Parameter = 'Interaction_GroupxStress';*/
/*	else if Effect = 'REST_VS_STRES*Gender' then Parameter = 'Interaction_StressxGender';*/
    else if Effect = 'Gender' then Parameter = 'Female_vs_male';

    keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=lnHF_SolutionF21;
    by Parameter _Imputation_;
run;

/*proc print data=lnHF_SolutionF21(firstobs=20 obs=40);*/
/*run;*/

proc sort data=lnHF_SolutionF21;
    by Parameter _Imputation_;
run;

proc mianalyze data=lnHF_SolutionF21;
    by Parameter;          * one pooled result per parameter label;
    modeleffects Estimate; * variable that holds the coefficient;
    stderr StdErr;         * variable that holds the SE;
run;





/*Model assumptions*/
proc sort data=hrv_long(where=(_Imputation_=1)) out=hrv_imp1;
  by Subject_ID Time;
run;

proc glimmix data=hrv_imp1 nobound method=rspl;
  class Subject_ID Time Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

  model lnHF =
        Group
        REST_VS_STRESS
        Gender
/*        Group*REST_VS_STRESS*/
		Gender*REST_VS_STRESS
        / solution ddfm=kr dist=normal link=identity;

  random intercept / subject=Subject_ID;

   /*AR(1) within subject over Time*/ 
  random _residual_ / subject=Subject_ID type=ar(1);

  output out=lnHF_Diag
         pred=Pred
         resid=Resid
         student=StuResid;
run;


proc sgplot data=lnHF_Diag;
  scatter x=Pred y=Resid;
  refline 0 / axis=y;
run;

proc univariate data=lnHF_Diag normal;
  var Resid;
  qqplot Resid / normal(mu=est sigma=est);
run;

proc sgplot data=lnHF_Diag;
  histogram Resid;
  density Resid;
run;











/*========================
	       lnLF
 ========================*/


/*Prelimnary mean structure*/

proc sort data=hrv_long;  
    by _Imputation_ Subject_ID Time;
run;

proc glm data=hrv_long;
    by _Imputation_;

    class Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

    model lnLF =
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
	by _imputation_;
    scatter x=Time y=r_lnLF / transparency=0.4;
    loess x=Time y=r_lnLF;
    title "OLS Residuals vs Time for lnLF";
run;

proc sgplot data=lnLF_olsresid;
	by _imputation_;
    vbox r_lnLF / category=REST_VS_STRESS;
    title "lnLF Residual Distribution by Condition (Rest vs Stress)";
run;

proc sgplot data=lnLF_olsresid;
    by _imputation_;
    vbox r_lnLF / category=Group;
    title "lnLF Residual Distribution by PMA Group";
run;

proc sgplot data=lnLF_olsresid;
	by _imputation_;
    series x=Time y=r_lnLF / group=Subject_ID transparency=0.8;
    title "lnLF Residual Profiles by Subject";
run;

data resvar;
    set lnLF_olsresid;
	by _imputation_;
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
    by _Imputation_;
    class Subject_ID Time;
    model r_lnLF = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

proc mixed data=lnLF_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnLF = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;


/*2) CS*/
proc mixed data=lnLF_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnLF = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=cs;
    ods output FitStatistics=CS_AIC;
run;

proc mixed data=lnLF_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnLF = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=cs;
    ods output FitStatistics=CS_AIC;
run;


/*3) Simple*/
proc mixed data=lnLF_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnLF = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;


proc mixed data=lnLF_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnLF = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;

/*AR(1) wins*/


/*Reduce r.eff structure*/

proc mixed data=hrv_long nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnLF =
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

proc mixed data=hrv_long nobound method=reml;
	class Subject_ID Time
		  Group (ref='0')
		  REST_VS_STRESS (ref='0') 
		  Gender (ref='m');

    model lnLF =
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

proc mixed data=hrv_long nobound method=ml;
    where _Imputation_ = 8;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnLF =
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

/*REST_VS_STRESSGroup is the most not significant term, but keep it for now, remove the second most non significant term*/


proc mixed data=hrv_long nobound method=ml;
    where _Imputation_ = 8;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnLF =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
        Group*Gender
/*        REST_VS_STRESS*Gender*/
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_noRG;
run;

/*Group*Gender is the most non significant term, remove the second most non significant interaction term (remember: 0.1 for interaction)*/

/*Reduce more*/
proc mixed data=hrv_long nobound method=ml;
    where _Imputation_ = 8;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnLF =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
/*        Group*Gender*/
/*        REST_VS_STRESS*Gender*/
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_noRG;
run;


/*Removing condition*Gender does not improve the model*/




/* 1. Final mixed model: lnLF ~ Group * REST_VS_STRESS + Gender
      Random intercept + AR(1) */


proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnLF =
        Group
        REST_VS_STRESS
        Gender
		Group*REST_VS_STRESS
        / solution ddfm=kr; 
    random intercept  / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = lnLF_SolutionF;
run;

/* Keep only the estimable rows (no reference levels) */
data lnLF_SolutionF;
    set lnLF_SolutionF;
    where StdErr ne .;  * drop rows like Group=1 with StdErr=. ;
run;

proc print data=lnLF_SolutionF(obs=20);
run;


data lnLF_SolutionF2;
    set lnLF_SolutionF;
    length Parameter $40;

    /* create a single label for each parameter */
    if Effect = 'Intercept' then Parameter = 'Intercept';
    else if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
    else if Effect = 'REST_VS_STRESS' then Parameter = 'Stress_vs_rest';
    else if Effect = 'Group*REST_VS_STRESS' then Parameter = 'Interaction_GroupxStress';
/*	else if Effect = 'REST_VS_STRES*Gender' then Parameter = 'Interaction_StressxGender';*/
    else if Effect = 'Gender' then Parameter = 'Female_vs_male';

    keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=lnLF_SolutionF2;
    by Parameter _Imputation_;
run;

proc print data=lnLF_SolutionF2(firstobs=20 obs=40);
run;

proc sort data=lnLF_SolutionF2;
    by Parameter _Imputation_;
run;

proc mianalyze data=lnLF_SolutionF2;
    by Parameter;          * one pooled result per parameter label;
    modeleffects Estimate; * variable that holds the coefficient;
    stderr StdErr;         * variable that holds the SE;
run;



/*=====================================================================*/


/*Main effects:  lnLF ~ Group * REST_VS_STRESS + Gender
 Random intercept + AR(1) */


proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnLF =
        Group
        REST_VS_STRESS
        Gender
        / solution ddfm=kr; 
    random intercept  / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = lnLF_Solution_main;
run;

/* Keep only the estimable rows (no reference levels) */
data lnLF_Solution_main;
    set lnLF_Solution_main;
    where StdErr ne .;  * drop rows like Group=1 with StdErr=. ;
run;

proc print data=lnLF_Solution_main(obs=20);
run;


data lnLF_Solution_main2;
    set lnLF_Solution_main;
    length Parameter $40;

    /* create a single label for each parameter */
    if Effect = 'Intercept' then Parameter = 'Intercept';
    else if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
    else if Effect = 'REST_VS_STRESS' then Parameter = 'Stress_vs_rest';
/*    else if Effect = 'Group*REST_VS_STRESS' then Parameter = 'Interaction_GroupxStress';*/
/*	else if Effect = 'Group*Gender' then Parameter = 'Interaction_GroupxGender';*/
    else if Effect = 'Gender' then Parameter = 'Female_vs_male';

    keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=lnLF_Solution_main2;
    by Parameter _Imputation_;
run;

proc print data=lnLF_Solution_main2(firstobs=20 obs=40);
run;

proc sort data=lnLF_Solution_main2;
    by Parameter _Imputation_;
run;

proc mianalyze data=lnLF_Solution_main2;
    by Parameter;          * one pooled result per parameter label;
    modeleffects Estimate; * variable that holds the coefficient;
    stderr StdErr;         * variable that holds the SE;
run;



/*Model assumptions*/
proc sort data=hrv_long(where=(_Imputation_=1)) out=hrv_imp1;
  by Subject_ID Time;
run;

proc glimmix data=hrv_imp1 nobound method=rspl;
  class Subject_ID Time Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

  model lnLF =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS

        / solution ddfm=kr dist=normal link=identity;

  random intercept / subject=Subject_ID;

   /*AR(1) within subject over Time*/ 
  random _residual_ / subject=Subject_ID type=ar(1);

  output out=lnLF_Diag
         pred=Pred
         resid=Resid
         student=StuResid;
run;


proc sgplot data=lnLF_Diag;
  scatter x=Pred y=Resid;
  refline 0 / axis=y;
run;

proc univariate data=lnLF_Diag normal;
  var Resid;
  qqplot Resid / normal(mu=est sigma=est);
run;

proc sgplot data=lnLF_Diag;
  histogram Resid;
  density Resid;
run;






/*==================
	     LFHF
  ==================*/

proc sort data=hrv_long;  
    by _Imputation_ Subject_ID Time;
run;

proc glm data=hrv_long;
    by _Imputation_;

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

/*Preliminary r.effects */
/*Explore Residuals to: 
? Does variance increase with condition or time?
? Does variance differ by group?
*/
proc print data=lnLFHF_olsresid(obs=10);run;

proc sgplot data=lnLFHF_olsresid;
	by _imputation_;
    scatter x=Time y=r_lnLFHF / transparency=0.4;
    loess x=Time y=r_lnLFHF;
    title "OLS Residuals vs Time for lnLFHF";
run;

proc sgplot data=lnLFHF_olsresid;
	by _imputation_;
    vbox r_lnLFHF / category=REST_VS_STRESS;
    title "lnLFHF Residual Distribution by Condition (Rest vs Stress)";
run;

proc sgplot data=lnLFHF_olsresid;
    by _imputation_;
    vbox r_lnLFHF / category=Group;
    title "lnLFHF Residual Distribution by PMA Group";
run;

proc sgplot data=lnLFHF_olsresid;
	by _imputation_;
    series x=Time y=r_lnLFHF / group=Subject_ID transparency=0.8;
    title "lnLFHF Residual Profiles by Subject";
run;

data resvar;
    set lnLFHF_olsresid;
	by _imputation_;
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
    by _Imputation_;
    class Subject_ID Time;
    model r_lnLFHF = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

proc mixed data=lnLFHF_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnLFHF = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

/*2) CS*/
proc mixed data=lnLFHF_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnLFHF = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=cs;
    ods output FitStatistics=CS_AIC;
run;

proc mixed data=lnLFHF_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnLFHF = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=cs;
    ods output FitStatistics=CS_AIC;
run;

/*3) Simple*/
proc mixed data=lnLFHF_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnLFHF = ;
    random intercept REST_VS_STRESS/ subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;

proc mixed data=lnLFHF_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnLFHF = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;

/*AR(1) wins*/

/*Reduce r.eff structure*/
proc mixed data=hrv_long nobound method=reml;
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
    repeated Time / subject=Subject_ID type=AR(1);

run;

proc mixed data=hrv_long nobound method=reml;
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
    repeated Time / subject=Subject_ID type=AR(1);

run;
/*Random intercept with AR(1)*/

/*==============================
	Mean structure reduction
  ==============================*/

proc mixed data=hrv_long nobound method=ml;
    where _Imputation_ = 5;
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
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_full
               Type3=Type3_full;
run;

/*Group*Gender is highly not significant, drop it*/

proc mixed data=hrv_long nobound method=ml;
    where _Imputation_ = 5;
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
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_noRG;
run;


/*REST_VS_STRESS*Gender is highly significant, just for trying, reduce more*/
proc mixed data=hrv_long nobound method=ml;
    where _Imputation_ = 5;
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
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_noRG;
run;

/*The test is significant, so the model with rest_vs_Stress*gender is important*/


/*==========Final model============*/

/* 1. Final mixed model: lnLFHF ~ Group * REST_VS_STRESS + REST_VS_STRESS * Gender
      Random intercept + AR(1) */


proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnLFHF =
        Group
        REST_VS_STRESS
        Gender
		Group*REST_VS_STRESS
		REST_VS_STRESS*Gender
        / solution ddfm=kr; 
    random intercept  / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = lnLFHF_SolutionF;
run;

/* Keep only the estimable rows (no reference levels) */
data lnLFHF_SolutionF;
    set lnLFHF_SolutionF;
    where StdErr ne .;  * drop rows like Group=1 with StdErr=. ;
run;

proc print data=lnLFHF_SolutionF(obs=20);
run;


data lnLFHF_SolutionF2;
    set lnLFHF_SolutionF;
    length Parameter $40;

    /* create a single label for each parameter */
    if Effect = 'Intercept' then Parameter = 'Intercept';
    else if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
    else if Effect = 'REST_VS_STRESS' then Parameter = 'Stress_vs_rest';
    else if Effect = 'Group*REST_VS_STRESS' then Parameter = 'Interaction_GroupxStress';
	else if Effect = 'REST_VS_STRES*Gender' then Parameter = 'Interaction_StressxGender';
    else if Effect = 'Gender' then Parameter = 'Female_vs_male';

    keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=lnLFHF_SolutionF2;
    by Parameter _Imputation_;
run;

proc print data=lnLFHF_SolutionF2(firstobs=20 obs=40);
run;

proc sort data=lnLFHF_SolutionF2;
    by Parameter _Imputation_;
run;

proc mianalyze data=lnLFHF_SolutionF2;
    by Parameter;          * one pooled result per parameter label;
    modeleffects Estimate; * variable that holds the coefficient;
    stderr StdErr;         * variable that holds the SE;
run;






/*Drop group*stress, keep stress*gender as it is highly significant

				lnLFHF ~ Group + rest_vs_Stress*Gender*/


proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
    class Subject_ID Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m') Time;

    model lnLFHF =
        Group
        REST_VS_STRESS
        Gender
/*		Group*REST_VS_STRESS*/
		REST_VS_STRESS*Gender
        / solution ddfm=kr; 
    random intercept  / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output SolutionF = lnLFHF_SolutionF_;
run;

/* Keep only the estimable rows (no reference levels) */
data lnLFHF_SolutionF_;
    set lnLFHF_SolutionF_;
    where StdErr ne .;  * drop rows like Group=1 with StdErr=. ;
run;

proc print data=lnLFHF_SolutionF_(obs=20);
run;


data lnLFHF_SolutionF_2;
    set lnLFHF_SolutionF_;
    length Parameter $40;

    /* create a single label for each parameter */
    if Effect = 'Intercept' then Parameter = 'Intercept';
    else if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
    else if Effect = 'REST_VS_STRESS' then Parameter = 'Stress_vs_rest';
/*    else if Effect = 'Group*REST_VS_STRESS' then Parameter = 'Interaction_GroupxStress';*/
	else if Effect = 'REST_VS_STRES*Gender' then Parameter = 'Interaction_StressxGender';
    else if Effect = 'Gender' then Parameter = 'Female_vs_male';

    keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=lnLFHF_SolutionF_2;
    by Parameter _Imputation_;
run;

proc print data=lnLFHF_SolutionF_2(firstobs=20 obs=40);
run;

proc sort data=lnLFHF_SolutionF_2;
    by Parameter _Imputation_;
run;

proc mianalyze data=lnLFHF_SolutionF_2;
    by Parameter;          * one pooled result per parameter label;
    modeleffects Estimate; * variable that holds the coefficient;
    stderr StdErr;         * variable that holds the SE;
run;




/*Model assumptions*/
proc sort data=hrv_long(where=(_Imputation_=1)) out=hrv_imp1;
  by Subject_ID Time;
run;

proc glimmix data=hrv_imp1 nobound method=rspl;
  class Subject_ID Time Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

  model lnLFHF =
        Group
        REST_VS_STRESS
        Gender
        Group*REST_VS_STRESS
		Gender*REST_VS_STRESS

        / solution ddfm=kr dist=normal link=identity;

  random intercept / subject=Subject_ID;

   /*AR(1) within subject over Time*/ 
  random _residual_ / subject=Subject_ID type=ar(1);

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












/*lnSCL*/

/*The questions that can be answered for lnSCL:
1- Is there group effect? */

proc import datafile='.../HRV/hrv_fcs_imputed_Long.xlsx'
dbms=xlsx
out=work.hrv_long
replace;
run;


proc glm data=hrv_long;
    where _Imputation_ = 1;

    class Group(ref='0') Gender(ref='m');

    model lnSCL =
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



/*Simple*/
proc mixed data=lnSCL_olsresid;
    class Subject_ID Time;
    model r_lnSCL = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=simple;
    ods output FitStatistics=Simple_AIC;
run;


/*Choose mean structure*/

proc mixed data=hrv_long nobound method=ML;
	where _imputation_ = 1;
    class Subject_ID Group(ref='0') Gender(ref='m');
    model lnSCL = Group*Gender / solution;
    random intercept / subject=Subject_ID;
run;

proc mixed data=hrv_long nobound method=ML;
	where _imputation_ = 1;
    class Subject_ID Group(ref='0') Gender(ref='m');
    model lnSCL = Group Gender / solution;
    random intercept / subject=Subject_ID;
run;



/*Final model: lnSCL ~ Group + Gender*/
proc mixed data=hrv_long nobound method=REML;
    where _Imputation_ = 5;

    class Subject_ID
          Group (ref='0')
          Gender (ref='m');

    model lnSCL = Group Gender / solution cl;

    random intercept / subject=Subject_ID;

    ods output SolutionF = lnSCL_SolutionF;
run;




/*Model assumptions*/
proc sort data=hrv_long(where=(_Imputation_=1)) out=hrv_imp1;
  by Subject_ID Time;
run;

proc glimmix data=hrv_imp1 nobound method=rspl;
  class Subject_ID Time Group(ref='0') REST_VS_STRESS(ref='0') Gender(ref='m');

  model lnSCL =
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





















/*=========================
		   MNAR
 ==========================*/

/*Merge pre-imputing wide data with the imputed (wide) data to be able to identify the originally missing observations*/
proc import datafile='.../hrv_imputed_Fcs.xlsx'
dbms=xlsx                                   
out=work.imputed_data                     
replace;                                    
run;


proc import datafile='.../HRV_pre_impute_wide.xlsx'
  out=work.pre_impute_wide
  dbms=xlsx
  replace;
run;



/*Create “originally missing” flags:
Prepare original dataset with “_orig” names, to be able to distinguish them from the imputed ones*/

proc sort data=hrv_pre_impute;  by Subject_ID; run;
proc sort data=imputed_data;  by Subject_ID; run;


data hrv_imputed_ln_flagged;
  merge
    imputed_data (in=inImp)
    hrv_pre_impute (in=inOrig
      rename = (
        /* RMSSD */
        lnRMSSD_msec_T1 = lnRMSSD_msec_T1_orig
        lnRMSSD_msec_T2 = lnRMSSD_msec_T2_orig
        lnRMSSD_msec_T3 = lnRMSSD_msec_T3_orig
        lnRMSSD_msec_T4 = lnRMSSD_msec_T4_orig
        lnRMSSD_msec_T5 = lnRMSSD_msec_T5_orig
        lnRMSSD_msec_T6 = lnRMSSD_msec_T6_orig

        /* SDNN */
        lnSDNN_msec_T1  = lnSDNN_msec_T1_orig
        lnSDNN_msec_T2  = lnSDNN_msec_T2_orig
        lnSDNN_msec_T3  = lnSDNN_msec_T3_orig
        lnSDNN_msec_T4  = lnSDNN_msec_T4_orig
        lnSDNN_msec_T5  = lnSDNN_msec_T5_orig
        lnSDNN_msec_T6  = lnSDNN_msec_T6_orig

        /* RSA */
        lnRSA0_msec_T1  = lnRSA0_msec_T1_orig
        lnRSA0_msec_T2  = lnRSA0_msec_T2_orig
        lnRSA0_msec_T3  = lnRSA0_msec_T3_orig
        lnRSA0_msec_T4  = lnRSA0_msec_T4_orig
        lnRSA0_msec_T5  = lnRSA0_msec_T5_orig
        lnRSA0_msec_T6  = lnRSA0_msec_T6_orig

        /* SCL */
        lnAverage_SCL_uS_T1 = lnAverage_SCL_uS_T1_orig
        lnAverage_SCL_uS_T2 = lnAverage_SCL_uS_T2_orig

        /* LF / HF */
        lnLF_ms_T1 = lnLF_ms_T1_orig
        lnLF_ms_T3 = lnLF_ms_T3_orig
        lnLF_ms_T4 = lnLF_ms_T4_orig
        lnLF_ms_T6 = lnLF_ms_T6_orig

        lnHF_ms_T1 = lnHF_ms_T1_orig
        lnHF_ms_T3 = lnHF_ms_T3_orig
        lnHF_ms_T4 = lnHF_ms_T4_orig
        lnHF_ms_T6 = lnHF_ms_T6_orig

        /* LFHF */
        lnLFHF_T1 = lnLFHF_T1_orig
        lnLFHF_T3 = lnLFHF_T3_orig
        lnLFHF_T4 = lnLFHF_T4_orig
        lnLFHF_T6 = lnLFHF_T6_orig

        /* PEP */
        PEP_msec_T1 = PEP_msec_T1_orig
        PEP_msec_T2 = PEP_msec_T2_orig
        PEP_msec_T3 = PEP_msec_T3_orig
        PEP_msec_T4 = PEP_msec_T4_orig
        PEP_msec_T5 = PEP_msec_T5_orig
        PEP_msec_T6 = PEP_msec_T6_orig
      )
    );
  by Subject_ID;
  if inImp;   /* keep all rows from imputed data (all _Imputation_) */

  /* ---------- Arrays with explicit variable lists ---------- */

  /* HRV variables */
  array orig_RMSSD[6] 
    lnRMSSD_msec_T1_orig lnRMSSD_msec_T2_orig lnRMSSD_msec_T3_orig
    lnRMSSD_msec_T4_orig lnRMSSD_msec_T5_orig lnRMSSD_msec_T6_orig;
  array Miss_RMSSD[6]
    Miss_lnRMSSD_T1 Miss_lnRMSSD_T2 Miss_lnRMSSD_T3
    Miss_lnRMSSD_T4 Miss_lnRMSSD_T5 Miss_lnRMSSD_T6;

  array orig_SDNN[6]
    lnSDNN_msec_T1_orig lnSDNN_msec_T2_orig lnSDNN_msec_T3_orig
    lnSDNN_msec_T4_orig lnSDNN_msec_T5_orig lnSDNN_msec_T6_orig;
  array Miss_SDNN[6]
    Miss_lnSDNN_T1 Miss_lnSDNN_T2 Miss_lnSDNN_T3
    Miss_lnSDNN_T4 Miss_lnSDNN_T5 Miss_lnSDNN_T6;

  array orig_RSA[6]
    lnRSA0_msec_T1_orig lnRSA0_msec_T2_orig lnRSA0_msec_T3_orig
    lnRSA0_msec_T4_orig lnRSA0_msec_T5_orig lnRSA0_msec_T6_orig;
  array Miss_RSA[6]
    Miss_lnRSA0_T1 Miss_lnRSA0_T2 Miss_lnRSA0_T3
    Miss_lnRSA0_T4 Miss_lnRSA0_T5 Miss_lnRSA0_T6;

  array orig_PEP[6]
    PEP_msec_T1_orig PEP_msec_T2_orig PEP_msec_T3_orig
    PEP_msec_T4_orig PEP_msec_T5_orig PEP_msec_T6_orig;
  array Miss_PEP[6]
    Miss_PEP_T1 Miss_PEP_T2 Miss_PEP_T3
    Miss_PEP_T4 Miss_PEP_T5 Miss_PEP_T6;

  /* SCL */
  array orig_SCL[2]
    lnAverage_SCL_uS_T1_orig lnAverage_SCL_uS_T2_orig;
  array Miss_SCL[2]
    Miss_lnSCL_T1 Miss_lnSCL_T2;

  /* LF / HF / LFHF at T1,3,4,6 */
  array orig_LF[4]
    lnLF_ms_T1_orig lnLF_ms_T3_orig lnLF_ms_T4_orig lnLF_ms_T6_orig;
  array Miss_LF[4]
    Miss_lnLF_T1 Miss_lnLF_T3 Miss_lnLF_T4 Miss_lnLF_T6;

  array orig_HF[4]
    lnHF_ms_T1_orig lnHF_ms_T3_orig lnHF_ms_T4_orig lnHF_ms_T6_orig;
  array Miss_HF[4]
    Miss_lnHF_T1 Miss_lnHF_T3 Miss_lnHF_T4 Miss_lnHF_T6;

  array orig_LFHF[4]
    lnLFHF_T1_orig lnLFHF_T3_orig lnLFHF_T4_orig lnLFHF_T6_orig;
  array Miss_LFHF[4]
    Miss_lnLFHF_T1 Miss_lnLFHF_T3 Miss_lnLFHF_T4 Miss_lnLFHF_T6;

  /* ---------- Fill flags: 1 if missing in original, 0 otherwise ---------- */
  do t = 1 to 6;
    Miss_RMSSD[t] = (orig_RMSSD[t] = .);
    Miss_SDNN[t]  = (orig_SDNN[t]  = .);
    Miss_RSA[t]   = (orig_RSA[t]   = .);
    Miss_PEP[t]   = (orig_PEP[t]   = .);
  end;

  do t = 1 to 2;
    Miss_SCL[t] = (orig_SCL[t] = .);
  end;

  do t = 1 to 4;
    Miss_LF[t]   = (orig_LF[t]   = .);
    Miss_HF[t]   = (orig_HF[t]   = .);
    Miss_LFHF[t] = (orig_LFHF[t] = .);
  end;

  drop t
       lnRMSSD_msec_T1_orig lnRMSSD_msec_T2_orig lnRMSSD_msec_T3_orig
       lnRMSSD_msec_T4_orig lnRMSSD_msec_T5_orig lnRMSSD_msec_T6_orig
       lnSDNN_msec_T1_orig  lnSDNN_msec_T2_orig  lnSDNN_msec_T3_orig
       lnSDNN_msec_T4_orig  lnSDNN_msec_T5_orig  lnSDNN_msec_T6_orig
       lnRSA0_msec_T1_orig  lnRSA0_msec_T2_orig  lnRSA0_msec_T3_orig
       lnRSA0_msec_T4_orig  lnRSA0_msec_T5_orig  lnRSA0_msec_T6_orig
       lnAverage_SCL_uS_T1_orig lnAverage_SCL_uS_T2_orig
       lnLF_ms_T1_orig lnLF_ms_T3_orig lnLF_ms_T4_orig lnLF_ms_T6_orig
       lnHF_ms_T1_orig lnHF_ms_T3_orig lnHF_ms_T4_orig lnHF_ms_T6_orig
       lnLFHF_T1_orig lnLFHF_T3_orig lnLFHF_T4_orig lnLFHF_T6_orig
       PEP_msec_T1_orig PEP_msec_T2_orig PEP_msec_T3_orig
       PEP_msec_T4_orig PEP_msec_T5_orig PEP_msec_T6_orig;
run;



proc print data=hrv_imputed_ln_flagged;
  by Subject_ID;
  var _Imputation_ PEP_msec_T1-PEP_msec_T6;
run;

proc print data=hrv_imputed_ln_flagged;
  by Subject_ID;
  var _Imputation_ lnRMSSD_msec_T1-lnRMSSD_msec_T6;
run;

proc print data=hrv_imputed_ln_flagged;
  where Subject_ID=101;
  var _Imputation_ lnSDNN_msec_T1-lnSDNN_msec_T6;
run;


proc print data=hrv_imputed_ln_flagged;
  by Subject_ID;
  var _Imputation_ lnRSA0_msec_T1-lnRSA0_msec_T6;
run;


proc print data=hrv_imputed_ln_flagged;
  where Subject_ID=101;
  var _Imputation_ lnHF_ms_T1 lnHF_ms_T3 lnHF_ms_T4 lnHF_ms_T6;
run;



proc export data=hrv_imputed_ln_flagged
  outfile='.../merged_imp_miss_indicator_SAS.xlsx'
  dbms=xlsx
  replace;
run;



/*================================
	  RESHAPE TO LONG FORMAT
 =================================*/

/* Use the SAS dataset that already has the Miss_* flags */
proc sort data=hrv_imputed_ln_flagged;
  by _Imputation_ Subject_ID;
run;

data hrv_long_imputed_miss_ind;
  set hrv_imputed_ln_flagged;
  by _Imputation_ Subject_ID;

  /* ---------- Arrays for values ---------- */
  array lnRMSSD_a[6]  lnRMSSD_msec_T1-lnRMSSD_msec_T6;
  array lnSDNN_a[6]   lnSDNN_msec_T1-lnSDNN_msec_T6;
  array lnRSA_a[6]    lnRSA0_msec_T1-lnRSA0_msec_T6;
  array PEP_a[6]      PEP_msec_T1-PEP_msec_T6;

  array lnSCL_a[2]    lnAverage_SCL_uS_T1-lnAverage_SCL_uS_T2;

  /* ---------- Arrays for missingness flags (wide) ---------- */
  array Miss_lnRMSSD_a[6] Miss_lnRMSSD_T1-Miss_lnRMSSD_T6;
  array Miss_lnSDNN_a[6]  Miss_lnSDNN_T1-Miss_lnSDNN_T6;
  array Miss_lnRSA_a[6]   Miss_lnRSA0_T1-Miss_lnRSA0_T6;
  array Miss_PEP_a[6]     Miss_PEP_T1-Miss_PEP_T6;

  array Miss_lnSCL_a[2]   Miss_lnSCL_T1-Miss_lnSCL_T2;

  /* Frequency-domain vars retained wide (T1,3,4,6 only) */
  retain lnHF_ms_T1 lnHF_ms_T3 lnHF_ms_T4 lnHF_ms_T6;
  retain lnLF_ms_T1 lnLF_ms_T3 lnLF_ms_T4 lnLF_ms_T6;
  retain lnLFHF_T1  lnLFHF_T3  lnLFHF_T4  lnLFHF_T6;

  /* and their missingness flags */
  retain Miss_lnHF_T1 Miss_lnHF_T3 Miss_lnHF_T4 Miss_lnHF_T6;
  retain Miss_lnLF_T1 Miss_lnLF_T3 Miss_lnLF_T4 Miss_lnLF_T6;
  retain Miss_lnLFHF_T1 Miss_lnLFHF_T3 Miss_lnLFHF_T4 Miss_lnLFHF_T6;

  /* ---------- Long reshaping ---------- */
  do Time = 1 to 6;

    /* ----- Mean structure vars + flags ----- */
    lnRMSSD       = lnRMSSD_a[Time];
    Miss_lnRMSSD  = Miss_lnRMSSD_a[Time];

    lnSDNN        = lnSDNN_a[Time];
    Miss_lnSDNN   = Miss_lnSDNN_a[Time];

    lnRSA         = lnRSA_a[Time];
    Miss_lnRSA    = Miss_lnRSA_a[Time];

    PEP_msec      = PEP_a[Time];
    Miss_PEP      = Miss_PEP_a[Time];

    /* ----- SCL (only T1,T2 observed by design) ----- */
    if Time in (1,2) then do;
      lnSCL      = lnSCL_a[Time];
      Miss_lnSCL = Miss_lnSCL_a[Time];
    end;
    else do;
      lnSCL      = .;
      Miss_lnSCL = .;   /* structurally missing (not even in pre-impute) */
    end;

    /* ----- Frequency-domain (T1,3,4,6 only) + flags ----- */
    select (Time);
      when (1) do;
        lnHF       = lnHF_ms_T1;
        lnLF       = lnLF_ms_T1;
        lnLFHF     = lnLFHF_T1;
        Miss_lnHF   = Miss_lnHF_T1;
        Miss_lnLF   = Miss_lnLF_T1;
        Miss_lnLFHF = Miss_lnLFHF_T1;
      end;
      when (2) do;
        lnHF       = .;
        lnLF       = .;
        lnLFHF     = .;
        Miss_lnHF   = .; /* structurally missing by design */
        Miss_lnLF   = .;
        Miss_lnLFHF = .;
      end;
      when (3) do;
        lnHF       = lnHF_ms_T3;
        lnLF       = lnLF_ms_T3;
        lnLFHF     = lnLFHF_T3;
        Miss_lnHF   = Miss_lnHF_T3;
        Miss_lnLF   = Miss_lnLF_T3;
        Miss_lnLFHF = Miss_lnLFHF_T3;
      end;
      when (4) do;
        lnHF       = lnHF_ms_T4;
        lnLF       = lnLF_ms_T4;
        lnLFHF     = lnLFHF_T4;
        Miss_lnHF   = Miss_lnHF_T4;
        Miss_lnLF   = Miss_lnLF_T4;
        Miss_lnLFHF = Miss_lnLFHF_T4;
      end;
      when (5) do;
        lnHF       = .;
        lnLF       = .;
        lnLFHF     = .;
        Miss_lnHF   = .; /* structurally missing by design */
        Miss_lnLF   = .;
        Miss_lnLFHF = .;
      end;
      when (6) do;
        lnHF       = lnHF_ms_T6;
        lnLF       = lnLF_ms_T6;
        lnLFHF     = lnLFHF_T6;
        Miss_lnHF   = Miss_lnHF_T6;
        Miss_lnLF   = Miss_lnLF_T6;
        Miss_lnLFHF = Miss_lnLFHF_T6;
      end;
      otherwise do;
        lnHF       = .;
        lnLF       = .;
        lnLFHF     = .;
        Miss_lnHF   = .;
        Miss_lnLF   = .;
        Miss_lnLFHF = .;
      end;
    end;

    /* ===== Condition variables ===== */
    REST_VS_STRESS = (Time in (2,3,4,5));   /* 1=Stress, 0=Rest (1,6) */
    label REST_VS_STRESS = "Condition: 1=Stress (T2–T5), 0=Rest (T1,T6)";
    format REST_VS_STRESS condition01_.;

    length PHASE $9;
    if Time=1 then PHASE='Baseline';
    else if Time in (2,3,4,5) then PHASE='Stress';
    else if Time=6 then PHASE='Recovery';
    label PHASE = "Phase (Baseline/Stress/Recovery)";

    output;
  end;

  keep _Imputation_ Subject_ID Gender Group Anxiety_mother Time
       REST_VS_STRESS PHASE
       lnRMSSD lnSDNN lnHF lnLF lnLFHF lnRSA lnSCL PEP_msec
       Miss_lnRMSSD Miss_lnSDNN Miss_lnHF Miss_lnLF Miss_lnLFHF
       Miss_lnRSA Miss_lnSCL Miss_PEP;
run;

proc export data=hrv_long_imputed_miss_ind
outfile='.../hrv_fcs_imputed_Long_miss_flagged.xlsx'
dbms=xlsx 
replace;
run;



proc import datafile='.../hrv_fcs_imputed_Long_miss_flagged.xlsx'
dbms=xlsx 
out=hrv_long_imputed_miss_ind
replace;
run;



/* Quick sanity checks */
proc freq data=hrv_long_imputed_miss_ind;
  tables _Imputation_*Time / nopercent norow nocol;
  tables Time*REST_VS_STRESS / nopercent norow nocol;
  tables Time*PHASE / nopercent norow nocol;
run;

proc means data=hrv_long_imputed_miss_ind n min mean max;
  class Time;
  var lnRMSSD lnSDNN lnHF lnLF lnLFHF lnRSA lnSCL PEP_msec;
  title "QC: Ranges by Time (long format)";
run;

/*Confirmation*/
/* Count nonmissing per Time for each variable to verify structural missingness */
proc means data=hrv_long_imputed_miss_ind n nmiss;
  class Time;
  var lnRMSSD lnSDNN lnHF lnLF lnLFHF lnRSA lnSCL PEP_msec;
  title "Nonmissing (N) and Missing (NMISS) by Time";
run;

/* Or a crisper table: one row per variable x time */
proc sql;
  create table nm_by_time as
  select Time,
         sum(lnRMSSD is not missing) as N_lnRMSSD,
         sum(lnSDNN  is not missing) as N_lnSDNN,
         sum(lnHF    is not missing) as N_lnHF,
         sum(lnLF    is not missing) as N_lnLF,
         sum(lnLFHF  is not missing) as N_lnLFHF,
         sum(lnRSA   is not missing) as N_lnRSA,
         sum(lnSCL   is not missing) as N_lnSCL,
         sum(PEP_msec is not missing) as N_PEP
  from hrv_long_imputed_miss_ind
  group by Time
  order by Time;
quit;


/*Compare with the long data from MAR to see if they are consistent*/
proc import datafile='.../hrv_fcs_imputed_Long.xlsx'
dbms=xlsx
out=work.hrv_long
replace;
run;

/*Perfect match*/


/*=======================================
		  MNAR delta-adjustment
  ======================================= */

/* Main macro: create MNAR-adjusted datasets */
%macro make_mnar_datasets(
    data      = hrv_long_imputed_miss_ind,
    outprefix = hrv_mnar
    );

  /* MNAR patterns: 1=stress only, 2=high only, 3=high×stress */
  %let patterns = stress high highstress;

  %do pi = 1 %to %sysfunc(countw(&patterns.));
    %let pattern = %scan(&patterns., &pi.);

    /* which indicator variable to use inside the DATA step */
    %if &pattern.=stress %then %let patt_var = I_stress;
    %else %if &pattern.=high %then %let patt_var = I_high;
    %else %if &pattern.=highstress %then %let patt_var = I_highstress;

    /* ---- Explicit delta grid (7 values) ---- */
    %do di = 1 %to 7;
      %if &di.=1 %then %let delta = -0.30;
      %else %if &di.=2 %then %let delta = -0.20;
      %else %if &di.=3 %then %let delta = -0.10;
      %else %if &di.=4 %then %let delta =  0;
      %else %if &di.=5 %then %let delta =  0.10;
      %else %if &di.=6 %then %let delta =  0.20;
      %else %if &di.=7 %then %let delta =  0.30;

      /* dataset name: e.g. hrv_mnar_stress_d1 ... d7 */
      %let outds = &outprefix._&pattern._d&di.;

      data &outds.;
        set &data.;

        /* pattern indicators (same for all outcomes) */
        I_stress     = (REST_VS_STRESS = 1);                      /* T2–T5, both groups      */
        I_high       = (Group = 1);                               /* high PMA, all times     */
        I_highstress = (Group = 1 and REST_VS_STRESS = 1);        /* high PMA under stress   */

        /* choose which indicator to use for this pattern */
        pattern_ind = &patt_var.;

        length MNAR_pattern $12;
        MNAR_pattern = "&pattern.";
        delta        = &delta.;   /* numeric value of this d */

        /* ---- Apply delta only to originally missing cells ---- */
        /* lnRMSSD */
        if Miss_lnRMSSD = 1 then lnRMSSD = lnRMSSD + &delta. * pattern_ind;

        /* lnSDNN */
        if Miss_lnSDNN  = 1 then lnSDNN  = lnSDNN  + &delta. * pattern_ind;

        /* lnRSA */
        if Miss_lnRSA   = 1 then lnRSA   = lnRSA   + &delta. * pattern_ind;

        
/*        if Miss_lnSCL   = 1 then lnSCL   = lnSCL   + &delta. * pattern_ind;*/

        /* Frequency-domain */
        if Miss_lnHF    = 1 then lnHF    = lnHF    + &delta. * pattern_ind;
        if Miss_lnLF    = 1 then lnLF    = lnLF    + &delta. * pattern_ind;
        if Miss_lnLFHF  = 1 then lnLFHF  = lnLFHF  + &delta. * pattern_ind;

        /* PEP (raw scale) */
        if Miss_PEP     = 1 then PEP_msec = PEP_msec + &delta. * pattern_ind;

      run;

    %end; /* delta loop (1–7) */

  %end; /* pattern loop */

%mend make_mnar_datasets;


/* ===== Run it (after deleting the old MNAR datasets if you want) ===== */
proc datasets lib=work nolist;
  delete hrv_mnar_:;
quit;

%make_mnar_datasets();


proc contents data=HRV_MNAR_HIGHSTRESS_D7(obs=20); run;







%put NOTE: DELTAS=&deltas.;
%put NOTE: NDeltas=%sysfunc(countw(&deltas.));





/*MACRO: 
- Loops over pattern ? {stress, high, highstress}

- Loops over d = 1…7 (corresponding to d grid)

- Fits your model for a chosen outcome

- Pools the estimates

- Stores them in one combined results dataset*/

%macro run_mnar_outcome(
    outcome = lnLFHF,              /* variable name */
    label   = lnLFHF,              /* label prefix for output datasets */
    extra_fx =REST_VS_STRESS*Gender,                    /* e.g. Gender*REST_VS_STRESS if needed */
    covtype = simple                 /* covariance structure for repeated */
    );

  %let patterns = stress high highstress;

  /* combined results across patterns & deltas */
  %let allres = &label._mnar_results;
  proc datasets lib=work nolist; delete &allres.; quit;

  %do pi = 1 %to %sysfunc(countw(&patterns.));
    %let pattern = %scan(&patterns., &pi.);

    %do di = 1 %to 7;
      %let ds_in = hrv_mnar_&pattern._d&di.;

      /* 1) Fit mixed model on this MNAR dataset */
      proc mixed data=&ds_in nobound method=reml;
        by _Imputation_;
        class Subject_ID 
              Group(ref='0') 
              REST_VS_STRESS(ref='0')      /* ref level handled by format, or use ref='Rest' */
              Gender(ref='m') 
              Time;

        model &outcome = Group Gender REST_VS_STRESS Group*REST_VS_STRESS &extra_fx
             / solution ddfm=kr;

        random intercept / subject=Subject_ID;
        repeated Time / subject=Subject_ID type=&covtype.;

        ods output SolutionF = &label._sol_&pattern._d&di.;
      run;

      /* 2) Keep estimable rows, create a clean Parameter label */
      data &label._sol_&pattern._d&di.;
        set &label._sol_&pattern._d&di.;
        where StdErr ne .;  /* drop reference levels */

        length Parameter $40;
        if Effect = 'Intercept'          then Parameter = 'Intercept';
        else if Effect = 'Group'         then Parameter = 'Group_high_vs_lowmed';
        else if Effect = 'REST_VS_STRESS' then Parameter = 'Stress_vs_rest';
        else if Effect = 'Gender'        then Parameter = 'Female_vs_male';
        else if Effect = 'Group*REST_VS_STRESS' then Parameter = 'Interaction_GroupxStress';
		else if Effect = 'REST_VS_STRES*Gender' then Parameter = 'Interaction_GenderxStress';
        /* extra_fx terms will appear with their Effect name, you can extend mapping if needed */

        /* Add identifiers for pattern & delta index */
        MNAR_pattern = "&pattern.";
        Delta_index  = &di.;
        keep _Imputation_ Parameter Estimate StdErr MNAR_pattern Delta_index;
      run;

      proc sort data=&label._sol_&pattern._d&di.;
        by Parameter _Imputation_;
      run;

      /* 3) Pool over imputations for this pattern × delta */
	 ods output ParameterEstimates=&label._pool_&pattern._d&di.;
	 proc mianalyze data=&label._sol_&pattern._d&di.;
     by Parameter;              * one pooled row per Parameter;
     modeleffects Estimate;     * coefficient;
     stderr StdErr;             * SE;
     run;
     ods output close;


      /* 4) Add pattern & delta info to pooled results and append to master */
      data &label._pool_&pattern._d&di.;
        set &label._pool_&pattern._d&di.;
        length Outcome $20;
        Outcome      = "&outcome.";
        MNAR_pattern = "&pattern.";
        Delta_index  = &di.;
      run;

      proc append base=&allres data=&label._pool_&pattern._d&di. force; run;

    %end; /* di loop */
  %end; /* pattern loop */

  /* Final combined result: one table for this outcome */
  proc sort data=&allres;
    by Outcome MNAR_pattern Delta_index Parameter;
  run;

%mend run_mnar_outcome;

/*lnRMSSD*/
%run_mnar_outcome(outcome=lnRMSSD, label=lnRMSSD, extra_fx=, covtype=AR(1));

proc datasets lib=work nolist;
  delete lnRMSSD_: ;
quit;


proc print data=LNRMSSD_MNAR_RESULTS; run;
/*Export (this is with interaction)*/
proc export data=LNRMSSD_MNAR_RESULTS 
outfile='.../lnRMSSD_MNAR_Interaction_Results.xlsx'
dbms=xlsx
replace;
run;



proc datasets lib=work nolist;
  delete lnSDNN: ;
quit;

/**/
%run_mnar_outcome(outcome=lnSDNN,  label=lnSDNN,  extra_fx=, covtype=AR(1));

proc export data=LNSDNN_MNAR_RESULTS
    outfile=".../lnSDNN_MNAR_Interaction_Results.xlsx"
    dbms=xlsx
    replace;
run;




%run_mnar_outcome(outcome=lnRSA,   label=lnRSA,   extra_fx=, covtype=AR(1));
/* etc. */
proc export data=LNRSA_MNAR_RESULTS
    outfile=".../lnRSA_MNAR_Interaction_Results.xlsx"
    dbms=xlsx
    replace;
run;


/**/


%run_mnar_outcome(outcome=PEP_msec,   label=PEP_msec,   extra_fx=, covtype=simple);
proc export data=PEP_msec_MNAR_RESULTS
    outfile=".../PEP_msec_MNAR_Interaction_Results.xlsx"
    dbms=xlsx
    replace;
run;



/*lnHF: Try same models as DL. Try same models as MI-MAR*/
%run_mnar_outcome(outcome=lnHF,   label=lnHF,   extra_fx=, covtype=simple);
proc export data=lnHF_MNAR_RESULTS
    outfile=".../lnHF_MNAR_Final_Interaction_Results.xlsx"
    dbms=xlsx
    replace;
run;




/*lnLFHF: Try same models as DL. Try same models as MI-MAR*/
%run_mnar_outcome(outcome=lnLFHF,   label=lnLFHF,   extra_fx=REST_VS_STRESS*Gender, covtype=simple);
proc export data=lnLFHF_MNAR_RESULTS
    outfile=".../lnLFHF_MNARFinal_interaction_results.xlsx"
    dbms=xlsx
    replace;
run;



/*lnLF*/
%run_mnar_outcome(outcome=lnLF,  label=lnLF,  extra_fx=, covtype=AR(1));
proc export data=LNLF_MNAR_RESULTS
    outfile=".../lnLF_MNAR_Interaction_Results.xlsx"
    dbms=xlsx
    replace;
run;



/*lnSCL: No need*/



/*Main effects model*/

%macro run_mnar_outcome(
    outcome = lnLFHF,          /* variable name */
    label   = lnLFHF,           /* label prefix for output datasets */
    extra_fx =REST_VS_STRESS*Gender,                    /* e.g. Gender*REST_VS_STRESS if needed */
    covtype = simple                 /* covariance structure for repeated */
    );

  %let patterns = stress high highstress;

  /* combined results across patterns & deltas */
  %let allres = &label._mnar_results;
  proc datasets lib=work nolist; delete &allres.; quit;

  %do pi = 1 %to %sysfunc(countw(&patterns.));
    %let pattern = %scan(&patterns., &pi.);

    %do di = 1 %to 7;
      %let ds_in = hrv_mnar_&pattern._d&di.;

      /* 1) Fit mixed model on this MNAR dataset */
      proc mixed data=&ds_in nobound method=reml;
        by _Imputation_;
        class Subject_ID 
              Group(ref='0') 
              REST_VS_STRESS(ref='0')      /* ref level handled by format, or use ref='Rest' */
              Gender(ref='m') 
              Time;

        model &outcome = Group Gender REST_VS_STRESS &extra_fx
             / solution ddfm=kr;

        random intercept / subject=Subject_ID;
        repeated Time / subject=Subject_ID type=&covtype.;

        ods output SolutionF = &label._sol_&pattern._d&di.;
      run;

      /* 2) Keep estimable rows, create a clean Parameter label */
      data &label._sol_&pattern._d&di.;
        set &label._sol_&pattern._d&di.;
        where StdErr ne .;  /* drop reference levels */

        length Parameter $40;
        if Effect = 'Intercept'          then Parameter = 'Intercept';
        else if Effect = 'Group'         then Parameter = 'Group_high_vs_lowmed';
        else if Effect = 'REST_VS_STRESS' then Parameter = 'Stress_vs_rest';
        else if Effect = 'Gender'        then Parameter = 'Female_vs_male';
/*        else if Effect = 'Group*REST_VS_STRESS' then Parameter = 'Interaction_GroupxStress';*/
		else if Effect = 'REST_VS_STRES*Gender' then Parameter = 'Interaction_GenderxStress';
        /* extra_fx terms will appear with their Effect name, you can extend mapping if needed */

        /* Add identifiers for pattern & delta index */
        MNAR_pattern = "&pattern.";
        Delta_index  = &di.;
        keep _Imputation_ Parameter Estimate StdErr MNAR_pattern Delta_index;
      run;

      proc sort data=&label._sol_&pattern._d&di.;
        by Parameter _Imputation_;
      run;

      /* 3) Pool over imputations for this pattern × delta */
	 ods output ParameterEstimates=&label._pool_&pattern._d&di.;
	 proc mianalyze data=&label._sol_&pattern._d&di.;
     by Parameter;              * one pooled row per Parameter;
     modeleffects Estimate;     * coefficient;
     stderr StdErr;             * SE;
     run;
     ods output close;


      /* 4) Add pattern & delta info to pooled results and append to master */
      data &label._pool_&pattern._d&di.;
        set &label._pool_&pattern._d&di.;
        length Outcome $20;
        Outcome      = "&outcome.";
        MNAR_pattern = "&pattern.";
        Delta_index  = &di.;
      run;

      proc append base=&allres data=&label._pool_&pattern._d&di. force; run;

    %end; /* di loop */
  %end; /* pattern loop */

  /* Final combined result: one table for this outcome */
  proc sort data=&allres;
    by Outcome MNAR_pattern Delta_index Parameter;
  run;

%mend run_mnar_outcome;

/*lnRMSSD*/
%run_mnar_outcome(outcome=lnRMSSD, label=lnRMSSD, extra_fx=, covtype=AR(1));

proc datasets lib=work nolist;
  delete lnRMSSD_: ;
quit;


proc print data=LNRMSSD_MNAR_RESULTS; run;
/*Export (this is without interaction)*/
proc export data=LNRMSSD_MNAR_RESULTS 
outfile='.../lnRMSSD_MNAR_Main_Results.xlsx'
dbms=xlsx
replace;
run;



proc datasets lib=work nolist;
  delete lnSDNN_: ;
quit;


/**/
%run_mnar_outcome(outcome=lnSDNN,  label=lnSDNN,  extra_fx=, covtype=AR(1));

proc export data=LNSDNN_MNAR_RESULTS
    outfile=".../lnSDNN_MNAR_Main_Results.xlsx"
    dbms=xlsx
    replace;
run;



proc datasets lib=work nolist;
  delete lnRSA_: ;
quit;


%run_mnar_outcome(outcome=lnRSA,   label=lnRSA,   extra_fx=, covtype=AR(1));
/* etc. */
proc export data=LNRSA_MNAR_RESULTS
    outfile=".../lnRSA_MNAR_Main_Results.xlsx"
    dbms=xlsx
    replace;
run;


/**/


proc datasets lib=work nolist;
  delete PEP_msec_: ;
quit;

%run_mnar_outcome(outcome=PEP_msec,   label=PEP_msec,   extra_fx=, covtype=simple);
proc export data=PEP_msec_MNAR_RESULTS
    outfile=".../PEP_msec_MNAR_Main_Results.xlsx"
    dbms=xlsx
    replace;
run;



proc datasets lib=work nolist;
  delete lnHF_: ;
quit;


/*lnHF: Try same models as DL. Try same models as MI-MAR*/
%run_mnar_outcome(outcome=lnHF,   label=lnHF,   extra_fx=, covtype=simple);
proc export data=lnHF_MNAR_RESULTS
    outfile=".../lnHF_MNAR_Final_Main_Results.xlsx"
    dbms=xlsx
    replace;
run;

%run_mnar_outcome(outcome=lnHF,   label=lnHF,   extra_fx=, covtype=AR(1));
proc export data=lnHF_MNAR_RESULTS
    outfile=".../lnHF_MNAR_Main_Results(2).xlsx"
    dbms=xlsx
    replace;
run;




/*lnHF after removing REST_VS_STRESS*Gender*/

/*lnLFHF: Try same models as DL. Try same models as MI-MAR*/
%run_mnar_outcome(outcome=lnLFHF,   label=lnLFHF,   extra_fx=REST_VS_STRESS*Gender, covtype=simple);
proc export data=lnLFHF_MNAR_RESULTS
    outfile="C.../lnLFHF_MNAR_Final_Main_Results(2).xlsx"
    dbms=xlsx
    replace;
run;



/*lnLF*/
%run_mnar_outcome(outcome=lnLF,  label=lnLF,  extra_fx=, covtype=AR(1));
proc export data=LNLF_MNAR_RESULTS
    outfile=".../lnLF_MNAR_Main_Results.xlsx"
    dbms=xlsx
    replace;
run;








