/*==============================================================
STEP 1 — Prepare clean pre-imputation HRV dataset
==============================================================*/

proc import datafile="C:\Users\Ahmed\OneDrive\Documents\Masters\Second_Year\Master Thesis Data Science\Prenatal stress study\data\HRV\HRV_Data__Wide_Format_.csv"
    out=hrv_wide
    dbms=csv
    replace;
    guessingrows=max;
run;

/*--------------------------------------------------------------
1. Drop all log10 variables (not needed)
--------------------------------------------------------------*/
data hrv_clean_base;
    set hrv_wide;
    drop lg10:;
run;

proc contents data=hrv_clean_base; run;

/*proc means data=hrv_clean_base n min mean max; */
/*var  */
/*run;*/


/*--------------------------------------------------------------
2. Replace lnRSA and lnSCL with correct transformations
   and keep only relevant physiological variables
--------------------------------------------------------------*/
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
    /* Drop HF/LF/LFHF at timepoints 2 and 5 */
    drop HF_ms_T2 LF_ms_T2 LFHF_T2 HF_ms_T5 LF_ms_T5 LFHF_T5;

    drop i;
run;

/*--------------------------------------------------------------
3. Quick QC check before imputation
--------------------------------------------------------------*/
proc means data=hrv_pre_impute n min mean max;
    var RMSSD_msec_T1 RMSSD_msec_T6 SDNN_msec_T1 SDNN_msec_T6 
        HF_ms_T1 HF_ms_T6 LF_ms_T1 LF_ms_T6 LFHF_T1 LFHF_T6
        RSA0_msec_T1 RSA0_msec_T6 Average_SCL_uS_T1 Average_SCL_uS_T2 PEP_msec_T1 PEP_msec_T6;
    title "QC: Check variable ranges before imputation";
run;

proc means data=hrv_pre_impute n min mean max;  /*check missingness at each time point*/
    var lnRSA0_msec_T1-lnRSA0_msec_T6 lnAverage_SCL_uS_T1-lnAverage_SCL_uS_T2
		lnSDNN_msec_T1-lnSDNN_msec_T6 lnRMSSD_msec_T1-lnRMSSD_msec_T6
		lnHF_ms_T1 lnHF_ms_T3 lnHF_ms_T4 lnHF_ms_T6 lnLF_ms_T1 lnLF_ms_T3 lnLF_ms_T4 lnLF_ms_T6 
		lnLFHF_T1 lnLFHF_T3 lnLFHF_T4 lnLFHF_T6
		PEP_msec_T1-PEP_msec_T6;
    title "QC: Check corrected log transformations (RSA, SCL) + other log transformations + PEP";
run;



/*==============================================================
STEP 2 — Multiple Imputation (Impute on Log Scale)
==============================================================*/
/* =========================
   FCS REGPMM on ln-scale
   (incl. PEP, lnSDNN5, lnRSA5, lnRMSSD5 with tailored block)
   ========================= */
proc sort data=hrv_pre_impute; by Subject_ID; run;

ods excel file="C:\Users\Ahmed\OneDrive\Documents\Masters\Second_Year\Master Thesis Data Science\Prenatal stress study\data\HRV\MI_Results_FCS_LN.xlsx"
          options(sheet_name="MI_FCS" embedded_titles='yes');

proc mi data=hrv_pre_impute
        out=hrv_imputed_ln
        seed=12345
        nimpute=10;    /* <-- removed ROUND= on proc line to silence CLASS warning */
  class Gender Group;

  /* Global PMM */
  fcs regpmm(/ k=5);

  /* --- Tailored fixes --- */
  /* lnRMSSD/SDNN/RSA T5: shrink predictors to high-coverage only */
  fcs regpmm( lnRMSSD_msec_T5 = Gender Group lnRMSSD_msec_T4 lnRMSSD_msec_T6);
   fcs regpmm( lnSDNN_msec_T5 = Gender Group lnSDNN_msec_T4 lnRMSSD_msec_T6);
   fcs regpmm( lnRSA0_msec_T5 = Gender Group lnRSA0_msec_T4 lnRSA0_msec_T6);


  /*PEP*/
  fcs regpmm( PEP_msec_T1 = Gender Group Anxiety_mother lnRMSSD_msec_T1 lnSDNN_msec_T1 );
  fcs regpmm( PEP_msec_T2 = Gender Group Anxiety_mother lnRMSSD_msec_T2 lnSDNN_msec_T2 PEP_msec_T1 );
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
  outfile='C:\Users\Ahmed\OneDrive\Documents\Masters\Second_Year\Master Thesis Data Science\Prenatal stress study\data\HRV\hrv_imputed_Fcs.xlsx'
  dbms=xlsx
  replace;
  sheet="FCS_imputed";
run;

proc import datafile='C:\Users\Ahmed\OneDrive\Documents\Masters\Second_Year\Master Thesis Data Science\Prenatal stress study\data\HRV\hrv_imputed_Fcs.xlsx'
dbms=xlsx                                   
out=work.imputed_data                     
replace;                                    
run;

/* Summary for PEP distribution (Imputation 1 only) */
proc means data=imputed_data n mean std min max skew kurt;
  where _Imputation_ = 1;
  var PEP_msec_T1-PEP_msec_T6;
  title "Descriptive statistics for PEP (Imputation 1)";
run;


/*==============================================================
Reshape imputed wide data to long format (LFHF at T1,T3,T4,T6)
==============================================================*/

/* Optional nice labels for outputs */
proc format;
  value condition01_ 0='Rest' 1='Stress';
  value $phase 'Baseline'='Baseline' 'Stress'='Stress' 'Recovery'='Recovery';
run;

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

  /* If names differ, adjust these: */
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

    /* 3-level phase (optional) */
    length PHASE $9;
    if Time=1 then PHASE='Baseline';
    else if Time in (2,3,4,5) then PHASE='Stress';
    else if Time=6 then PHASE='Recovery';
    label PHASE = "Phase (Baseline/Stress/Recovery)";

    output;
  end;

  keep _Imputation_ Subject_ID Gender Group Anxiety_mother Time
       REST_VS_STRESS PHASE
       lnRMSSD lnSDNN lnHF lnLF lnLFHF lnRSA lnSCL PEP_msec;
run;

/* Quick sanity checks */
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
  outfile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/HRV/hrv_fcs_imputed_Long.xlsx'
  dbms=xlsx
  replace;
  sheet="Fcs_imputed_long";
run;



proc import datafile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/HRV/hrv_fcs_imputed_Long.xlsx'
dbms=xlsx
out=work.hrv_long
replace;
run;
proc print data=hrv_long(obs=20);run;



/*======================================================
 					Exploration
 =======================================================*/

/*-----------------------------------------------------------
  Semi-variogram for one outcome and one imputation --> Helps in deciding for the 3 components of variance.
  Based on Molenberghs & Verbeke, Ch. 7 (slide 153)

  Remove your systematic trend, use residuals to explore correlation. 
  ex: given that the total variability is constant, u can assume CS if correlaton is the same.
-----------------------------------------------------------*/

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
  if y1 = y2;                             /* within-subject pairs */
  vario = (v1 - v2)**2 / 2;               /* semi-variance */
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






/*======================
       Modeling
=======================*/

/*=======
   RMSSD
  =======*/

/*Following Chapter 11:
11.3 Prelimnary mean structure*/


proc sort data=hrv_long;  
    by _Imputation_ Subject_ID Time;
run;

proc glm data=hrv_long;
    by _Imputation_;

    class Group REST_VS_STRESS Gender;

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

/*11.4 preliminary r.effects */
/*Explore Residuals to: 
? Does variance increase with condition or time?
? Does variance differ by group?
*/

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

/*11.5 Check Residuals Covariance*/


/*Explore residuals to see:
? Is correlation strong between nearby time points?
? Does correlation decay with time?
? Is correlation stronger under stress than rest?
? Are residuals “bunched” by subject?
? Which covariance structure is most appropriate?*/




/*After seeing the plots, the candidates for the covariance structure are: AR, CS, UN, let's try*/
/*1) UN*/
proc mixed data=rmssd_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnRMSSD = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=UN;
    ods output FitStatistics=UN_AIC;
run;

/*2) AR(1)*/
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
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=CS;
    ods output FitStatistics=CS_AIC;
run;



/*Random intercept with AR(1)*/


/*==============================
	Mean structure reduction
  ==============================*/

proc mixed data=hrv_long method=ml;
    where _Imputation_ = 2;
    class Subject_ID Group REST_VS_STRESS Gender Time;

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




proc mixed data=hrv_long method=ml;
    where _Imputation_ = 2;
    class Subject_ID Group REST_VS_STRESS Gender Time;

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
proc mixed data=hrv_long method=ml;
    where _Imputation_ = 2;
    class Subject_ID Group REST_VS_STRESS Gender Time;

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


/*Reduce more*/
proc mixed data=hrv_long method=ml;
    where _Imputation_ = 1;
    class Subject_ID Group REST_VS_STRESS Gender Time;

    model lnRMSSD =
        Group
        REST_VS_STRESS
        Gender
/*        Group*REST_VS_STRESS*/
/*        Group*Gender*/
        /* REST_VS_STRESS*Gender removed */
        / solution;

    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

    ods output FitStatistics=Fit_noRG;
run;
/*Do not forget to compare the last model with the first one*/
data LRT; p_val = 1 - probchi(0, 1);run;
proc print data=LRT; run;



/*Fit the final model*/
/* Final mixed model for lnRMSSD across 10 imputations */

/* 0. Sort data by imputation */
proc sort data=hrv_long;
    by _Imputation_;
run;

/* 1. Final mixed model: lnRMSSD ~ Group * REST_VS_STRESS + Gender
      Random intercept + AR(1) */
/* From your PROC MIXED */
proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
    class Subject_ID Group REST_VS_STRESS Gender Time;

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

proc sort data=RMSSD_Parms2;
    by Parameter _Imputation_;
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













/*=======
   SDNN
  =======*/

/*Following Chapter 11:
11.3 Prelimnary mean structure*/


proc sort data=hrv_long;  
    by _Imputation_ Subject_ID Time;
run;

proc glm data=hrv_long;
    by _Imputation_;

    class Group REST_VS_STRESS Gender;

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

/*11.4 preliminary r.effects */
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

/*11.5 Check Residuals Covariance*/


/*Explore residuals to see:
? Is correlation strong between nearby time points?
? Does correlation decay with time?
? Is correlation stronger under stress than rest?
? Are residuals “bunched” by subject?
? Which covariance structure is most appropriate?*/


/*============================DO NOT FORGET TO SAVE THE PLOTS TO USE THEM IN THE THESIS IN SHAA ALLAH==========================*/



/*After seeing the plots, the candidates for the covariance structure are: AR, CS, UN, let's try*/
/*1) UN*/
proc mixed data=sdnn_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnSDNN = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=UN;
    ods output FitStatistics=UN_AIC;
run;

/*2) AR(1)*/
proc mixed data=sdnn_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnSDNN = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

/*3) CS*/
proc mixed data=sdnn_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnSDNN = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=CS;
    ods output FitStatistics=CS_AIC;
run;









/*Random intercept with AR(1)*/


/*==============================
	Mean structure reduction
  ==============================*/

proc mixed data=hrv_long method=ml;
    where _Imputation_ = 5;
    class Subject_ID Group REST_VS_STRESS Gender Time;

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




proc mixed data=hrv_long method=ml;
    where _Imputation_ = 5;
    class Subject_ID Group REST_VS_STRESS Gender Time;

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
proc mixed data=hrv_long method=ml;
    where _Imputation_ = 5;
    class Subject_ID Group REST_VS_STRESS Gender Time;

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


/*Reduce more*/
proc mixed data=hrv_long method=ml;
    where _Imputation_ = 1;
    class Subject_ID Group REST_VS_STRESS Gender Time;

    model lnRMSSD =
        Group
        REST_VS_STRESS
        Gender
/*        Group*REST_VS_STRESS*/
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
/* From your PROC MIXED */
proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
    class Subject_ID Group REST_VS_STRESS Gender Time;

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






/*================
		RSA
  ================*/

/*Following Chapter 11:
11.3 Prelimnary mean structure*/


proc sort data=hrv_long;  
    by _Imputation_ Subject_ID Time;
run;

proc glm data=hrv_long;
    by _Imputation_;

    class Group REST_VS_STRESS Gender;

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

/*11.4 preliminary r.effects */
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

/*11.5 Check Residuals Covariance*/


/*Explore residuals to see:
? Is correlation strong between nearby time points?
? Does correlation decay with time?
? Is correlation stronger under stress than rest?
? Are residuals “bunched” by subject?
? Which covariance structure is most appropriate?*/


/*============================DO NOT FORGET TO SAVE THE PLOTS TO USE THEM IN THE THESIS IN SHAA ALLAH==========================*/



/*After seeing the plots, the candidates for the covariance structure are: AR, CS, UN, let's try*/
/*1) UN*/
proc mixed data=rsa_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnRSA = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=UN;
    ods output FitStatistics=UN_AIC;
run;

/*2) AR(1)*/
proc mixed data=rsa_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnRSA = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

/*3) CS*/
proc mixed data=rsa_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnRSA = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=CS;
    ods output FitStatistics=CS_AIC;
run;





/*Random intercept with AR(1)*/


/*==============================
	Mean structure reduction
  ==============================*/

proc mixed data=hrv_long method=ml;
    where _Imputation_ = 10;
    class Subject_ID Group REST_VS_STRESS Gender Time;

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
    class Subject_ID Group REST_VS_STRESS Gender Time;

    model lnRSA =
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
proc mixed data=hrv_long method=ml;
    where _Imputation_ = 10;
    class Subject_ID Group REST_VS_STRESS Gender Time;

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
/* From your PROC MIXED */
proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
    class Subject_ID Group REST_VS_STRESS Gender Time;

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








































/*================
		PEP
  ================*/

/*Following Chapter 11:
11.3 Prelimnary mean structure*/


proc sort data=hrv_long;  
    by _Imputation_ Subject_ID Time;
run;

proc glm data=hrv_long;
    by _Imputation_;

    class Group REST_VS_STRESS Gender;

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

/*11.5 Check Residuals Covariance*/


/*Explore residuals to see:
? Is correlation strong between nearby time points?
? Does correlation decay with time?
? Is correlation stronger under stress than rest?
? Are residuals “bunched” by subject?
? Which covariance structure is most appropriate?*/


/*============================DO NOT FORGET TO SAVE THE PLOTS TO USE THEM IN THE THESIS IN SHAA ALLAH==========================*/



/*After seeing the plots, the candidates for the covariance structure are: AR, CS, UN, let's try*/
/*1) UN*/
proc mixed data=pep_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_pep = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=UN;
    ods output FitStatistics=UN_AIC;
run;

/*2) AR(1)*/
proc mixed data=pep_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_pep = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);
    ods output FitStatistics=AR1_AIC;
run;

/*3) CS*/
proc mixed data=rsa_olsresid;
    by _Imputation_;
    class Subject_ID Time;
    model r_lnRSA = ;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=CS;
    ods output FitStatistics=CS_AIC;
run;





/*Random intercept with AR(1)*/


/*==============================
	Mean structure reduction
  ==============================*/

proc mixed data=hrv_long method=ml;
    where _Imputation_ = 10;
    class Subject_ID Group REST_VS_STRESS Gender Time;

    model PEP_msec =
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
    class Subject_ID Group REST_VS_STRESS Gender Time;

    model PEP_msec =
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
proc mixed data=hrv_long method=ml;
    where _Imputation_ = 10;
    class Subject_ID Group REST_VS_STRESS Gender Time;

    model PEP_msec =
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
/* From your PROC MIXED */
proc mixed data=hrv_long nobound method=reml;
    by _Imputation_;
    class Subject_ID Group REST_VS_STRESS Gender Time;

    model PEP_msec =
        Group
        REST_VS_STRESS
        Group*REST_VS_STRESS
        Gender
        / solution ddfm=kr;
    random intercept / subject=Subject_ID;
    repeated Time / subject=Subject_ID type=AR(1);

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


