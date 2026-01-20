/* Step 1: Set the library path where the .sas7bdat file is stored */
/*libname mydata 'D:/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/';*/

proc datasets lib=mydata;
run;

DATA aas;
    SET mydata.LAST_CORTISOLAMYLASE;
RUN;

proc contents data=aas;
run;

DATA aas_clean;
    SET aas;
    DROP firstnam surname birthday ;
RUN;

proc means data=aas_clean NMISS N; run;


proc import datafile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/aas_namesremoved.xlsx'
	out=work.aas_data
	dbms=xlsx 
    replace;
run;

proc contents data=aas_data; run;

proc means data=aas_data n nmiss;
  var Amylase1 Amylase2 Amylase3 Amylase4 Amylase5 Amylase6 Amylase7 Amylase8 
	  Cortisol1 Cortisol2 Cortisol3 Cortisol4 Cortisol5 Cortisol6 Cortisol7 Cortisol8 
	  AACR_1 AACR_2 AACR_3 AACR_4 AACR_5 AACR_6 AACR_7 AACR_8
	  AmyOVERcort1 AmyOVERcort2 AmyOVERcort3 AmyOVERcort4 AmyOVERcort5 AmyOVERcort6 AmyOVERcort7 AmyOVERcort8;
run;



/*First: Change 999 to .*/
data aas_clean;
  set aas_data;

  array vars[*] Amylase1 Amylase2 Amylase3 Amylase4 Amylase5 Amylase6 Amylase7 Amylase8 
	  Cortisol1 Cortisol2 Cortisol3 Cortisol4 Cortisol5 Cortisol6 Cortisol7 Cortisol8 
	  AACR_1 AACR_2 AACR_3 AACR_4 AACR_5 AACR_6 AACR_7 AACR_8
	  AmyOVERcort1 AmyOVERcort2 AmyOVERcort3 AmyOVERcort4 AmyOVERcort5 AmyOVERcort6 AmyOVERcort7 AmyOVERcort8;

  do i = 1 to dim(vars);
    if vars[i] = 999 then vars[i] = .;
  end;

  drop i;
run;

proc means data=aas_clean n nmiss min max;
  var Amylase1 Amylase2 Amylase3 Amylase4 Amylase5 Amylase6 Amylase7 Amylase8 
	  Cortisol1 Cortisol2 Cortisol3 Cortisol4 Cortisol5 Cortisol6 Cortisol7 Cortisol8 
	  AACR_1 AACR_2 AACR_3 AACR_4 AACR_5 AACR_6 AACR_7 AACR_8
	  AmyOVERcort1 AmyOVERcort2 AmyOVERcort3 AmyOVERcort4 AmyOVERcort5 AmyOVERcort6 AmyOVERcort7 AmyOVERcort8;
run;

data aas_wide_ln;
  set aas_clean;

  array Amy[8]  Amylase1 Amylase2 Amylase3 Amylase4 Amylase5 Amylase6 Amylase7 Amylase8;
  array Cort[8] Cortisol1 Cortisol2 Cortisol3 Cortisol4 Cortisol5 Cortisol6 Cortisol7 Cortisol8;

  array lnAmy[8]  lnAmy_S1 lnAmy_S2 lnAmy_S3 lnAmy_S4 lnAmy_S5 lnAmy_S6 lnAmy_S7 lnAmy_S8;
  array lnCort[8] lnCort_S1 lnCort_S2 lnCort_S3 lnCort_S4 lnCort_S5 lnCort_S6 lnCort_S7 lnCort_S8;
  array lnAACR[8] lnAACR_S1 lnAACR_S2 lnAACR_S3 lnAACR_S4 lnAACR_S5 lnAACR_S6 lnAACR_S7 lnAACR_S8;

  do i = 1 to 8;
    if Amy[i]  > 0 then lnAmy[i]  = log(Amy[i]);  else lnAmy[i]  = .;
    if Cort[i] > 0 then lnCort[i] = log(Cort[i]); else lnCort[i] = .;

    if Amy[i] > 0 and Cort[i] > 0 then lnAACR[i] = log(Amy[i] / Cort[i]);
    else lnAACR[i] = .;
  end;

  drop i;
run;


data aas_wide_ln;
  set aas_wide_ln;

  if Group = 2 then Group = 0;
run;

data aas_long;
  set aas_wide_ln;

  array lnAmy[8]  lnAmy_S1 lnAmy_S2 lnAmy_S3 lnAmy_S4 lnAmy_S5 lnAmy_S6 lnAmy_S7 lnAmy_S8;
  array lnCort[8] lnCort_S1 lnCort_S2 lnCort_S3 lnCort_S4 lnCort_S5 lnCort_S6 lnCort_S7 lnCort_S8;
  array lnAACR[8] lnAACR_S1 lnAACR_S2 lnAACR_S3 lnAACR_S4 lnAACR_S5 lnAACR_S6 lnAACR_S7 lnAACR_S8;

  do Sample = 1 to 8;
    lnAmy  = lnAmy[Sample];
    lnCort = lnCort[Sample];
    lnAACR = lnAACR[Sample];

    /* keep row if at least one biomarker is observed */
    if not missing(lnAmy) or not missing(lnCort) then output;
  end;

  keep ID Group gender Sample lnAmy lnCort lnAACR;
run;

proc sort data=aas_long;
  by ID Sample;
run;

proc import datafile="C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/brainage_saliva_samples_times on stickers.xlsx"
    out=stick_raw
    dbms=xlsx
    replace;
    sheet="hupla";
    getnames=yes;
run;

proc sort data=stick_raw;
  by Number Date Time;
run;

data stick_long;
  set stick_raw;
  by Number;
  retain Sample;

  if first.Number then Sample = 0;
  Sample + 1;

  /* FORCE numeric ID */
  ID = input(Number, best32.);

  length Time_clean $8;
  Time_clean = scan(strip(put(Time, $50.)), -1, '=');
  if countc(Time_clean, ':') = 1 then Time_clean = cats(Time_clean, ':00');

  datetime = dhms(
      datepart(Date),
      hour(input(Time_clean, time8.)),
      minute(input(Time_clean, time8.)),
      second(input(Time_clean, time8.))
  );

  format datetime datetime20.;
  keep ID Sample datetime;
run;


data aas_long;
  set aas_long;
  ID = input(ID, best32.);
run;



proc sort data=aas_long;    by ID Sample; run;
proc sort data=stick_long; by ID Sample; run;

data aas_long_time;
  merge aas_long(in=a) stick_long(in=b);
  by ID Sample;
  if a;
run;

proc means data=aas_long_time n;
  class Sample;
  var datetime;
run;

proc print data=aas_long_time(obs=40); run;




proc export data=aas_log
	outfile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/aas_pre_impute.xlsx'
	dbms=xlsx 
    replace;
run;

proc import datafile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/aas_pre_impute.xlsx'
	dbms=xlsx
	out=work.aas_pre_impute 
    replace;
run;
























ods excel file="C:\Users\Ahmed\OneDrive\Documents\Masters\Second_Year\Master Thesis Data Science\Prenatal stress study\data\Cortisol&AAS\AAS_MI_Results_FCS_LN.xlsx"
          options(sheet_name="MI_FCS" embedded_titles='yes');

proc mi data=aas_pre_impute out=aas_imputed_ln seed=54321 nimpute=10;
  class gender Group;

  fcs regpmm;

  var
    gender Group
    lnAmy1 lnAmy2 lnAmy3 lnAmy4 lnAmy5 lnAmy6 lnAmy7 lnAmy8
    lnCort1 lnCort2 lnCort3 lnCort4 lnCort5 lnCort6 lnCort7 lnCort8;
run;


proc means data=aas_imputed_ln n nmiss min max;
var lnAmy1 lnAmy2 lnAmy3 lnAmy4 lnAmy5 lnAmy6 lnAmy7 lnAmy8
    lnCort1 lnCort2 lnCort3 lnCort4 lnCort5 lnCort6 lnCort7 lnCort8;
run;

/*169 is fully missing*/
proc print data=aas_imputed_ln;
where ID = 169; run;

data aas_imputed_ln;
  set aas_imputed_ln;
  if ID = 169 then delete;
run;

/*Check again*/
proc means data=aas_imputed_ln n nmiss min max;
var lnAmy1 lnAmy2 lnAmy3 lnAmy4 lnAmy5 lnAmy6 lnAmy7 lnAmy8
    lnCort1 lnCort2 lnCort3 lnCort4 lnCort5 lnCort6 lnCort7 lnCort8;
run;


/*Recalculate lnAACR 1-8*/
data aas_imputed_ln;
  set aas_imputed_ln;
  array amy[8] lnAmy1 lnAmy2 lnAmy3 lnAmy4 lnAmy5 lnAmy6 lnAmy7 lnAmy8;
  array cort[8] lnCort1 lnCort2 lnCort3 lnCort4 lnCort5 lnCort6 lnCort7 lnCort8;
  array aacr[8] lnAACR_1 lnAACR_2 lnAACR_3 lnAACR_4 lnAACR_5 lnAACR_6 lnAACR_7 lnAACR_8;

  do i = 1 to 8;
    if amy[i] ne . and cort[i] ne . then aacr[i] = amy[i] - cort[i];
  end;

  drop i;
run;

/*Check for the last time before reshaping*/
proc means data=aas_imputed_ln n nmiss min max;
var lnAmy1 lnAmy2 lnAmy3 lnAmy4 lnAmy5 lnAmy6 lnAmy7 lnAmy8
    lnCort1 lnCort2 lnCort3 lnCort4 lnCort5 lnCort6 lnCort7 lnCort8
	lnAACR_1 lnAACR_2 lnAACR_3 lnAACR_4 lnAACR_5 lnAACR_6 lnAACR_7 lnAACR_8;
run;

/*Reshape to long format*/
data aas_long;
  set aas_imputed_ln;

  array amy[8] lnAmy1 lnAmy2 lnAmy3 lnAmy4 lnAmy5 lnAmy6 lnAmy7 lnAmy8;
  array cort[8] lnCort1 lnCort2 lnCort3 lnCort4 lnCort5 lnCort6 lnCort7 lnCort8;
  array aacr[8] lnAACR_1 lnAACR_2 lnAACR_3 lnAACR_4 lnAACR_5 lnAACR_6 lnAACR_7 lnAACR_8;

  do Sample = 1 to 8;
    lnAmy = amy[Sample];
    lnCort = cort[Sample];
    lnAACR = aacr[Sample];

	/*Assume: Rest = sample 1 and 2, Stress = 3-4, Recovery = 5-8*/

	if Sample in (1, 2) then Phase = "Rest";
	else if Sample in (3, 4) then Phase = "Stress";
	else Phase = "Recovery";

    output;
  end;

  keep ID gender Group Phase _Imputation_ Sample lnAmy lnCort lnAACR;
run;


proc contents data=aas_long; run;

proc print data=aas_long(obs=20);run;

proc means data=aas_long n nmiss min mean median max; run; 


proc print data=aas_long;
var ID _Imputation_ gender Group;
where ID in (118, 120, 121);
run;

/*Since original data did not have gender recorded for IDs 118, 120 and 121; the model imputed these values. However, they are all females*/
data aas_long;
  set aas_long;

  if ID in (118, 120, 121) then Gender = "f";
run;

proc print data=aas_long;
var ID _Imputation_ gender Group;
where ID in (118, 120, 121);
run;

/*Also, in HRV data, groups are coded as follows: 0 for low, 1 for high group. However here: 1 for high, 2 for low! change to 0 = low, 1=high */
data aas_long;
  set aas_long;
  if Group = 2 then Group = 0;
run;


proc export data=aas_long 
	outfile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/aas_long_FcsImputed.xlsx'
	dbms=xlsx 
    replace;
run;



proc import datafile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/aas_long_FcsImputed.xlsx'
	dbms=xlsx
	out=work.aas_long 
    replace;
run;

proc contents data=aas_long; run;



/*###################################################################################################
										Merging datasets
	For each imputed observation, attach the real-world time at which that saliva sample was taken
  ###################################################################################################*/
proc import datafile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/aas_long_prepared_with_times_FIXED.xlsx'
	dbms=xlsx
	out=work.aas_long_prepared 
    replace;
run;


proc import datafile="C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/aas_metrics_log.xlsx"
    out=metrics
    dbms=xlsx
    replace;
    getnames=yes;
run;

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

proc print data=metrics_with_cov(obs=20); run;

proc export data=metrics_with_cov 
	outfile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/aas_metricsF.xlsx'
	dbms=xlsx 
    replace;
run;





/*Add lnAARC in both datasets*/
proc import datafile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/aas_long_prepared_with_times_FIXED.xlsx'
	dbms=xlsx
	out=work.aas_long_prepared 
    replace;
run;

data aas_long_prepared;
    set aas_long_prepared;

    /* log alpha-amylase / cortisol ratio */
    lnAACR = lnAmy - lnCort;
run;



proc import datafile="C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/aas_metricsF.xlsx"
    out=metrics
    dbms=xlsx
    replace;
    getnames=yes;
run;


data metrics;
    set metrics;

    /* baseline ratio (S2) */
    lnAACR_s2 = lnAmy_s2 - lnCort_s2;

    /* peak ratio (S4–S6) */
    lnAACR_peak46 = lnAmy_peak46 - lnCort_peak46;

    /* delta (log ratio of ratios) */
    lnAACR_delta = lnAACR_peak46 - lnAACR_s2;

    /* AUC ratio (log scale) */
    auc_lnAACR = auc_lnAmy - auc_lnCort;

    /* recovery slope ratio */
    slope_lnAACR = slope_lnAmy - slope_lnCort;
run;


data check_long;
    set aas_long_prepared;

    lnAACR_check = lnAmy - lnCort;
    diff_lnAACR  = lnAACR - lnAACR_check;
run;

proc means data=check_long n nmiss min max;
    var diff_lnAACR;
run;


proc sql;
    select _Imputation_,
           min(diff_lnAACR) as min_diff,
           max(diff_lnAACR) as max_diff
    from check_long
    group by _Imputation_;
quit;

proc means data=check_long n nmiss;
    var lnAmy lnCort lnAACR;
run;


data check_metrics;
    set metrics;

    lnAACR_s2_check    = lnAmy_s2 - lnCort_s2;
    diff_lnAACR_s2     = lnAACR_s2 - lnAACR_s2_check;

    lnAACR_peak46_check = lnAmy_peak46 - lnCort_peak46;
    diff_lnAACR_peak46  = lnAACR_peak46 - lnAACR_peak46_check;

    lnAACR_delta_check  = lnAACR_peak46_check - lnAACR_s2_check;
    diff_lnAACR_delta   = lnAACR_delta - lnAACR_delta_check;

    auc_lnAACR_check    = auc_lnAmy - auc_lnCort;
    diff_auc_lnAACR     = auc_lnAACR - auc_lnAACR_check;

    slope_lnAACR_check  = slope_lnAmy - slope_lnCort;
    diff_slope_lnAACR   = slope_lnAACR - slope_lnAACR_check;
run;



proc means data=check_metrics n nmiss min max;
    var diff_lnAACR_s2
        diff_lnAACR_peak46
        diff_lnAACR_delta
        diff_auc_lnAACR
        diff_slope_lnAACR;
run;


proc sql;
    select _Imputation_,
           max(abs(diff_lnAACR_s2))    as max_s2,
           max(abs(diff_lnAACR_peak46)) as max_peak,
           max(abs(diff_lnAACR_delta))  as max_delta
    from check_metrics
    group by _Imputation_;
quit;


proc corr data=check_long;
    var lnAACR lnAmy lnCort;
run;




data aasF_no_outliers;
  set aas_f;
  if ID in (106,165) and Sample = 8 then delete;
run;


/*======================================================
 					Exploration
 =======================================================*/

/*-----------------------------------------------------------
  Semi-variogram to decide for the 3 variance components. check Ch. 7

  Remove the systematic trend, use residuals to explore correlation. 
-----------------------------------------------------------*/
%macro semivariogram_cpt(
    ds      = aasF_no_outliers,
    idvar   = ID,
    timevar = time_from_CPT_min,
    yvar    = lnAACR
);

/* 1. CPT window only */
data _cpt;
  set &ds;
  where Sample >= 4 and Sample <= 8;
  keep &idvar &timevar &yvar;
run;

/* 2. Remove mean time trend */
proc glm data=_cpt noprint;
  model &yvar = &timevar;
  output out=_resid r=residual;
run; quit;

/* 3. Create within-subject pairs manually */
proc sort data=_resid; by &idvar &timevar; run;

data _pairs;
  set _resid;
  by &idvar;
  retain t_prev r_prev;

  if first.&idvar then do;
    t_prev = .;
    r_prev = .;
  end;
  else do;
    distance = abs(&timevar - t_prev);
    vario    = (residual - r_prev)**2 / 2;
    output;
  end;

  t_prev = &timevar;
  r_prev = residual;
run;

/* 4. Estimate total variance (between-subject approximation) */
proc means data=_resid noprint;
  var residual;
  output out=_totalvar var=totvar;
run;

data _null_;
  set _totalvar;
  call symputx("totvar", totvar);
run;

/* 5. LOESS smoothing */
proc loess data=_pairs;
  ods output ScoreResults=_vario_loess;
  model vario = distance;
  score data=_pairs;
run;

proc sort data=_vario_loess; by distance; run;

/* 6. Plot */
symbol1 c=red   v=dot  h=0.2;
symbol2 c=black i=join w=2;

axis1 label=(h=2 'Time lag (minutes)') minor=none;
axis2 label=(h=2 a=90 'Semi-variance') minor=none;

title h=3 "Semi-variogram of residuals (&yvar)";

proc gplot data=_vario_loess;
  plot vario*distance=1
       p_vario*distance=2 /
       overlay haxis=axis1 vaxis=axis2
       vref=&totvar lvref=3;
	   axis2 label=(h=2 a=90 'Semi-variance')
      value=(h=1.5)
      order=(0 to 2 by 0.5)
      minor=none;

run; quit;

%mend;


%semivariogram_cpt(
  ds=aasF_no_outliers,
  yvar=lnAACR
);














proc export data=metrics 
	outfile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/aas_metricsF.xlsx'
	dbms=xlsx 
    replace;
run;


proc export data=aas_long_prepared 
	outfile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/aas_long_prepared_with_times_FIXED.xlsx'
	dbms=xlsx 
    replace;
run;



proc import datafile="C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/aas_CPTmodel_prepared_observed.xlsx"
    out=aas_f
    dbms=xlsx
    replace;
    getnames=yes;
run;

proc means n nmiss std min max mean;
var lnCortisol lnAmy lnAACR; run;

proc means n nmiss min max mean;
var time_from_CPT_min; run;

/*First, exclude the <CPT samples*/
data aas_f;
set aas_f;
where Sample >= 4 and Sample <= 8;
run;
proc means n nmiss min max mean;
var time_from_CPT_min; run;

proc print data=aas_f(obs=20);run;
proc print data=aas_f;
  where ID in ("106","165");
run;

/*data without the two extreme time_From_CPT_min values*/
data aas_f_noextreme;
  set aas_f;
  where Sample >= 4 and Sample <= 8;
  if ID in (106,165) and Sample = 8 then delete;
run;

proc print data=aas_f_noextreme(obs=20);run;
proc print data=aas_f_noextreme;
  where ID in ("106","165");
run;


proc sort data=aas_f;  
    by ID Sample;
run;

/*Create time^2 and center to reduce collinearity*/

proc means data=aas_f noprint;
  var time_from_CPT_min;
  output out=_tmean mean=mean_t;
run;
proc print data=_tmean;run;

data aas_11_3;
  if _n_=1 then set _tmean;
  set aas_f;
  where Sample >= 4 and Sample <= 8;

  t_c  = time_from_CPT_min - mean_t;
  t_c2 = t_c*t_c;
run;

proc print data=aas_11_3(obs=30);run;



/*============================================
				   Modeling
  ============================================*/

/*=========================
		 lnCortisol
  =========================*/

/*Preliminary mean structure*/
/*Most elaborte OLS model*/
proc glm data=aas_11_3;
  class Group0(ref='0') gender(ref='m');
  model lnCortisol =
        t_c t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
/*        Group0*gender*t_c*/
/*        Group0*gender*t_c2*/
        / solution;
  output out=aas_olsresid r=r_lnCort;
run; quit;


proc print data=aas_olsresid(obs=20); run;
/*Even if the matrix is singular, OLS residuals are still valid. SAS computes residuals with a generalized inverse.
I am NOT interpreting the estimates, i ONLY need the residuals, so singularity should not be a problem at this stage.*/


/*11.4 preliminary r.effects */
/*Explore Residuals to: 
? Does variance increase time?
? Does variance differ by group/gender?
*/



proc sgplot data=aas_olsresid;
    scatter x=time_from_CPT_min y=r_lnCort / transparency=0.4;
    loess x=time_from_CPT_min y=r_lnCort;
    title "OLS Residuals vs Time for lnCortisol";
run;

proc sgplot data=aas_olsresid;
    vbox r_lnCort / category=gender;
    title "lnCortisol Residual Distribution by Gender";
run;

proc sgplot data=aas_olsresid;
    vbox r_lnCort / category=Group0;
    title "lnCortisol Residual Distribution by PMA Group";
run;

proc sgplot data=aas_olsresid;
    series x=time_from_CPT_min y=r_lnCort / group=ID transparency=0.8;
    title "lnCortisol Residual Profiles by Subject";
run;

data resvar;
    set aas_olsresid;
    r2 = r_lnCort * r_lnCort;
run;

proc sgplot data=resvar;

    scatter x=time_from_CPT_min y=r2 / transparency=0.5;
    loess x=time_from_CPT_min y=r2;
    title "lnCortisol Squared Residuals vs Time from CPT";
run;



/*11.5 Check Random slope(s) and Residuals Covariance*/

proc mixed data=aas_olsresid;
  class ID Group0(ref='0') gender(ref='m') Sample;

  model r_lnCort =;

  random intercept t_c t_c2/ subject=ID;
  repeated Sample / subject=ID type=AR(1);

  ods output FitStatistics=Fit_full
             Type3=Type3_full;
run;

proc mixed data=aas_olsresid;
  class ID Group0(ref='0') gender(ref='m') Sample;

  model r_lnCort =;

  random intercept t_c t_c2/ subject=ID;
  repeated Sample / subject=ID type=simple;

  ods output FitStatistics=Fit_full
             Type3=Type3_full;
run;

/*AR(1) is the chosen residual structure (Also supported by variogram and residual plots)*/


/*Reduce the random effects*/

proc mixed data=aas_11_3 nobound method=reml;
  class ID Group0(ref='0') gender(ref='m') Sample;

model lnCortisol =
        t_c t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
/*        Group0*gender*t_c*/
/*        Group0*gender*t_c2*/
        / solution ddfm=kr;

  random intercept t_c t_c2/ subject=ID;
  repeated / subject=ID type=AR(1);
run;


/*Not possible, reduce more*/
proc mixed data=aas_11_3 nobound method=reml;
  class ID Group0(ref='0') gender(ref='m') Sample;

model lnCortisol =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
/*        Group0*gender*t_c*/
/*        Group0*gender*t_c2*/
        / solution ddfm=kr;

  random intercept t_c/ subject=ID;
  repeated / subject=ID type=AR(1);
run;


/*Time slope is extremely small*/

proc mixed data=aas_11_3 nobound method=reml;
  class ID Group0(ref='0') gender(ref='m') Sample;

model lnCortisol =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
/*        Group0*gender*t_c*/
/*        Group0*gender*t_c2*/
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;

/*Both REML log-likelihood test and AIC support reduction to random intercept only*/

/*So we have: Random intercept model with AR(1) residuals structure*/






/*Final step: Reduction of mean structure (with ML)*/

proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnCortisol =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
/*        Group0*gender*t_c*/
/*        Group0*gender*t_c2*/
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;

proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnCortisol =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;


/*Drop Group0*gender*/

proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnCortisol =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;

/*LRT and AIC support reducing the model*/

/*Drop both t_c2*gender and t_c2 group*/
proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnCortisol =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c gender*t_c
/*        Group0*t_c2*/
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;

/*LRT and AIC support reducing the model*/

/*Drop t_c2*Group0*/
/*proc mixed data=aas_11_3 nobound method=ml;*/
/*class ID Group0(ref='0') gender(ref='m') Sample;*/
/**/
/*model lnCortisol =*/
/*        t_c */
/*		t_c2 */
/*        Group0 gender*/
/*        Group0*t_c gender*t_c*/
/*        / solution ddfm=kr;*/
/**/
/*  random intercept/ subject=ID;*/
/*  repeated / subject=ID type=AR(1);*/
/*run;*/

/*Since interactions of interest, and since interactions with time^2 are removed, and since time^2 is not significant, drop it*/
/*Proceed with centered time even if t_c2 is dropped for consistency with Amy and AACR*/

proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnCortisol =
        t_c  
        Group0 gender
        Group0*t_c gender*t_c
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;

/*Drop gender x time since groupxtime is a key interaction*/
proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnCortisol =
        t_c  
        Group0 gender
        Group0*t_c
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;
/*Compare the final model with the first model -> final model is supported*/



/*Final model: lnCortisol ~ Gender + centered time_from_CPT_min*PMA Group*/
proc mixed data=aas_11_3 nobound method=reml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnCortisol =
        t_c  
        Group0 gender
        Group0*t_c
        / solution cl ddfm=kr;

  random intercept/ subject=ID;
  repeated Sample/ subject=ID type=AR(1);
run;

/*Final main effects model: Gender + centered time_from_CPT_min + PMA Group*/
proc mixed data=aas_11_3 nobound method=reml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnCortisol =
        t_c  
        Group0 gender
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated Sample/ subject=ID type=AR(1);
run;



/*Now fit the two final models on excluded two extreme time_from_CPT_min data*/
data aas_11_3_noex;
  if _n_=1 then set _tmean;
  set aas_f_noextreme;
  where Sample >= 4 and Sample <= 8;
  t_c  = time_from_CPT_min - mean_t;
/*  t_c2 = t_c*t_c;*/
run;

proc print data=aas_11_3_noex(obs=20); run;
proc print data=aas_11_3_noex; where ID in ("106", "165"); run; 

/*Interaction model*/
proc mixed data=aas_11_3_noex nobound method=reml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnCortisol =
        t_c  
        Group0 gender
        Group0*t_c
        / solution cl ddfm=kr;

  random intercept/ subject=ID;
  repeated Sample/ subject=ID type=AR(1);
run;

/*opposite direction*/

/*Final main effects model: Gender + centered time_from_CPT_min + PMA Group*/



/*-----------------------
  	Model Assumptions
  -----------------------*/
  proc glimmix data=aas_11_3 nobound method=rspl;
  class ID Group0(ref='0') gender(ref='m') Sample;

  model lnCortisol =
        t_c 
        Group0 gender
        Group0*t_c
        / solution ddfm=kr dist=normal link=identity;

  random intercept / subject=ID;

/*   AR(1) within subject over Time */
  random _residual_ / subject=ID type=ar(1);

  output out=lnCort_Diag
         pred=Pred
         resid=Resid
         student=StuResid;
run;
proc print data=lnCort_Diag(obs=10); run;

proc sgplot data=lnCort_Diag;
  scatter x=Pred y=Resid;
  refline 0 / axis=y;
  title "Model Residuals vs Time for lnCortisol";
run;

proc univariate data=lnCort_Diag normal;
  var Resid;
  qqplot Resid / normal(mu=est sigma=est);
  title "QQ plot for lnCortisol Residuals";

run;

proc sgplot data=lnCort_Diag;
  histogram Resid;
  density Resid;
  title "lnCortisol Residuals Distribution";
run;


/*Check assumptions when excluding the two extremes*/
proc glimmix data=aas_11_3_noex nobound method=rspl;
  class ID Group0(ref='0') gender(ref='m') Sample;

  model lnCortisol =
        t_c 
        Group0 gender
        Group0*t_c
        / solution ddfm=kr dist=normal link=identity;

  random intercept / subject=ID;

/*   AR(1) within subject over Time */
  random _residual_ / subject=ID type=ar(1);

  output out=lnCort_Diag
         pred=Pred
         resid=Resid
         student=StuResid;
run;
proc print data=lnCort_Diag(obs=10); run;

proc sgplot data=lnCort_Diag;
  scatter x=Pred y=Resid;
  refline 0 / axis=y;
  title "Model Residuals vs Time for lnCortisol (excluding two extreme time values)";
run;

proc univariate data=lnCort_Diag normal;
  var Resid;
  qqplot Resid / normal(mu=est sigma=est);
  title "QQ plot for lnCortisol Residuals (excluding two extreme time values)";

run;

proc sgplot data=lnCort_Diag;
  histogram Resid;
  density Resid;
  title "lnCortisol Residuals Distribution (excluding two extreme time values)";
run;

/*Same, good*/



/*Now we can go back to original not centered time_from_CPT_min 
/**/
/*proc mixed data=aas_f nobound method=ml;*/
/*class ID Group0(ref='0') gender(ref='m') Sample;*/
/**/
/*model lnCortisol =*/
/*        time_from_CPT_min */
/*        Group0 gender*/
/*        Group0*time_from_CPT_min */
/*		gender*time_from_CPT_min*/
/*        / solution ddfm=kr;*/
/**/
/*  random intercept/ subject=ID;*/
/*  repeated / subject=ID type=AR(1);*/
/*run;*/
/**/
/**/
/**/
/**/
/*gender*time is highly not significant (no evidence that gender is a moderator), drop it*/
/**/
/*proc mixed data=aas_f nobound method=ml;*/
/*class ID Group0(ref='0') gender(ref='m') Sample;*/
/**/
/*model lnCortisol =*/
/*        time_from_CPT_min */
/*        Group0 gender*/
/*        Group0*time_from_CPT_min */
/*        / solution ddfm=kr;*/
/**/
/*  random intercept/ subject=ID;*/
/*  repeated / subject=ID type=AR(1);*/
/*run;*/
/**/
/*/*Compare the final model with the first model*/
/**/
/**/
/*proc mixed data=aas_11_3 nobound method=ml;*/
/*class ID Group0(ref='0') gender(ref='m') Sample;*/
/**/
/*model lnCortisol =*/
/*        t_c */
/*        Group0 gender*/
/*        Group0*t_c */
/*        / solution ddfm=kr;*/
/*
/*  random intercept/ subject=ID;*/
/*  repeated / subject=ID type=AR(1);*/
/*run;*/


/*Final Model: lnCortisol ~ Gender + time_from_CPT_min*PMA Group*/

/*Fit under REML*/



/*proc mixed data=aas_f nobound method=reml;*/
/*class ID Group0(ref='0') gender(ref='m') Sample;*/
/**/
/*model lnCortisol =*/
/*        time_from_CPT_min */
/*        Group0 gender*/
/*        Group0*time_from_CPT_min */
/*        / solution ddfm=kr;*/
/**/
/*  random intercept/ subject=ID;*/
/*  repeated / subject=ID type=AR(1);*/
/*run;*/


/*proc mixed data=aas_f_noextreme nobound method=reml;*/
/*class ID Group0(ref='0') gender(ref='m') Sample;*/
/**/
/*model lnCortisol =*/
/*        time_from_CPT_min */
/*        Group0 gender*/
/*        Group0*time_from_CPT_min */
/*        / solution ddfm=kr;*/
/**/
/*  random intercept/ subject=ID;*/
/*  repeated / subject=ID type=AR(1);*/
/*run;*/
/**/
/**/
/**/
/*proc mixed data=aas_f nobound method=reml;*/
/*  class ID Group0(ref='0') gender(ref='m');*/
/*  model lnCortisol =*/
/*        time_from_CPT_min*/
/*        Group0*/
/*        gender*/
/*        Group0*time_from_CPT_min*/
/*        / solution cl ddfm=kr;*/
/**/
/*  random intercept / subject=ID;*/
/*  repeated / subject=ID type=AR(1);*/
/*  run;*/
/**/




data aas_11_3_noex;
  if _n_=1 then set _tmean;
  set aas_f_noextreme;
  where Sample >= 4 and Sample <= 8;
  t_c  = time_from_CPT_min - mean_t;
  t_c2 = t_c*t_c;
run;

proc print data=aas_11_3(obs=20);run;
proc print data=aas_11_3; where ID in ("106", "165"); run;
proc print data=aas_11_3_noex(obs=20);run;

proc print data=aas_11_3_noex; where ID in ("106", "165"); run;


/*================
	   lnAmy
  ================*/

/*Preliminary mean structure*/

proc glm data=aas_11_3;
  class Group0(ref='0') gender(ref='m');
  model lnAmy =
        t_c t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
/*        Group0*gender*t_c*/
/*        Group0*gender*t_c2*/
        / solution;
  output out=aas_olsresid r=r_lnAmy;
run; quit;


proc print data=aas_olsresid(obs=20); run;
/*Even if the matrix is singular, OLS residuals are still valid. SAS computes residuals with a generalized inverse.
I am NOT interpreting the estimates, i ONLY need the residuals, so singularity should not be a problem at this stage.*/


/*11.4 preliminary r.effects */
/*Explore Residuals to: 
? Does variance increase time?
? Does variance differ by group/gender?
*/



proc sgplot data=aas_olsresid;
    scatter x=time_from_CPT_min y=r_lnAmy / transparency=0.4;
    loess x=time_from_CPT_min y=r_lnAmy;
    title "OLS Residuals vs Time for lnAmy";
run;

proc sgplot data=aas_olsresid;
    vbox r_lnAmy / category=gender;
    title "lnAmy Residuals Distribution by Gender";
run;

proc sgplot data=aas_olsresid;
    vbox r_lnAmy / category=Group0;
    title "lnAmy Residuals Distribution by PMA Group";
run;

proc sgplot data=aas_olsresid;
    series x=time_from_CPT_min y=r_lnAmy / group=ID transparency=0.8;
    title "lnAmy Residuals Profiles by Subject";
run;

data resvar;
    set aas_olsresid;
    r2 = r_lnAmy * r_lnAmy;
run;

proc sgplot data=resvar;

    scatter x=time_from_CPT_min y=r2 / transparency=0.5;
    loess x=time_from_CPT_min y=r2;
    title "lnAmy Squared Residuals vs Time from CPT";
run;




/*Reduce the random effects*/

proc mixed data=aas_11_3 nobound method=reml;
  class ID Group0(ref='0') gender(ref='m') Sample;

model lnCortisol =
        t_c t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
/*        Group0*gender*t_c*/
/*        Group0*gender*t_c2*/
        / solution ddfm=kr;

  random intercept t_c t_c2/ subject=ID;
  repeated / subject=ID type=AR(1);
run;


/*Not possible, reduce more*/
proc mixed data=aas_11_3 nobound method=reml;
  class ID Group0(ref='0') gender(ref='m') Sample;

model lnCortisol =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
        / solution ddfm=kr;

  random intercept t_c/ subject=ID;
  repeated / subject=ID type=AR(1);
run;


/*Time slope is extremely small*/

proc mixed data=aas_11_3 nobound method=reml;
  class ID Group0(ref='0') gender(ref='m') Sample;

model lnCortisol =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;

/*Both REML log-likelihood test and AIC support reduction to random intercept only*/



/*11.5 Check Random slope(s) and Residuals Covariance*/

proc mixed data=aas_olsresid;
  class ID Group0(ref='0') gender(ref='m') Sample;

  model r_lnAmy =;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);

  ods output FitStatistics=Fit_full
             Type3=Type3_full;
run;

proc mixed data=aas_olsresid;
  class ID Group0(ref='0') gender(ref='m') Sample;

  model r_lnAmy =;

  random intercept / subject=ID;
  repeated / subject=ID type=simple;

  ods output FitStatistics=Fit_full
             Type3=Type3_full;
run;

proc mixed data=aas_olsresid;
  class ID Group0(ref='0') gender(ref='m') Sample;

  model r_lnAmy =;

  random intercept/ subject=ID;
  repeated Sample / subject=ID type=CS;

  ods output FitStatistics=Fit_full
             Type3=Type3_full;
run;

/*AR(1) is the chosen residual structure (Also supported by variogram and residual plots)*/
/*So we have: Random intercept model with AR(1) residuals structure*/






/*Final step: Reduction of mean structure (with ML)*/

proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnAmy =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
/*        Group0*gender*t_c*/
/*        Group0*gender*t_c2*/
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;

/*Drop both t_c2*gender and t_c2group*/

proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnAmy =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c gender*t_c
		Group0*gender
/*		gender*t_c2*/
/*        Group0*t_c2 */
/*        Group0*gender*t_c*/
/*        Group0*gender*t_c2*/
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;
/*The LRT for dropping both time^2 interactions is significant and AIC worsens, so there is evidence that together they improve model fit.*/
/*So we are still here*/
proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnAmy =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
/*        Group0*gender*t_c*/
/*        Group0*gender*t_c2*/
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;

/*Drop both first order time interactions while keeping the higher order ones*/

proc mixed data=aas_11_3 nobound method=ml;
  class ID Group0(ref='0') gender(ref='m') Sample;

  model lnAmy =
        t_c 
        t_c2
        Group0 gender
        Group0*gender
        t_c2*Group0
        t_c2*gender
        / solution ddfm=kr;

  random intercept / subject=ID;
  repeated / subject=ID type=AR(1);
run;

/*Not significant LRT, supports dropping the first order. Just for checking, test dropping the higher order terms again*/
proc mixed data=aas_11_3 nobound method=ml;
  class ID Group0(ref='0') gender(ref='m') Sample;

  model lnAmy =
        t_c 
        t_c2
        Group0 gender
        Group0*gender
/*        t_c2*Group0*/
/*        t_c2*gender*/
        / solution ddfm=kr;

  random intercept / subject=ID;
  repeated / subject=ID type=AR(1);
run;
/*Nop, the model favors them. We are here:*/
proc mixed data=aas_11_3 nobound method=ml;
  class ID Group0(ref='0') gender(ref='m') Sample;

  model lnAmy =
        t_c2
        t_c 
        Group0 gender
        Group0*gender
        t_c2*Group0
        t_c2*gender
        / solution ddfm=kr;

  random intercept / subject=ID;
  repeated / subject=ID type=AR(1);
run;

/*Just for trying, drop group*gender*/
proc mixed data=aas_11_3 nobound method=ml;
  class ID Group0(ref='0') gender(ref='m') Sample;

  model lnAmy =
        t_c 
        t_c2
        Group0 gender
/*        Group0*gender*/
        t_c2*Group0
        t_c2*gender
        / solution ddfm=kr;

  random intercept / subject=ID;
  repeated / subject=ID type=AR(1);
run;
/*No, the final model is:*/
proc mixed data=aas_11_3 nobound method=ml;
  class ID Group0(ref='0') gender(ref='m') Sample;

  model lnAmy =
        t_c2
        t_c 
        Group0 gender
        Group0*gender
        t_c2*Group0
        t_c2*gender
        / solution ddfm=kr;

  random intercept / subject=ID;
  repeated / subject=ID type=AR(1);
run;



/*Final Model: lnAmy ~ centered time_from_CPT_min + centered time_from_CPT_min^2*PMA Group + centered time_from_CPT_min^2*Gender */

/*Fit under REML*/
proc mixed data=aas_11_3 nobound method=reml;
  class ID Group0(ref='0') gender(ref='m') Sample;

  model lnAmy =
        t_c2
        t_c 
        Group0 gender
        Group0*gender
        t_c2*Group0
        t_c2*gender
        / solution cl ddfm=kr;

  random intercept / subject=ID;
  repeated / subject=ID type=AR(1);
run;


/*Fit the same model on the noextreme data*/
proc mixed data=aas_11_3_noex nobound method=reml;
  class ID Group0(ref='0') gender(ref='m') Sample;

  model lnAmy =
        t_c2
        t_c 
        Group0 gender
        Group0*gender
        t_c2*Group0
        t_c2*gender
        / solution cl ddfm=kr;

  random intercept / subject=ID;
  repeated / subject=ID type=AR(1);
run;


/*Consistent*/


/*-----------------------
  	Model Assumptions
  -----------------------*/
  proc glimmix data=aas_11_3 nobound method=rspl;
  class ID Group0(ref='0') gender(ref='m') Sample;

 model lnAmy = 
        t_c2
        t_c 
        Group0 gender
        Group0*gender
        t_c2*Group0
        t_c2*gender
        / solution ddfm=kr dist=normal link=identity;

  random intercept / subject=ID;

/*   AR(1) within subject over Time */
  random _residual_ / subject=ID type=ar(1);

  output out=lnAmy_Diag
         pred=Pred
         resid=Resid
         student=StuResid;
run;
proc print data=lnAmy_Diag(obs=10); run;

proc sgplot data=lnAmy_Diag;
  scatter x=Pred y=Resid;
  refline 0 / axis=y;
  title "Model Residuals vs Time for lnAmy";
run;

proc univariate data=lnAmy_Diag normal;
  var Resid;
  qqplot Resid / normal(mu=est sigma=est);
  title "QQ plot for lnAmy Residuals";

run;

proc sgplot data=lnAmy_Diag;
  histogram Resid;
  density Resid;
  title "lnAmy Residuals Distribution";
run;


/*Check assumptions from the model with noextremes*/
proc glimmix data=aas_11_3_noex nobound method=rspl;
  class ID Group0(ref='0') gender(ref='m') Sample;

 model lnAmy = 
        t_c2
        t_c 
        Group0 gender
        Group0*gender
        t_c2*Group0
        t_c2*gender
        / solution ddfm=kr dist=normal link=identity;

  random intercept / subject=ID;

/*   AR(1) within subject over Time */
  random _residual_ / subject=ID type=ar(1);

  output out=lnAmy_Diag
         pred=Pred
         resid=Resid
         student=StuResid;
run;
proc print data=lnAmy_Diag(obs=10); run;

proc sgplot data=lnAmy_Diag;
  scatter x=Pred y=Resid;
  refline 0 / axis=y;
  title "Model Residuals vs Time for lnAmy";
run;

proc univariate data=lnAmy_Diag normal;
  var Resid;
  qqplot Resid / normal(mu=est sigma=est);
  title "QQ plot for lnAmy Residuals";

run;

proc sgplot data=lnAmy_Diag;
  histogram Resid;
  density Resid;
  title "lnAmy Residuals Distribution";
run;

/*Same*/




/*=================
	    AACR
  =================*/


/*Preliminary mean structure*/

proc glm data=aas_11_3;
  class Group0(ref='0') gender(ref='m');
  model lnAACR =
        t_c t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
        Group0*gender*t_c
        Group0*gender*t_c2
        / solution;
  output out=aas_olsresid r=r_lnAACR;
run; quit;


proc print data=aas_olsresid(obs=20); run;
/*Even if the matrix is singular, OLS residuals are still valid. SAS computes residuals with a generalized inverse.
I am NOT interpreting the estimates, i ONLY need the residuals, so singularity should not be a problem at this stage.*/


/*11.4 preliminary r.effects */
/*Explore Residuals to: 
? Does variance increase time?
? Does variance differ by group/gender?
*/



proc sgplot data=aas_olsresid;
    scatter x=time_from_CPT_min y=r_lnAACR / transparency=0.4;
    loess x=time_from_CPT_min y=r_lnAACR;
    title "OLS Residuals vs Time for lnAACR";
run;

proc sgplot data=aas_olsresid;
    vbox r_lnAACR / category=gender;
    title "lnAACR Residuals Distribution by Gender";
run;

proc sgplot data=aas_olsresid;
    vbox r_lnAACR / category=Group0;
    title "lnAACR Residuals Distribution by PMA Group";
run;

proc sgplot data=aas_olsresid;
    series x=time_from_CPT_min y=r_lnAACR / group=ID transparency=0.8;
    title "lnAACR Residuals Profiles by Subject";
run;

data resvar;
    set aas_olsresid;
    r2 = r_lnAACR * r_lnAACR;
run;

proc sgplot data=resvar;

    scatter x=time_from_CPT_min y=r2 / transparency=0.5;
    loess x=time_from_CPT_min y=r2;
    title "lnAACR Squared Residuals vs Time from CPT";
run;




/*Reduce the random effects*/

proc mixed data=aas_11_3 nobound method=reml;
  class ID Group0(ref='0') gender(ref='m') Sample;

model lnAACR =
        t_c t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
        Group0*gender*t_c
        Group0*gender*t_c2
        / solution ddfm=kr;

  random intercept t_c t_c2/ subject=ID;
  repeated / subject=ID type=AR(1);
run;


/*Not possible, reduce more*/
proc mixed data=aas_11_3 nobound method=reml;
  class ID Group0(ref='0') gender(ref='m') Sample;

model lnAACR =
        t_c t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
        Group0*gender*t_c
        Group0*gender*t_c2
        / solution ddfm=kr;

  random intercept t_c/ subject=ID;
  repeated / subject=ID type=AR(1);
run;


/*Still*/

proc mixed data=aas_11_3 nobound method=reml;
  class ID Group0(ref='0') gender(ref='m') Sample;

model lnAACR =
        t_c t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
        Group0*gender*t_c
        Group0*gender*t_c2
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;

/*Both REML log-likelihood test and AIC support reduction to random intercept only*/



/*11.5 Check Random slope(s) and Residuals Covariance*/

proc mixed data=aas_olsresid;
  class ID Group0(ref='0') gender(ref='m') Sample;

  model r_lnAACR =;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);

  ods output FitStatistics=Fit_full
             Type3=Type3_full;
run;

proc mixed data=aas_olsresid;
  class ID Group0(ref='0') gender(ref='m') Sample;

  model r_lnAACR =;

  random intercept / subject=ID;
  repeated / subject=ID type=simple;

  ods output FitStatistics=Fit_full
             Type3=Type3_full;
run;

proc mixed data=aas_olsresid;
  class ID Group0(ref='0') gender(ref='m') Sample;

  model r_lnAACR =;

  random intercept/ subject=ID;
  repeated Sample / subject=ID type=CS;

  ods output FitStatistics=Fit_full
             Type3=Type3_full;
run;

/*AR(1) is the chosen residual structure (Also supported by variogram and residual plots)*/
/*So we have: Random intercept model with AR(1) residuals structure*/







/*Final step: Reduction of mean structure (with ML)*/

proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnAACR =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
        Group0*gender*t_c
        Group0*gender*t_c2
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;


/*Drop Group0*gender*t_c2*/
proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnAACR =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
        Group0*gender*t_c
/*        Group0*gender*t_c2*/
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;


/*Drop Group0*gender*t_c*/
proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnAACR =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
/*        Group0*gender*t_c*/
/*        Group0*gender*t_c2*/
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;


/*Drop t_c2*gender */
proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnAACR =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 
/*		gender*t_c2*/
        Group0*gender
/*        Group0*gender*t_c*/
/*        Group0*gender*t_c2*/
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;


/*Drop gender*t_c*/

proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnAACR =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c 
/*		gender*t_c*/
        Group0*t_c2 
/*		gender*t_c2*/
        Group0*gender
/*        Group0*gender*t_c*/
/*        Group0*gender*t_c2*/
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;


/*Drop Group0*gender*/
proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnAACR =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c 
/*		gender*t_c*/
        Group0*t_c2 
/*		gender*t_c2*/
/*        Group0*gender*/
/*        Group0*gender*t_c*/
/*        Group0*gender*t_c2*/
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;

/*NO so:*/
proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnAACR =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c 
/*		gender*t_c*/
        Group0*t_c2 
/*		gender*t_c2*/
        Group0*gender
/*        Group0*gender*t_c*/
/*        Group0*gender*t_c2*/
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;



/*Final Model: lnAACR ~ time time^2 Group0 gender Group0^time Group*time^2 Group0*gender*/

proc mixed data=aas_11_3 nobound method=reml;
class ID Group0(ref='0') gender(ref='m') Sample;

model lnAACR =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c 
        Group0*t_c2 
        Group0*gender
        / solution cl ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;




/*-----------------------
  	Model Assumptions
  -----------------------*/
  proc glimmix data=aas_11_3 nobound method=rspl;
  class ID Group0(ref='0') gender(ref='m');

model lnAACR =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c 
        Group0*t_c2 
        Group0*gender
        / solution ddfm=kr dist=normal link=identity;

  random intercept / subject=ID;

/*   AR(1) within subject over Time */
  random _residual_ / subject=ID type=ar(1);

  output out=lnAACR_Diag
         pred=Pred
         resid=Resid
         student=StuResid;
run;
proc print data=lnAACR_Diag(obs=10); run;

proc sgplot data=lnAACR_Diag;
  scatter x=Pred y=Resid;
  refline 0 / axis=y;
  title "Model Residuals vs Time for lnAACR";
run;

proc univariate data=lnAACR_Diag normal;
  var Resid;
  qqplot Resid / normal(mu=est sigma=est);
  title "QQ plot for lnAACR Residuals";

run;

proc sgplot data=lnAACR_Diag;
  histogram Resid;
  density Resid;
  title "lnAACR Residuals Distribution";
run;



/*================================================
			Second model: Metrics data
  ================================================*/

proc import datafile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/aas_metrics_MI_fromLong.xlsx'
	out=work.aas_metricsF
	dbms=xlsx 
    replace;
run;

proc contents data=aas_metricsF; run;

proc glm data=aas_metricsF;
  by _Imputation_;
  class Group(ref='0') gender(ref='m');

  model lnCort_delta =
        Group
        gender
        Group*gender
        / solution;

  ods output ParameterEstimates=Delta_Cort_PE;
run;
quit;



data Delta_Cort_PE2;
  set Delta_Cort_PE;
  where StdErr ne .;

  length Parameter $40;

  if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
  else if Effect = 'gender' then Parameter = 'Female_vs_male';
  else if Effect = 'Group*gender' then Parameter = 'Interaction_GroupxGender';

  keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=Delta_Cort_PE2;
  by Parameter _Imputation_;
run;


proc mianalyze data=Delta_Cort_PE2;
  by Parameter;
  modeleffects Estimate;
  stderr StdErr;
run;





/*================*/
proc glm data=aas_metricsF;
  by _Imputation_;
  class Group(ref='0') gender(ref='m');

  model lnAmy_delta =
        Group
        gender
        Group*gender
        / solution;

  ods output ParameterEstimates=Delta_Amy_PE;
run;
quit;



data Delta_Amy_PE2;
  set Delta_Amy_PE;
  where StdErr ne .;

  length Parameter $40;

  if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
  else if Effect = 'gender' then Parameter = 'Female_vs_male';
  else if Effect = 'Group*gender' then Parameter = 'Interaction_GroupxGender';

  keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=Delta_Amy_PE2;
  by Parameter _Imputation_;
run;


proc mianalyze data=Delta_Amy_PE2;
  by Parameter;
  modeleffects Estimate;
  stderr StdErr;
run;







proc glm data=aas_metricsF;
  by _Imputation_;
  class Group(ref='0') gender(ref='m');

  model lnAACR_delta =
        Group
        gender
        Group*gender
        / solution;

  ods output ParameterEstimates=Delta_AACR_PE;
run;
quit;



data Delta_AACR_PE2;
  set Delta_AACR_PE;
  where StdErr ne .;

  length Parameter $40;

  if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
  else if Effect = 'gender' then Parameter = 'Female_vs_male';
  else if Effect = 'Group*gender' then Parameter = 'Interaction_GroupxGender';

  keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=Delta_AACR_PE2;
  by Parameter _Imputation_;
run;


proc mianalyze data=Delta_AACR_PE2;
  by Parameter;
  modeleffects Estimate;
  stderr StdErr;
run;



/*==============================*/
proc glm data=aas_metricsF;
  by _Imputation_;
  class Group(ref='0') gender(ref='m');

  model slope_lnCort =
        Group
        gender
        Group*gender
        / solution;

  ods output ParameterEstimates=Slope_Cort_PE;
run;
quit;



data Slope_Cort_PE2;
  set Slope_Cort_PE;
  where StdErr ne .;

  length Parameter $40;

  if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
  else if Effect = 'gender' then Parameter = 'Female_vs_male';
  else if Effect = 'Group*gender' then Parameter = 'Interaction_GroupxGender';

  keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=Slope_Cort_PE2;
  by Parameter _Imputation_;
run;


proc mianalyze data=Slope_Cort_PE2;
  by Parameter;
  modeleffects Estimate;
  stderr StdErr;
run;






proc glm data=aas_metricsF;
  by _Imputation_;
  class Group(ref='0') gender(ref='m');

  model slope_lnAmy =
        Group
        gender
        Group*gender
        / solution;

  ods output ParameterEstimates=Slope_Amy_PE;
run;
quit;



data Slope_Amy_PE2;
  set Slope_Amy_PE;
  where StdErr ne .;

  length Parameter $40;

  if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
  else if Effect = 'gender' then Parameter = 'Female_vs_male';
  else if Effect = 'Group*gender' then Parameter = 'Interaction_GroupxGender';

  keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=Slope_Amy_PE2;
  by Parameter _Imputation_;
run;


proc mianalyze data=Slope_Amy_PE2;
  by Parameter;
  modeleffects Estimate;
  stderr StdErr;
run;








proc glm data=aas_metricsF;
  by _Imputation_;
  class Group(ref='0') gender(ref='m');

  model slope_lnAACR =
        Group
        gender
        Group*gender
        / solution;

  ods output ParameterEstimates=Slope_AACR_PE;
run;
quit;



data Slope_AACR_PE2;
  set Slope_AACR_PE;
  where StdErr ne .;

  length Parameter $40;

  if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
  else if Effect = 'gender' then Parameter = 'Female_vs_male';
  else if Effect = 'Group*gender' then Parameter = 'Interaction_GroupxGender';

  keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=Slope_AACR_PE2;
  by Parameter _Imputation_;
run;


proc mianalyze data=Slope_AACR_PE2;
  by Parameter;
  modeleffects Estimate;
  stderr StdErr;
run;



/*-----------------------------------------*/
proc glm data=aas_metricsF;
  by _Imputation_;
  class Group(ref='0') gender(ref='m');

  model auc_lnCort =
        Group
        gender
        Group*gender
        / solution;

  ods output ParameterEstimates=auc_Cort_PE;
run;
quit;


data auc_Cort_PE2;
  set auc_Cort_PE;
  where StdErr ne .;

  length Parameter $40;

  if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
  else if Effect = 'gender' then Parameter = 'Female_vs_male';
  else if Effect = 'Group*gender' then Parameter = 'Interaction_GroupxGender';

  keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=auc_Cort_PE2;
  by Parameter _Imputation_;
run;


proc mianalyze data=auc_Cort_PE2;
  by Parameter;
  modeleffects Estimate;
  stderr StdErr;
run;







proc glm data=aas_metricsF;
  by _Imputation_;
  class Group(ref='0') gender(ref='m');

  model auc_lnAmy =
        Group
        gender
        Group*gender
        / solution;

  ods output ParameterEstimates=auc_Amy_PE;
run;
quit;


data auc_Amy_PE2;
  set auc_Amy_PE;
  where StdErr ne .;

  length Parameter $40;

  if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
  else if Effect = 'gender' then Parameter = 'Female_vs_male';
  else if Effect = 'Group*gender' then Parameter = 'Interaction_GroupxGender';

  keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=auc_Amy_PE2;
  by Parameter _Imputation_;
run;


proc mianalyze data=auc_Amy_PE2;
  by Parameter;
  modeleffects Estimate;
  stderr StdErr;
run;







proc glm data=aas_metricsF;
  by _Imputation_;
  class Group(ref='0') gender(ref='m');

  model auc_lnAACR =
        Group
        gender
        Group*gender
        / solution;

  ods output ParameterEstimates=auc_AACR_PE;
run;
quit;


data auc_AACR_PE2;
  set auc_AACR_PE;
  where StdErr ne .;

  length Parameter $40;

  if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
  else if Effect = 'gender' then Parameter = 'Female_vs_male';
  else if Effect = 'Group*gender' then Parameter = 'Interaction_GroupxGender';

  keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=auc_AACR_PE2;
  by Parameter _Imputation_;
run;


proc mianalyze data=auc_AACR_PE2;
  by Parameter;
  modeleffects Estimate;
  stderr StdErr;
run;




/*--------------------------------*/
proc glm data=aas_metricsF;
  by _Imputation_;
  class Group(ref='0') gender(ref='m');

  model lnCort_peak =
        Group
        gender
        Group*gender
        / solution;

  ods output ParameterEstimates=peak_Cort_PE;
run;
quit;


data peak_Cort_PE2;
  set peak_Cort_PE;
  where StdErr ne .;

  length Parameter $40;

  if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
  else if Effect = 'gender' then Parameter = 'Female_vs_male';
  else if Effect = 'Group*gender' then Parameter = 'Interaction_GroupxGender';

  keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=peak_Cort_PE2;
  by Parameter _Imputation_;
run;


proc mianalyze data=peak_Cort_PE2;
  by Parameter;
  modeleffects Estimate;
  stderr StdErr;
run;




proc glm data=aas_metricsF;
  by _Imputation_;
  class Group(ref='0') gender(ref='m');

  model lnAmy_peak =
        Group
        gender
        Group*gender
        / solution;

  ods output ParameterEstimates=peak_Amy_PE;
run;
quit;


data peak_Amy_PE2;
  set peak_Amy_PE;
  where StdErr ne .;

  length Parameter $40;

  if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
  else if Effect = 'gender' then Parameter = 'Female_vs_male';
  else if Effect = 'Group*gender' then Parameter = 'Interaction_GroupxGender';

  keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=peak_Amy_PE2;
  by Parameter _Imputation_;
run;


proc mianalyze data=peak_Amy_PE2;
  by Parameter;
  modeleffects Estimate;
  stderr StdErr;
run;





proc glm data=aas_metricsF;
  by _Imputation_;
  class Group(ref='0') gender(ref='m');

  model lnAACR_peak =
        Group
        gender
        Group*gender
        / solution;

  ods output ParameterEstimates=peak_AACR_PE;
run;
quit;


data peak_AACR_PE2;
  set peak_AACR_PE;
  where StdErr ne .;

  length Parameter $40;

  if Effect = 'Group' then Parameter = 'Group_high_vs_lowmed';
  else if Effect = 'gender' then Parameter = 'Female_vs_male';
  else if Effect = 'Group*gender' then Parameter = 'Interaction_GroupxGender';

  keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=peak_AACR_PE2;
  by Parameter _Imputation_;
run;


proc mianalyze data=peak_AACR_PE2;
  by Parameter;
  modeleffects Estimate;
  stderr StdErr;
run;





/*===================================================
					    MNAR 
  ===================================================*/


/*MAR assumption: missing values come from the same mean model as observed ones

MNAR says: What if the missing lnOutcome values come from a slightly shifted mean compared to the observed ones?*/


proc import datafile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/old/aas_long_prepared_with_times_FIXED.xlsx'
	dbms=xlsx
	out=work.aas_long_prepared 
    replace;
run;
proc import datafile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/aas_namesremoved.xlsx'
	out=work.aas_data
	dbms=xlsx 
    replace;
run;


/* Reshape to long for Cortisol and Amy only */
data aas_orig_long;
  set aas_data;
  array Cort[8] Cortisol1 Cortisol2 Cortisol3 Cortisol4 Cortisol5 Cortisol6 Cortisol7 Cortisol8;
  array Amy [8] Amylase1 Amylase2 Amylase3 Amylase4 Amylase5 Amylase6 Amylase7 Amylase8;

  do Sample = 1 to 8;
    Cortisol = Cort[Sample];
    Amylase  = Amy[Sample];
    output;
  end;

  keep ID Sample Cortisol Amylase;
run;

/* Flag originally-missing on ORIGINAL scale */
data aas_orig_long;
  set aas_orig_long;

  /* original missingness flags */
  Miss_orig_Cort = (Cortisol = 999 or Cortisol <= 0 or Cortisol = .);
  Miss_orig_Amy  = (Amylase  = 999 or Amylase  <= 0 or Amylase  = .);

  /* AACR depends on both */
  Miss_orig_AACR = (Miss_orig_Cort or Miss_orig_Amy);
run;


proc print data=aas_orig_long(obs=20); run;




proc sort data=aas_long_prepared;    by ID Sample; run;
proc sort data=aas_orig_long;  by ID Sample; run;

data aas_long_mi_flagged;
  merge aas_long_prepared(in=a) aas_orig_long(in=b);
  by ID Sample;
  if a;

  /* if someone exists in MI file but not in orig (e.g., split IDs), set missing flags to 0 */
  if b=0 then do;
    Miss_orig_Cort = 0;
    Miss_orig_Amy  = 0;
    Miss_orig_AACR = 0;
  end;
run;





proc print data=aas_long_mi_flagged(obs=20); run;

proc export data=aas_long_mi_flagged
	outfile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/aas_long_mi_flagged.xlsx'
	dbms=xlsx 
    replace;
run;


proc import datafile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/aas_long_mi_flagged.xlsx'
out=aas_long_mi_flagged
dbms=xlsx
replace;
run;


data aas_cort_mnar;
      set aas_long_mi_flagged;
      where Sample >= 4;
run;


proc sort data=aas_cort_mnar;  
    by _Imputation_ ID Sample;
run;


proc mixed data=aas_cort_mnar nobound method=reml;
    by _Imputation_;
    class ID Group(ref='0')gender(ref='m') Sample;

    model lnCort =
        Group
        min_from_s4
        gender
		Group*min_from_s4
        / solution cl ddfm=kr; 
    random intercept  / subject=ID;
    repeated Sample / subject=ID type=AR(1);

    ods output SolutionF = lnCor_solutions;
run;

/* Keep only the estimable rows (no reference levels) */
data lnCor_solutions;
    set lnCor_solutions;
    where StdErr ne .;  * drop rows like Group=1 with StdErr=. ;
run;

proc print data=lnCor_solutions(obs=20);
run;


data lnCor_solutions2;
    set lnCor_solutions;
    length Parameter $40;

    /* create a single label for each parameter */
    if Effect = 'Intercept' then Parameter = 'Intercept';
    else if Effect = 'Group' then Parameter = 'Group_high_vs_low';
    else if Effect = 'min_from_s4' then Parameter = 'Time_from_S4';
	else if Effect='min_from_s4*Group' then Parameter='Interaction_GroupxTime'
    else if Effect = 'gender' then Parameter = 'Female_vs_male';

    keep _Imputation_ Parameter Estimate StdErr;
run;

proc sort data=lnCor_solutions2;
    by Parameter _Imputation_;
run;

proc print data=lnCor_solutions2(firstobs=20 obs=40);
run;

proc sort data=lnCor_solutions2;
    by Parameter _Imputation_;
run;

proc mianalyze data=lnCor_solutions2;
    by Parameter;          * one pooled result per parameter label;
    modeleffects Estimate; * variable that holds the coefficient;
    stderr StdErr;         * variable that holds the SE;
run;













/*MNAR: delta shift based on time*/

/*Logic: 
for delta_early in (0, ±0.1, ±0.2):
  for delta_rec in (0, ±0.1, ±0.2, ±0.3):
     apply:
       if Sample in (4,5,6) & Miss_orig_Cort ? + delta_early
       if Sample in (7,8)   & Miss_orig_Cort ? + delta_rec
     fit model
     pool */


/*Delta grids (log-scale)*/
%let early_grid = 0 -0.1 0.1 -0.2 0.2;          /* S4–S6 */
%let rec_grid   = 0 -0.1 0.1 -0.2 0.2 -0.3 0.3; /* S7–S8 */


proc datasets lib=work nolist;
  delete ALL_MNAR_CORT;
quit;



/*Macro for cortisol only, post sample 4*/

%macro MNAR_CORTISOL;

%local i j de dr;
%do i=1 %to %sysfunc(countw(&early_grid));
  %let de = %scan(&early_grid,&i);

  %do j=1 %to %sysfunc(countw(&rec_grid));
    %let dr = %scan(&rec_grid,&j);

    /* Apply phase-specific delta */
    data aas_cort_mnar;
      set aas_long_mi_flagged;
      where Sample >= 4;

      lnCort_MNAR = lnCort;

      if Miss_orig_Cort = 1 then do;
        if Sample in (4,5,6) then lnCort_MNAR = lnCort + &de.;
        else if Sample in (7,8) then lnCort_MNAR = lnCort + &dr.;
      end;
    run;

    proc sort data=aas_cort_mnar; by _Imputation_; run;

    ods output SolutionF=sol_cort;
    proc mixed data=aas_cort_mnar nobound method=reml;
      by _Imputation_;
      class ID Group(ref='0') gender(ref='m') Sample;
      model lnCort_MNAR =
            min_from_s4
            Group gender
            Group*min_from_s4
            / solution ddfm=kr;
      random intercept / subject=ID;
      repeated Sample / subject=ID type=AR(1);
    run;
    ods output close;

    /* Prepare for pooling */
    data sol_cort2;
      set sol_cort;
      length Parameter $40 MNAR_pattern $20;

      MNAR_pattern = "phase_specific";
      Delta_early  = &de.;
      Delta_rec    = &dr.;

      if Effect='min_from_s4' then Parameter='Time_from_S4';
	  else if Effect='Intercept' then Parameter='Intercept';
      else if Effect='Group' then Parameter='Group_high_vs_lowmed';
      else if Effect='gender' then Parameter='Female_vs_male';
      else if Effect='min_from_s4*Group' then Parameter='Interaction_GroupxTime';
      else delete;

      if missing(StdErr) then delete;

      keep _Imputation_ Parameter Estimate StdErr MNAR_pattern Delta_early Delta_rec;
    run;

    proc sort data=sol_cort2;
      by Parameter _Imputation_;
    run;

    ods output ParameterEstimates=pool_cort;
    proc mianalyze data=sol_cort2;
      by Parameter;
      modeleffects Estimate;
      stderr StdErr;
    run;
    ods output close;

   data pool_cort;
  	set pool_cort;
  	length Delta_index $20 Outcome $20 MNAR_pattern $20;
  	Outcome      = "Cortisol";
  	MNAR_pattern = "phase_specific";

  /* HRV-style delta identifier */
 	 Delta_index = cats("E", &de., "_R", &dr.);
   run;


    proc append base=ALL_MNAR_CORT data=pool_cort force; run;

  %end;
%end;

%mend;


%MNAR_CORTISOL;



proc contents data=aas_long_mi_flagged; run;



proc freq data=ALL_MNAR_CORT;
  tables Parameter;
run;




proc print data=ALL_MNAR_CORT(obs=20); run;


%let early_grid = 0 -0.1 0.1 -0.2 0.2;          /* S4–S6 */
%let rec_grid   = 0 -0.1 0.1 -0.2 0.2 -0.3 0.3; /* S7–S8 */



/*Amylase*/

proc means data=aas_long_mi_flagged noprint;
  where Sample >= 4 and Sample <= 8;
  var min_from_s4;
  output out=_tmean mean=mean_t;
run;

data aas_long_mi_flagged_tc;
  if _n_=1 then set _tmean;
  set aas_long_mi_flagged;
  where Sample >= 4 and Sample <= 8;

  t_c  = min_from_s4 - mean_t;
  t_c2 = t_c*t_c;
run;


proc print data=aas_long_mi_flagged_tc(obs=10);run;

proc sort data=aas_long_mi_flagged_tc; by _imputation_ ID Sample; run;

%let early_grid = 0 -0.1 0.1 -0.2 0.2;          /* S4–S6 */
%let rec_grid   = 0 -0.1 0.1 -0.2 0.2 -0.3 0.3; /* S7–S8 */

proc datasets lib=work nolist;
  delete ALL_MNAR_AMY;
quit;



%macro MNAR_LNAMY;

%local i j de dr;
%do i=1 %to %sysfunc(countw(&early_grid));
  %let de = %scan(&early_grid,&i);

  %do j=1 %to %sysfunc(countw(&rec_grid));
    %let dr = %scan(&rec_grid,&j);

    /* Apply phase-specific delta to lnAmy only if originally missing */
    data aas_amy_mnar;
      set aas_long_mi_flagged_tc;
      where Sample >= 4 and Sample <= 8;

      lnAmy_MNAR = lnAmy;

      if Miss_orig_Amy = 1 then do;
        if Sample in (4,5,6) then lnAmy_MNAR = lnAmy + &de.;
        else if Sample in (7,8) then lnAmy_MNAR = lnAmy + &dr.;
      end;
    run;

    proc sort data=aas_amy_mnar; by _Imputation_; run;

    ods output SolutionF=sol_amy;
    proc mixed data=aas_amy_mnar nobound method=reml;
      by _Imputation_;
      class ID Group(ref='0') gender(ref='m') Sample;

      model lnAmy_MNAR =
            t_c
            t_c2
            Group
            gender
            gender*t_c
            gender*t_c2
            Group*gender
            / solution cl ddfm=kr;

      random intercept / subject=ID;
      repeated Sample / subject=ID type=AR(1);
    run;
    ods output close;

    /* Prepare for pooling */
    data sol_amy2;
      set sol_amy;
      length Parameter $60 MNAR_pattern $20;

      MNAR_pattern = "phase_specific";
      Delta_early  = &de.;
      Delta_rec    = &dr.;

      if Effect='Intercept' then Parameter='Intercept';
      else if Effect='t_c' then Parameter='t_c';
      else if Effect='t_c2' then Parameter='t_c2';
      else if Effect='Group' then Parameter='Group_high_vs_lowmed';
      else if Effect='gender' then Parameter='Female_vs_male';
      else if Effect='t_c*gender' then Parameter='Interaction_Femalext_c';
      else if Effect='t_c2*gender' then Parameter='Interaction_Femalext_c2';
      else if Effect='Group*gender' then Parameter='Interaction_GroupxGender';
      else delete;

      if missing(StdErr) then delete;

      keep _Imputation_ Parameter Estimate StdErr MNAR_pattern Delta_early Delta_rec;
    run;

    proc sort data=sol_amy2; by Parameter _Imputation_; run;

    ods output ParameterEstimates=pool_amy;
    proc mianalyze data=sol_amy2;
      by Parameter;
      modeleffects Estimate;
      stderr StdErr;
    run;
    ods output close;

    data pool_amy;
      set pool_amy;
      length Delta_index $30 Outcome $20 MNAR_pattern $20;
      Outcome      = "lnAmy";
      MNAR_pattern = "phase_specific";
      Delta_index  = cats("E", &de., "_R", &dr.);
    run;

    proc append base=ALL_MNAR_AMY data=pool_amy force; run;

  %end;
%end;

%mend;



%MNAR_LNAMY;


proc print data=AAS_AMY_MNAR(obs=30); run;





proc means data=AAS_AMY_MNAR n mean min max;
  where Sample>=4;
  var lnAmy lnAmy_MNAR;
run;




/*lnAACR*/

%let early_grid = 0 -0.1 0.1 -0.2 0.2;          /* S4–S6 */
%let rec_grid   = 0 -0.1 0.1 -0.2 0.2 -0.3 0.3; /* S7–S8 */

proc datasets lib=work nolist;
  delete ALL_MNAR_AACR;
quit;



%macro MNAR_LNAACR;

%local i j de dr;
%do i=1 %to %sysfunc(countw(&early_grid));
  %let de = %scan(&early_grid,&i);

  %do j=1 %to %sysfunc(countw(&rec_grid));
    %let dr = %scan(&rec_grid,&j);

    /* Apply phase-specific delta to lnAmy only if originally missing */
    data aas_aacr_mnar;
      set aas_long_mi_flagged_tc;
      where Sample >= 4 and Sample <= 8;

      lnAACR_MNAR = lnAACR;

      if Miss_orig_AACR = 1 then do;
        if Sample in (4,5,6) then lnAACR_MNAR = lnAACR + &de.;
        else if Sample in (7,8) then lnAACR_MNAR = lnAACR + &dr.;
      end;
    run;

    proc sort data=aas_aacr_mnar; by _Imputation_; run;

    ods output SolutionF=sol_aacr;
    proc mixed data=aas_aacr_mnar nobound method=reml;
      by _Imputation_;
      class ID Group(ref='0') gender(ref='m') Sample;

      model lnAACR_MNAR =
        t_c 
		t_c2 
        Group gender
        Group*t_c 
        Group*t_c2 
        Group*gender
            / solution cl ddfm=kr;

      random intercept / subject=ID;
      repeated Sample / subject=ID type=AR(1);
    run;
    ods output close;

    /* Prepare for pooling */
    data sol_aacr2;
      set sol_aacr;
      length Parameter $60 MNAR_pattern $20;

      MNAR_pattern = "phase_specific";
      Delta_early  = &de.;
      Delta_rec    = &dr.;

      if Effect='Intercept' then Parameter='Intercept';
      else if Effect='t_c' then Parameter='t_c';
      else if Effect='t_c2' then Parameter='t_c2';
      else if Effect='Group' then Parameter='Group_high_vs_lowmed';
      else if Effect='gender' then Parameter='Female_vs_male';
      else if Effect='t_c*Group' then Parameter='Interaction_HighPMAxt_c';
      else if Effect='t_c2*Group' then Parameter='Interaction_HighPMAxt_c2';
      else if Effect='Group*gender' then Parameter='Interaction_GroupxGender';
      else delete;

      if missing(StdErr) then delete;

      keep _Imputation_ Parameter Estimate StdErr MNAR_pattern Delta_early Delta_rec;
    run;

    proc sort data=sol_aacr2; by Parameter _Imputation_; run;

    ods output ParameterEstimates=pool_aacr;
    proc mianalyze data=sol_amy2;
      by Parameter;
      modeleffects Estimate;
      stderr StdErr;
    run;
    ods output close;

    data pool_aacr;
      set pool_aacr;
      length Delta_index $30 Outcome $20 MNAR_pattern $20;
      Outcome      = "lnAACR";
      MNAR_pattern = "phase_specific";
      Delta_index  = cats("E", &de., "_R", &dr.);
    run;

    proc append base=ALL_MNAR_AACR data=pool_aacr force; run;

  %end;
%end;

%mend;



%MNAR_LNAACR;


proc print data=AAS_AACR_MNAR(obs=30); run;




/* De-duplicate pooled MNAR results (works for any outcome table) */
%macro dedup_mnar(ds=, out=);
proc sort data=&ds out=&out nodupkey;
  by Outcome MNAR_pattern Delta_index Parameter;
run;
%mend;

%dedup_mnar(ds=ALL_MNAR_CORT, out=ALL_MNAR_CORT);
%dedup_mnar(ds=ALL_MNAR_AMY,  out=ALL_MNAR_AMY);
%dedup_mnar(ds=ALL_MNAR_AACR,  out=ALL_MNAR_AACR);



proc export data=ALL_MNAR_CORT
outfile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/MNAR_lnCort.xlsx'
dbms=xlsx 
replace;
run;



proc export data=ALL_MNAR_AMY
outfile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/MNAR_lnAmy.xlsx'
dbms=xlsx 
replace;
run;


proc export data=ALL_MNAR_AACR
outfile='C:/Users/Ahmed/OneDrive/Documents/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/MNAR_lnAACR.xlsx'
dbms=xlsx 
replace;
run;


