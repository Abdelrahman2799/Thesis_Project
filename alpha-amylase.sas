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
  if ID not in (106, 165);
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




/*============================================
				   Modeling
  ============================================*/


/*=========================
		 lnCortisol
  =========================*/

proc sort data=aas_f;  
    by ID Sample;
run;


/*Preliminary mean structure*/
/*Create time^2 and center to reduce collinearity*/

proc means data=aas_f noprint;
  var time_from_CPT_min;
  output out=_tmean mean=mean_t;
run;

data aas_11_3;
  if _n_=1 then set _tmean;
  set aas_f;
  where Sample >= 4 and Sample <= 8;

  t_c  = time_from_CPT_min - mean_t;
  t_c2 = t_c*t_c;
run;


proc glm data=aas_11_3;
  class Group0(ref='0') gender(ref='m');
  model lnCortisol =
        t_c t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
        Group0*gender*t_c
        Group0*gender*t_c2
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
  class ID Group0 Gender(ref='m') Sample;

  model r_lnCort =;

  random intercept t_c t_c2/ subject=ID;
  repeated Sample / subject=ID type=AR(1);

  ods output FitStatistics=Fit_full
             Type3=Type3_full;
run;

proc mixed data=aas_olsresid;
  class ID Group0 Gender(ref='m') Sample;

  model r_lnCort =;

  random intercept t_c t_c2/ subject=ID;
  repeated Sample / subject=ID type=simple;

  ods output FitStatistics=Fit_full
             Type3=Type3_full;
run;

proc mixed data=aas_olsresid;
  class ID Group0 Gender(ref='m') Sample;

  model r_lnCort =;

  random intercept t_c t_c2/ subject=ID;
  repeated Sample / subject=ID type=un;

  ods output FitStatistics=Fit_full
             Type3=Type3_full;
run;

/*AR(1) is the chosen residual structure (Also supported by variogram and residual plots)*/


/*Reduce the random effects*/

proc mixed data=aas_11_3 nobound method=reml;
  class ID Group0(ref='0') Gender(ref='m') Sample;

model lnCortisol =
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
  class ID Group0(ref='0') Gender(ref='m') Sample;

model lnCortisol =
        t_c 
/*		t_c2 */
        Group0 gender
        Group0*t_c gender*t_c
/*        Group0*t_c2 gender*t_c2*/
        Group0*gender
/*        Group0*gender*t_c*/
/*        Group0*gender*t_c2*/
        / solution ddfm=kr;

  random intercept t_c/ subject=ID;
  repeated / subject=ID type=AR(1);
run;


/*Time slope is extremely small*/

proc mixed data=aas_11_3 nobound method=reml;
  class ID Group0(ref='0') Gender(ref='m') Sample;

model lnCortisol =
        t_c 
/*		t_c2 */
        Group0 gender
        Group0*t_c gender*t_c
/*        Group0*t_c2 gender*t_c2*/
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
class ID Group0(ref='0') Gender(ref='m') Sample;

model lnCortisol =
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
class ID Group0(ref='0') Gender(ref='m') Sample;

model lnCortisol =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2 gender*t_c2
        Group0*gender
        Group0*gender*t_c
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;

/*Supported by likelihood test and AIC, drop it safely. Next, drop: t_c*Group0*gender*/
proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') Gender(ref='m') Sample;

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
class ID Group0(ref='0') Gender(ref='m') Sample;

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

/*Drop t_c2*gender*/
proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') Gender(ref='m') Sample;

model lnCortisol =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        Group0*t_c2
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;

/*Drop t_c2*Group0*/
proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') Gender(ref='m') Sample;

model lnCortisol =
        t_c 
		t_c2 
        Group0 gender
        Group0*t_c gender*t_c
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;

/*Since interactions of interest, and since interactions with time^2 are removed, and since time^2 is not significant, drop it

Now we can go back to original not centered time_from_CPT_min */

proc mixed data=aas_f nobound method=ml;
class ID Group0(ref='0') Gender(ref='m') Sample;

model lnCortisol =
        time_from_CPT_min 
        Group0 gender
        Group0*time_from_CPT_min 
		gender*time_from_CPT_min
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;

/*gender*time is highly not significant (no evidence that gender is a moderator), drop it*/

proc mixed data=aas_f nobound method=ml;
class ID Group0(ref='0') Gender(ref='m') Sample;

model lnCortisol =
        time_from_CPT_min 
        Group0 gender
        Group0*time_from_CPT_min 
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;

/*Compare the final model with the first model*/


proc mixed data=aas_11_3 nobound method=ml;
class ID Group0(ref='0') Gender(ref='m') Sample;

model lnCortisol =
        t_c 
        Group0 gender
        Group0*t_c 
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;


/*Final Model: lnCortisol ~ Gender + time_from_CPT_min*PMA Group*/

/*Fit under REML*/
proc mixed data=aas_f nobound method=reml;
class ID Group0(ref='0') Gender(ref='m') Sample;

model lnCortisol =
        time_from_CPT_min 
        Group0 gender
        Group0*time_from_CPT_min 
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;



proc mixed data=aas_f nobound method=reml;
  class ID Group0(ref='0') gender(ref='m');
  model lnCortisol =
        time_from_CPT_min
        Group0
        gender
        Group0*time_from_CPT_min
        / solution cl ddfm=kr;

  random intercept / subject=ID;
  repeated / subject=ID type=AR(1);
  run;

/*  /* Slopes (log scale) */*/
/*  estimate "Slope time (LMA)" */
/*           time_from_CPT_min 1*/
/*           Group0*time_from_CPT_min 0 / cl;*/
/**/
/*  estimate "Slope time (High PMA)" */
/*           time_from_CPT_min 1*/
/*           Group0*time_from_CPT_min 1 / cl;*/
/**/
/*  /* Difference in slopes (High - LMA) */*/
/*  estimate "Slope difference (High-LMA)"*/
/*           Group0*time_from_CPT_min 1 / cl;*/
/*run;




/*-----------------------
  	Model Assumptions
  -----------------------*/
  proc glimmix data=aas_f nobound method=rspl;
  class ID Group0(ref='0') gender(ref='m');

  model lnCortisol =
        time_from_CPT_min 
        Group0 gender
        Group0*time_from_CPT_min
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
run;

proc univariate data=lnCort_Diag normal;
  var Resid;
  qqplot Resid / normal(mu=est sigma=est);
run;

proc sgplot data=lnCort_Diag;
  histogram Resid;
  density Resid;
run;







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
        Group0*gender*t_c
        Group0*gender*t_c2
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
    title "lnAmy Residual Distribution by Gender";
run;

proc sgplot data=aas_olsresid;
    vbox r_lnAmy / category=Group0;
    title "lnAmy Residual Distribution by PMA Group";
run;

proc sgplot data=aas_olsresid;
    series x=time_from_CPT_min y=r_lnAmy / group=ID transparency=0.8;
    title "lnAmy Residual Profiles by Subject";
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



/*11.5 Check Random slope(s) and Residuals Covariance*/

proc mixed data=aas_olsresid;
  class ID Group0 Gender(ref='m') Sample;

  model r_lnCort =;

  random intercept t_c t_c2/ subject=ID;
  repeated Sample / subject=ID type=AR(1);

  ods output FitStatistics=Fit_full
             Type3=Type3_full;
run;

proc mixed data=aas_olsresid;
  class ID Group0 Gender(ref='m') Sample;

  model r_lnCort =;

  random intercept t_c t_c2/ subject=ID;
  repeated Sample / subject=ID type=simple;

  ods output FitStatistics=Fit_full
             Type3=Type3_full;
run;

proc mixed data=aas_olsresid;
  class ID Group0 Gender(ref='m') Sample;

  model r_lnCort =;

  random intercept t_c t_c2/ subject=ID;
  repeated Sample / subject=ID type=un;

  ods output FitStatistics=Fit_full
             Type3=Type3_full;
run;

/*AR(1) is the chosen residual structure (Also supported by variogram and residual plots)*/


/*Reduce the random effects*/

proc mixed data=aas_11_3 nobound method=reml;
  class ID Group0(ref='0') Gender(ref='m') Sample;

model lnCortisol =
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
  class ID Group0(ref='0') Gender(ref='m') Sample;

model lnCortisol =
        t_c 
/*		t_c2 */
        Group0 gender
        Group0*t_c gender*t_c
/*        Group0*t_c2 gender*t_c2*/
        Group0*gender
/*        Group0*gender*t_c*/
/*        Group0*gender*t_c2*/
        / solution ddfm=kr;

  random intercept t_c/ subject=ID;
  repeated / subject=ID type=AR(1);
run;


/*Time slope is extremely small*/

proc mixed data=aas_11_3 nobound method=reml;
  class ID Group0(ref='0') Gender(ref='m') Sample;

model lnCortisol =
        t_c 
/*		t_c2 */
        Group0 gender
        Group0*t_c gender*t_c
/*        Group0*t_c2 gender*t_c2*/
        Group0*gender
/*        Group0*gender*t_c*/
/*        Group0*gender*t_c2*/
        / solution ddfm=kr;

  random intercept/ subject=ID;
  repeated / subject=ID type=AR(1);
run;

/*Both REML log-likelihood test and AIC support reduction to random intercept only*/

/*So we have: Random intercept model with AR(1) residuals structure*/
