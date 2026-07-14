/* cap input rows for the captured run */
options obs=100;

/* ------------------------------------------------------------------
   Mock HRV long-format dataset for the MAR mixed model.

   Upstream hrv_long is the reshaped, imputed HRV table (built from
   imported xlsx; paths stripped for portability). This block builds a
   small synthetic long table with the columns the author's PROC MIXED
   reads: Subject_ID, Time (1-6), Group, Gender, the REST_VS_STRESS
   condition flag derived from Time, and an lnRMSSD outcome with a
   subject random intercept, a mild condition effect and AR(1)-style
   autocorrelation so the model has structure to estimate. 12 subjects
   x 6 timepoints = 72 rows. Values are invented (seed fixed for a
   reproducible capture).
   ------------------------------------------------------------------ */
data hrv_long;
  call streaminit(20260714);
  do Subject_ID = 1 to 12;
    Group  = mod(Subject_ID, 2);              /* 0 / 1 */
    if mod(Subject_ID, 3) = 0 then Gender = 'f'; else Gender = 'm';
    u_int  = rand('normal', 0, 0.25);         /* subject random intercept */
    prev   = 0;
    do Time = 1 to 6;
      REST_VS_STRESS = (Time in (2,3,4,5));
      e      = 0.6*prev + rand('normal', 0, 0.20);   /* AR(1)-style errors */
      prev   = e;
      lnRMSSD = 3.30
              + 0.15*Group
              + 0.25*REST_VS_STRESS
              - 0.10*(Gender = 'f')
              + 0.08*Group*REST_VS_STRESS
              + u_int + e;
      output;
    end;
  end;
  keep Subject_ID Time Group Gender REST_VS_STRESS lnRMSSD;
run;

proc sort data=hrv_long; by Subject_ID Time; run;
