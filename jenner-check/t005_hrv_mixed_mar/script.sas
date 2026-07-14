/* ============================================================
   MAR linear mixed model for lnRMSSD (from HRV_MI_MAR_MNAR.sas)

   The author's substantive mixed model under MAR: a subject random
   intercept plus AR(1) autocorrelation across the six repeated
   timepoints, with Group / condition (REST_VS_STRESS) / Gender main
   effects and their two-way interactions as fixed effects. Fitted by
   REML. Reproduced unmodified against the mock long table in
   autoexec.sas.
   ============================================================ */

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
