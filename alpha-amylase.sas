/* Step 1: Set the library path where the .sas7bdat file is stored */
libname mydata 'D:/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/';

proc datasets lib=mydata;
run;

DATA aas;
    SET mydata.LAST_CORTISOLAMYLASE;
RUN;



/* Step 2: Print the first few rows to check the data */
proc contents data=aas;
run;

DATA aas_clean;
    SET aas;
    DROP firstnam surname birthday ;
RUN;

proc means data=aas_clean NMISS N; run;


proc export data=aas_clean 
    outfile='D:/Masters/Second_Year/Master Thesis Data Science/Prenatal stress study/data/Cortisol&AAS/aas_namesremoved.xlsx'
    dbms=xlsx 
    replace;
    sheet="My Sheet";
run;

