libname ssdata 'X:/CommonPrograms/Oral Health Services Research Core/DMAS 2019/Eligibility and Claims and Providers/Pregnant';
libname ddata 'X:/CommonPrograms/Oral Health Services Research Core/DMAS 2019/Eligibility and Claims and Providers/Claims under 21';
libname ddata2 'X:/CommonPrograms/Oral Health Services Research Core/Medicaid/ADA Study Data/Jonathan/Data/NewPTL';

/*proc contents data=ssdata.sfy2015_ffs_preg_ge21yo_90day;*/
/*run;*/

proc export 
  data=ddata2.planningdistricts
  dbms=xlsx 
  outfile="X:/CommonPrograms/Oral Health Services Research Core/Spiro/Claims_Project/data/planning_districts.xlsx" 
  replace;
run;

proc export 
  data=ssdata.Pregnant_claims_20152018
  dbms=xlsx 
  outfile="X:/CommonPrograms/Oral Health Services Research Core/Spiro/Claims_Project/data/Pregnant_claims_2015_2018.xlsx" 
  replace;
run;

proc export 
  data=ssdata.sfy2015_claims_preg_ge21yo_90day
  dbms=xlsx 
  outfile="C:\Users\stili\Downloads\sfy2015_claims_preg_ge21yo_90day.xlsx" 
  replace;
run;

proc export 
  data=ssdata.sfy2015_ffs_preg_ge21yo_90day
  dbms=xlsx 
  outfile="C:\Users\stili\Downloads\sfy2015_ffs_preg_ge21yo_90day.xlsx" 
  replace;
run;

proc export 
  data=ssdata.sfy2015_mco_preg_ge21yo_90day
  dbms=xlsx 
  outfile="C:\Users\stili\Downloads\sfy2015_mco_preg_ge21yo_90day.xlsx" 
  replace;
run;

proc export 
  data=ssdata.sfy2016_claims_preg_ge21yo_90day
  dbms=xlsx 
  outfile="C:\Users\stili\Downloads\sfy2016_claims_preg_ge21yo_90day.xlsx" 
  replace;
run;

proc export 
  data=ssdata.sfy2016_ffs_preg_ge21yo_90day
  dbms=xlsx 
  outfile="C:\Users\stili\Downloads\sfy2016_ffs_preg_ge21yo_90day.xlsx" 
  replace;
run;

proc export 
  data=ssdata.sfy2016_mco_preg_ge21yo_90day
  dbms=xlsx 
  outfile="C:\Users\stili\Downloads\sfy2016_mco_preg_ge21yo_90day.xlsx" 
  replace;
run;

proc export 
  data=ssdata.sfy2017_claims_preg_ge21yo_90day
  dbms=xlsx 
  outfile="C:\Users\stili\Downloads\sfy2017_claims_preg_ge21yo_90day.xlsx" 
  replace;
run;

proc export 
  data=ssdata.sfy2017_ffs_preg_ge21yo_90day
  dbms=xlsx 
  outfile="C:\Users\stili\Downloads\sfy2017_ffs_preg_ge21yo_90day.xlsx" 
  replace;
run;

proc export 
  data=ssdata.sfy2017_mco_preg_ge21yo_90day
  dbms=xlsx 
  outfile="C:\Users\stili\Downloads\sfy2017_mco_preg_ge21yo_90day.xlsx" 
  replace;
run;

proc export 
  data=ssdata.sfy2018_claims_preg_ge21yo_90day
  dbms=xlsx 
  outfile="C:\Users\stili\Downloads\sfy2018_claims_preg_ge21yo_90day.xlsx" 
  replace;
run;

proc export 
  data=ssdata.sfy2018_ffs_preg_ge21yo_90day
  dbms=xlsx 
  outfile="C:\Users\stili\Downloads\sfy2018_ffs_preg_ge21yo_90day.xlsx" 
  replace;
run;

proc export 
  data=ssdata.sfy2018_mco_preg_ge21yo_90day
  dbms=xlsx 
  outfile="C:\Users\stili\Downloads\sfy2018_mco_preg_ge21yo_90day.xlsx" 
  replace;
run;

/*proc export */
/*  data=ssdata.dq_51_files_2015_2018*/
/*  dbms=xlsx */
/*  outfile="C:\Users\stili\Downloads\dq_51_files_2015_2018.xlsx" */
/*  replace;*/
/*run;*/

/*==================================================================================*/

proc contents data=ssdata.dq_51_files_2015_2018;
run;

proc freq data=ssdata.dq_51_files_2015_2018;
tables Sub_group_Name / missing nocol norow nopercent;
run;
