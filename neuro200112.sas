********* MACROs for definitions ****;
*** macro's generally assume that the following: **;
**  ds: dataset: variables in it may have outcome labels: they are used in reporting;
**
**  period: subsequent values of time variable (period) are considered equal spaced in AR analysis ;
**  period_min / period_max: for making graphs the minimal and maximal period;
**  trt: if a period contains both trt=0 and trt=1 observations, ;
**        then it is assumed to be a period with trt=1 and the average is taken ;
** 	trt: intervention variable (trt) are coded typically 0,1  ;
**  outcome: the outcome variable (outcome) is given for each subject in a cluster ;
**  	a binary variable must be coded 0,1 ;

** macro's provided:
** %rm_design: gives overview of number of subject in each period and their treatment assignment; 
** %sw_cont_descrip: gives descriptives for repeated measures of a continuous outcome;
** %sw_bin_descrip: idem for a binary outcome;
** %explore_ar: to compare all AR models with lag =1, 2, ..., upper_lag and OLS 
                in terms of residual plots, fit plots, and fit summary;
** %its_infer: provides, and if requested saves, results of
        a) backwards autoregressive regression, 
        b)ordinary least squares with Newey-West corrected standard errors 
        c) AR regression with specified lag (nlag);
** %its_cont: both descriptives (%sw_cont_descrip) and analyses (%its_infer) for continuous outcome;
** %its_bin: idem for binary;

%macro rm_design(ds=ds, cluster=center,trt=trt, period=period);
* gives overview of number of subject in each period and their treatment assignment; 
title4 "treatments applied per cluster-period";
title4 "check whether each cluster-period has one treatment?";
proc tabulate data=&ds;class &cluster &period &trt;format &period 4.0; 
table &cluster*&trt, &period*N*f=4. /rts=15 ;
run;
%mend;


* descriptives;
%macro sw_cont_descrip(ds=ds,outcome=, cluster=center, period=period, trt=trt,
outcome_min=-1000, outcome_max=+1000,period_min=1,period_max=14,dsdescrip=);
* gives descriptives for repeated measures of a continuous outcome;

*get outcome label;
data _null_; set &ds; call symput('outcome_label', vlabel(&outcome));run;
title3 "description   ****&outcome: &outcome_label *****";
title4 "all values";
proc means data=&ds n nmiss min max p5 p95 Q1 median Q3 mean std; var &outcome; run; 
proc sgpanel data=&ds;  
panelby &cluster / onepanel;
histogram &outcome;
run;
title4 "data size per cluster-period";
proc tabulate data=&ds; class &cluster &period; var &outcome;
table &cluster, &period="&outcome: N per cluster-period"*&outcome=' '*(N=' ')*f=5.0 / rts=10;  run;
title4 "mean per cluster-period";
proc tabulate data=&ds; class &cluster &period; var &outcome;
table &cluster, &period="&outcome: mean per cluster-period"*&outcome=' '*(mean=' ')*f=5.2 / rts=10;  run;

title3 "analysis    ****&outcome: &outcome_label ****";
title4 "time series plot per cluster)";
title5 "(note: periods with trt=0 and 1 are considered trt=1";
proc means data=&ds noprint; id &trt; class &cluster &period;
var &outcome; output out=_ClusPeriodMeans mean(&outcome)=mean;run;
proc sgpanel data=_clusPeriodmeans;
panelby &cluster / onepanel; *or columns=1 to get only one column;
series x=&period y=mean;
scatter x=&period y=mean /group=&trt markerattrs=(size=9) ;
rowaxis integer min=&outcome_min max=&outcome_max alternate;
colaxis min=&period_min max=&period_max alternate integer;
run;
* to do: time series analysis per cluster using standard techniques?*;
title4 "disregarding possible time trend:";
proc means data=&ds n nmiss min max p5 p95 Q1 median Q3 mean std; class &trt;var &outcome; run;
data _shift; set &ds;
if &trt=1 then intervention_&outcome=&outcome; 
if &trt=0 then control_&outcome=&outcome;
run;
title5 "shift in distribution by intervention: over *all* clusters";
proc sgplot data=_shift;
histogram intervention_&outcome /transparency=0.3;
histogram control_&outcome /transparency=0.6;
run;
title5 "shift in distribution by intervention: by cluster";
proc sgpanel data=_shift;  
panelby &cluster / onepanel;
histogram intervention_&outcome /transparency=0.3;
histogram control_&outcome /transparency=0.6;
run;
title5 "shift in means, medians, boxplots"; 
proc sgpanel data=&ds;  
panelby &cluster / onepanel;
*vbar &trt / response=&outcome stat=mean;
hbox &outcome / category=&trt;
run;
*show descriptives and if need save them***;
proc means data=&ds n nmiss mean std median Q1 Q3; var &outcome; class &trt;
%IF &dsdescrip ne %THEN %DO; 
output out=_descrip n=n nmiss=nmiss mean=mean std=std median=median Q1=Q1 Q3=Q3 ;
%END;
run;
%IF &dsdescrip ne %THEN %DO; 
data _descrip; length outcome $30; length outcome_label $ 100; length stat $ 10;
set _descrip; outcome="&outcome";outcome_label="&outcome_label";
array aux[7] n nmiss mean std median Q1 Q3; 
do i=1 to 7;
stat=vname( aux[i]); value=aux[i];output;
end; 
drop  n nmiss mean std median Q1 Q3; 
if _type_=1;
keep outcome outcome_label &trt stat value;
run;
proc append force base=&dsdescrip data=_descrip;run;
%END;
%mend sw_cont_descrip;



%macro sw_bin_descrip(ds=ds, outcome=,cluster=center, period=period, trt=trt,period_min=1, period_max=14,
dsdescrip=
); 
** assumes variable is {0,1} coded;
** gives descriptives for repeated measures of a continuous outcome;

data &ds; set &ds; 
percent_&outcome=100*&outcome;* for calculating percentages via a means-operator;
dummy=1;* for having a categorical variable with only one value;
run;
*get outcome label;
data _null_; set &ds; call symput('outcome_label', vlabel(&outcome));run;
title3 "description   **** &outcome: &outcome_label ****";
title4 "all values";
proc freq data=&ds; table &outcome /missing; run; 
title4 "percentage &outcome=1 (assuming 0,1 coding)";
proc format; value dummyft 1=' ';run;
proc sgpanel data=&ds;format dummy dummyft.; 
panelby &cluster / onepanel;
* need dummy variable to have a category variable for vbar statement;
vbar dummy /  stat=mean response=percent_&outcome;
colaxis label=' ';
run;
title4 "data size per cluster-period";
proc tabulate data=&ds; class &cluster &period; var &outcome;
table &cluster, &period="&outcome: N per cluster-period"*&outcome=' '*(N=' ')*f=5.0 / rts=10;  run;
title4 "percent per cluster-period";
proc tabulate data=&ds; class &cluster &period; var percent_&outcome;
table &cluster, &period="&outcome: percent per cluster-period"*percent_&outcome=' '*(mean=' ')*f=5.2 / rts=10;run;


title3 "analysis   **** &outcome: &outcome_label ****";
title4 "time series plot per cluster";
proc means data=&ds noprint; id &trt; class &cluster &period;
var percent_&outcome; output out=_ClusPeriodMeans mean(percent_&outcome)=percent;run;
proc sgpanel data=_clusPeriodmeans;
panelby &cluster / onepanel; *or columns=1 to get only one column;
series x=&period y=percent;
scatter x=&period y=percent /group=&trt markerattrs=(size=9) ;
rowaxis min=0 max=100 alternate;
colaxis min=&period_min max=&period_max alternate integer;
run;
* to do: time series analysis per cluster using standard techniques?*;
title4 "disregarding possible time trend:";
title5 "shift in percentage by intervention";
proc sgpanel data=&ds;  
panelby &cluster / onepanel;
vbar &trt / response=percent_&outcome stat=mean;
run;
* show descriptives and save if asked ;
%IF &dsdescrip ne %THEN ods output CrossTabFreqs=_descrip;; 
proc freq data=&ds; table &outcome*&trt / missing norow nopercent; run;
%IF &dsdescrip ne %THEN %DO;
ods output close;
data _descrip; length stat $ 10;length outcome $30; length outcome_label $ 100; 
set _descrip; 
outcome="&outcome";outcome_label="&outcome_label";
if &trt ne . and &outcome=. then do;
stat="ntot"; value=frequency; output;
end;
if &outcome=1 and &trt ne . then do; 
stat="n"; value=frequency; output;
stat="perc"; value=colpercent; output;
end;
keep outcome outcome_label &trt stat value;
run;
proc append force base=&dsdescrip data=_descrip;run;
%END;
%mend sw_bin_descrip;


%macro its_infer(ds=ds,outcome=, cluster=center,period=period,trt=trt,
slstay=0.10,nlag=1, dseff=, binary=0);
* provides the following analyses for an interrupted time series;
* a) backwards autoregressive regression;
* b) ordinary least squares with Newey-West corrected standard errors; 
* c) autoregressive regression with specified lag: &nlag;
* the results are saved in &dseff if requested;
* if binary=1 then the cluster-period averages are logodds transformed;
*     and the results are on logodds scale;  

*get outcome label;
data _null_; set &ds; call symput('outcome_label', vlabel(&outcome));run;

* make the dataset with cluster period means; 
proc sort data=&ds;by &cluster &period;
proc means data=&ds noprint; id &trt; class &cluster &period;
var &outcome; output out=_ClusPeriodMeans mean(&outcome)=mean;run;
* select only the cluster period means;
data _ClusPeriodMeans; 
set _ClusPeriodMeans(where=(&cluster ne . and &period ne . and &trt ne .));
* calculate postperiod: the time since start of trt;
if lag(&trt)=0 or _n_=1 then postperiod=0; else if &trt=1 then postperiod+1; 
* logodds transform if a a binary outcome;
%IF &binary=1 %THEN %DO; 
	if mean=0 then mean=0.000001; else if mean =1 then mean=0.999999;
	mean = log( mean/ (1-mean)  ); * log odds;
	label mean='log odds';
%END;
run;

* maximum lag: number of measurments minus 1;
proc sql noprint;
select nlobs into : nobs
from dictionary.tables
where libname='WORK'
and memname='_CLUSPERIODMEANS';
quit;
%local maxlag; %let maxlag=%sysevalf(&nobs - 1);

* the OlS analysis is always done, but: ;
* if nlag=0 and output dataset is requested;
* then we record the OLS instead of the autoregressive analysis;
%IF (&nlag=0) and (&dseff ne ) %THEN 
%DO;
  ods output OLSEst.Fitsummary=_fitsummary;
  ods output  OLSEst.ParameterEstimates=_solutions;
%END;
title4 "interrupted time series analysis";
title5 "UNCORRECTED (OLS) regression (for comparison)";
proc autoreg data=_clusperiodmeans;by &cluster; 
*linear trend;
model mean = &period &trt postperiod*&trt / 
method=ml loglikl dw=&maxlag dwprob
plots (only) = (standardresidual fitplot acf pacf whitenoise)
;
* can use plots (UNPACKpanel) = ... to get the plots separately;
run;
* process the estimates, note no check on OLS convergence needed;
%IF (&dseff ne ) and (&nlag=0) %THEN 
%DO; 
  ods output close;
  * get error degrees of freedom (DFE) to calculate approximate 95%-CI of &trt estimates;
  * see http://support.sas.com/kb/22/097.html;
  data _null_;set _fitsummary(where=(label2="DFE")); call symput('DFE',nvalue2);run;
  data _solutions; 
	length variable $50;length outcome $30;length outcome_label $100;length model $200;
	set _solutions; 
	if index(variable,"&trt")>0; 
	outcome="&outcome";outcome_label="&outcome_label";
	model="ITS: &period &trt postperiod*&trt // (!) OLS: linear trend";
	lower=estimate - tinv(0.975,&dfe)*stderr; upper=estimate + tinv(0.975, &dfe)*stderr;
	rename variable=label;
	keep &cluster outcome outcome_label variable estimate stderr probt lower upper model;
	run;
  proc append force base=&dseff data=_solutions;run;
%END;

* perform autoregressive analysis provided &nlag = 1, 2,3, ... and non-empty;
%IF (&nlag ne ) and (&nlag ne 0) %THEN 
%DO; 
 title5 "autoregressive model with lag=&nlag";
 * not to output;
 ods exclude 
 OLSEst.Fitsummary OLSEst.DWTest OLSEst.ParameterEstimates FinalModel.ParameterEstimatesGivenAR;
	* save estimates if requested;
	%IF &dseff ne %THEN 
	%DO;
	ods output FinalModel.ConvergenceStatus=_converged_nlag;
	ods output FinalModel.Fitsummary=_fitsummary;
	ods output  FinalModel.ParameterEstimates=_solutions;* not the initial OLS estimates;
	%END;
 proc autoreg data=_clusperiodmeans;by &cluster; 
 model mean = &period &trt postperiod*&trt /nlag=&nlag dw=&maxlag dwprob 
 method=ml loglikl 
 plots (only) = (standardresidual fitplot acf pacf whitenoise)
 ;* can use plots (UNPACKpanel) = ... to get the plots separately;
 run;
	* process the estimates;
	%IF &dseff ne %THEN
	%DO; 
	 ods output close;
	 * convergence status; 
	 data _null_; set _converged_nlag; call symput('nonconverged', status);run;
	* if not converged, estimates etc are set missing;
	%IF &nonconverged ne 0 %THEN %DO; 
	 	data _solutions;
		length label $50;length outcome $30;length outcome_label $100;length model $200;
		set _converged_nlag; * to get the &cluster variable value;
		outcome="&outcome";outcome_label="&outcome_label";
		label="&trt";estimate=.;stderr=.;probt=.;lower=.;upper=.;
		* tell type of model and what the non-convergence problem was;
		model=catx(': ',"ITS lag &nlag NOT converged", reason);
	    keep &cluster outcome outcome_label label estimate stderr probt lower upper model ;
		run;
		%END;
	* if converged;
	%IF &nonconverged eq 0 %THEN %DO; 
	 	* get error degrees of freedom (DFE) to calculate approximate 95%-CI of &trt estimates;
	 	* see http://support.sas.com/kb/22/097.html;
	 	data _null_;set _fitsummary(where=(label2="DFE")); call symput('DFE',nvalue2);run;
	 	data _solutions; 
		length variable $50;length outcome $30;length outcome_label $100;length model $200;
		set _solutions; 
		if index(variable,"&trt")>0; 
		outcome="&outcome";outcome_label="&outcome_label";
		model="ITS: &period &trt postperiod*&trt // AR(&nlag lags): linear trend";
		lower=estimate - tinv(0.975,&dfe)*stderr; upper=estimate + tinv(0.975, &dfe)*stderr;
		rename variable=label;
		keep &cluster outcome outcome_label variable estimate stderr probt lower upper model;
		run;
		%END;
	proc append force base=&dseff data=_solutions;run;
	%END; * process effects;
%END;

title5 "autoregressive model with backward selection: only AR terms with p < &slstay";
* a particular trouble is that if the backwards removes all AR parameters, it does not
* fit a model, and no ods datasets for FinalModel.ParameterEstimates and no ConvergenceStatus
* are created, so we have to work around that to provide the OLS as fitted model;

* save estimates;
%IF &dseff ne %THEN 
%DO;
 ods output OLSEst.ParameterEstimates=_OLSsolutions; * OLS estimates only;
 ods output Fitsummary=_Fitsummary; * OLS + Final backward model (if different from OLS);
 ods output ParameterEstimates=_solutions; * OLS + Final backward model (if different from OLS);
 * create dataset keeping convergence status (default: not converged);
*  for the case it is not created as ods dataset which happens if all AR lags are removed;	
 data _converged_backwards; set _ClusPeriodMeans(keep=&cluster obs=1); status=1;reason=""; run;
 ods output ConvergenceStatus (nowarn)=_converged_backwards; * if not created, give no warning;
%END;
proc autoreg data=_clusperiodmeans;by &cluster; 
* we start from the largest model: nlag=&maxlag;
model mean = &period &trt postperiod*&trt /nlag=&maxlag backstep slstay=&slstay 
method=ml loglikl
plots (only) = (standardresidual fitplot acf pacf whitenoise)
;
run;
%IF &dseff ne %THEN
%DO; 
 ods output close;
 * depending on convergence status, status=0 means convergenced, else estimates etc are set missing;
 data _null_; set _converged_backwards; call symput('nonconverged', status);run;
 %IF &nonconverged ne 0 %THEN %DO; 
 	data _solutions;
	length label $50;length outcome $30;length outcome_label $100;length model $200;
	set _converged_backwards; * to get the &cluster variable value;
	outcome="&outcome";outcome_label="&outcome_label";
	label="&trt";estimate=.;stderr=.;probt=.;lower=.;upper=.;
	* tell type of model and what the non-convergence problem was;
	model=catx(': ',"ITS backward NOT converged", reason);
    keep &cluster outcome outcome_label label estimate stderr probt lower upper model ;
	run;
	%END;
 %IF &nonconverged eq 0 %THEN %DO; 
	* check if the backward selection removed all the AR lags, so reduces to OLS;
 	* this is done by checking whether _OLSsolutions;
 	* and _solutions (=OLS + autoreg if not all lag removed) are equal, i.e. same number of obs;
	proc sql noprint;
   	select nlobs into : nobs_ols
   	from dictionary.tables
   	where libname='WORK'
    and memname='_OLSSOLUTIONS';
	quit;
	proc sql noprint;
   	select nlobs into : nobs_all
   	from dictionary.tables
   	where libname='WORK'
    and memname='_SOLUTIONS';
	quit;
	* if backwards selection reduced to OLS:..  ;
	%IF &nobs_all eq &nobs_ols %THEN %DO;
		* taking dataset _solutions (=_olssolutions) is ok;
	%END;
	* else backward selection does not reduce to OLS;
	%IF &nobs_all > &nobs_ols %THEN %DO;
		data _solutions; set _solutions;
			if _n_ > &nobs_ols;* skip the records of the OLS model to get those of the final model;
		run; 
	%END;
 	* get error degrees of freedom (DFE) to calculate approximate 95%-CI of &trt estimates;
	* see http://support.sas.com/kb/22/097.html;
	data _null_;set _fitsummary(where=(label2="DFE")); call symput('DFE',nvalue2);run;	
	* now get estimates from _solutions; 
	data _solutions; 
	length variable $50;length outcome $30;length outcome_label $100;length model $200;
	set _solutions; 
	if index(variable,"&trt")>0;
	outcome="&outcome";outcome_label="&outcome_label";
	%IF &nobs_all eq &nobs_ols %THEN %DO;
	model="ITS: &period &trt postperiod*&trt // AR(backwards) REMOVED ALL => OLS: linear trend ";
	%END;
	%IF &nobs_all > &nobs_ols %THEN %DO;
	model="ITS: &period &trt postperiod*&trt // AR(backwards) selected AR: linear trend";
	%END;	
	lower=estimate - tinv(0.975,&dfe)*stderr; upper=estimate + tinv(0.975, &dfe)*stderr;
	rename variable=label;
	keep &cluster outcome outcome_label variable estimate stderr probt lower upper model ;
	run;
	%END;
 proc append force base=&dseff data=_solutions;run;
%END;


title5 "OLS regression with Newey-West corrected standard errors";
* save estimates;
%IF &dseff ne %THEN 
%DO;
ods output  ParameterEstimates=_solutions;* OLS estimates with Newey-West stderr;
%END;
proc autoreg data=_clusperiodmeans;by &cluster; 
*linear trend;
model mean = &period &trt postperiod*&trt /covest=neweywest 
method=ml loglikl plots=none
;
run;
%IF &dseff ne %THEN
%DO; 
 ods output close;
 * no convergence status available.. will always converge (?);
 *;
 * get error degrees of freedom (DFE) to calculate approximate 95%-CI of &trt estimates;
 * see http://support.sas.com/kb/22/097.html;
 data _null_;set _fitsummary(where=(label2="DFE")); call symput('DFE',nvalue2);run; 
 data _solutions; 
 length variable $50;length outcome $30;length outcome_label $100;length model $200;
 set _solutions; 
 if index(variable,"&trt")>0; 
 outcome="&outcome";outcome_label="&outcome_label";
 model="ITS: &period &trt postperiod*&trt // OLS(Newey-West stderr) linear trend";
 lower=estimate - tinv(0.975,&dfe)*stderr; upper=estimate + tinv(0.975, &dfe)*stderr;
 rename variable=label;
 keep &cluster outcome outcome_label variable estimate stderr probt lower upper model ;
 run;
%END;
proc append force base=&dseff data=_solutions;run;
/* 
title "newey-west with specified lag= &lag";* using proc model;
%let lag=1;
proc model data=_clusperiodmeans;by &cluster;
parms b0 b1 b2 b3;
mean= b0 + b1*&period + b2*&trt + b3*postperiod;
fit mean / covb gmm kernel=(bart,&lag +1,0) vardef=n;
test b1;
test b3;
run;quit; 
*/
%mend its_infer;




*** descriptives and inferential together; 
* continuous;
%macro its_cont(ds=ds,outcome=, cluster=center, period=period, trt=trt,
outcome_min=-1000, outcome_max=+1000,period_min=1,period_max=14,
slstay=0.10,nlag=,dsdescrip=,dseff=);
	* descriptive;
	%sw_cont_descrip(ds=&ds,outcome=&outcome, cluster=&cluster, period=&period, trt=&trt,
	outcome_min=&outcome_min, outcome_max=&outcome_max,period_min=&period_min,
	period_max=&period_max,dsdescrip=&dsdescrip);
	* inferential;
	%its_infer(ds=&ds,outcome=&outcome, cluster=&cluster,period=&period,trt=&trt,
	slstay=&slstay,nlag=&nlag, dseff=&dseff);
%mend its_cont;

* binary;
%macro its_bin(ds=ds,outcome=, cluster=center, period=period, trt=trt,
period_min=1,period_max=14,
slstay=0.10,nlag=,dsdescrip=,dseff=);
	* descriptive;
	%sw_bin_descrip(ds=&ds,outcome=&outcome, cluster=&cluster, period=&period, trt=&trt,
	period_min=&period_min,period_max=&period_max,dsdescrip=&dsdescrip); 
	* inferential;
	%its_infer(ds=&ds,outcome=&outcome, cluster=&cluster,period=&period,trt=&trt,
	slstay=&slstay,nlag=&nlag, dseff=&dseff);
%mend its_bin;



********* READ 	DATA *******;
options pagesize=60;
* get formats;
options fmtsearch= (dir.formats);
libname dir ".";
data neuro;set dir.underpin_neuro;run;
* show variables in dataset;
proc contents data=neuro;run;

* get dataset;
data ds; set neuro;
trt=control_intervention;
where period < 999; *no need to exclude wrong inclusion;
*shortening names;
* continuous outcomes;
dcf= del_coma_free_days_alive; label dcf='delirium-coma free (days)';
dlr_d= delirium_days; label dlr_d='delirium (days)';
coma_d=coma_days; label coma_d='coma (days)';
sed_d=sedation_days; label sed_d='sedation (days)';
med_d=delirium_medication_days; label med_d='delirium medication (days)';
vent_d=ventilator_days; label vent_d='mechanical ventilation (days)';
fix_d=fixation_days; label fix_d='duration physical restraints (days)';
los_icu=los_icu; label los_icu='length of icu stay (days)';
los_hosp=los_hospital; label los_hosp='length of hospital stay (days)';
* binary outcomes;
dlr_prev=Delirium_medication_prevention_t; label dlr_prev='delier prevention'; 
dlr_trt=Delirium_medication_prevention_1; label dlr_trt='delier treatment';
dlr_prevtrt=Delirium_medication_prevention_2; label dlr_prevtrt='prevention+treatment';
dlr=delirium_incidence; label dlr='delirium incidence';
retub=reintubation; label retub='reinbution';
readm=readmission_icu; label readm='readmission'; 
remov=unplanned_removal_lda; label remov='unplanned removal device';
fixed=physical_restraints_incidence; label fixed='physically restrained';
d28=died_28; label d28='died within 28 days';
d90=died_90; label d90='died within 90 days';
* recode from 1=yes,2=no to 0=no, 1=yes;
Cogn_disab=2-Cognitieve_stoornissen;label cogn_disab='cognitive disabilities';
Alcohol=2-Alcoholmisbruik;label alcohol='alcohol abuse';
urgent_admis=2-acute_opname; label urgent_admis='urgent admission';
steriods=2- Corticostero__den;label steriods='steroid use';
resp_insuf=2-resp_insuff;label resp_insuf='respiratory insufficiency';
* log transforms;
dlr_d_log=log(dlr_d+1);label dlr_d_log='log delirium days (+1)';
coma_d_log=log(coma_d +1);label coma_d_log='log coma days (+1)';
sed_d_log=log(sed_d +1);label sed_d_log='log sedation days (+1)';
vent_d_log=log(vent_d+1);label vent_d_log='log ventilation days (+1)'; 
fix_d_log=log(fix_d+1);label fix_d_log='log fixation days (+1)'; 
los_icu_log=log(los_icu);label los_icu_log='log LOS ICU (+0)'; * min =1;
los_hosp_log=log(los_hosp); label los_hosp_log='log LOS hosp (+0)';* min =1;
run;

********** DEBUGGING ***********************************;
* options macrogen symbolgen mlogic mprint;
* options nomacrogen nosymbolgen nomlogic nomprint;
* ods trace on;
* ods trace off;


%macro explore_ar(ds=ds,outcome=, cluster=center,period=period,trt=trt,
upper_lag=, slstay=0.10, logodds=0);
** to compare OLS vs all AR models with lag =1, 2, ..., upper_lag,; 
**             vs backward AR;
** in terms of residual plots, fit plots, and fit summary;
** default upper_lag is number of measurements minus 
     number of parameters in the regression;
** no output is saved, as this is meant for visual investigation;
** data transformed to logodds scale if &logodds=1;

*get outcome label;
data _null_; set &ds; call symput('outcome_label', vlabel(&outcome));run;

* make the dataset with cluster period means; 
proc sort data=&ds;by &cluster &period;
proc means data=&ds noprint; id &trt; class &cluster &period;
var &outcome; output out=_ClusPeriodMeans mean(&outcome)=mean;run;
* select only the cluster period means;
data _ClusPeriodMeans; 
set _ClusPeriodMeans(where=(&cluster ne . and &period ne . and &trt ne .));
* calculate postperiod: the time since start of trt;
if lag(&trt)=0 or _n_=1 then postperiod=0; else if &trt=1 then postperiod+1; 
* logodds transform if a a binary outcome;
%IF &logodds=1 %THEN %DO; 
	if mean=0 then mean=0.000001; else if mean =1 then mean=0.999999;
	mean = log( mean/ (1-mean)  ); * log odds;
	label mean='log odds';
%END;
run;

* plot to see the time series;
title4 "&outcome (&outcome_label)";
%IF &logodds=1 %THEN 
	%DO; 
	title4 "&outcome (&outcome_label) LOGODDS scale";
	footnote1 h=0.8 "0% is set to 0.0001% -> logodds=-13.8, 100% is set to 0.9999% -> logodds=+13.8";
	%END;
title5 "interrupted time series analysis";
proc sgpanel data=_ClusPeriodmeans;
panelby &cluster / onepanel; *or columns=1 to get only one column;
series x=&period y=mean;
scatter x=&period y=mean /group=&trt markerattrs=(size=9) ;
rowaxis alternate;
colaxis alternate integer;
run;
footnote1 " ";

* maximum lag: number of measurements minus 1;
proc sql noprint;
select nlobs into : nobs
from dictionary.tables
where libname='WORK'
and memname='_CLUSPERIODMEANS';
quit;
%local maxlag; %let maxlag=%sysevalf(&nobs - 1);

**************** OLS ******************************;	
	options orientation=landscape;	
title6 "UNCORRECTED (OLS) regression (for comparison)";
	ods exclude dependent fitsummary parameterestimates;* not to any viewer/ods dest.;
	ods output fitsummary=_fitsummary; 
	ods output diagnosticspanel=_panel;
	ods output OLSEst.parameterestimates=_solutions;
proc autoreg data=_clusperiodmeans;by &cluster; 
*linear trend;
model mean = &period &trt postperiod*&trt / 
method=ml loglikl dw=&maxlag dwprob
plots (only) = (standardresidual fitplot acf pacf whitenoise)
;
* can use plots (UNPACKpanel) = ... to get the plots separately;
run;
	ods output close;

* keep the fitsummary, and set it anew and to the OLS fitsummary;
data _all_fits; set _fitsummary; outcome="&outcome";model="0 lags (ols)"; run; 
*keep the residual plot and set it anew and to the OLS residual plot;
data _all_plots; set _panel; 
	if id ne .;
	outcome="&outcome";model="0 lags (ols)";keep outcome model id _RESIDUAL_SQRT_CONDVAR__;run;
*keep the solutions, and set it anew and to the OLS solutions;
data _all_solutions; 
	length model $200;length outcome $30;length variable $50;
	set _solutions; 
	if index(variable,"&trt")>0; 
	model="0 lags (ols)";outcome="&outcome";
	rename variable=label;
	keep &cluster outcome variable estimate stderr probt model ;
	run;
***************all AR models ***************************;
* if no upper_lag provided then take the (error degrees of freedom from OLS) - 1;
* -1 to hold one df for error term, error degrees of freedom from OLS already substract the df for the
  fixed parameters in the model from the total df (~ number of measurements);
data _null_;set _fitsummary(where=(label2="DFE")); 
call symput('DFE_ols',nvalue2);run;	
%IF &upper_lag eq %THEN %let upper_lag=%sysevalf(&dfe_ols -1);

%DO nlag=1 %TO &upper_lag; 
title6 "autoregressive model with lag=&nlag";
%put "AR with &nlag lags";* to log;
	ods exclude dependent fitsummary parameterestimates corrgraph 
		ParameterEstimatesGivenAR ARParameterEstimates PreMSE;*  * not to any viewer/ods dest.;
	* save: ;
	ods output ConvergenceStatus (nowarn)=_converged;
	ods output FinalModel.fitsummary(nowarn)=_fitsummary; * only the AR model fit, then if present;
	ods output diagnosticspanel(nowarn)=_panel; *only if present;
	ods output FinalModel.ParameterEstimates (nowarn)=_solutions; * only id present;
proc autoreg data=_clusperiodmeans;by &cluster; 
model mean = &period &trt postperiod*&trt /nlag=&nlag  
method=ml loglikl 
plots (only ) = (standardresidual fitplot acf pacf whitenoise)
;* can use plots (UNPACKpanel) = ... to get the plots separately;
run;
	ods output close;
* keep fit statistics, if AR model could be fitted;
data _null_; set _converged; call symput('nonconverged', status);run;
 %IF &nonconverged eq 0 %THEN
 %DO; 
	data _fitsummary; set _fitsummary; 
		outcome="&outcome";model="&nlag lags (ar)";run;
	proc append force base=_all_fits data=_fitsummary;run;
	data _standardresidualplot; set _panel;
		if id ne .;
		outcome="&outcome";model="&nlag lags (ar)";
		keep outcome model id _RESIDUAL_SQRT_CONDVAR__;run;
	proc append force base=_all_plots data=_standardresidualplot;run;
 	data _solutions; 
 		length model $200;length outcome $30;length variable $50;
 		set _solutions; 
 		if index(variable,"&trt")>0; 
 		model="&nlag lags (ar)";outcome="&outcome";
		rename variable=label;
 		keep &cluster outcome variable estimate stderr probt model ;
 		run;
	proc append force base=_all_solutions data=_solutions;run;
 %END;
%END;
ods exclude none;* all back output on;
***************** AR backwards *****************************;
%put "AR backwards";* to log;
title6 "autoregressive BACKWARD model from lag=&upper_lag";
	ods exclude dependent fitsummary corrgraph ParameterEstimates 
		ParameterEstimatesGivenAR ExpAutocorr 
		ARParameterEstimates PreMSE;*  * not to any viewer/ods dest.;
	* save: ;
	ods output OLSEst.ParameterEstimates=_OLSsolutions; * OLS estimates only;
 	ods output Fitsummary=_Fitsummary; * OLS + Final backward model (if different from OLS);
 	ods output ParameterEstimates=_solutions; * OLS + Final backward model (if different from OLS);
 	ods output diagnosticspanel(nowarn)=_panel; *only if present;
	* create dataset keeping convergence status (default: not converged) 
	* for the case it is not created as ods dataset, which happens if all AR lags are removed;	
	data _converged_backwards; set _ClusPeriodMeans(keep=&cluster obs=1); status=1;reason=""; run;
 	ods output ConvergenceStatus (nowarn)=_converged_backwards; * if not created, give no warning;
* we start from the largest model, this has &dfe_ols -1 degrees of freedom;
%local max_backward; %let max_backward=%sysevalf(&dfe_ols -1);
proc autoreg data=_clusperiodmeans;by &cluster; 
model mean = &period &trt postperiod*&trt /backstep nlag=&max_backward slstay=&slstay 
method=ml loglikl
plots (only) = (standardresidual fitplot acf pacf whitenoise)
;
run;
 * depending on convergence status, status=0 means convergenced, else estimates etc are set missing;
 data _null_; set _converged_backwards; call symput('nonconverged', status);run;
 %IF &nonconverged eq 0 %THEN 
 %DO; 
	* check if the backward selection removed all the AR lags, so reduces to OLS;
 	* this is done by checking whether _OLSsolutions;
 	* and _solutions (=OLS + autoreg if not all lag removed) are equal, i.e. same number of obs;
	proc sql noprint;
   	select nlobs into : nobs_ols
   	from dictionary.tables
   	where libname='WORK'
    and memname='_OLSSOLUTIONS';
	quit;
	proc sql noprint;
   	select nlobs into : nobs_all
   	from dictionary.tables
   	where libname='WORK'
    and memname='_SOLUTIONS';
	quit;
	* if backwards selection reduced to OLS:..  ;
	%IF &nobs_all eq &nobs_ols %THEN %DO;
		* taking dataset _solutions (=_olssolutions) is ok, so no action here;
		* taking dataset _fitsummary (= OLS fit) is ok, no action here;
	%END;
	* else backward selection does not reduce to OLS;
	%IF &nobs_all > &nobs_ols %THEN %DO;
		data _solutions; set _solutions;
			if _n_ > &nobs_ols;* skip the records of the OLS model to get those of the final model;
		run; 
		data _fitsummary; set _fitsummary;
			if _n_ > 8; * first 8 records are the OLS values, then come the final model values;
		run;
	%END;
	data _fitsummary; set _fitsummary; 
		outcome="&outcome";model="backw ar";run;
	proc append force base=_all_fits data=_fitsummary;run;
 	data _solutions; 
 		length model $200;length outcome $30;length variable $50;
 		set _solutions; 
 		if index(variable,"&trt")>0; 
 		model="backw from ar(&max_backward)";outcome="&outcome";
		rename variable=label;
 		keep &cluster outcome variable estimate stderr probt model ;
 		run;
	proc append force base=_all_solutions data=_solutions;run;
	data _standardresidualplot; set _panel;
		if id ne .;
		outcome="&outcome";model="backw ar";
		keep outcome model id _RESIDUAL_SQRT_CONDVAR__;run;
	proc append force base=_all_plots data=_standardresidualplot;run;
  %END; 
ods exclude none;* all output back on;

** reporting and saving;
	options orientation=portrait;
title6 "residual plots";
proc sgpanel data=_all_plots noautolegend;
   panelby model / columns=5 onepanel; 
   needle x=id y=_RESIDUAL_SQRT_CONDVAR__;
   scatter x=id y=_RESIDUAL_SQRT_CONDVAR__/ markerattrs=(symbol=circle);
   refline -2 2 /axis=y;
   rowaxis min=-2 max=2;
   colaxis integer;* integer tick marks;
run;
	options orientation=landscape;
title6 "fit summaries";
data _report(drop=label1 cvalue1 nvalue1 label2 cvalue2 nvalue2); set _all_fits;
length label $ 32;
if missing(label1)=0 then do; label=label1; value=nValue1;output;end;
if missing(label2)=0 then do; label=label2; value=nValue2; output;end;
run; 
proc tabulate data=_report; class model label;var value;
table model, label*(value=' ')*(mean=' ')*f=8.3;
run;
title6 "solution summaries";
proc print data=_all_solutions;run;
	options orientation=portrait;
%mend explore_ar;

%explore_ar(outcome=retub, binary=1);

***** DATA CHECKS? **************************;

***** ANALYSIS *********************************;

	ods pdf style=statistical file="underpin_neuro.pdf" compress=9 startpage=no ;
Title "UNDERPIN neurology: no covariate correction";
%let pathname =%sysget(SAS_EXECFILEPATH);
footnote "program &pathname";*place and name of program used;

/* uniform macro parameters;
%let ds=ds;%let cluster=center;%let period_min=1;%let period_max=14;
%let period=period;%let trt=trt;%let slstay=0.10;%let dseff=table;
*/

** perhaps leave out period 7?;
** or make a different collection point?;
title2 "original time periods";
%rm_design(ds=ds, cluster=center,trt=trt, period=period);

title2 " ";
* delirium-coma free;
title2 "OLS best fit";
%explore_ar(outcome=dcf);
%its_cont(outcome=dcf,outcome_min=0,outcome_max=28, nlag=0, dseff=table3, dsdescrip=table3_cont);

*delirium days;
*%its_cont(outcome=dlr_d, outcome_min=0, outcome_max=28,nlag=3, dseff=table3, dsdescrip=table3_cont);
	*log;
title2 "OLS best fit";
%explore_ar(outcome=dlr_d_log);
%its_cont(outcome=dlr_d_log, outcome_min=0, outcome_max=4,nlag=0, dseff=table3, dsdescrip=table3_cont);


* coma days;
*%its_cont(outcome=coma_d, outcome_min=0, outcome_max=28, nlag=1, dseff=table3, dsdescrip=table3_cont);
	* log;
title2 "OLS best fit";
%explore_ar(outcome=coma_d_log);
%its_cont(outcome=coma_d_log, outcome_min=0, outcome_max=4, nlag=0, dseff=table3, dsdescrip=table3_cont);

* sedation days; * increase!;
*%its_cont(outcome=sed_d, outcome_min=0, outcome_max=28, nlag=2, dseff=table3, dsdescrip=table3_cont);
	*log;
title2 "lag 0-2,but fit for 0 best and white noise best";
%explore_ar(outcome=sed_d_log);
%its_cont(outcome=sed_d_log, nlag=0,outcome_min=0, outcome_max=4,dseff=table3, dsdescrip=table3_cont);

* delirium medication days: prevention;
title1 "prevention: only 3 measurements, only OLS analysis done";
data ds1; set ds; where dlr_prev=1;label med_d='medication days (prevention)';
med_d_log=log(med_d+1);label med_d_log='log medication days (+1) (prevention)';run;
*%its_cont(ds=ds1, outcome=med_d, outcome_min=0, outcome_max=28,nlag=2, dseff=table3, dsdescrip=table3_cont);
%its_cont(ds=ds1, outcome=med_d_log, outcome_min=0, outcome_max=4,nlag=0, dseff=table3, dsdescrip=table3_cont);


**** NICE example ***;

* delirium medication days: treatment;
title1 "treatment";
title2 "lag=0 or 2, fits indecisive, whitenoise probs for 2 better";
data ds1; set ds; where dlr_trt=1;label med_d='medication days (treatment)';
med_d_log=log(med_d+1);label med_d_log='log medication days (+1) (treatment)';run;
*%its_cont(ds=ds1, outcome=med_d, outcome_min=0, outcome_max=28,nlag=2, dseff=table3, dsdescrip=table3_cont);
%explore_ar(ds=ds1, outcome=med_d_log);
%its_cont(ds=ds1, outcome=med_d_log, outcome_min=0, outcome_max=0,nlag=2, dseff=table3, dsdescrip=table3_cont);



* delirium medication days: prevention and treatment;
title1 "prevention and treatment";
title2 "not sufficient numbers of measurements";
data ds1; set ds; where dlr_prevtrt=1;label med_d='medication days (treat+prev)';
med_d_log=log(med_d+1);label med_d_log='log medication days (+1) (treat+prev)';run;
*%its_cont(ds=ds1, outcome=med_d, outcome_min=0, outcome_max=28,nlag=2, dseff=table3, dsdescrip=table3_cont);
%explore_ar(ds=ds1,outcome=med_d_log);
%its_cont(ds=ds1, outcome=med_d_log, nlag=0,outcome_min=0, outcome_max=4, dseff=table3, dsdescrip=table3_cont);

title1 "";
*delirium incidence;
title2 "OLS best fit";
%explore_ar(outcome=dlr);
%its_bin(outcome=dlr, nlag=0, dseff=table3, dsdescrip=table3_bin); 


*mechanical ventilation days;
*%its_cont(outcome=vent_d, outcome_min=0, outcome_max=28,nlag=2, dseff=table3, dsdescrip=table3_cont);
	* log;
title2 "lag 0 or 1, 1 has better whithe noise prob";
%explore_ar(outcome=vent_d_log);
%its_cont(outcome=vent_d_log, nlag=1, outcome_min=0, outcome_max=4, dseff=table3, dsdescrip=table3_cont);


* re intubation ;
title2 "lag 0 or 2, 0 has better white noise prob";
%explore_ar(outcome=retub);
%its_bin(outcome=retub, nlag=0, dseff=table3, dsdescrip=table3_bin); 

**NICE EXAMPLE;
*readmission;
title2 "lag 0,2,3,4,5: 2 has the better AICC and whithe noise prob";
%explore_ar(outcome=readm);
%its_bin(outcome=readm,nlag=2, dseff=table3, dsdescrip=table3_bin); 


* unplanned removal devices;
title2 "lag 0 or 1, but 0 has better whithe noise prob";
%explore_ar(outcome=remov);
%its_bin(outcome=remov, nlag=0, dseff=table3, dsdescrip=table3_bin); 

** NICE EXAMPLE**;
* physical restraints;
title2 "lag 0 or 5, but 0 has better white noise prob";
%explore_ar(outcome=fixed);
%its_bin(outcome=fixed, nlag=0, dseff=table3, dsdescrip=table3_bin); 


* physical restraints duration;

*%its_cont(outcome=fix_d, nlag=2, dseff=table3, dsdescrip=table3_cont);
	*log;
title2 "lag 0 or 5, but 0 has better whithe noise prob";
%explore_ar(outcome=fix_d_log);
%its_cont(outcome=fix_d_log, outcome_min=0, outcome_max=4, nlag=0, dseff=table3, dsdescrip=table3_cont); 


*LOS ICU;
* %its_cont(outcome=los_icu, outcome_min=1, outcome_max=130, nlag=2, dseff=table3, dsdescrip=table3_cont);
	*log;
title2 "lag 0 or 5, but 0 has better white noise prob.";
%explore_ar(outcome=los_icu_log);
%its_cont(outcome=los_icu_log, outcome_min=0, outcome_max=5, nlag=0, dseff=table3, dsdescrip=table3_cont);


*LOS hosp;
* %its_cont(outcome=los_hosp, outcome_min=1, outcome_max=150, nlag=2, dseff=table3, dsdescrip=table3_cont);
	*log;
title2 "lag 0 or 5, but 0 has better white noise prob";
%explore_ar(outcome=los_hosp_log);
%its_cont(outcome=los_hosp_log, outcome_min=0, outcome_max=5,nlag=0, dseff=table3, dsdescrip=table3_cont);

*** NICE example ***;
*D28;
title2 "lag 0,2,4, but 4 has bad white noise prob., 0 or 2 equal, then 0";
%explore_ar(outcome=d28);
%its_bin(outcome=d28, nlag=0, dseff=table3, dsdescrip=table3_bin); 

*D90;
title2 "lag 0-4, but 3,4 bad white noise, 0-2 seem quite similar, 0 best in AICC and white noise";
%explore_ar(outcome=d90);
%its_bin(outcome=d90, nlag=0, dseff=table3, dsdescrip=table3_bin); 
	ods pdf close;

** save the tables;
libname dir ".";
data dir.table3_bin; set table3_bin;run;
data dir.table3_cont; set table3_cont;run;
data dir.table3; set table3;run;


** report;
	ods rtf style=minimal file="underpin_neuro_bestmodels.doc";
title "selected model based on fit within OLS and AR models";
	options fmtsearch= (dir.formats); * to get formats;
	options orientation=landscape;
	options pagesize=70;
libname dir ".";
data table3; set dir.table3;
* exponentiate log transformed values;
if index(outcome, 'log') > 0 then 
do; 
MR=exp(estimate); MR_lower=exp(lower); MR_upper=exp(upper); 
end;
run;

data table3_final; set table3; 
if MR ne . then do; estimate = .; lower=.; upper=.;end;run;
proc print data=table3_final noobs; 
by center;
* select only the best models between OLS and AR models (so not Newey-West or backwards selection); 
where index(model, "Newey-West")=0 and index(model, "backwards")=0;
var outcome outcome_label label estimate lower upper MR MR_lower MR_upper probt model;
run; 
	ods rtf close;


/** chosen not to report ;
title "OLS with Newey-West corrected standard errors";
proc print data=table3 noobs; 
by center;
where index(model, "Newey-West")>0;
var outcome outcome_label label estimate lower upper probt model;
run; 

title "AR with backward selection of lags";
proc print data=table3 noobs; 
by center;
where index(model, "backwards")>0;
var outcome outcome_label label estimate lower upper probt model;
run; 
*/
