** based on neuro_200108.sas;
** to do: add AR backward to explore together with fit measure;



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
        c) if requested: AR regression with specified lag (nlag);
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
scatter x=&period y=mean /group=trt markerattrs=(size=9) ;
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
scatter x=&period y=percent /group=trt markerattrs=(size=9) ;
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
slstay=0.10,nlag, dseff=);
* provides the following analyses for an interrupted time series;
* a) backwards autoregressive regression;
* b) ordinary least squares with Newey-West corrected standard errors; 
* c) autoregressive regression with specified lag: &nlag;
* the results are saved in &dseff if requested;

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
 * create dataset keeping convergence status for the case it is not created as ods dataset;
 * which happens if all AR lags are removed;	
 data _converged_backwards; set _ClusPeriodMeans(keep=&cluster obs=1); status=0;reason=""; run;
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
	* else backward selection keeps some AR terms: ..;
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




********** DEBUGGING ***********************************;
* options macrogen symbolgen mlogic mprint;
* options nomacrogen nosymbolgen nomlogic nomprint;
* ods trace on;
* ods trace off;

%macro explore_ar(ds=ds,outcome=, cluster=center,period=period,trt=trt,
upper_lag=, slstay=0.10);
** to compare all AR models with lag =1, 2, ..., upper_lag, OLS,; 
**             and backward AR;
** in terms of residual plots, fit plots, and fit summary;
** default upper_lag is number of measurements minus 
     number of parameters in the regression;

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
run;

* maximum lag: number of measurements minus 1;
proc sql noprint;
select nlobs into : nobs
from dictionary.tables
where libname='WORK'
and memname='_CLUSPERIODMEANS';
quit;
%local maxlag; %let maxlag=%sysevalf(&nobs - 1);
	
	options orientation=landscape;	
title4 "&outcome (&outcome_label)";
title5 "interrupted time series analysis";
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

* if no upper_lag provided then take the (error degrees of freedom from OLS) - 1;
* -1 to hold one df for error term, error degrees of freedom from OLS already substract the df for the
  fixed parameters in the model from the total df (~ number of measurements);
data _null_;set _fitsummary(where=(label2="DFE")); 
call symput('DFE_ols',nvalue2);run;	%IF &upper_lag eq %THEN %let upper_lag=%sysevalf(&dfe_ols -1);

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
/*
%put "AR backwards";* to log;
title6 "autoregressive BACKWARD model from lag=&upper_lag";
	ods output OLSEst.ParameterEstimates=_OLSsolutions; * OLS estimates only;
	ods output Fitsummary=_Fitsummary; * OLS + Final backward model (if different from OLS);
	ods output ParameterEstimates=_solutions; * OLS + Final backward model (if different from OLS);
	ods output diagnosticspanel(nowarn)=_panel; *only if present;
	* create dataset keeping convergence status for the case it is not created as ods dataset;
 	* which happens if all AR lags are removed;	
 	data _converged_backwards; set _ClusPeriodMeans(keep=&cluster obs=1); status=0;reason=""; run;
 	ods output ConvergenceStatus (nowarn)=_converged_backwards; * if not created, give no warning;
proc autoreg data=_clusperiodmeans;by &cluster; 
* we start from the largest model: nlag=&maxlag;
model mean = &period &trt postperiod*&trt /backstep nlag=&upper_lag  slstay=&slstay 
method=ml loglikl
plots (only) = (standardresidual fitplot acf pacf whitenoise)
;
run;
***;
here to do: get the right diagnostic panel, the fit statistics and estimates;
** ;
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
	* else backward selection keeps some AR terms: ..;
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
*/

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

********* READ 	DATA *******;
options pagesize=60;
* get formats;
options fmtsearch= (dir.formats);
libname dir ".";
** to do: get a dataset ...;

 
***** DATA CHECKS? **************************;

***** ANALYSIS *********************************;

	ods pdf style=statistical file="ITS_study.pdf" compress=9 startpage=no ;
Title "ITS study";
%let pathname =%sysget(SAS_EXECFILEPATH);
footnote "program &pathname";*place and name of program used;



** perhaps leave out period 7?;
** or make a different collection point?;
title2 "original time periods";
%rm_design(ds=ds, cluster=center,trt=trt, period=period);

* example on continuous outcome, output to Table3; 
* example on binary outcome; 

** report;
ods rtf style=minimal file="ITS_study.doc";
title "selected model based on fit within OLS and AR models";
proc print data=table3 noobs; 
by center;
where index(model, "Newey-West")=0 and index(model, "backwards")=0;
var outcome outcome_label label estimate lower upper probt model;
run; 


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
ods rtf close;
