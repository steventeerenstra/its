
********* MACROs  ****;
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
**      it also fits OLS with Newey-West corrected std errors;
**      if requested: the selected (best) model can be marked;
**      if requested: a dataset containing solutions from all models 
                      with the marked best model can be saved for post-processing;
** %its_cont: both descriptives (%sw_cont_descrip) and analyses (%explore_ar) for continuous outcome;
** %its_bin: idem for binary;
**;
** typically %its_cont/bin or explore_ar is first run;
**  then the selected model is put as extra argument;
**  and finally the output for a report by post-processing the saved solutions dataset; 

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

%macro explore_ar(ds=ds,outcome=, cluster=center,period=period,trt=trt,
upper_lag=, slstay=0.10, logodds=0, select=, only=, dseff=);
** if &only= then compare OLS vs all AR models with lag =1, 2, ..., upper_lag,; 
**    vs backward AR;
**    in terms of residual plots, fit plots, and fit summary;
**    and also: OLS with Newey-West corrected std.errors is conducted;
** if &only=bw then only the OLS and backward AR are fitted, 
**	  and backward is marked (selection=1);
** if &only=nw then idem dito for the Newey-West analysis;
**
** default upper_lag is number of measurements minus 
       number of parameters in the regression;
** the cluster-period means are transformed to logodds scale if &logodds=1;
**     this assumes of course that the binary outcome is {0,1}-coded; 
** output (solutions) can be saved to &dseff if non-nil;
** 	  in this output, the variable selection (=0,1) marks the selected analysis;
** &select=n (with n=0,1,2, ..) marks the AR(n) analysis as selected analysis;
**          select=0 is the OLS analysis
** &select=bw marks the backward AR analysis as selected analysis;
** &select=nw marks the OLS with Newey-West corrected std.errors as selected; 
** ds_eff saves the &trt estimates and marks the selected analysis;

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
label mean="&outcome";
* logodds transform if a a binary outcome;
%IF &logodds=1 %THEN %DO; 
	if mean=0 then mean=0.000001; else if mean =1 then mean=0.999999;
	mean = log( mean/ (1-mean)  ); * log odds;
	label mean="log odds &outcome";
%END;
run;

* plot to see the time series;
%IF &logodds=1 %THEN 
	%DO; 
	title4 "&outcome (&outcome_label) LOGODDS scale";
	footnote1 h=0.8 "for binary outcomes: 0% is set to 0.0001% -> logodds=-13.8, 100% is set to 99.9999% -> logodds=+13.8";
	%END;
%ELSE %DO;
	title4 "&outcome (&outcome_label)";
	footnote " ";
	%END;
title5 "interrupted time series analysis";
proc sgpanel data=_ClusPeriodmeans;
panelby &cluster / onepanel; *or columns=1 to get only one column;
series x=&period y=mean;
scatter x=&period y=mean /group=&trt markerattrs=(symbol=CircleFilled size=11) ;
rowaxis alternate;
colaxis alternate integer;
%IF &logodds=1 %THEN %DO; 
refline -13.8 /axis=y label="0%"; 
refline 13.8 / axis=y label="100%";
%END;
run;


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
	ods output fitsummary=_fitsummary_ols; 
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

* keep the fitsummary, and set it anew by setting it to the OLS fitsummary (should always exist);
data _all_fits; set _fitsummary_ols; 
	outcome="&outcome";model="0 lags (ols)"; 
	if label2="DFE" then do; call symput('dfe_ols', nvalue2); end;*pass on the error dfe; 
	run; 
*keep the residual plot and set it anew and to the OLS residual plot;
data _all_plots; set _panel; 
	if id ne .;
	outcome="&outcome";model="0 lags (ols)";keep outcome model id _RESIDUAL_SQRT_CONDVAR__;run;
*keep the solutions, and set it anew by setting it to the OLS solutions (should always exist);
data _all_solutions; 
	length outcome $30;length outcome_label $100;length variable $50;
	length model $50; length model_descrip $200; 
	length estimate lower upper stderr probt 8;* to order the sequence for printing;
	set _solutions; 
	if index(variable,"&trt")>0; 
	outcome="&outcome"; outcome_label="&outcome_label";%IF &logodds=1 %THEN 
	%DO;outcome="log odds &outcome";outcome_label="log odds &outcome_label";%END;
	model="0 lags (ols)";
	model_descrip="ITS: &period &trt postperiod*&trt // OLS";
		%IF &dfe_ols ge 1 %THEN %DO;
	lower=estimate - tinv(0.975,&dfe_ols)*stderr; upper=estimate + tinv(0.975, &dfe_ols)*stderr;
		%END; %ELSE %DO; lower=.; upper=.;%END;
	select=0; %IF &select=0 %THEN %DO; select=1;%END;
	rename variable=label;
	keep &cluster outcome outcome_label variable estimate lower upper stderr probt 
		model model_descrip select;
	run;

*************; %IF &only= %THEN %DO;********************;
***************all AR models ***************************;
* if no upper_lag provided then take the (error degrees of freedom from OLS) - 1;
* -1 to hold one df for error term, error degrees of freedom from OLS already substracts; 
*  the df for the fixed parameters in the model from the total df (~ number of measurements);
%IF &upper_lag eq %THEN %let upper_lag=%sysevalf(&dfe_ols -1);

%local nonconverged; 
%DO nlag=1 %TO &upper_lag; 
title6 "autoregressive model with lag=&nlag";
%put "AR with &nlag lags (&outcome)";* to log;
	ods exclude dependent fitsummary parameterestimates corrgraph 
		ParameterEstimatesGivenAR ARParameterEstimates PreMSE;*  * not to any viewer/ods dest.;
	* save: ;
	ods output ConvergenceStatus (nowarn)=_converged_&nlag; *specific convergence check;
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
* checking convergence, if AR converged then the convergence dataset is created and status=0;
%let nonconverged=1;
data _null_; set _converged_&nlag; call symput('nonconverged', status);run;
	* if converged: ;
	%IF &nonconverged eq 0 %THEN
 	%DO; 
	data _fitsummary; set _fitsummary; 
		outcome="&outcome";model="&nlag lags (ar)";
		if label2="DFE" then do; call symput('dfe', nvalue2); end;*pass on the error dfe; 
		run;
	data _standardresidualplot; set _panel;
		if id ne .;
		outcome="&outcome";model="&nlag lags (ar)";
		keep outcome model id _RESIDUAL_SQRT_CONDVAR__;run;
 	data _solutions; 
		length outcome $30;length outcome_label $100;length variable $50;
		length model $50; length model_descrip $200;
		set _solutions; 
		if index(variable,"&trt")>0; 
		outcome="&outcome"; outcome_label="&outcome_label";%IF &logodds=1 %THEN 
		%DO;outcome="log odds &outcome";outcome_label="log odds &outcome_label";%END;
		model="&nlag lags (ar)";
		model_descrip="ITS: &period &trt postperiod*&trt // AR(lag 0 till &nlag)";
			%IF &dfe ge 1 %THEN %DO;
		lower=estimate - tinv(0.975,&dfe)*stderr; upper=estimate + tinv(0.975, &dfe)*stderr;
			%END; %ELSE %DO; lower=.; upper=.;%END;
		select=0; %IF &select=&nlag %THEN %DO; select=1;%END;
		rename variable=label;
 		keep &cluster outcome outcome_label variable estimate stderr probt lower upper 
			model model_descrip select;
 		run;
 	%END;
 	* if not converged (convergence dataset not created or created with status ne 0;
 	%IF &nonconverged ne 0 %THEN
 	%DO;
	* note that by proc append later on, all statistics/estimates are set to missing;
	data _fitsummary; 
		set _fitsummary_ols; * take the variables from the OLS output;
		model="&nlag lags (ar)";outcome="&outcome";
		cValue1="";nValue1=.;cValue2="";nValue2=.;*put to missing since not converged;
		run;
 	data _solutions; 
		length outcome $30;length outcome_label $100;length label $50;
		length model $50; length model_descrip $200;
		outcome="&outcome"; outcome_label="&outcome_label";%IF &logodds=1 %THEN 
		%DO;outcome="log odds &outcome";outcome_label="log odds &outcome_label";%END;
		label="&trt";estimate=.;stderr=.;probt=.;lower=.;upper=.;
		select=0; * non-convergent model will not be chosen;
		set _converged_&nlag; * to get the &cluster variable value and reason non-convergence;
		model="&nlag lags NOT converged";	
		model_descrip=catx(': ',"ITS lag &nlag NOT converged", reason);
		keep &cluster outcome outcome_label label estimate stderr probt lower upper 
			model model_descrip select;
 		run;
	%END;
proc append force base=_all_fits data=_fitsummary;run;
proc append force base=_all_plots data=_standardresidualplot;run;
proc append force base=_all_solutions data=_solutions;run;
%END;
ods exclude none;* all back output on;
****************; %END; ** end of IF &only= *****************************;

*****************;%IF (&only= or &only=bw) %THEN %DO; ****************;
***************** AR backwards *************************************;
%put "AR backwards (&outcome)";* to log;
* we start from the largest model, this has &dfe_ols -1 degrees of freedom,but at least 1;
%local max_backward; %let max_backward=%sysfunc(max(1,%sysevalf(&dfe_ols -1)));
title6 "autoregressive BACKWARD model from lag=&max_backward";
	ods exclude dependent fitsummary corrgraph ParameterEstimates 
		ParameterEstimatesGivenAR ExpAutocorr 
		ARParameterEstimates PreMSE;*  * not to any viewer/ods dest.;
	* save: ;
	ods output OLSEst.ParameterEstimates=_OLSsolutions; * OLS estimates only;
 	ods output Fitsummary=_Fitsummary; * OLS + Final backward model (if different from OLS);
 	ods output ParameterEstimates=_solutions; * OLS + Final backward model (if different from OLS);
 	ods output diagnosticspanel(nowarn)=_panel; *only if present;
proc autoreg data=_clusperiodmeans;by &cluster; 
model mean = &period &trt postperiod*&trt /backstep nlag=&max_backward slstay=&slstay 
method=ml loglikl
plots (only) = (standardresidual fitplot acf pacf whitenoise)
;
run;
* we assume that OLS fit will always work, so backwards will either reduce to OLS;
* or some AR terms will be left; 
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
data _fitsummary; set _fitsummary; 
	outcome="&outcome";model="backw=>OLS";run;
 data _solutions; 
 	length outcome $30;length outcome_label $100;length variable $50;
	length model $50; length model_descrip $200;
 	set _solutions; 
	if index(variable,"&trt")>0;
	outcome="&outcome"; outcome_label="&outcome_label";%IF &logodds=1 %THEN 
	%DO;outcome="log odds &outcome";outcome_label="log odds &outcome_label";%END;
	model="backw=>OLS"; 
	model_descrip="ITS: &period &trt postperiod*&trt // backwards AR from &max_backward => OLS";
		%IF &dfe_ols ge 1 %THEN %DO;
	lower=estimate - tinv(0.975,&dfe_ols)*stderr; upper=estimate + tinv(0.975, &dfe_ols)*stderr;
		%END; %ELSE %DO; lower=.; upper=.;%END;
	* if selected or only analysis, then mark the backward analysis;
	select=0; %IF (&select=bw or &only=bw) %THEN %DO; select=1;%END;
	rename variable=label;
 	keep &cluster outcome outcome_label variable estimate stderr probt lower upper 
		model model_descrip select;
 	run;
data _standardresidualplot; set _panel;
	if id ne .;
	outcome="&outcome";model="bw=>OLS";
	keep outcome model id _RESIDUAL_SQRT_CONDVAR__;run;
%END;
* else backward selection does not reduce to OLS;
%IF &nobs_all > &nobs_ols %THEN %DO;
data _fitsummary; set _fitsummary;
		if _n_ > 8; * first 8 records are the OLS values, then come the final model values;
		outcome="&outcome";model="backw AR";
		if label2="DFE" then do; call symput('dfe_bw', nvalue2); end;*pass on the error dfe; 
	run;
 data _solutions; 
 	length outcome $30;length outcome_label $100;length variable $50;
	length model $50; length model_descrip $200;
 	set _solutions; 
 	if _n_ > &nobs_ols;* skip the records of the OLS model to get those of the final model;
	if index(variable,"&trt")>0; 
	outcome="&outcome"; outcome_label="&outcome_label";%IF &logodds=1 %THEN 
	%DO;outcome="log odds &outcome";outcome_label="log odds &outcome_label";%END;
	model="backw AR";
	model_descrip="ITS: &period &trt postperiod*&trt // backwards AR from &max_backward";
		%IF &dfe_bw ge 1 %THEN %DO;
	lower=estimate - tinv(0.975,&dfe_bw)*stderr; upper=estimate + tinv(0.975, &dfe_bw)*stderr;
		%END; %ELSE %DO; lower=.; upper=.;%END;
	select=0; %IF &select=bw %THEN %DO; select=1;%END;
	rename variable=label;
 	keep &cluster outcome outcome_label variable estimate stderr probt lower upper 
		model model_descrip select;
 	run;
data _standardresidualplot; set _panel;
	if id ne .;
	outcome="&outcome";model="bw AR";
	keep outcome model id _RESIDUAL_SQRT_CONDVAR__;run;
%END;
proc append force base=_all_fits data=_fitsummary;run;
proc append force base=_all_solutions data=_solutions;run;
proc append force base=_all_plots data=_standardresidualplot;run;
ods exclude none;* all output back on;
************; %END; ** end of the IF (&only= or &only=bw) ****************;
 
***********;%IF (&only= or &only=nw) %THEN %DO;****************************;
*********** OLS with Newey-West standard errors ***************************;
	ods exclude dependent fitsummary parameterestimates;* not to any viewer/ods dest.;
	ods output fitsummary=_fitsummary; * same as for OLS;
	ods output  ParameterEstimates=_solutions;* OLS estimates with Newey-West stderr;
proc autoreg data=_clusperiodmeans;by &cluster; 
*linear trend;
model mean = &period &trt postperiod*&trt /covest=neweywest 
method=ml loglikl plots=none
;
run;
	ods output close;
 * no convergence status available.. will always converge (?);
data _fitsummary; set _fitsummary; 
	outcome="&outcome";model="ols(NWse)"; 
	if label2="DFE" then do; call symput('dfe_nw', nvalue2); end;*pass on the error dfe; 
	run; 
* no residual plot as only SE are corrected;
 *keep the solutions, and set it anew by setting it to the OLS solutions (should always exist);
data _solutions; 
	length outcome $30;length outcome_label $100;length variable $50;
	length model $50; length model_descrip $200;
	set _solutions; 
	if index(variable,"&trt")>0; 
	outcome="&outcome"; outcome_label="&outcome_label";%IF &logodds=1 %THEN 
	%DO;outcome="log odds &outcome";outcome_label="log odds &outcome_label";%END;
	model="ols(NWse)";
	model_descrip="ITS: &period &trt postperiod*&trt // OLS with Newey-West corrected std.err.";
	lower=estimate - tinv(0.975,&dfe_nw)*stderr; upper=estimate + tinv(0.975, &dfe_nw)*stderr;
	* if the selected or only analysis, then mark the Newey-West analysis *************;
	select=0; %IF (&select=nw or &only=nw) %THEN %DO; select=1;%END;
	rename variable=label;
	keep &cluster outcome outcome_label variable estimate lower upper stderr probt 
		model model_descrip select;
	run;
proc append force base=_all_fits data=_fitsummary;run;
proc append force base=_all_solutions data=_solutions;run;
************; %END; ** end of the IF (&only= or &only=nw) ****************;

******** reporting*********;
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
****** saving *****;
%IF &dseff ne %THEN %DO; proc append force base=&dseff data=_all_solutions;run;%END;
%mend explore_ar;

*** descriptives and inferential together; 
* continuous;
%macro its_cont(ds=ds,outcome=, cluster=center, period=period, trt=trt,
outcome_min=-1000, outcome_max=+1000,period_min=1,period_max=14,
upper_lag=, slstay=0.10,select=, only=,dsdescrip=,dseff=);
	* descriptive;
	%sw_cont_descrip(ds=&ds,outcome=&outcome, cluster=&cluster, period=&period, trt=&trt,
	outcome_min=&outcome_min, outcome_max=&outcome_max,period_min=&period_min,
	period_max=&period_max,dsdescrip=&dsdescrip);
	* inferential;
	%explore_ar(ds=&ds,outcome=&outcome, cluster=&cluster,period=&period,trt=&trt,
	upper_lag=&upper_lag, slstay=&slstay,logodds=0, select=&select,only=&only, dseff=&dseff);
%mend its_cont;

* binary;
%macro its_bin(ds=ds,outcome=, logodds=1, cluster=center, period=period, trt=trt,
period_min=1,period_max=14,
upper_lag=, slstay=0.10,select=, only=,dsdescrip=,dseff=);
	* descriptive;
	%sw_bin_descrip(ds=&ds,outcome=&outcome, cluster=&cluster, period=&period, trt=&trt,
	period_min=&period_min,period_max=&period_max,dsdescrip=&dsdescrip); 
	* inferential;
	%explore_ar(ds=&ds,outcome=&outcome, cluster=&cluster,period=&period,trt=&trt,
	upper_lag=&upper_lag, slstay=&slstay,logodds=&logodds, select=&select, only=&only, dseff=&dseff);
%mend its_bin;


********* READ 	DATA *******;
options pagesize=60;
* get formats;
options fmtsearch= (dir.formats);
libname dir ".";


data ds; set ..;run;
* show variables in dataset;
proc contents data=..;run;



********** DEBUGGING ***********************************;
* options macrogen symbolgen mlogic mprint;
* options nomacrogen nosymbolgen nomlogic nomprint;
* ods trace on;
* ods trace off;



***** DATA CHECKS? **************************;



***** ANALYSIS *********************************;

	ods pdf style=statistical file="....pdf" compress=9 startpage=no ;
Title "...";
%let pathname =%sysget(SAS_EXECFILEPATH);
footnote "program &pathname";*place and name of program used;



title2 "original time periods";
%rm_design(ds=ds, cluster=center,trt=trt, period=period);

title2 " ";
* continuous outcome ;
title2 "backward AR best fit";
%its_cont(outcome=..,outcome_min=0,outcome_max=28, select=bw, dseff=table3, dsdescrip=table3_cont);


*binary outcome, analysis on logodds, for later odds ratio;
title2 "OLS best fit=backwards, although residuals a bit worse than AR(3), but white noise worse for AR(3)";
%its_bin(outcome=..., logodds=1, select=0,dseff=table3, dsdescrip=table3_bin); 


	ods pdf close;

** save the tables;
libname dir ".";
data dir.table3_bin; set table3_bin;run;
data dir.table3_cont; set table3_cont;run;
data dir.table3; set table3;run;


** report;
	ods rtf style=minimal file="....doc";
title "selected model based on fit within OLS and AR models";
	options fmtsearch= (dir.formats); * to get formats;
	options orientation=landscape;
	options pagesize=70;
libname dir ".";
data table3; set dir.table3;
* exponentiate estimates for continuous variables that were log transformed;
*   these had variable names with _log in it;
if index(outcome, 'log') > 0 and index(outcome,'log odds')=0 then 
do; 
MR=exp(estimate); MR_lower=exp(lower); MR_upper=exp(upper); 
end;
* idem for log odds values;
if index(outcome,'log odds')> 0 then
do;
OR=exp(estimate);OR_lower=exp(lower);OR_upper=exp(upper);
end;
run;

data table3_final; set table3; 
if MR ne . then do; estimate = .; lower=.; upper=.;end;run;
proc print data=table3_final noobs; 
by center;
* selected best models; 
where selection=1;
var outcome outcome_label label estimate lower upper MR MR_lower MR_upper probt model;
run; 
	ods rtf close;


title "OLS with Newey-West corrected standard errors";
proc print data=table3 noobs; 
by center;
where index(model, "ols(NWse)")>0;
var outcome outcome_label label estimate lower upper probt model;
run; 

title "AR with backward selection of lags";
proc print data=table3 noobs; 
by center;
where index(model, "backw")>0;
var outcome outcome_label label estimate lower upper probt model;
run; 


