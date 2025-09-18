**************general DATA MANAGEMENT ***********************;
**** read data *****;
options pagesize=60;
*> change directory (in windows);
x ' cd C:\Users\steer\surfdrive\Actief\19 Santeon mITS SHOUT\03 Kidney (Noel Engels)\analyses'; *framework;
*x ' cd C:\Users\st\surfdrive\Actief\19 Santeon mITS SHOUT\03 Kidney (Noel Engels)\analyses'; *lenovo;


libname dir '.';
libname data '..\data'; 

*> **read formats ****;
options fmtsearch= (data.formats);;



/*  what are unformatted values;
proc freq data=data.decision; 
tables study_phase; 
format _ALL_ ;
run ;
*compare this with formatted values;
proc freq data=data.decision; 
tables study_phase; 
;
run ;

data _ds; set data.decision;
zkh=hospital; 
year=year(T0_survey_date); month=month(T0_survey_date);
cmonth= (year-2019)*12+ month;* sequential month;
intervention=0*(study_phase=1) + 1*(study_phase=2); * study_phase is coded: 1=pre, 2=post;
run;
/* check recoding
proc freq data=_ds; table zkh*hospital T0_survey_date*year 
       T0_survey_date*month intervention*study_phase
        /missing noperc nocol norow;run;
proc freq data=_ds; table cmonth*month/missing norow nocol noperc;run;
proc means data=ds mean nmiss; var year cmonth;run;
*/



**********************************************************;
************* macros **************************************;
%macro ExecPrg;

%if %sysfunc(getoption(sysin)) ne %str() %then %do;
  /* Batch Execution */
%sysfunc(getoption(sysin))
%end;
%else %do;
  /* Interactive Execution */
  %sysget(SAS_EXECFILEPATH)
%end;
%mend execprg;


%macro summarize(ds_in=, cluster=zkh, period=cmonth, intervention=intervention, ds_variables=, ds_summary=summary);
/*** input: 
- a dataset &ds_in with &cluster, &period, &intervention variable;
    variable &intervention should be {0,1}-coded
- a dataset &ds_variables with the outcome variables that are present in &ds_in and 
     user specification of their labels 
output:
- a summary dataset &ds_summary with cluster period and average outcomes 
**/



**** in three steps aggregrate all variables within month;
* step 1: make macro variable varlist;
data _null_;
 length allvars $2000;
 retain allvars ' ';
 set &ds_variables end=eof;
 allvars = trim(left(allvars))||' '||left(varname);
 if eof then call symput('varlist', allvars);
 run;
%put &varlist;*check;

* step 2: aggregate in _ds each variable in &varlist;
proc means data=&ds_in  noprint mean n stddev;
	class &cluster &period &intervention;
	var &varlist;
	output out=_sum0 
		mean(&varlist)= n(&varlist)= std(&varlist)=/autoname
		; * possibly add covariates like mean(differentiation)=mean_diff etc;
run;
* step 3: keep only records with zkh cmonth intervention and outcome;
data _sum1; 
	set _sum0;
	where zkh ne "" and intervention ne . and cmonth ne .;
run;

********* add the labels in two steps ***********************************;
* step 1: make label statements from dataset; 
%let semicolon='; '; * to protect if from being executed;
data _null_;
 length label_statement $1000;
 retain label_statement ' ';
 set ds_variables end=eof;
 label_statement = 'label '||trim(left(varname))||'_mean ='||trim(left(varlabel))||&semicolon||left(label_statement);
 if eof then call symput('label_statements', label_statement);
 run;
*step 2: add the labels using the label statements;
data _sum2; set _sum1;
&label_statements;
run;


*** calculate and add time start intervention ********************************;
proc sort data=_sum2;
	by &cluster &period &intervention;
run;
proc means data=_sum1 noprint;
	by &cluster;
	where &intervention=1;
	var &period;
	output out=_changepoint min(&period)=change;
run;

* output the summary dataset;
data &ds_summary;*** add time since intervention to dataset;
	merge _sum2 _changepoint;
	by &cluster;
	time_intv=&intervention*(&period-change);
run;

/* clean up;
proc datasets nolist; delete _: ; *all datasets starting with _; ;run;quit;
*/
%mend;



%macro ar_model1vs2(ds=summary, outcome_name=, outcome_n=,
cluster=zkh, period=cmonth, intervention=intervention, 
model1_label=%str(no trend), model1=%str(intervention),
model2_label=%str(trend),model2=%str(intervention cmonth time_intv),
details=1,period_fmt=outfmt);

*** this macro analyzes a dataset (_sum) specified as ds_summary that; 
*** that is made in the summarize macro and which contains; 
*** &cluster-&period averages of &outcome_name with &outcome_n and; 
*** &intervention as {0,1}-coded.;
****Moreover, the variable "change" is the first &period ;
**** with the intervention=1 that has non-missing data for &outcome_name; 
**** and "time_int" is the number of periods since period="change";
****
**** period_fmt is a format to identify periods with subjects in both intervention and control;
****     only if specified such an additional table is generated; 
*************************; 

** get outcome label;  
%local outcome_label;
data _null_;
set &ds (obs=1);
call symputx('outcome_label', vlabel(&outcome_name));
run;

title1 "outcome: &outcome_label";

* remove records that have cluster-periods with &outcome_name = . and &outcome_n = 0;
*    note that %summarize can produce such records if its incoming dataset has records with 
*    missing outcome variable in an entire cluster-period; 
data _ds; set &ds(where=(&outcome_name ne . or &outcome_n ne 0));run;

*********** data availability and descriptives of the outcome *************;
title2 "data availability per cluster-period WITHOUT periods that are missing in all &cluster";
proc tabulate data=_ds; 
	class &cluster &period &intervention;
var &outcome_n;
table &period, &cluster*&intervention*&outcome_n=" "*sum=" "*f=4.0;
run;

title2 "data availability per cluster-period WITH periods that are missing in all &cluster";
* if a period has no data in all clusters, it will not be shown;
* to achieve it is shown, we perform several steps; 
*> step 1: determine for each cluster the first and last month with non-missing outcome;
proc means data=_ds min max noprint; 
	class &cluster; var &period;
output out=_all_periods0		min=min_period max=max_period;
run;
*>step 2: a dataset that contains for each cluster all intermediate periods;
*         with missing data;
data _all_periods1; 
	set _all_periods0(keep=&cluster min_period max_period _type_);
	if _type_=0 then delete;
	*now only _type_=1 is left: the min and max per cluster of &period;
	do &period=min_period to max_period;
		do intervention=0 to 1; 
			&outcome_n= .; 
		output;
		end;
	end;
drop _type_;
run;
*> step 3: combine these with the records that have data availability;
*          also change as the first &cluster-&period where the intervention is;
data _all_periods2; 
set _ds(keep=&cluster &period &outcome_n intervention)
	_all_periods1(keep=&cluster &period &outcome_n intervention);
run;
*> step 4: summarize data-availability per cluster-period by intervention ***;
*          (as there could data-availability in both intervention and control);
*          and take the max of &outcome_n so that missing periods 
*          have now explictly data-availability=. ***;
*          while periods with data have these data from the original dataset (_ds); 
proc sort data=_all_periods2; by &cluster &period intervention;
proc means  data=_all_periods2 max noprint; class &cluster &period intervention;
  var &outcome_n ; 
  output out=_all_periods3 max(&outcome_n)=n_subj_clusterperiod;
run;
*> step 5:  only _type_=7 i.e. averages of &period &cluster intervention combinations;
data _all_periods; set _all_periods3(where=(_type_=7));run;
*> step 6: now tabulate;
proc tabulate data=_all_periods; class &cluster &period intervention;
var n_subj_clusterperiod;
table &period, &cluster*intervention*n_subj_clusterperiod=" "*sum=" "*f=4.0;
run; 

title2 "for each &cluster: how many &cluster-&period have no, 1, 2, 3, ... patients";
footnote "the dot (.) means no data-availability for &outcome_name";
footnote2 "if a &cluster-&period has both intervention and control data, these are combined";
* this needs similar steps as above, but not duplicating across intervention=0, 1;
* > we first create a dataset with for each cluster empty records for each period that is 
*   between the first period with >=1 subject up to including the last such period;
data _all_periods1_nodup; 
	set _all_periods0(keep=&cluster min_period max_period _type_);
	if _type_=0 then delete;
	*now only _type_=1 is left: the min and max per cluster of &period;
	do &period=min_period to max_period;
		&outcome_n= .; 
		output;
	end;
drop _type_;
run;
*> step 3: combine these with the only records (cluster-periods) that have >=1 subject;
*          also change as the first &cluster-&period where the intervention is;
data _all_periods2_nodup; 
set _ds(keep=&cluster &period &outcome_n )
	_all_periods1_nodup(keep=&cluster &period &outcome_n ); 
run;
*> step 4: summarize data-availability per cluster-period  ***;
*          and take the max of &outcome_n so that missing periods 
*          have now explictly data-availability=. ***;
*          while periods with data have these data from the original dataset (_ds); 
proc sort data=_all_periods2_nodup; by &cluster &period ;
proc means  data=_all_periods2_nodup sum noprint; class &cluster &period ;
  var &outcome_n ; 
  * note:  "sum", so both control and intervention data in a cluster-period are combined;
  output out=_all_periods3_nodup sum(&outcome_n)=n_subj_clusterperiod;
run;
proc freq data=_all_periods3_nodup(where=(_type_=3));
	table &cluster*n_subj_clusterperiod /nopercent norow nocol missing;
run; 
footnote " ";


title2 "data availabilty summarized in a graph";
*explicit specified imagine name to force to get this graph, otherwise it was not generated;
ods graphics  /imagename="data_available" outputfmt=png; 
proc sgpanel data=_ds;
panelby &cluster / onepanel;
needle x=&period y=&outcome_n / group=&intervention;
run; 


title2 "which &cluster have in which &period BOTH control and intervention data in the same &cluster-&period and how often occurs this";
proc means data=_all_periods noprint; class &cluster &period; var n_subj_clusterperiod;
output out=_nmiss_clusterperiod    nmiss(n_subj_clusterperiod)=nmiss_clusterperiod;
run; 
proc freq data=_nmiss_clusterperiod(where=(nmiss_clusterperiod=0 and _type_=3));
table &cluster*&period/nopercent nocum nocol norow ; run;
%IF &period_fmt ne %str() %THEN %DO; 
	ods startpage=no;
	proc freq data=_nmiss_clusterperiod(where=(nmiss_clusterperiod=0 and _type_=3));
	table &cluster*&period/nopercent nocum nocol norow ; 
	format &period outfmt. ;run;
	ods startpage=now;
%END;


title2 "what is the mean* across all (non-empty) control &cluster-&period cell-averages; 
        idem across all (non-empty) intervention &cluster-&period cell-averages (so no correction for time trend)";
footnote "* this is thus not the mean across all control patients or all intervention patients";
*the proc tabulate can be removed if we are sure everything goes well;
proc tabulate data=_ds; class &cluster &intervention;
var &outcome_name;
table &cluster,&intervention*&outcome_name=" "*mean*f=7.3;
run;
*and then only the proc means is kept and reported via proc tabulate;
ods startpage=no;
ods exclude all; 
ods output statistics=_summary_cluster; * only output the statistics;
* use the descending option in proc sort and the order=data option in proc ttest:
*     to get the intervention minus control values; 
proc sort data=_ds; by &cluster descending &intervention;run;
proc ttest data=_ds order=data; by &cluster;class &intervention ; var &outcome_name;
run;
ods output close;
ods exclude none;
data _summary_cluster; set _summary_cluster; 
	if trim(class)="Diff(1-2)" then type="Diff"; else type=class;*remove the uninformative description;
	rename mean=clusavg; *to avoid confusion; 
run;
proc tabulate data=_summary_cluster; class &cluster type ;var clusavg;
table &cluster, type="&intervention"*clusavg="mean"*min=" "*f=7.3;
run;
ods startpage=now;





title2 "trajectories per cluster";* show the data available; 
footnote " ";
proc sort data=_ds;
	by &cluster &period;
*explicitly specified imagename otherwise it is not generated;
ods graphics /imagename="trajectories" outputfmt=png;
proc sgpanel data=_ds;	* possibly add se;
	panelby &cluster /columns=2;
	*this format statement makes sure character formats attached to &outcome_name are not used; 
	*otherwise no graps are produced (due to the character format;
	format &outcome_name best12.; 
	scatter x=&period y=&outcome_name /group=&intervention;
	series x=&period y=&outcome_name;
	rowaxis ;
run;
options nomprint nomlogic nosymbolgen;

********************** comparison of models for each cluster *************************************;
**** step 1: get the clusters; 
data _ds_clusters; set _ds; keep &cluster;
proc sort data=_ds_clusters;by &cluster;
data _null_;
 length allclusters $1000;
 retain allclusters ' ';
 set _ds_clusters end=eof;by &cluster;
 if first.&cluster then allclusters = trim(left(allclusters))||' '||left(&cluster);
 if eof then call symput('list_clusters', allclusters);
 run;
%put All cluster names are: &list_clusters;

ods noproctitle;
ods graphics on / width=5.5in border;
title " "; * to get more space;
**** step 2: for each cluster: compare DiagnosticPlot for model1 and model2;

proc odstext;
p "For outcome &outcome_label, " / style=[fontsize=14pt just=l];
p "in the following pages, you see"/style=[fontsize=14pt ];
p "for each cluster " /style=[fontsize=14pt ];
p "    first panel: model fit with &model1_label (fixed effects: &model1)" /style=[fontsize=14pt ];
p "    second panel: model fit with &model2_label (fixed effects: &model2)" /style=[fontsize=14pt ];
p " the upper cells in each panel are from left to right: " / style=[fontsize=12pt just=l];
p " ---serial residuals (worse is: series that are alternating above and below zero 
             / series that have runs above zero and/or runs below zero" / style=[fontsize=11pt just=l];
p "----observed (dots) versus predicted (line) (good is that the predicted line fits 
                                           middle through the observed)" / style=[fontsize=11pt just=l];
p "----histogram of residuals (good is that this has a central peak)" / style=[fontsize=11pt just=l];
p "the lower celss in each panel are from left to right: " / style=[fontsize=11pt just=l];
p "----white noise probabilities (good is that they do not exceed the 0.05 or even worse the 0.01 line" / style=[fontsize=11pt just=l];
p "----autocorrelation function (good is that the bars, except the first one, are within the grey area)" / style=[fontsize=11pt just=l];
p "----partial autocorrelation function (good is that the bars, except the first one, are within the grey area)" / style=[fontsize=11pt just=l];
run;

%local cluster_id; %local i;
%do i=1 %to %sysfunc(countw(&list_clusters));
	%let cluster_id=%scan(&list_clusters,&i);
	* now get a quotes around the cluster_id as it is a character value, see https://communities.sas.com/t5/SAS-Programming/How-to-add-single-quote-to-a-macro-variable-within-a-macro/td-p/703220;
	%let cluster_id_quoted=%sysfunc(quote(&cluster_id,%str(%')));
ods startpage=now;
proc autoreg data=_ds(where=(&cluster=&cluster_id_quoted)); 
	by &cluster; 
	model &outcome_name=&model1 /nlag=1 dw=5 dwprob;
	output out=_res_pred_1_&cluster_id pm=pred rm=resid;
	ods select DiagnosticsPanel;
	ods output FinalModel.FitSummary=_fit1_&cluster_id;
	ods output FinalModel.ParameterEstimates=_est1_&cluster_id;
run;
ods startpage=no;
proc autoreg data=_ds(where=(&cluster=&cluster_id_quoted)); 
	by &cluster; 
	model &outcome_name=&model2 /nlag=1 dw=5 dwprob;
	output out=_res_pred_2_&cluster_id pm=pred rm=resid;
	ods select DiagnosticsPanel;
	ods output FinalModel.FitSummary=_fit2_&cluster_id;
	ods output FinalModel.ParameterEstimates=_est2_&cluster_id;
run;
%end;


* reset;
ods proctitle;
ods graphics on / reset=all;
ods startpage=on;
title "outcome is &outcome_label";

** residuals/predicted ***;
data _res_pred_1; 
length type_analysis $50;
set _res_pred_1_: ; 
type_analysis="&model1_label";
run;
data _res_pred_2;
length type_analysis $50;
 set _res_pred_2_: ; 
type_analysis="&model2_label";
run;
data _res_pred; set _res_pred_1 _res_pred_2;run;
proc sort data=_res_pred; by &cluster type_analysis;
title2 "compare the serial residuals*";
title3 "&model1_label in first row, &model2_label in second row";
footnote5 "* which model has fewer alternations of postive-negative residuals and/or fewer runs of positive residuals and negative residuals"; 
proc sgpanel data=_res_pred;
panelby &cluster type_analysis /layout=lattice columns=3;
needle x=&period y=resid;
run;
title2 "compare the observed vs fit (which model's predicted line fits the data points better)";
title3 "&model1_label in first row, &model2_label in second row";
footnote " ";
proc sgpanel data=_res_pred;
panelby &cluster type_analysis /layout=lattice columns=3;
	*this format statement makes sure character formats attached to &outcome_name are not used; 
	*otherwise no graps are produced (due to the character format;
	format &outcome_name best12.; 
series x=&period y=pred;
scatter x=&period y=&outcome_name;
run;
title2 "compare the distribution of residuals (more symmetrical with central peak is better)";
title3 "(&model1_label in first row, &model2_label in second row)";
proc sgpanel data=_res_pred;
panelby &cluster type_analysis /layout=lattice columns=3;
histogram resid;
run;

*** compare AICc ***; 

title "outcome is &outcome_label";
title2 "comparing the fit of model 1* with that of model 2* in terms of AICC** (smaller is better)";
footnote1 "* model 1 is &model1_label (fixed effects: &model1); model 2 is &model2_label (fixed effects: &model2)";
footnote2 "** the AICC assesses whether a better fit of a more complex model (e.g. more covariates) outweighs the advantage of a simpler model";
data _fit1; set _fit1_: ;run;
data _fit2; set _fit2_: ;run;
proc sort data=_fit1;by &cluster;
proc sort data=_fit2;by &cluster;
data _fit_aicc; 
merge _fit1(rename=(nvalue2=AICC_1) 
		    keep=&cluster label2 nvalue2 where=(label2="AICC"))
	  _fit2(rename=(nvalue2=AICC_2) 
	  		keep=&cluster label2 nvalue2 where=(label2="AICC"));
by &cluster;run;
proc print data=_fit_aicc noobs;run;
footnote " ";

*** compare RMSE ***;
title2 "comparing the fit of model 1* with that of model 2* in terms of RMSE** (smaller is better)";
footnote1 "* model 1 is &model1_label (fixed effects: &model1); model 2 is &model2_label (fixed effects: &model2)";
footnote2 "** the RMSE measures the typical distance from observed to predicted values";
data _fit_rmse; 
merge _fit1(rename=(nvalue2=RMSE_1) 
			keep=&cluster label2 nvalue2 where=(label2="Root MSE")) 
      _fit2(rename=(nvalue2=RMSE_2) keep=
      		keep=&cluster label2 nvalue2 where=(label2="Root MSE"));
by &cluster;run;
proc print data=_fit_rmse noobs;run;
footnote " ";

*** combining estimates using meta-analysis ***;

** model 1 *****************************;
title2 "model 1: &model1_label (fixed effects: &model1)";
title3 "combine estimates across the cluster using inverse variance weighting";
data _est1; set _est1_: ; ; run;* all studies estimates;

**calculate meta-analysis mean;
** step 1: calculate meta-analysis mean;
data _ma1_step1; set _est1; if variable="&intervention";w=(stderr)**(-2); run;
** step 2: calculate the sums of weighted estimates and sum of weights over studies; 
data _ma1_step2; 
set _ma1_step1(keep= &cluster estimate stderr w ) end=last; 
retain sum_est_w 0 sum_w 0 ;
sum_est_w=estimate*w + sum_est_w;
sum_w=w + sum_w;
if last then do; ma=sum_est_w/sum_w;  se_ma=sqrt(1/sum_w); end;
run;
** step 3: the meta-analysis mean from the last record;
data _ma1_est; set _ma1_step2; 
if ma ne .;
keep &cluster variable estimate stderr low up probt;
&cluster="*ma*"; variable="intervention";estimate=ma; stderr=se_ma; low=estimate -1.96*stderr;up=estimate+1.96*stderr;
probt=2*(1- probnorm(abs(estimate)/stderr));
run;

* calculate 95%-CI for each study: we need the dfe;
* see https://support.sas.com/kb/22/097.html;
** first extract the DFE from the fit and merge it to the estimates;
data _fit_test; set _fit1(rename=(nvalue2=DFE_1) keep=&cluster label2 nvalue2 where=(label2="DFE"));run;

proc sort data=_est1 ; by descending &cluster ;run;
proc sort data=_fit1; by descending &cluster ;run;
data _est1dfe; 
merge _fit1(rename=(nvalue2=DFE_1) keep=&cluster label2 nvalue2 where=(label2="DFE"))
	  _est1(where=(variable="&intervention"));
by descending &cluster;
* add the upper and lower limit;
low=estimate - tinv(0.975,dfe_1)*stderr;
up=estimate + tinv(0.975,dfe_1)*stderr;
run;

** add the overal meta-analysis estimate and only select the intervention estimate;
data _forest1;set _est1dfe _ma1_est;if variable="intervention";run;
	*describe the range of effects in a footnote2 to the table of effects;
	proc means data=_forest1 min max std noprint; var estimate; 
	output out=_forest1_spread min(estimate)=min_est max(estimate)=max_est std(estimate)= std_est;
	run;
	data _null_; set _forest1_spread; 
	call symputx('min_est',min_est);  call symputx('max_est', max_est);  call symputx('std_est', std_est); 
	run;
footnote "*ma* is the estimated average effect from the meta-analysis";
footnote2 "estimated effects range from %sysfunc(putn(&min_est, 8.3)) to %sysfunc(putn(&max_est,8.3)) 
          with standard deviation %sysfunc(putn(&std_est,8.3))";
proc print data=_forest1 noobs; var &cluster estimate stderr low up probt; run;
footnote " ";

* create a simple forest plot: https://support.sas.com/resources/papers/proceedings10/195-2010.pdf; 
proc sgplot data=_forest1; 
 scatter x=estimate y=&cluster / xerrorlower=low xerrorupper=up markerattrs=(symbol=DiamondFilled size=8); 
 refline 0 / axis=x; 
 yaxis discreteorder=formatted label="&cluster"; 
 xaxis label="intervention effect and 95% CI " ; 
run;
footnote " ";


***Model 2 ******************************************;
title2 "model 2: &model2_label (fixed effects: &model2)";
title3 "combine estimates across the cluster using inverse variance weighting";
data _est2; set _est2_: ; ; run;* all studies estimates;

**calculate meta-analysis mean;
** step 1: calculate meta-analysis mean;
data _ma2_step1; set _est2; if variable="&intervention";w=(stderr)**(-2); run;
** step 2: calculate the sums of weighted estimates and sum of weights over studies; 
data _ma2_step2; 
set _ma2_step1(keep= &cluster estimate stderr w ) end=last; 
retain sum_est_w 0 sum_w 0 ;
sum_est_w=estimate*w + sum_est_w;
sum_w=w + sum_w;
if last then do; ma=sum_est_w/sum_w;  se_ma=sqrt(1/sum_w); end;
run;
** step 3: the meta-analysis mean from the last record;
data _ma2_est; set _ma2_step2; 
if ma ne .;
keep &cluster variable estimate stderr low up probt;
&cluster="*ma*"; variable="intervention";estimate=ma; stderr=se_ma; low=estimate -1.96*stderr;up=estimate+1.96*stderr;
probt=2*(1- probnorm(abs(estimate)/stderr));
run;

* calculate 95%-CI for each study: we need the dfe;
* see https://support.sas.com/kb/22/097.html;
** first extract the DFE from the fit and merge it to the estimates;
data _fit_test; set _fit2(rename=(nvalue2=DFE_1) keep=&cluster label2 nvalue2 where=(label2="DFE"));run;

proc sort data=_est2 ; by descending &cluster ;run;
proc sort data=_fit2; by descending &cluster ;run;
data _est2dfe; 
merge _fit2(rename=(nvalue2=DFE_1) keep=&cluster label2 nvalue2 where=(label2="DFE"))
	  _est2(where=(variable="&intervention"));
by descending &cluster;
* add the upper and lower limit;
low=estimate - tinv(0.975,dfe_1)*stderr;
up=estimate + tinv(0.975,dfe_1)*stderr;
run;

** add the overal meta-analysis estimate and only select the intervention estimate;
data _forest2;set _est2dfe _ma2_est;if variable="intervention";run;
	*describe the range of effects in a footnote2 to the table of effects;
	proc means data=_forest2 min max std noprint; var estimate; 
	output out=_forest2_spread min(estimate)=min_est max(estimate)=max_est std(estimate)= std_est;
	run;
	data _null_; set _forest2_spread; 
	call symputx('min_est',min_est);  call symputx('max_est', max_est);  call symputx('std_est', std_est); 
	run;
footnote "*ma* is the estimated average effect from the meta-analysis";
footnote2 "estimated effects range from %sysfunc(putn(&min_est, 8.3)) to %sysfunc(putn(&max_est,8.3)) 
          with standard deviation %sysfunc(putn(&std_est,8.3))";
proc print data=_forest2 noobs; var &cluster estimate stderr low up probt; run;
footnote " ";

* create a simple forest plot: https://support.sas.com/resources/papers/proceedings10/195-2010.pdf; 
proc sgplot data=_forest2; 
 scatter x=estimate y=&cluster / xerrorlower=low xerrorupper=up markerattrs=(symbol=DiamondFilled size=8); 
 refline 0 / axis=x; 
 yaxis discreteorder=formatted label="&cluster"; 
 xaxis label="intervention effect and 95% CI " ; 
run;

********************** DETAIlS of the analysis if asked ************;
%IF &details eq 1 %THEN %DO;
ods startpage=now;
proc odstext;
p "Details of model 1 (&model1_label) by &cluster"/style=[fontsize=20pt just=c];;
p "fixed effects of model 1: &model1"/style=[fontsize=20pt just=c];
run;
ods startpage=now;
proc autoreg data=_ds; 
	by &cluster;
	model &outcome_name=&model1 /nlag=1 dw=5 dwprob;
run;

ods startpage=now;
proc odstext;
p "Details of model 2 (&model2_label) by &cluster"/style=[fontsize=20pt just=c];;
p "fixed effects of model 2: &model2"/style=[fontsize=20pt just=c ];
run;
ods startpage=now;
proc autoreg data=_ds; 
	by &cluster;
	model &outcome_name=&model2 /nlag=1 dw=5 dwprob;
run;
%END;

* clean up;
/*proc datasets nolist; delete _: ; *all datasets starting with _; ;run;quit;*/
title " ";
%mend;

*********************************************************;
***********	ANALYSIS ************************************;
*********************************************************;
*folder to save to;
*%let path=C:\Users\st\surfdrive\Actief\19 Santeon mITS SHOUT\03 Kidney (Noel Engels)\analyses\results;*lenovo;
%let path=C:\Users\steer\surfdrive\Actief\19 Santeon mITS SHOUT\03 Kidney (Noel Engels)\results;*framework;
%let style=%str(/style=[fontsize=14pt just=l];); 
%let date=250909;
%let details=0;
%let period_fmt=outfmt;
%let period_fmt=outfmt;
options threads nodate;* nodate to remove the date in the rtf file;
* data specific format to identify which periods with double records for all variables;
*    see example https://communities.sas.com/t5/SAS-Programming/Build-a-Format-from-a-Dataset/td-p/904329;
data format_tbl;
do cmonth=1 to 100;
month=mod(cmonth-1,12)+1; * the month in a year as number;
year=2019+floor((cmonth-1)/12); 
description=catx("/",year,month);
output;
end;
run;
data work.outfmt(keep=start label fmtname hlo);  
set work.Format_Tbl(rename=(cmonth=start description=label)) end=last;  
fmtname='outfmt';type='n';
output;
if last then do;start=' ';hlo='o';label='other';output;end;
run;
proc format library=work cntlin=work.outfmt;
run;


 
*************** first set of variables from dataset data.decision ********************************************;
*** dataset specific recoding *****; 
data decision; set data.decision;
zkh=strip(put(hospital, 8.));* make character as the other macro code was built this way;
                             * also remove leading and trailing blanks;  
year=year(T0_survey_date); month=month(T0_survey_date);
cmonth= (year-2019)*12+ month;* sequential month;
* intervention should be {0,1}-coded;
intervention=0*(study_phase=1) + 1*(study_phase=2); * study_phase is coded: 1=pre, 2=post;
run;
%let ds_name=decision; *for the coming variables;
**** as we cannot take all variables at once given errors in macro string length ***;
data ds_variables; 
length varname $40.; length varlabel $50.; * to allow variable lengths;
input varname varlabel & ; * with two spaces or more at the end of varlabel reading ends;
datalines;   
SDMQ9_sum  	"SDM Q9 (sum score)"  
SDMQ9_percentage   "SDM Q9 (percentage score)"
CPS_1   "CPS question 1"
CPS_2   "CPS question 2"
Collaborate_sum   "collaboRATE (sum score)"
Collaborate_percentage   "collaboRATE (percentage score)"
;
run;
%summarize(ds_in=decision, ds_variables=ds_variables, ds_summary=summary);

/* not done as percentages with so low numbers are not analyzable with time series: 
SDMQ9_percentage_above_90   "SDMQ-9 percentage score above 90"
SDMQ9_percentage_of_100   "SDMQ-9 percentage score above 100"
*/ 

/* other variables to do
DCS_total_score   "dcs: *total* score"
DCS_uncertainty_subscore   "dcs: *uncertainty* subscore"
DCS_informed_subscore   "dcs: *informed* subscore"
DCS_values_clarity_subscore   "dcs: *clarity* subscore"
DCS_support_subscore   "dcs: *support* subscore"
DCS_effective_decision_subscore   "dcs": *effective decision* subscore"
*/

* ... for each variable in the dataset ....;
%let varname=sdmq9_sum;
ods rtf file="&path/&date._&varname..rtf" style=pearlj;
proc odstext;
p "******************** &varname ***********************************" &style; *double quotes otherwise macrovar not resolved;
p 'Note that the following hosp have measurements in both the intervention and control: zkh 1 2 3'&style;
p 'All hosp have missing months (79 across all hosp): hosp 1,2 have around 5 missings; hosp 3, 5, 6,7 have around 12, hosp 4 has 21.'&style;
p 'hosp 4 and 5 have sparse data.'&style;
p 'most hosp-period have only 1 patient, followed by 2 or 3. None hosp has more than 7 patients in a hosp-month.' &style; 
p ' ' &style; 
p 'Fit is reasonable: '&style;
p 'In terms of histograms residuals, AICC, RMSE the model without (change in) time trend, so only-jump-model generally trend fits better than or comparable'&style;
p 'to a model that allows also a (change in) time trend, (except zkh 6 perhaps) so easiest to take the model without trend'&style;
p ' ' &style;
p 'There is then an numerical positive effect in hosp 4, 6, 7; the is no effect the rest (hosp 5 even negative). '&style;
p 'Only hosp 6 has p< 0.05 in itself.' &style;
p 'given the heterogeneity in results it is questionable to make a combined estimate across the hosp.' &style;
p 'it may be better to see what distinguishes hosp with numerically positive effect from those without. '&style; 
p "*************************************************************************"&style;
p " program:  %execPrg " / style=[fontsize=10pt just=l]; 
run;

%ar_model1vs2(ds=summary, 
outcome_name=&varname._mean, outcome_n=&varname._n, details=&details); 
ods rtf close;





%let varname=sdmq9_percentage;
ods rtf file="&path/&date._&varname..rtf" style=pearlj;
proc odstext;
p "******************** &varname ***********************************" &style; *double quotes otherwise macrovar not resolved;
p 'essentially SDMQ9 (percentage score) is the same as SDQM9 (total score) apart from a scale change.' &style;
p ' '&style;
p 'Note that the following hosp have measurements in both the intervention and control: zkh 1 2 3'&style;
p 'All hosp have missing months (79 across all hosp): hosp 1,2 have around 5 missings; hosp 3, 5, 6,7 have around 12, hosp 4 has 21.'&style;
p 'hosp 4 and 5 have sparse data.'&style;
p 'most hosp-period have only 1 patient, followed by 2 or 3. None hosp has more than 7 patients in a hosp-month.' &style; 
p ' ' &style; 
p 'Fit is reasonable: '&style;
p 'In terms of histograms residuals, AICC, RMSE the model without (change in) time trend, so only-jump-model generally trend fits better than or comparable'&style;
p 'to a model that allows also a (change in) time trend, (except zkh 6 perhaps) so easiest to take the model without trend'&style;
p ' ' &style;
p 'There is then an numerical positive effect in hosp 4, 6, 7; the is no effect the rest (hosp 5 even negative). '&style;
p 'Only hosp 6 has p< 0.05 in itself.' &style;
p 'given the heterogeneity in results it is questionable to make a combined estimate across the hosp.' &style;
p 'it may be better to see what distinguishes hosp with numerically positive effect from those without. '&style; 
p '********************************************************************'&style;
p " program:  %execPrg " / style=[fontsize=10pt just=l]; 
run;
%ar_model1vs2(ds=summary, 
outcome_name=&varname._mean, outcome_n=&varname._n, details=&details); 
ods rtf close;




%let varname=cps_1;
ods rtf file="&path/&date._&varname..rtf" style=pearlj;
proc odstext;
p "******************** &varname ***********************************" &style; *double quotes otherwise macrovar not resolved;
p 'Note that the following hosp have measurements in both the intervention and control: zkh 1 2 3'&style;
p 'All hosp have missing months (79 across all hosp): hosp 1,2 have around 5 missings; hosp 3, 5, 6,7 have around 12, hosp 4 has 21.'&style;
p 'hosp 4 and 5 have sparse data.'&style;
p 'most hosp-period have only 1 patient, followed by 2 or 3. None hosp has more than 8 patients in a hosp-month.' &style; 
p ' ' &style; 
p 'Fit is reasonable: '&style;
p 'Comparing model 1 (only a jump) to model 2 (that also allows a (change in) time trend): ' &style;
p 'histograms residuals (mostly better), AICC (always better), RMSE (mostly smaller except for 1,2,7) ' &style;
p 'the model 1 without (change in) time trend, so only-jump-model generally trend fits better than or comparable to model 2'&style;
p ' ' &style;
p 'There is then an numerical positive effect (more control preference) in hosp 1, 3 4, 5, 6; the effect is numerically negative in 2 and 7. '&style;
p 'Only hosp 6 has p< 0.05 in itself.' &style;
p 'given that heterogeneity in results it is not large (and most effects in the same direction), it seem sensible to make a combined estimate across the hosp.' &style;
p 'Therefore the meta-analysis estimate is interpretable (but: p> 0.05). (Model 2 gives the same conclusion).'&style; 
p '********************************************************************'&style;
p " program:  %execPrg " / style=[fontsize=10pt just=l]; 
run;
%ar_model1vs2(ds=summary, 
outcome_name=&varname._mean, outcome_n=&varname._n, details=&details); 
ods rtf close;
/* to see the format labels, in order to see which direction is better: higher is more self choice
proc freq data=data.decision; table cps_1;run;
proc freq data=data.decision; table cps_1; format _all_; run;
*/



%let varname=cps_2;
ods rtf file="&path/&date._&varname..rtf" style=pearlj;
proc odstext;
p "******************** &varname ***********************************" &style; *double quotes otherwise macrovar not resolved;
p 'Note that the following hosp have measurements in both the intervention and control: zkh 1 2 3'&style;
p 'All hosp have missing months (79 across all hosp): hosp 1,2 have around 5 missings; hosp 3, 5, 6,7 have around 12, hosp 4 has 21.'&style;
p 'hosp 4 and 5 have sparse data.'&style;
p 'most hosp-period have only 1 patient, followed by 2 or 3. None hosp has more than 8 patients in a hosp-month.' &style; 
p ' ' &style; 
p '>>>>>note that in hospital 4 there is an outlier (one 23 and one 34), maybe this is a mistake<<<<' &style;
p ' ' &style; 
p 'Fit is reasonable: '&style;
p 'Comparing model 1 (only a jump) to model 2 (that also allows a (change in) time trend): ' &style;
p 'histograms residuals (mostly better), AICC (mostly better, except for zkh 2,3), RMSE (mostly *worse* except for 5,6) ' &style;
p 'model 2 better in terms of RSME however model 1 better in AICC, so the simpler model 1 is preferred. '&style;
p ' ' &style;
p 'There is then an numerical negative effect (less actual control) in hosp 2, 3, 6, 7 (and 4 but that may be the outliers); ' &style;
p 'The effect is numerically positive in 1 and 5. '&style;
p 'given that heterogeneity in results it is not large (and most effects in the same direction), it seem sensible to make a combined estimate across the hosp.' &style;
p 'Therefore the meta-analysis estimate is interpretable (but: p> 0.05). '&style; 
p 'This conclusion is the same for model 2 (so with change in time trend allowed.' &style; 
p '********************************************************************'&style;
p " program:  %execPrg " / style=[fontsize=10pt just=l]; 
run;
%ar_model1vs2(ds=summary, 
outcome_name=&varname._mean, outcome_n=&varname._n, details=&details); 
ods rtf close;
/* to see the format labels, in order to see which direction is better: higher is more self choice
proc freq data=data.decision; table cps_2;run;
proc freq data=data.decision; table cps_2; format _all_; run;
*/



%let varname=Collaborate_sum;
ods rtf file="&path/&date._&varname..rtf" style=pearlj;
proc odstext;
p "******************** &varname ***********************************" &style; *double quotes otherwise macrovar not resolved;
p 'Note that the following hosp have measurements in both the intervention and control: zkh 1 2 3'&style;
p 'All hosp have missing months (79 across all hosp): hosp 1,2 have around 5 missings; hosp 3, 5, 6,7 have around 12, hosp 4 has 21.'&style;
p 'hosp 4 and 5 have sparse data.'&style;
p 'most hosp-period have only 1 patient, followed by 2 or 3. None hosp has more than 8 patients in a hosp-month.' &style; 
p ' ' &style; 
p 'Fit is moderate, especially because hosp 3,4,5 have some outlying lowe values : '&style;
p 'Comparing model 1 (only a jump) to model 2 (that also allows a (change in) time trend): ' &style;
p 'histograms residuals (mostly better or similar), AICC (mostly better, except for zkh 6, 7), RMSE (half-half: better for 2,3,4) ' &style;
p 'model 2 mostly better in terms of RSME however model 1 mostly better in AICC, so the simpler model 1 is preferred. '&style;
p ' ' &style;
p 'There is then an numerical postive effect in hosp 1, 3, 4, 6; ' &style;
p 'The effect is numerically negative in 5 and 7. '&style;
p 'given that heterogeneity in results it is not that large (and most effects in the same direction), it seem sensible to make a combined estimate across the hosp.' &style;
p 'Therefore the meta-analysis estimate is interpretable (but: p> 0.05). '&style; 
p 'This conclusion is the same for model 2 (so with change in time trend allowed).' &style; 
p '********************************************************************'&style;
p " program:  %execPrg " / style=[fontsize=10pt just=l]; 
run;
%ar_model1vs2(ds=summary, 
outcome_name=&varname._mean, outcome_n=&varname._n, details=&details); 
ods rtf close;



%let varname=Collaborate_percentage;
ods rtf file="&path/&date._&varname..rtf" style=pearlj;
proc odstext;
p "******************** &varname ***********************************" &style; *double quotes otherwise macrovar not resolved;
p 'Essentially the same as collaboRATE sum but rescaled' &style;
p ' ' &style; 
p 'Note that the following hosp have measurements in both the intervention and control: zkh 1 2 3'&style;
p 'All hosp have missing months (79 across all hosp): hosp 1,2 have around 5 missings; hosp 3, 5, 6,7 have around 12, hosp 4 has 21.'&style;
p 'hosp 4 and 5 have sparse data.'&style;
p 'most hosp-period have only 1 patient, followed by 2 or 3. None hosp has more than 8 patients in a hosp-month.' &style; 
p ' ' &style; 
p 'Fit is moderate, especially because hosp 3,4,5 have some outlying lowe values : '&style;
p 'Comparing model 1 (only a jump) to model 2 (that also allows a (change in) time trend): ' &style;
p 'histograms residuals (mostly better or similar), AICC (mostly better, except for zkh 6,7), RMSE (half-half: better for 2,3,4) ' &style;
p 'model 2 mostly better in terms of RSME however model 1 mostly better in AICC, so the simpler model 1 is preferred. '&style;
p ' ' &style;
p 'There is then an numerical postive effect in hosp 1, 3, 4, 6; ' &style;
p 'The effect is numerically negative in 5 and 7. '&style;
p 'given that heterogeneity in results it is not that large (and most effects in the same direction), it seem sensible to make a combined estimate across the hosp.' &style;
p 'Therefore the meta-analysis estimate is interpretable (but: p> 0.05). '&style; 
p 'This conclusion is the same for model 2 (so with change in time trend allowed).' &style; 
p '********************************************************************'&style;
p " program:  %execPrg " / style=[fontsize=10pt just=l]; 
run;
%ar_model1vs2(ds=summary, 
outcome_name=&varname._mean, outcome_n=&varname._n, details=&details); 
ods rtf close;





*************** second series of variables from dataset data.decision ********************************************;
*** dataset specific recoding *****; 
data decision; set data.decision;
zkh=strip(put(hospital, 8.));* make character as the other macro code was built this way;
                             * also remove leading and trailing blanks;  
year=year(T0_survey_date); month=month(T0_survey_date);
cmonth= (year-2019)*12+ month;* sequential month;
* intervention should be {0,1}-coded;
intervention=0*(study_phase=1) + 1*(study_phase=2); * study_phase is coded: 1=pre, 2=post;
run;
%let ds_name=decision;   
data ds_variables; 
length varname $40.; length varlabel $50.; * to allow variable lengths;
input varname varlabel & ; * with two spaces or more at the end of varlabel reading ends;
datalines;   
DCS_total   "dcs (total)"
DCS_uncertainty   "dcs (uncertainty)"
DCS_informed   "dcs (informed)"
DCS_support   "dcs (support)"
DCS_eff_decision   "dcs (eff. decision)"
DCS_valclarity   "dcs (value clarity)"
;
run;
%summarize(ds_in=&ds_name, ds_variables=ds_variables, ds_summary=summary);


%let varname=DCS_total;
ods rtf file="&path/&date._&varname..rtf" style=pearlj;
proc odstext;
p "******************** &varname ***********************************" &style; *double quotes otherwise macrovar not resolved;
p 'Note that the following hosp have measurements in both the intervention and control: zkh 1 2 3'&style;
p 'All hosp have missing months (79 across all hosp): hosp 1,2 have around 5 missings; hosp 3, 5, 6,7 have around 12, hosp 4 has 21.'&style;
p 'hosp 4 and 5 have sparse data.'&style;
p 'most hosp-period have only 1 patient, followed by 2 or 3. None hosp has more than 8 patients in a hosp-month.' &style; 
p ' ' &style; 
p 'Fit is reasonable '&style;
p 'Comparing model 1 (only a jump) to model 2 (that also allows a (change in) time trend): ' &style;
p 'histograms residuals (mostly better or similar), AICC (mostly better, except for zkh 3), RMSE (better for 1,2, 4,5,7) ' &style;
p 'model 1 mostly better in AICC, RSME, so the simpler model 1 is preferred. '&style;
p ' ' &style;
p 'There is then an numerical postive effect in hosp 2, 5, 6; ' &style;
p 'The effect is numerically negative in 1, 3, 4, 7. '&style;
p 'given that heterogeneity in results it is not that large (and most effects in the same direction), it seem sensible to make a combined estimate across the hosp.' &style;
p 'Therefore the meta-analysis estimate is interpretable (but: p> 0.05). '&style; 
p 'This conclusion is the same for model 2 (so with change in time trend allowed).' &style; 
p '********************************************************************'&style;
p " program:  %execPrg " / style=[fontsize=10pt just=l]; 
run;
%ar_model1vs2(ds=summary, 
outcome_name=&varname._mean, outcome_n=&varname._n, details=&details); 
ods rtf close;





%let varname=DCS_uncertainty;
ods rtf file="&path/&date._&varname..rtf" style=pearlj;
proc odstext;
p "******************** &varname ***********************************" &style; *double quotes otherwise macrovar not resolved;
p 'Note that the following hosp have measurements in both the intervention and control: zkh 1 2 3'&style;
p 'All hosp have missing months (79 across all hosp): hosp 1,2 have around 5 missings; hosp 3, 5, 6,7 have around 12, hosp 4 has 21.'&style;
p 'hosp 4 and 5 have sparse data.'&style;
p 'most hosp-period have only 1 patient, followed by 2 or 3. None hosp has more than 8 patients in a hosp-month.' &style; 
p ' ' &style; 
p 'Fit is reasonable '&style;
p 'Comparing model 1 (only a jump) to model 2 (that also allows a (change in) time trend): ' &style;
p 'histograms residuals (mostly better or similar, perhaps in hosp 3 and 5 better for model 2), AICC (mostly better, except for zkh 6), RMSE (except for zkh 5,6) ' &style;
p 'model 1 mostly better in AICC, RSME, so the simpler model 1 is preferred. '&style;
p ' ' &style;
p 'There is then an numerical postive effect in hosp 2, 5, 6; ' &style;
p 'The effect is numerically negative in 1, 3, 4, 7. '&style;
p 'given that heterogeneity in results it is not that large (and most effects in the same direction), it seem sensible to make a combined estimate across the hosp.' &style;
p 'Therefore the meta-analysis estimate is interpretable (but: p> 0.05). '&style; 
p 'This conclusion is the same for model 2 (so with change in time trend allowed).' &style; 
p '********************************************************************'&style;
p " program:  %execPrg " / style=[fontsize=10pt just=l]; 
run;
%ar_model1vs2(ds=summary, 
outcome_name=&varname._mean, outcome_n=&varname._n, details=&details); 
ods rtf close;




%let varname=DCS_informed;
ods rtf file="&path/&date._&varname..rtf" style=pearlj;
proc odstext;
p "******************** &varname ***********************************" &style; *double quotes otherwise macrovar not resolved;
p 'Note that the following hosp have measurements in both the intervention and control: zkh 1 2 3'&style;
p 'All hosp have missing months (79 across all hosp): hosp 1,2 have around 5 missings; hosp 3, 5, 6,7 have around 12, hosp 4 has 21.'&style;
p 'hosp 4 and 5 have sparse data.'&style;
p 'most hosp-period have only 1 patient, followed by 2 or 3. None hosp has more than 8 patients in a hosp-month.' &style; 
p ' ' &style; 
p 'Fit is reasonable '&style;
p 'Comparing model 1 (only a jump) to model 2 (that also allows a (change in) time trend): ' &style;
p 'histograms residuals (mostly better or similar, except high outligh in zkh 1, 2), AICC (always better), RMSE (except for zkh 1,4) ' &style;
p 'model 1 mostly better in AICC, RSME, so the simpler model 1 is preferred. '&style;
p ' ' &style;
p 'There is then an numerical postive effect in hosp 2, 5; ' &style;
p 'The effect is numerically negative in 1, 3, 4, 6, 7. '&style;
p 'given that heterogeneity in results it is not that large (and most effects around 0), it seem sensible to make a combined estimate across the hosp.' &style;
p 'Therefore the meta-analysis estimate is interpretable (but: p> 0.05). '&style; 
p 'This conclusion is the same for model 2 (so with change in time trend allowed).' &style; 
p '********************************************************************'&style;
p " program:  %execPrg " / style=[fontsize=10pt just=l]; 
run;
%ar_model1vs2(ds=summary, 
outcome_name=&varname._mean, outcome_n=&varname._n, details=&details); 
ods rtf close;




%let varname=DCS_support;
ods rtf file="&path/&date._&varname..rtf" style=pearlj;
proc odstext;
p "******************** &varname ***********************************" &style; *double quotes otherwise macrovar not resolved;
p 'Note that the following hosp have measurements in both the intervention and control: zkh 1 2 3'&style;
p 'All hosp have missing months (79 across all hosp): hosp 1,2 have around 5 missings; hosp 3, 5, 6,7 have around 12, hosp 4 has 21.'&style;
p 'hosp 4 and 5 have sparse data.'&style;
p 'most hosp-period have only 1 patient, followed by 2 or 3. None hosp has more than 8 patients in a hosp-month.' &style; 
p ' ' &style; 
p 'Fit is reasonable '&style;
p 'Comparing model 1 (only a jump) to model 2 (that also allows a (change in) time trend): ' &style;
p 'histograms residuals (mostly better or similar, but outlier in zkh 1), AICC (better except for zkh 6), RMSE (except for zkh 1,3,4,6) ' &style;
p 'model 1 mostly better in AICC, so the simpler model 1 is preferred. '&style;
p ' ' &style;
p 'There is then an numerical postive effect in hosp 1,2,5; ' &style;
p 'The effect is numerically negative in 3,4,6,7. '&style;
p 'given that heterogeneity in results it is not that large (and most effects around 0), it seem sensible to make a combined estimate across the hosp.' &style;
p 'Therefore the meta-analysis estimate is interpretable (but: p> 0.05). '&style; 
p 'This conclusion is the same for model 2 (so with change in time trend allowed).' &style; 
p '********************************************************************'&style;
p " program:  %execPrg " / style=[fontsize=10pt just=l]; 
run;
%ar_model1vs2(ds=summary, 
outcome_name=&varname._mean, outcome_n=&varname._n, details=&details); 
ods rtf close;



%let varname=DCS_eff_decision;
ods rtf file="&path/&date._&varname..rtf" style=pearlj;
proc odstext;
p "******************** &varname ***********************************" &style; *double quotes otherwise macrovar not resolved;
p 'Note that the following hosp have measurements in both the intervention and control: zkh 1 2 3'&style;
p 'All hosp have missing months (79 across all hosp): hosp 1,2 have around 5 missings; hosp 3, 5, 6,7 have around 12, hosp 4 has 21.'&style;
p 'hosp 4 and 5 have sparse data.'&style;
p 'most hosp-period have only 1 patient, followed by 2 or 3. None hosp has more than 8 patients in a hosp-month.' &style; 
p ' ' &style; 
p 'Fit is reasonable '&style;
p 'Comparing model 1 (only a jump) to model 2 (that also allows a (change in) time trend): ' &style;
p 'histograms residuals (mostly better or similar, but outlier in zkh 1,2), AICC (better), RMSE (except for zkh 1, 3) ' &style;
p 'model 1 mostly better in AICC, so the simpler model 1 is preferred. '&style;
p ' ' &style;
p 'There is then an numerical postive effect in hosp 1,2,5; ' &style;
p 'The effect is numerically negative in 3,4,6,7. '&style;
p 'given that heterogeneity in results it is not that large (and most effects around 0), it seem sensible to make a combined estimate across the hosp.' &style;
p 'Therefore the meta-analysis estimate is interpretable (but: p> 0.05). '&style; 
p 'This conclusion is the same for model 2 (so with change in time trend allowed).' &style; 
p '********************************************************************'&style;
p " program:  %execPrg " / style=[fontsize=10pt just=l]; 
run;
%ar_model1vs2(ds=summary, 
outcome_name=&varname._mean, outcome_n=&varname._n, details=&details); 
ods rtf close;



%let varname=DCS_valclarity;
ods rtf file="&path/&date._&varname..rtf" style=pearlj;
proc odstext;
p "******************** &varname ***********************************" &style; *double quotes otherwise macrovar not resolved;
p 'Note that the following hosp have measurements in both the intervention and control: zkh 1 2 3'&style;
p 'All hosp have missing months (79 across all hosp): hosp 1,2 have around 5 missings; hosp 3, 5, 6,7 have around 12, hosp 4 has 21.'&style;
p 'hosp 4 and 5 have sparse data.'&style;
p 'most hosp-period have only 1 patient, followed by 2 or 3. None hosp has more than 8 patients in a hosp-month.' &style; 
p ' ' &style; 
p 'Fit is reasonable '&style;
p 'Comparing model 1 (only a jump) to model 2 (that also allows a (change in) time trend): ' &style;
p 'histograms residuals (mostly better or similar, but outlier in zkh 1), AICC (better), RMSE (except for zkh 2,4,6) ' &style;
p 'model 1 mostly better in AICC, so the simpler model 1 is preferred. '&style;
p ' ' &style;
p 'There is then an numerical postive effect in hosp 6,7; ' &style;
p 'The effect is numerically negative in 1,2,3,4. '&style;
p 'given that heterogeneity in results it is not that large (and most effects around 0), it seem sensible to make a combined estimate across the hosp.' &style;
p 'Therefore the meta-analysis estimate is interpretable (but: p> 0.05). '&style; 
p 'This conclusion is the same for model 2 (so with change in time trend allowed).' &style; 
p '********************************************************************'&style;
p " program:  %execPrg " / style=[fontsize=10pt just=l]; 
run;
%ar_model1vs2(ds=summary, 
outcome_name=&varname._mean, outcome_n=&varname._n, details=&details); 
ods rtf close;



*******************************************************************************************************;
***************************DRS_T1 from other dataset***************************************************;
*******************************************************************************************************;
*** dataset specific recoding *****; 
data drs_t1; set data.drs;
zkh=strip(put(hospital, 8.));* make character as the other macro code was built this way;
                             * also remove leading and trailing blanks;  
year=year(T1_survey_date); month=month(T1_survey_date);
cmonth= (year-2019)*12+ month;* sequential month;
* intervention should be {0,1}-coded;
intervention=0*(study_phase=1) + 1*(study_phase=2); * study_phase is coded: 1=pre, 2=post;
if T1_survey_date ne . and T2_survey_date eq .; * select only the T1 survey;
drs_t1=drs_final_score; *drs_final_score holds drs at T1 and at T2;
run;
%let ds_name=drs_t1; *for the coming variables;
**** as we cannot take all variables at once given errors in macro string length ***;
data ds_variables; 
length varname $40.; length varlabel $50.; * to allow variable lengths;
input varname varlabel & ; * with two spaces or more at the end of varlabel reading ends;
datalines;   
DRS_t1  	"DRS at T1 (final)"  
;
run;
%summarize(ds_in=drs_t1, ds_variables=ds_variables, ds_summary=summary);

%let varname=DRS_t1;
ods rtf file="&path/&date._&varname..rtf" style=pearlj;
proc odstext;
p "******************** &varname ***********************************" &style; *double quotes otherwise macrovar not resolved;
p 'Note that the following hosp have measurements in both the intervention and control: zkh 1 2 3'&style;
p 'All hosp have missing months (85 across all hosp): hosp 1,2 have around 5 missings; hosp 3, 5, 6,7 have around 12, hosp 4 has 25.'&style;
p 'hosp 4 and 5 have sparse data.'&style;
p 'most hosp-period have only 1 patient, followed by 2 or 3. None hosp has more than 7 patients in a hosp-month.' &style; 
p ' ' &style; 
p 'Fit is reasonable '&style;
p 'Comparing model 1 (only a jump) to model 2 (that also allows a (change in) time trend): ' &style;
p 'histograms residuals (mostly better or similar, but outlier in zkh 1), AICC (better in zkh 2,4,6,7), RMSE (better in zkh 7) ' &style;
p 'model 1 mostly better in AICC (almost never better in RMSE), but the simpler model 1 is preferred. '&style;
p ' ' &style;
p 'There is then an numerical negative effect in all zkh; ' &style;
p 'The effect is numerically negative in 1,2,3,4. '&style;
p 'given that there is little heterogeneity in results and all negative, it seem sensible to make a combined estimate across the hosp.' &style;
p 'Therefore the meta-analysis estimate is interpretable (but: p < 0.05). '&style; 
p 'The effect in model 2 vary around 0 (zkh 2,3,5 positive; 1,4,6,7 negative) and is inderdermined.' &style; 
p '********************************************************************'&style;
p " program:  %execPrg " / style=[fontsize=10pt just=l]; 
run;
%ar_model1vs2(ds=summary, 
outcome_name=&varname._mean, outcome_n=&varname._n, details=&details,period_fmt=&period_fmt); 
ods rtf close;




*******************************************************************************************************;
***************************DRS_T2 from other dataset***************************************************;
*******************************************************************************************************;
*** dataset specific recoding *****; 
data drs_t2; set data.drs;
zkh=strip(put(hospital, 8.));* make character as the other macro code was built this way;
                             * also remove leading and trailing blanks;  
year=year(T2_survey_date); month=month(T2_survey_date);
cmonth= (year-2019)*12+ month;* sequential month;
* intervention should be {0,1}-coded;
intervention=0*(study_phase=1) + 1*(study_phase=2); * study_phase is coded: 1=pre, 2=post;
if T2_survey_date ne . and T1_survey_date eq .; * select only the T1 survey;
drs_t2=drs_final_score; *drs_final_score holds drs at T1 and at T2;
run;
%let ds_name=drs_t2; *for the coming variables;
**** as we cannot take all variables at once given errors in macro string length ***;
data ds_variables; 
length varname $40.; length varlabel $50.; * to allow variable lengths;
input varname varlabel & ; * with two spaces or more at the end of varlabel reading ends;
datalines;   
DRS_t2  	"DRS at T2 (final)"  
;
run;
%summarize(ds_in=drs_t2, ds_variables=ds_variables, ds_summary=summary);

%let varname=DRS_t2;
ods rtf file="&path/&date._&varname..rtf" style=pearlj;
proc odstext;
p "******************** &varname ***********************************" &style; *double quotes otherwise macrovar not resolved;
p 'Note that the following hosp have measurements in both the intervention and control: zkh 1 2 3'&style;
p 'All hosp have missing months (89 across all hosp): hosp 1,2 have around 5 missings; hosp 3, 5, 6,7 have around 12, hosp 4 has 25.'&style;
p 'hosp 4 and 5 have sparse data.'&style;
p 'most hosp-period have only 1 patient, followed by 2 or 3. None hosp has more than 7 patients in a hosp-month.' &style; 
p ' ' &style; 
p 'Fit is reasonable '&style;
p 'Comparing model 1 (only a jump) to model 2 (that also allows a (change in) time trend): ' &style;
p 'histograms residuals (mostly better or similar, but outlier in zkh 1), AICC (better in zkh 2,4,6,7), RMSE (better in zkh 7) ' &style;
p 'model 1 mostly better in AICC (almost never better in RMSE), but the simpler model 1 is preferred. '&style;
p ' ' &style;
p 'There is then an numerical negative effect in all zkh; ' &style;
p 'The effect is numerically negative in 1,2,3,4. '&style;
p 'given that there is little heterogeneity in results and all negative, it seem sensible to make a combined estimate across the hosp.' &style;
p 'Therefore the meta-analysis estimate is interpretable (but: p < 0.05). '&style; 
p 'The effect in model 2 vary around 0 (zkh 2,3,5 positive; 1,4,6,7 negative) and is inderdermined.' &style; 
p '********************************************************************'&style;
p " program:  %execPrg " / style=[fontsize=10pt just=l]; 
run;
%ar_model1vs2(ds=summary, 
outcome_name=&varname._mean, outcome_n=&varname._n, details=&details,period_fmt=&period_fmt); 
ods rtf close;


*knowledge;
*******************************************************************************************************;
***************************Knowledge from other dataset***************************************************;
*******************************************************************************************************;
*** dataset specific recoding *****; 
data know; set data.Knowledge;
zkh=strip(put(hospital, 8.));* make character as the other macro code was built this way;
                             * also remove leading and trailing blanks;  
year=year(T0_survey_date); month=month(T0_survey_date);
cmonth= (year-2019)*12+ month;* sequential month;
* intervention should be {0,1}-coded;
intervention=0*(study_phase=1) + 1*(study_phase=2); * study_phase is coded: 1=pre, 2=post;
knowledge=total_knowledge_score;
run;
/* check: total_knowledge_score is rounded
proc freq data=know; table knowledge*total_knowledge_score;format total_knowledge_score best12.;run;
*/
%let ds_name=know;   
data ds_variables; 
length varname $40.; length varlabel $50.; * to allow variable lengths;
input varname varlabel & ; * with two spaces or more at the end of varlabel reading ends;
datalines;   
knowledge   "knowledge (total)"
;
run;
%summarize(ds_in=&ds_name, ds_variables=ds_variables, ds_summary=summary);
%let varname=knowledge;
ods rtf file="&path/&date._&varname..rtf" style=pearlj;
proc odstext;
p "******************** &varname ***********************************" &style; *double quotes otherwise macrovar not resolved;
p 'Note that the following hosp have measurements in both the intervention and control: zkh 1 2 3'&style;
p 'All hosp have missing months (79 across all hosp): hosp 1,2 have around 5 missings; hosp 3, 5, 6,7 have around 12, hosp 4 has 21.'&style;
p 'hosp 4 and 5 have sparse data.'&style;
p 'most hosp-period have only 1 patient, followed by 2 or 3. None hosp has more than 8 patients in a hosp-month.' &style; 
p ' ' &style; 
p 'Fit is reasonable '&style;
p 'Comparing model 1 (only a jump) to model 2 (that also allows a (change in) time trend): ' &style;
p 'histograms residuals (mostly better or similar), AICC (mostly better, except for zkh 5, 7), RMSE (except for 4,5,6,7) ' &style;
p 'model 1 mostly better in AICC, RSME, so the simpler model 1 is preferred. '&style;
p ' ' &style;
p 'There is then an numerical postive effect in hosp 1,2,3,4, 5, 6; ' &style;
p 'The effect is numerically negative in 7. '&style;
p 'given that heterogeneity in results it is not that large (and most effects in the same direction), it seem sensible to make a combined estimate across the hosp.' &style;
p 'Therefore the meta-analysis estimate is interpretable (p< 0.05). '&style; 
p 'This conclusion for model 2 is effects positive and negative around 0 (more an underdetermined effect).' &style; 
p '********************************************************************'&style;
p " program:  %execPrg " / style=[fontsize=10pt just=l]; 
run;
%ar_model1vs2(ds=summary, 
outcome_name=&varname._mean, outcome_n=&varname._n, details=&details); 
ods rtf close;



