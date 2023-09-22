
**************DATA MANAGEMENT ***********************;
*** data set contains records per months, these are first aggregated ***;
**** read data *****;
options pagesize=60;
* set the working directory;
data _null_;
      rc=dlgcdir("/home/u63416505/SHOUT_BC");
      put rc=;
run;
libname data './data';
/* get formats, not needed here;
options fmtsearch= (data.formats);
*/
/*  what are values for intervention:
proc freq data=bc0; table phase_vrag/missing;run;
*/
data sec1; set data.shout_bc_sec;
zkh=Site.Abbreviation; 
year=year(datedt0); month=month(datedt0);
cmonth= (year-2019)*12+ month;* sequential month;
intervention=input(phase_vrag, 1.0);
* variable pt_simple was made character, make it numeric;
* (value #N/A gives an error, so handle it separately; 
if pt_simple="#N/A" then pt_simple_num=.; else pt_simple_num=input(pt_simple,8.0);
** coll trans between 0 and 100%, transform to -infty to + infty;
if coll_trans>=100 then logit_coll_trans=log(0.999/(1-0.999)); 
else if coll_trans=<0 then logit_coll_trans=log(0.001/(1-0.001));
else logit_coll_trans=log( (coll_trans/100)/(1-coll_trans/100) );
run;
/* check recoding 
proc freq data=sec1; table phase_vrag*intervention/missing;run;
proc freq data=sec1;table pt_simple*pt_simple_num/missing;run;
*/


** variables to be analyzed;
data ds_variables; 
length varname $30.; length varlabel $50.; * to allow variable lengths;
input varname varlabel & ; * with two spaces or more at the end of varlabel reading ends;
datalines;   
coll_trans  		"CollaboRATE"  
logit_coll_trans  	"Logit of CollaboRATE/100"  
dcs_tot_trans 		"Decisional Conflict Scale"  
curebeliefs 		"Cure beliefs"  
personalcontrol 	"Control beliefs"  
cws_score 			"Cancer Worry Scale"  
risico_1 			"Risk estimation"  
risico_2 			"Risk appraisal"  
risico_3 			"Risk comparison"  
PCS_score 			"SF-12: Physical Component Score"  
MCS_score 			"SF-12: Mental Component Score"
;
run;



**** in three steps aggregrate all variables within month;
* step 1: make macro variable varlist;
data _null_;
 length allvars $1000;
 retain allvars ' ';
 set ds_variables end=eof;
 allvars = trim(left(allvars))||' '||left(varname);
 if eof then call symput('varlist', allvars);
 run;
%put &varlist;*check;

* step 2: aggregate each variable in varlist;
proc means data=sec1  noprint mean n stddev;
	class zkh cmonth intervention;
	var &varlist;
	output out=_sum0 
		mean(differentiation)=mean_diff mean(hormther)=mean_horm
		mean(adjchemther)=mean_adjchem mean(pt_simple_num)=mean_pt
		mean(pn_simple)=mean_pn
		mean(&varlist)= n(&varlist)= std(&varlist)=/autoname
		; 
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
	by zkh cmonth intervention;
run;
proc means data=_sum1 noprint;
	by zkh;
	where intervention=1;
	var cmonth;
	output out=_changepoint min(cmonth)=change;
run;

data _sum;*** add time since intervention to dataset;
	merge _sum2 _changepoint;
	by zkh;
	time_intv=intervention*(cmonth-change);
run;


**********************************************************;
************* macros **************************************;

** for in the macro, make them local if necessary;
** macro compares two models differing in fixed effects **;
%macro ar_model1vs2(ds=_sum, outcome_name=, outcome_n=,
cluster=zkh, period=cmonth, intervention=intervention, 
model1_label=%str(no trend), model1=%str(intervention),
model2_label=%str(trend),model2=%str(intervention cmonth time_intv));


** get outcome label; 
%local outcome_label;
data _null_;
set &ds (obs=1);
call symputx('outcome_label', vlabel(&outcome_name));
run;

title1 "outcome: &outcome_label";

title2 "data availability per cluster-period";
proc tabulate data=&ds; class &cluster &period &intervention;
var &outcome_n;
table &period, &cluster*&intervention*&outcome_n=" "*sum=" "*f=4.0;
run;

title2 "descriptive statistics control vs intervention (without correction)";
footnote "mean per &cluster is taken over all &cluster - &period means";
proc tabulate data=&ds; class &cluster &intervention;
var &outcome_name;
table &cluster,&intervention*&outcome_name=" "*mean*f=7.3;
run;
footnote " ";

title2 "trajectories per cluster";* show the data available; 
proc sort data=&ds;
	by &cluster &period;
proc sgpanel data=&ds;	* possibly add se;
	panelby &cluster /columns=3;
	scatter x=&period y=&outcome_name /group=&intervention;
	series x=&period y=&outcome_name;
	rowaxis ;
run;


***** comparison of models for each cluster *****************************************;
**** step 1: get the clusters; 
data _ds_clusters; set &ds; keep &cluster;
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
p "for each cluster " /style=[fontsize=14pt ];
p "first panel: model fit with &model1_label (fixed effects: &model1)" /style=[fontsize=14pt ];
p "second panel: model fit with &model2_label (fixed effects: &model2)" /style=[fontsize=14pt ];
p "outcome is &outcome_label" /style=[fontsize=14pt ];;
run;

%local cluster_id; %local i;
%do i=1 %to %sysfunc(countw(&list_clusters));
	%let cluster_id=%scan(&list_clusters,&i);
	* now get a quotes around the cluster_id as it is a character value, see https://communities.sas.com/t5/SAS-Programming/How-to-add-single-quote-to-a-macro-variable-within-a-macro/td-p/703220;
	%let cluster_id_quoted=%sysfunc(quote(&cluster_id,%str(%')));
ods startpage=now;
proc autoreg data=&ds(where=(&cluster=&cluster_id_quoted)); 
	by &cluster;
	model &outcome_name=&model1 /nlag=1 dw=5 dwprob;
	output out=_res_pred_1_&cluster_id pm=pred rm=resid;
	ods select DiagnosticsPanel;
	ods output FinalModel.FitSummary=_fit1_&cluster_id;
	ods output FinalModel.ParameterEstimates=_est1_&cluster_id;
run;
ods startpage=no;
proc autoreg data=&ds(where=(&cluster=&cluster_id_quoted)); 
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
title2 "compare the serial residuals";
title3 "&model1_label in first row, &model2_label in second row";
proc sgpanel data=_res_pred;
panelby &cluster type_analysis /layout=lattice columns=3;
needle x=&period y=resid;
run;
title2 "compare the observed vs fit";
title3 "&model1_label in first row, &model2_label in second row";
proc sgpanel data=_res_pred;
panelby &cluster type_analysis /layout=lattice columns=3;
series x=&period y=pred;
scatter x=&period y=&outcome_name;
run;
title2 "compare the distribution of residuals (more symmetrical is better)";
title3 "(&model1_label in first row, &model2_label in second row)";
proc sgpanel data=_res_pred;
panelby &cluster type_analysis /layout=lattice columns=3;
histogram resid;
run;

*** compare AICc ***; 

title "outcome is &outcome_label";
title2 "comparing the fit of model 1* with that of model 2* in terms of AICC** (smaller is better)";
footnote1 "model 1 is &model1_label (fixed effects: &model1); model 2 is &model2_label (fixed effects: &model2)";
footnote2 "**the AICC weights whether better fit exceeds the automatically better fit due to using more covariates";
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
footnote "";

*** compare RMSE ***;
title2 "comparing the fit of model 1* with that of model 2* in terms of RMSE** (smaller is better)";
footnote1 "model 1 is &model1_label (fixed effects: &model1); model 2 is &model2_label (fixed effects: &model2)";
footnote2 "*the RMSE measures the typical distance from observed to predicted values";
data _fit_rmse; 
merge _fit1(rename=(nvalue2=RMSE_1) 
			keep=&cluster label2 nvalue2 where=(label2="Root MSE")) 
      _fit2(rename=(nvalue2=RMSE_2) keep=
      		keep=&cluster label2 nvalue2 where=(label2="Root MSE"));
by &cluster;run;
proc print data=_fit_rmse noobs;run;
footnote "";

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
footnote "*ma* is the estimated average effect from the meta-analysis";
proc print data=_forest1 noobs; var &cluster estimate stderr low up probt; run;
footnote "";

* create a simple forest plot: https://support.sas.com/resources/papers/proceedings10/195-2010.pdf; 
proc sgplot data=_forest1; 
 scatter x=estimate y=&cluster / xerrorlower=low xerrorupper=up markerattrs=(symbol=DiamondFilled size=8); 
 refline 0 / axis=x; 
 yaxis discreteorder=formatted label="&cluster"; 
 xaxis label="intervention effect and 95% CI " ; 
run;


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
footnote "*ma* is the estimated average effect from the meta-analysis";
proc print data=_forest2 noobs; var &cluster estimate stderr low up probt; run;
footnote "";

* create a simple forest plot: https://support.sas.com/resources/papers/proceedings10/195-2010.pdf; 
proc sgplot data=_forest2; 
 scatter x=estimate y=&cluster / xerrorlower=low xerrorupper=up markerattrs=(symbol=DiamondFilled size=8); 
 refline 0 / axis=x; 
 yaxis discreteorder=formatted label="&cluster"; 
 xaxis label="intervention effect and 95% CI " ; 
run;

*** details here ************;
ods startpage=now;
proc odstext;
p "Details of model 1 (&model1_label) by &cluster"/style=[fontsize=14pt ];;
p "fixed effects of model 1: &model1"/style=[fontsize=14pt ];
run;
ods startpage=now;
proc autoreg data=&ds; 
	by &cluster;
	model &outcome_name=&model1 /nlag=1 dw=5 dwprob;
run;

ods startpage=now;
proc odstext;
p "Details of model 2 (&model2_label) by &cluster"/style=[fontsize=14pt ];;
p "fixed effects of model 2: &model2"/style=[fontsize=14pt ];
run;
ods startpage=now;
proc autoreg data=&ds; 
	by &cluster;
	model &outcome_name=&model2 /nlag=1 dw=5 dwprob;
run;

%mend;

*********************************************************;
***********	ANALYSIS ************************************;
*********************************************************;

* CollaboRATE;
ods rtf file="/home/u63416505/SHOUT_BC/results/230830_sec_colltrans.rtf";
%let style=%str(/style=[fontsize=14pt just=c];);
proc odstext;
p '***************************************************************'&style;
p 'comments .....'&style;
p 'comments ...........'&style;
p 'it seems that the trend fits a bit better, but not much better than no trend'&style;
p 'so easiest to take the model without trend'&style;
p '***************************************************************'&style;
run;
%ar_model1vs2(ds=_sum, 
outcome_name=coll_trans_mean, outcome_n=coll_trans_n); 
ods rtf close;

