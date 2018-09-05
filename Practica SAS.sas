options validvarname=any;

ods listing close;

ods listing gpath="/home/jdebran0/Advanced_Data_Mining/data_output";

libname kc_lib '/home/jdebran0/KeepCodingLib';


/*************************************************
                  Cargar dataset
*************************************************/
data bankadditionalfull(label="Bank Marketing" rename=('emp.var.rate'n=emp_var_rate 'cons.price.idx'n=cons_price_idx 'cons.conf.idx'n=cons_conf_idx 'nr.employed'n=nr_employed));
	set kc_lib.bank_additional_full;
	label age = ; *(numeric);
	label job = "type of job"; *(categorical: 'admin.','blue-collar','entrepreneur','housemaid','management','retired','self-employed','services','student','technician','unemployed','unknown');
	label marital = "marital status"; *(categorical: 'divorced','married','single','unknown' note: 'divorced' means divorced or widowed);
	label education = ; *(categorical: 'basic.4y','basic.6y','basic.9y','high.school','illiterate','professional.course','university.degree','unknown');
	label default = "has credit in default?"; *(categorical: 'no','yes','unknown');
	label housing = "has housing loan?"; *(categorical: 'no','yes','unknown');
	label loan = "has personal loan?"; *(categorical: 'no','yes','unknown');
	label contact = "contact communication type"; *(categorical: 'cellular','telephone') ;
	label month = "last contact month of year"; *(categorical: 'jan', 'feb', 'mar', ..., 'nov', 'dec');
	label day_of_week = "last contact day of the week"; *(categorical: 'mon','tue','wed','thu','fri');
	label duration = "last contact duration, in seconds"; *(numeric). Important note: this attribute highly affects the output target (e.g., if duration=0 then y='no'). Yet, the duration is not known before a call is performed. Also, after the end of the call y is obviously known. Thus, this input should only be included for benchmark purposes and should be discarded if the intention is to have a realistic predictive model.;
	label campaign = "number of contacts performed during this campaign and for this client"; *(numeric, includes last contact);
	label pdays = "number of days that passed by after the client was last contacted from a previous campaign"; *(numeric 999 means client was not previously contacted);
	label previous = "number of contacts performed before this campaign and for this client"; *(numeric);
	label poutcome = "outcome of the previous marketing campaign"; *(categorical: 'failure','nonexistent','success');
	label 'emp.var.rate'n = "employment variation rate - quarterly indicator"; *(numeric);
	label 'cons.price.idx'n = "consumer price index - monthly indicator"; *(numeric);
	label 'cons.conf.idx'n = "consumer confidence index - monthly indicator"; *(numeric);
	label euribor3m = "euribor 3 month rate - daily indicator"; *(numeric);
	label 'nr.employed'n = "number of employees - quarterly indicator"; *(numeric);
	label y = "has the client subscribed a term deposit?"; *(binary: 'yes','no');
run;


/*************************************************
                Comprobar missings
*************************************************/
proc means data=bankadditionalfull maxdec=2 n nmiss; run;


/*************************************************
                Comprobar repetidos
*************************************************/
proc sort data=bankadditionalfull noduprecs out=bankadditionalfull; by _ALL_ ; run;


/*************************************************
   Histograma y boxplot para variables continuas
*************************************************/
ods graphics on;
proc ttest data=bankadditionalfull sides=2 h0=0 plots(only)=(summary);
	class y;
	var _NUMERIC_;
run;
ods graphics off;


/*************************************************
       Bar charts para variables categoricas
*************************************************/
%macro barplots();
	proc contents data=bankadditionalfull noprint out=_contents_ (keep=name type); run;

	proc sql noprint;
		select strip(name) into : catvars separated by ' ' from _contents_ where type=2;
	quit;

	proc delete data = _contents_;

	%let nwords=%sysfunc(countw(&catvars));

	%do i=1 %to &nwords;
		%let y_var = %scan(&catvars, &i);
		proc sgplot data=bankadditionalfull;
			vbar &y_var / group=y groupdisplay=stack;
			yaxis grid;
		run;
	%end;
%mend;

%barplots();


/*************************************************
       Tratamiento y seleccion de variables
*************************************************/
data bankProcessed(drop=age default duration campaign pdays previous emp_var_rate cons_price_idx cons_conf_idx euribor3m nr_employed y);
	set bankadditionalfull;

	if (age<25) 			then ageCat = 1;
	else if (25<=age<30)	then ageCat = 2;
	else if (30<=age<35)	then ageCat = 3;
	else if (35<=age<40)	then ageCat = 4;
	else if (40<=age<45)	then ageCat = 5;
	else if (45<=age<50)	then ageCat = 6;
	else if (50<=age<60)	then ageCat = 7;
	else if (age>=60)		then ageCat = 8;

	if job= 'admin.' then job = 'admin';

	select (month);
		when ('jan', 'feb', 'mar')	quarter = 1;
		when ('apr', 'may', 'jun')	quarter = 2;
		when ('jul', 'aug', 'sep')	quarter = 3;
		when ('oct', 'nov', 'dec')	quarter = 4;
	end;

	campaign = campaign - 1;
	if (campaign<=3) then campaignCat = campaign;
	else campaignCat = 4;

	if (pdays<7)				then pdaysCat = 1;
	else if (7<=pdays<14)	then pdaysCat = 2;
	else if (14<=pdays<21)	then pdaysCat = 3;
	else if (21<=pdays<28)	then pdaysCat = 4;
	else if (pdays>=28)		then pdaysCat = 5;

	if (previous=0) then previousCat = 0;
	else previousCat = 1;

	if (emp_var_rate<=-1.8)					then emp_var_rateCat = 1;
	else if (-1.8<emp_var_rate<=-0.1)	then emp_var_rateCat = 2;
	else if (emp_var_rate>-0.1)			then emp_var_rateCat = 3;

	if (cons_price_idx<93.06)					then cons_price_idxCat = 1;
	else if (93.06<=cons_price_idx<93.92)	then cons_price_idxCat = 2;
	else if (cons_price_idx>=93.92)			then cons_price_idxCat = 3;

	if (cons_conf_idx<-45.9)				then cons_conf_idxCat = 1;
	else if (-45.9<=cons_conf_idx<-42)	then cons_conf_idxCat = 2;
	else if (-42<=cons_conf_idx<-36.4)	then cons_conf_idxCat = 3;
	else if (cons_conf_idx>=-36.4)		then cons_conf_idxCat = 4;

	if (euribor3m<1.3)					then euribor3mCat = 1;
	else if (1.3<=euribor3m<4.19)		then euribor3mCat = 2;
	else if (4.19<=euribor3m<4.86)	then euribor3mCat = 3;
	else if (4.86<=euribor3m<4.962)	then euribor3mCat = 4;
	else if (euribor3m>=4.962)			then euribor3mCat = 5;

	if (nr_employed<5099.1)						then nr_employedCat = 1;
	else if (5099.1<=nr_employed<5191.02)	then nr_employedCat = 2;
	else if (nr_employed>=5191.02)			then nr_employedCat = 3;

	if (y='no') then yN = 0;
	else yN = 1;
run;


/*************************************************
     Dumificacion y creacion de interacciones
*************************************************/
proc glmmod data=bankProcessed;
	class job--nr_employedCat;
	model yN=ageCat campaignCat cons_conf_idxCat cons_price_idxCat contact day_of_week education emp_var_rateCat
				euribor3mCat housing job loan marital month nr_employedCat pdaysCat poutcome previousCat quarter
				euribor3mCat*nr_employedCat euribor3mCat*emp_var_rateCat
				day_of_week*campaignCat day_of_week*previousCat day_of_week*contact
				month*day_of_week month*previousCat month*pdaysCat month*contact
				quarter*day_of_week quarter*previousCat quarter*pdaysCat quarter*contact;
	ods output designpoints=bankDummy;
run;


/*
Macro seleccion modelo: Procedimiento Logistico
t_input  = Tabla Input
vardepen = Variable Dependiente
varindep = Variable(s) Independiente(s)
semi_ini = Valor Inicial de la semilla
semi_fin = Valor Final de la semilla
 */
%macro logistic (t_input, vardepen, varindep, semi_ini, semi_fin );
ods trace on /listing;
%do semilla=&semi_ini. %to &semi_fin.;

 ods output EffectInModel= efectoslog;/*Test de Wald de efectos en el modelo*/
 ods output FitStatistics= ajustelog; /*"Estadisticos de ajuste", AIC */
 ods output ParameterEstimates= estimalog;/*"Estimadores de parametro"*/
 ods output ModelBuildingSummary=modelolog; /*Resumen modelo, efectos*/
 ods output RSquare=ajusteRlog; /*R-cuadrado y Max-rescalado R-cuadrado*/

 proc logistic data=&t_input. EXACTOPTIONS (seed=&semilla.) ;
  class &varindep.;
  model &vardepen.(event='1') = &varindep.
     / selection=stepwise details rsquare NOCHECK;
 run;

 data un1; i=12; set efectoslog; set ajustelog; point=i; run;
 data un2; i=12; set un1; set estimalog; point=i; run;
 data un3; i=12; set un2; set modelolog; point=i; run;
 data union&semilla.; i=12; set un3; set ajusteRlog; point=i; run;

 proc append  base=t_models  data=union&semilla.  force; run;
 proc sql; drop table union&semilla.; quit;

%end;
ods html close;
proc sql; drop table un1,un2,un3; quit;
proc sql; drop table efectoslog,ajustelog,ajusteRlog,estimalog,modelolog; quit;

%mend;


/*************************************************
                 Macro logistica
*************************************************/
ods exclude all;
ods noresults;
%logistic (bankDummy, yN, 'ageCat 1'n--'contact*quarter telephone 4'n, 23174, 23180);
ods results;
ods exclude none;



/*************************************************
            Analisis de los resultados
*************************************************/
proc freq data=t_models (keep=effect ProbChiSq); tables effect*ProbChiSq /norow nocol nopercent; run;
proc sql; select distinct * from t_models (keep=effect nvalue1 rename=(nvalue1=RCuadrado)) order by RCuadrado desc; quit;
proc sql; select distinct * from t_models (keep=effect StdErr) order by StdErr; quit;


/*************************************************
              Tabla de clasificacion
*************************************************/
proc freq data = bankDummy; tables yN*'emp_var_r*euribor3mC 1 2'n /norow nocol nopercent relrisk; run;
proc freq data = bankDummy; tables yN*'nr_employedCat 1'n /norow nocol nopercent relrisk; run;
proc freq data = bankDummy; tables yN*'pdaysCat 5'n /norow nocol nopercent relrisk; run;


/*************************************************
       Tabla de sensibilidad y especificidad
                     Curva ROC
*************************************************/
ods graphics on;
proc logistic data=bankDummy desc PLOTS(MAXPOINTS=NONE);
	model yN(event='1') = 'emp_var_r*euribor3mC 1 2'n 'nr_employedCat 1'n 'pdaysCat 5'n
								/ ctable pprob = (.05 to 1 by .05) outroc=roc;
run;
ods graphics off;


/*************************************************
    Generar datos de clientes y su probabilidad
*************************************************/
data bankFinal;
	id = _n_;
	set bankadditionalfull;

	if (emp_var_rate<=-1.8) and (1.3<=euribor3m<4.19) then var1 = 1;
	else var1 = 0;

	if (nr_employed<5099.1) then var2 = 1;
	else var2 = 0;

	if (pdays>=28) then var3 = 1;
	else var3 = 0;
run;

proc logistic data=bankFinal desc PLOTS(MAXPOINTS=NONE) noprint;
	model y(event='yes') = var1 var2 var3 / ctable pprob = (.05 to 1 by .05);
	output out=bankPredicted p=y_predicted;
run;


/*************************************************
                 Clientes Top 10%
*************************************************/
proc sql noprint;
	create table bankTop10 as
		select * from bankPredicted order by y_predicted desc;
quit;

%let n_cols = &sqlobs;

data bankTop10;
	set bankTop10;
	if _n_ / &n_cols. ge .1 then stop;
run;


/*************************************************
                Clientes Random 5%
*************************************************/
proc sql noprint;
	select distinct id into :y_list separated by ' ' from bankTop10;
quit;

proc surveyselect data=bankPredicted (where = (id not in (&y_list))) noprint
	out=bankRandom5 method=srs sampsize=%sysevalf(&n_cols. * 0.05, integer);
run;

proc delete data=bankPredicted; run;


/*************************************************
                 Clientes Top 10%
                         +
                Clientes Random 5%
*************************************************/
data bankFinal;
	set bankTop10 bankRandom5;
run;

proc delete data=bankTop10 bankRandom5; run;

proc sort data=bankFinal
	out=bankFinal(drop = id y var1 var2 var3 _LEVEL_ y_predicted); by id; run;
