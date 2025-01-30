DATA work.lfstats5digNOC;
SET RTRAdata.LFS202125 (keep = ID PROV ERTAB AGE LFSSTAT NOC_5 SYEAR);

/* Province B.C.*/
if PROV = 59;

/* Age 15 or over */
if AGE >= 15;

/*Labour Force Status*/

length lf_stat $20;
if LFSSTAT IN (1,2) then lf_stat = 'Employed';
else if LFSSTAT in (3,4,5) then lf_stat = 'Unemployed';
else lf_stat = 'Unknown';

/*Second half of the NOCS, missing in other half*/

if NOC_5 > 49999;


run;

%RTRAFreq(
     InputDataset=work.lfstats5digNOC,
     OutputName=stat2125NOCp2,
     ClassVarList= SYEAR ERTAB lf_stat NOC_5,
     UserWeight=FINALWT);
run;
