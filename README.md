# COVerAGE-DB_Czechia
The code for automatical download and pre-edit of Czech COVID-19 data. 
The original data are available at the Ministery of Health [website](onemocneni-aktualne.mzcr.cz/api/v2/covid-19).
The data are collected for the purposes of the [COVerAge-DB](github.com/timriffe/covid_age).

The data for *cases/deaths* are structurade by Date (values are cummulative), NUTS3 regions ("kraje"), age group (0-100+ by 5), sex
The data for *vaccination coverage* are structured by Date (values are cummulative), NUTS3 regions ("kraje"), age group (0-17, 18-24, 25-80+ by 5y), vaccine type (brand). The first and second dose separated.
