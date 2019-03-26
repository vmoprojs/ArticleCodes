****This code prepares the variables from the required data imputs in ENIGUH dataset.
**** Author: Víctor Morales Oñate
**** Adpted from: Paul Carillo
**** Date: Jan-25-2016

****************************************************************
* this part computes the income of the main occupation of the non-asalariados

GET
  FILE='/Users/victormoralesonate/Documents/Bases de datos/Base ENIGHU/perbi.sav'.

*  '/Users/victormoralesonate/Documents/Bases de datos/Base ENIGHU/perbi.sav'



STRING idper (A40).
COMPUTE idper = CONCAT(ciudad,zona,sector,vivienda,hogar,secuenci,persona) .
VARIABLE LABELS idper 'identifier individual'.
EXECUTE .

SORT CASES BY
  idper (A) .

FILTER OFF.
USE ALL.
SELECT IF(codigo='1404004'|codigo='1404005'|codigo='1404006'|codigo='1404007'|codigo='1404008'|codigo='1404009'|codigo='1404010').
EXECUTE .

STRING id_y (A2).
COMPUTE id_y = SUBSTR(codigo,1,2) .
VARIABLE LABELS id_y 'codigo de ingresos' .
EXECUTE .

AGGREGATE
  /OUTFILE='/Users/victormoralesonate/Documents/Bases de datos/Base ENIGHU/tot_nasal.sav'
  /BREAK=idper
  /pbiv_1 = MEAN(pbiv).
*************************************************************************

*************************************************************************
* this is the income of the main ocupation of the asalariados

GET
  FILE='/Users/victormoralesonate/Documents/Bases de datos/Base ENIGHU/pera.sav'.

STRING idper (A40).
COMPUTE idper = CONCAT(ciudad,zona,sector,vivienda,hogar,secuenci,persona) .
VARIABLE LABELS idper 'identifier individual'.
EXECUTE .

STRING id_y (A2).
COMPUTE id_y = SUBSTR(codigo,1,2) .
VARIABLE LABELS id_y 'codigo de ingresos' .
EXECUTE .

SORT CASES BY
  idper (A) .


FILTER OFF.
USE ALL.
SELECT IF(id_y='14').
EXECUTE .
AGGREGATE
  /OUTFILE='/Users/victormoralesonate/Documents/Bases de datos/Base ENIGHU/tot_ing_asal.sav'
  /BREAK=idper
  /pa05_1 = SUM(pa05).

**************************************************************************

**************************************************************************
* "personas.sav" to be matched:

GET
  FILE='/Users/victormoralesonate/Documents/Bases de datos/Base ENIGHU/personas.sav'.

STRING idper (A40).
COMPUTE idper = CONCAT(ciudad,zona,sector,vivienda,hogar,secuenci,persona) .
VARIABLE LABELS idper 'identifier individual'.
EXECUTE .

SORT CASES BY
  idper (A) .

MATCH FILES /FILE=*
 /FILE='/Users/victormoralesonate/Documents/Bases de datos/Base ENIGHU/tot_ing_asal.sav'
 /BY idper.
EXECUTE.

MATCH FILES /FILE=*
 /FILE='/Users/victormoralesonate/Documents/Bases de datos/Base ENIGHU/tot_nasal.sav'
 /BY idper.
EXECUTE.

SAVE OUTFILE='/Users/victormoralesonate/Documents/Bases de datos/Base ENIGHU/educ.sav'
   /COMPRESSED.

***************************************************************************************

* "eyears" Education years is created here:
 ***********************
* a separated file is created ("edtemp.sav"):

GET
  FILE='/Users/victormoralesonate/Documents/Bases de datos/Base ENIGHU/personas.sav'.

STRING idper (A40).
COMPUTE idper = CONCAT(ciudad,zona,sector,vivienda,hogar,secuenci,persona) .
VARIABLE LABELS idper 'identifier individual' .
EXECUTE .

SORT CASES BY
  idper (A) .


SAVE OUTFILE='/Users/victormoralesonate/Documents/Bases de datos/Base ENIGHU/edtemp.sav'
  /DROP=ciudad zona sector vivienda hogar secuenci persona pe02 pe03 pe04
  pe05 pe06 pe06a pe07 pe08 pe09 pe11 pe12 pe13 pe14 pe15 pe16 pe16a pe17 pe17a pe18
  pe18a pe19 pe19a pe20 pe21 pe22 pe23 pe24 pe25 pe26 pe27 pe28 pe29 pe30 pe31
  pe32 pe33 pe33a pe33b pe34 pe35 pe35a pe36 pe37 pe38 pe39 pe40 pe41 pe42
  pe43 pe43a pe44 pe45 percep pertar fexp region dominio estrato periodo
  semana /COMPRESSED.



**********************

GET
  FILE='/Users/victormoralesonate/Documents/Bases de datos/Base ENIGHU/educ.sav'.

* Not PEA people is dropped:

FILTER OFF.
USE ALL.
SELECT IF(pe03>15).
EXECUTE .


* Education is computed:


IF (pe16=2) edp = pe17 .
VARIABLE LABELS edp 'nivel de ducacion padre' .
EXECUTE .

IF (pe16=2) edpa = pe17a .
VARIABLE LABELS edp 'nivel de ducacion padre' .
EXECUTE .

IF (pe18=2) edm = pe19 .
VARIABLE LABELS edp 'nivel de ducacion padre' .
EXECUTE .

IF (pe18=2) edma = pe19a .
VARIABLE LABELS edp 'nivel d educacion padre' .
EXECUTE .

* People's ID is created:

STRING codedp (A40).
IF (pe16=1&pe16a<10) codedp = CONCAT(ciudad,zona,sector,vivienda,hogar,secuenci,'0'
 ,string(pe16a,N1)) .
VARIABLE LABELS codedp 'education code for father' .
EXECUTE .

STRING codedm (A40).
IF (pe18=1&pe18a<10) codedm = CONCAT(ciudad,zona,sector,vivienda,hogar,secuenci,'0'
 ,string(pe18a,N1)) .
VARIABLE LABELS codedm 'education code for mother' .
EXECUTE .

SORT CASES BY
  codedp (A) .

MATCH FILES /FILE=*
 /TABLE='/Users/victormoralesonate/Documents/Bases de datos/Base ENIGHU/edtemp.sav'
 /RENAME  pe10=pe10_f pe10a=pe10a_f idper=codedp
 /BY codedp.
EXECUTE.

SORT CASES BY
  codedm (A) .

MATCH FILES /FILE=*
 /TABLE='/Users/victormoralesonate/Documents/Bases de datos/Base ENIGHU/edtemp.sav'
 /RENAME  pe10=pe10_m pe10a=pe10a_m idper=codedm
 /BY codedm.
EXECUTE.

SORT CASES BY
  idper (A) .

* For the father:.

IF (pe10_f<4 & pe16=1) eyears_f = 0 .
EXECUTE .
IF (pe10_f=4&pe16=1) eyears_f = pe10a_f .
EXECUTE .
IF (pe10_f=5&pe16=1) eyears_f = 6+pe10a_f .
EXECUTE .
IF (pe10_f=6&pe16=1) eyears_f = 12+pe10a_f .
EXECUTE .
IF (pe10_f=7&pe16=1) eyears_f = 17+pe10a_f .
EXECUTE .

IF (edp<3 & pe16=2) eyears_f = 0 .
EXECUTE .
IF (edp=3&pe16=2) eyears_f = edpa .
EXECUTE .
IF (edp=4&pe16=2) eyears_f = 6+edpa .
EXECUTE .
IF (edp=5&pe16=2) eyears_f = 12+edpa .
EXECUTE .
IF (edp=6&pe16=2) eyears_f = 17+edpa .
EXECUTE .

* For the Mother:

IF (pe10_m<4 & pe18=1) eyears_m = 0 .
EXECUTE .
IF (pe10_m=4&pe18=1) eyears_m = pe10a_m .
EXECUTE .
IF (pe10_m=5&pe18=1) eyears_m = 6+pe10a_m .
EXECUTE .
IF (pe10_m=6&pe18=1) eyears_m = 12+pe10a_m .
EXECUTE .
IF (pe10_m=7&pe18=1) eyears_m = 17+pe10a_m .
EXECUTE .

IF (edm<3 & pe18=2) eyears_m = 0 .
EXECUTE .
IF (edm=3&pe18=2) eyears_m = edma .
EXECUTE .
IF (edm=4&pe18=2) eyears_m = 6+edma .
EXECUTE .
IF (edm=5&pe18=2) eyears_m = 12+edma .
EXECUTE .
IF (edm=6&pe18=2) eyears_m = 17+edma .
EXECUTE .

* For the rest

IF (pe10<4) eyears = 0 .
EXECUTE .
IF (pe10=4) eyears = pe10a .
EXECUTE .
IF (pe10=5) eyears = 6+pe10a .
EXECUTE .
IF (pe10=6) eyears = 12+pe10a .
EXECUTE .
IF (pe10=7) eyears = 17+pe10a .
EXECUTE .

* This is for the income.

IF (MISSING(pa05_1)=0 & MISSING(pbiv_1)=0 ) ly1 =LN(pa05_1+pbiv_1) .
EXECUTE .
IF (MISSING(pa05_1)=0 & MISSING(pbiv_1)=1 ) ly1 =LN(pa05_1) .
EXECUTE .
IF (MISSING(pa05_1)=1 & MISSING(pbiv_1)=0 & pbiv_1>0) ly1 =LN(pbiv_1) .
EXECUTE .

COMPUTE ly2 = ly1-LN(pe33*4) .
EXECUTE .

IF (pe02=2) female = 1 .
EXECUTE .
IF (pe02=1) female = 0 .
EXECUTE .

COMPUTE age = pe03 .
EXECUTE .

COMPUTE age2 = pe03*pe03 .
EXECUTE .

COMPUTE tenure = pe32 .
EXECUTE .

COMPUTE tenure2 = pe32*pe32 .
EXECUTE .

IF (pe05=3) casado = 1 .
EXECUTE .
IF (pe05<>3) casado = 0 .
EXECUTE .

COMPUTE white = 0 .
EXECUTE .
IF (pe15=3) white = 1 .
EXECUTE .

COMPUTE black = 0 .
EXECUTE .
IF (pe15=1|pe15=4|pe15=5) black = 1 .
EXECUTE .

COMPUTE public = 0 .
EXECUTE .
IF (pe30=1) public = 1 .
EXECUTE .

IF (pe10=7) e_pos = 1 .
EXECUTE .
IF (pe10<>7) e_pos = 0 .
EXECUTE .




* **********************************************************************1
* Variables for estimating the model are calculated here:

SELECT IF((pe30=1 | pe30=2) & pe33>30 & pa05_1>100).
EXECUTE .


COMPUTE ly1 = LN(pa05_1) .
EXECUTE .

COMPUTE ly2 = LN(pa05_1/pe33) .
EXECUTE .

IF (pe02=2) female = 1 .
EXECUTE .
IF (pe02=1) female = 0 .
EXECUTE .

COMPUTE age = pe03 .
EXECUTE .

COMPUTE age2 = pe03*pe03 .
EXECUTE .

IF (pe05=3) casado = 1 .
EXECUTE .
IF (pe05<>3) casado = 0 .
EXECUTE .

IF (pe10=4) e_prim = 1 .
EXECUTE .
IF (pe10<>4) e_prim = 0 .
EXECUTE .

IF (pe10=5) e_sec = 1 .
EXECUTE .
IF (pe10<>5) e_sec = 0 .
EXECUTE .

IF (pe10=6) e_sup = 1 .
EXECUTE .
IF (pe10<>6) e_sup = 0 .
EXECUTE .

IF (pe10=7) e_pos = 1 .
EXECUTE .
IF (pe10<>7) e_pos = 0 .
EXECUTE .

IF (pe10<4) eyears = 0 .
EXECUTE .
IF (pe10=4) eyears = pe10a .
EXECUTE .
IF (pe10=5) eyears = 6+pe10a .
EXECUTE .
IF (pe10=6) eyears = 12+pe10a .
EXECUTE .
IF (pe10=7) eyears = 17+pe10a .
EXECUTE .

COMPUTE white = 0 .
EXECUTE .
IF (pe15=3) white = 1 .
EXECUTE .

COMPUTE black = 0 .
EXECUTE .
IF (pe15=1|pe15=4|pe15=5) black = 1 .
EXECUTE .

COMPUTE public = 0 .
EXECUTE .
IF (pe30=1) public = 1 .
EXECUTE .

* Save the database as STATA file

SAVE TRANSLATE OUTFILE=
    '/Users/victormoralesonate/Documents/Articles/Analitika/RegExp/dataRegExp.dta'
  /TYPE=STATA
  /VERSION=8
  /EDITION=SE
  /MAP
  /REPLACE.
