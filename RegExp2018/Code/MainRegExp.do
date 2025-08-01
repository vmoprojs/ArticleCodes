* Importo los datos 
clear all
set more off
use "/Users/victormoralesonate/Documents/Articles/Analitika/RegExp/dataRegExp.dta", clear


* Filtro para trabajar con la misma submuestra:
gen filtro = 0
*replace filtro = 1 if pe28 >=6500 & pe28 < 9500
replace filtro = 1 if pe28 >=6500 & pe28 <= 9900

* filtro1: primaria
gen filtro1 = 0
replace filtro1 = 1 if pe28 >=8000 & pe28 <= 8010

tab pe02 if filtro1 ==1

* filtro2: secundaria
gen filtro2 = 0
replace filtro2 = 1 if pe28 >=8020 & pe28 <= 8022

tab pe02 if filtro2 ==1

* filtro3: trabajadores hospitalarios
gen filtro3 = 0
replace filtro3 = 1 if pe28 ==8511

tab pe02 if filtro3 ==1



* Cuadro 1
* Ingreso mensual: pa05_1
* Horas de trabajo semanales: pe33
generate salhor = pa05_1/(pe33*4)
 
summ pa05_1 pe33 salhor female age  eyears e_pos white black casado public if filtro == 1

mean pa05_1 if filtro == 1
mean pa05_1 if filtro == 1 [aw=fexp]
svy: mean pa05_1 if filtro == 1

foreach x in pa05_1 pe33 salhor female age  eyears e_pos white black casado public {
mean `x' if filtro == 1
mean `x' if filtro == 1 [aw=fexp]
svy: mean `x' if filtro == 1
}


foreach x in pa05_1 pe33 salhor female age  eyears e_pos white black casado public {
mean `x' if filtro1 == 1
mean `x' if filtro1 == 1 [aw=fexp]
svy: mean `x' if filtro1 == 1
}

foreach x in pa05_1 pe33 salhor female age  eyears e_pos white black casado public {
mean `x' if filtro2 == 1
mean `x' if filtro2 == 1 [aw=fexp]
svy: mean `x' if filtro2 == 1
}


foreach x in pa05_1 pe33 salhor female age  eyears e_pos white black casado public {
mean `x' if filtro3 == 1
mean `x' if filtro3 == 1 [aw=fexp]
svy: mean `x' if filtro3 == 1
}

* Cuadro N¼ 2 de Carrillo 2004:
* Columna 1: Total Muestra. Nota: No coincide la constante pero si todo lo dem‡s
reg ly2 female age age2  eyears e_pos white black casado public if filtro ==1, robust
predict r, resid
kdensity r, normal
pnorm r
qnorm r
iqr r
swilk r
* Columna 2: Educaci—n primaria 
reg ly2 female age age2  eyears e_pos white black casado public if  filtro1==1, robust
* Columna 3: Educaci—n primaria 
reg ly2 female age age2  eyears e_pos white black casado public if  filtro2==1, robust
* Columna 4: Trabajadores hospitalarios
reg ly2 female age age2  eyears e_pos white black casado public if  filtro3==1, robust


run "/Users/victormoralesonate/Documents/Articles/Analitika/RegExp/DISE_MUESTRAL_ENIGHU_2004.do"

svyset [pweight=fexp], strata(dominio1) psu(upm)

* Dise–o muestral 
* Columna 1: Total Muestra.
svy: reg ly2 female age age2  eyears e_pos white black casado public if filtro ==1
* Columna 2: Educaci—n primaria 
svy: reg ly2 female age age2  eyears e_pos white black casado public if  filtro1==1
* Columna 3: Educaci—n primaria 
svy: reg ly2 female age age2  eyears e_pos white black casado public if  filtro2==1
* Columna 4: Trabajadores hospitalarios
svy: reg ly2 female age age2  eyears e_pos white black casado public if  filtro3==1

* Comparando descriptivos:

* sexo:
tab female if filtro == 1
tab female [aw = fexp] if filtro ==1, sum (fexp)
* Posgrado:
tab e_pos if filtro == 1
tab e_pos [aw = fexp] if filtro ==1, sum (fexp)
* White:
tab white if filtro == 1
tab white [aw = fexp] if filtro ==1, sum (fexp)
* Black:
tab black if filtro == 1
tab black [aw = fexp] if filtro ==1, sum (fexp)
* Casado:
tab casado if filtro == 1
tab casado [aw = fexp] if filtro ==1, sum (fexp)
* Public:
tab public if filtro == 1
tab public [aw = fexp] if filtro ==1, sum (fexp)


* Mean values as a whole:
mean pa05_1 pe33 salhor female age  eyears e_pos white black casado public if filtro == 1 
mean pa05_1 pe33 salhor female age  eyears e_pos white black casado public [aw = fexp] if filtro == 1
svy: mean pa05_1 pe33 salhor female age  eyears e_pos white black casado public if filtro == 1 



* Comparaci—n de modelos:
*1: Da igual comparar AIC o BIC con las opciones robut o sin robust
reg ly2 female age age2  eyears e_pos white black casado public if  filtro1==1
estat ic
reg ly2 female age age2  eyears e_pos white black casado public if  filtro1==1, robust
estat ic

*2: ÀHay diferencia usando el test de wald?
reg ly2 female age age2  eyears e_pos white black casado public if  filtro1==1
est store m1
reg ly2 female age age2  eyears e_pos white black casado public if  filtro1==1, robust
est store m2
ftest m1 m2


reg ly2 female age age2  eyears e_pos white black casado public if  filtro1==1, robust
svy: reg ly2 female age age2  eyears e_pos white black casado public if filtro ==1


* https://www3.nd.edu/~rwilliam/stats2/SvyCautions.pdf

