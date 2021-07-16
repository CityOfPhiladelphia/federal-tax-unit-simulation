# federal-tax-unit-simulation
Code for creating tax units from the American Community Survey. This reflects tax dependency rules current to 2020. 

This version of the code reflects files to simulate families in Pennsylvania, but it can accomodate any data. 

Data is drawn from IPUMS and importantly the work of Liana Fox, Brian Glassman, and Jose Pacas at the Census. Those folks are awesome, here is their work: https://www.census.gov/library/working-papers/2020/demo/SEHSD-WP2020-09.html

This is to be plugged into tax-calculator, a package from the Policy Simulation Library: http://taxcalc.pslmodels.org/


The three files used to combine are 
  1. psam42        --   the Census ACS PUMS file
  2. spm_pu_2018   --   the Fox, Glassman, Pacas file
  3. usa_00014     --   the IPUMS file with relationship identifiers
