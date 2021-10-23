from Utility import *


inFileName = 'turnoverML'
outcome = 'Long_emp_retention'
factors = ['Extraversion', 'Grey_wage']
factorVals= {
  'Extraversion': [1, 2, 3, 4],
  'Grey_wage': [0, 1]
}
excludeFactors = ['ID', 'stag', 'event', 'left']
method = 'Xlearner'
executeBaselineML(inFileName, method, outcome, factors, factorVals, excludeFactors)

method = 'DRL'
executeBaselineML(inFileName, method, outcome, factors, factorVals, excludeFactors)





    
    
