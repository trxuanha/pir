from Utility import *

inFileName = 'adultML'
outcome = 'Prof'
factors = ['Education', 'WorkHour', 'SelfEmp']

factorVals= {
  'Education': [1, 2, 3],
  'WorkHour': [1, 2, 3, 4],
  'SelfEmp': [0, 1]
}

excludeFactors = []
method = 'Xlearner'
executeBaselineML(inFileName, method, outcome, factors, factorVals, excludeFactors)

method = 'DRL'
executeBaselineML(inFileName, method, outcome, factors, factorVals, excludeFactors)





    
    
