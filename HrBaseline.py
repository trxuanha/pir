from Utility import *


inFileName = 'hrML'
outcome = 'time_spend_company'
factors = ['satisfaction_level', 'average_montly_hours']

factorVals= {
  'satisfaction_level': [1, 2, 3],
  'average_montly_hours': [1, 2, 3, 4, 5]
}

excludeFactors = ['ID', 'stag', 'event', 'left']
method = 'Xlearner'
executeBaselineML(inFileName, method, outcome, factors, factorVals, excludeFactors)

method = 'DRL'
executeBaselineML(inFileName, method, outcome, factors, factorVals, excludeFactors)





    
    
