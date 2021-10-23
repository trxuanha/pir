
import os
import sys
import statistics 
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib as mpl, matplotlib.pyplot as plt
import os.path
from os import path
import random

from Utility import *

plotGBar = True
plotTrend = False
Rank = True

BASE_DIR = os.getcwd()
baseInFolder = os.path.join(BASE_DIR, 'output')


def plotTrendLine(inputName, factors, outcomeName, allAxis, datasetName, gcount, minY=None, maxY=None):

    for cause in factors:
    
        factor = 'FOLLOW_REC'
        methods = []
        gainScores = []
        qiniScores = []
        auucScores = []
        
        startCount = gcount
        perPop = [0.25, 0.5,  0.75, 1.0]        
        tickLabel  = ['0.25','0.5', '0.75', '1.0']     
        tickLabel = None
        
        method = 'CT'
        methods.append(method)
        prefileName = inputName + '_' + method + '_'  
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName +'/' + cause
        gain, gini = getAuucScore(fullResultFolder, 5, prefileName, postfileName, outcomeName, factor)
        gainScores.append(gain)
        qiniScores.append(gini)
        opiAuc = getAUUCTopGroup(fullResultFolder, 5, prefileName, postfileName, outcomeName, False)
        
        print(method + ":" + str(opiAuc))  
        auucScores.append(opiAuc) 
        newMolde = genQiniDataML(fullResultFolder, 5, prefileName, postfileName, method, outcomeName)
        improvementModels = pd.DataFrame({})
        improvementModels = improvementModels.append(newMolde)
        improvementModels['uplift'] = improvementModels['uplift']* 100
        improvementModels['grUplift'] = improvementModels['grUplift']* 100     
        plotBarImprovementTopV2(improvementModels, [method], allAxis, startCount, perPop, tickLabel, minY, maxY)
        startCount = startCount + 1
        
        
        method = 'FT'
        methods.append(method)
        prefileName = inputName + '_' + method + '_'  
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName +'/' + cause
        gain, gini = getAuucScore(fullResultFolder, 5, prefileName, postfileName, outcomeName, factor)
        gainScores.append(gain)
        qiniScores.append(gini)
        opiAuc = getAUUCTopGroup(fullResultFolder, 5, prefileName, postfileName, outcomeName, False)
        
        print(method + ":" + str(opiAuc))  
        auucScores.append(opiAuc) 
        newMolde = genQiniDataML(fullResultFolder, 5, prefileName, postfileName, method, outcomeName)
        improvementModels = pd.DataFrame({})
        improvementModels = improvementModels.append(newMolde)
        improvementModels['uplift'] = improvementModels['uplift']* 100
        improvementModels['grUplift'] = improvementModels['grUplift']* 100     
        plotBarImprovementTopV2(improvementModels, [method], allAxis, startCount, perPop, tickLabel, minY, maxY)
        startCount = startCount + 1

        method = 'TST'
        methods.append(method)
        prefileName = inputName + '_' + method + '_'  
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName +'/' + cause
        gain, gini = getAuucScore(fullResultFolder, 5, prefileName, postfileName, outcomeName, factor)
        gainScores.append(gain)
        qiniScores.append(gini)
        opiAuc = getAUUCTopGroup(fullResultFolder, 5, prefileName, postfileName, outcomeName, False)
        
        print(method + ":" + str(opiAuc))  
        auucScores.append(opiAuc) 
        newMolde = genQiniDataML(fullResultFolder, 5, prefileName, postfileName, method, outcomeName)
        improvementModels = pd.DataFrame({})
        improvementModels = improvementModels.append(newMolde)
        improvementModels['uplift'] = improvementModels['uplift']* 100
        improvementModels['grUplift'] = improvementModels['grUplift']* 100     
        plotBarImprovementTopV2(improvementModels, [method], allAxis, startCount, perPop, tickLabel, minY, maxY)
        startCount = startCount + 1

        method = 'TOT'
        methods.append(method)
        prefileName = inputName + '_' + method + '_'  
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName +'/' + cause
        gain, gini = getAuucScore(fullResultFolder, 5, prefileName, postfileName, outcomeName, factor)
        gainScores.append(gain)
        qiniScores.append(gini)
        opiAuc = getAUUCTopGroup(fullResultFolder, 5, prefileName, postfileName, outcomeName, False)
        
        print(method + ":" + str(opiAuc))  
        auucScores.append(opiAuc) 
        newMolde = genQiniDataML(fullResultFolder, 5, prefileName, postfileName, method, outcomeName)
        improvementModels = pd.DataFrame({})
        improvementModels = improvementModels.append(newMolde)
        improvementModels['uplift'] = improvementModels['uplift']* 100
        improvementModels['grUplift'] = improvementModels['grUplift']* 100     
        plotBarImprovementTopV2(improvementModels, [method], allAxis, startCount, perPop, tickLabel, minY, maxY)
        startCount = startCount + 1
        
        method = 'CF'
        methods.append(method)
        prefileName = inputName + '_' + method + '_'  
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName +'/' + cause
        gain, gini = getAuucScore(fullResultFolder, 5, prefileName, postfileName, outcomeName, factor)
        gainScores.append(gain)
        qiniScores.append(gini)
        opiAuc = getAUUCTopGroup(fullResultFolder, 5, prefileName, postfileName, outcomeName, False)
        
        print(method + ":" + str(opiAuc))  
        auucScores.append(opiAuc) 
        newMolde = genQiniDataML(fullResultFolder, 5, prefileName, postfileName, method, outcomeName)
        improvementModels = pd.DataFrame({})
        improvementModels = improvementModels.append(newMolde)
        improvementModels['uplift'] = improvementModels['uplift']* 100
        improvementModels['grUplift'] = improvementModels['grUplift']* 100     
        plotBarImprovementTopV2(improvementModels, [method], allAxis, startCount, perPop, tickLabel, minY, maxY)
        startCount = startCount + 1

        method = 'Xlearner'
        methods.append(method)
        prefileName = inputName + '_' + method + '_'  
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName +'/' + cause
        gain, gini = getAuucScore(fullResultFolder, 5, prefileName, postfileName, outcomeName, factor)
        gainScores.append(gain)
        qiniScores.append(gini)
        opiAuc = getAUUCTopGroup(fullResultFolder, 5, prefileName, postfileName, outcomeName, False)
        
        print(method + ":" + str(opiAuc))  
        auucScores.append(opiAuc) 
        newMolde = genQiniDataML(fullResultFolder, 5, prefileName, postfileName, method, outcomeName)
        improvementModels = pd.DataFrame({})
        improvementModels = improvementModels.append(newMolde)
        improvementModels['uplift'] = improvementModels['uplift']* 100
        improvementModels['grUplift'] = improvementModels['grUplift']* 100     
        plotBarImprovementTopV2(improvementModels, [method], allAxis, startCount, perPop, tickLabel, minY, maxY)
        startCount = startCount + 1
        
        
        method = 'DRL'
        methods.append(method)
        prefileName = inputName + '_' + method + '_'  
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName +'/' + cause
        gain, gini = getAuucScore(fullResultFolder, 5, prefileName, postfileName, outcomeName, factor)
        gainScores.append(gain)
        qiniScores.append(gini)
        opiAuc = getAUUCTopGroup(fullResultFolder, 5, prefileName, postfileName, outcomeName, False)
        
        print(method + ":" + str(opiAuc))  
        auucScores.append(opiAuc) 
        newMolde = genQiniDataML(fullResultFolder, 5, prefileName, postfileName, method, outcomeName)
        improvementModels = pd.DataFrame({})
        improvementModels = improvementModels.append(newMolde)
        improvementModels['uplift'] = improvementModels['uplift']* 100
        improvementModels['grUplift'] = improvementModels['grUplift']* 100     
        plotBarImprovementTopV2(improvementModels, [method], allAxis, startCount, perPop, tickLabel, minY, maxY)
        startCount = startCount + 1
        
        method = 'PIR'
        methods.append('PIR')
        prefileName = inputName + '_' + method + '_'  
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName +'/' + cause
        gain, gini = getAuucScore(fullResultFolder, 5, prefileName, postfileName, outcomeName, factor)
        gainScores.append(gain)
        qiniScores.append(gini)
        opiAuc = getAUUCTopGroup(fullResultFolder, 5, prefileName, postfileName, outcomeName, False)
        
        print(method + ":" + str(opiAuc))  
        auucScores.append(opiAuc) 
        newMolde = genQiniDataML(fullResultFolder, 5, prefileName, postfileName, method, outcomeName)
        improvementModels = pd.DataFrame({})
        improvementModels = improvementModels.append(newMolde)
        improvementModels['uplift'] = improvementModels['uplift']* 100
        improvementModels['grUplift'] = improvementModels['grUplift']* 100     
        plotBarImprovementTopV2(improvementModels, [method], allAxis, startCount, perPop, tickLabel, minY, maxY)
        startCount = startCount + 1 



        
def plotAUUCBar(inputName, factors, outcomeName, iaxis, datasetName, gcount, minY=None):


    factors.append('bestFactor')
    
    column_names = ['Dataset', 'CT', 'FT', 'TST', 'TOT', 'CF', 'XL', 'DRL', 'PIR']
    res = pd.DataFrame(columns = column_names)
    
    for factor in factors:  
        
        dictVal = {}
        
        if(inputName == 'adultML'):
            dictVal['Dataset'] = 'ACI'

        if(inputName == 'turnoverML'):
            dictVal['Dataset'] = 'TO'
            
        if(inputName == 'hrML'):
            dictVal['Dataset'] = 'HR'

            
        
        methods = []
        gainScores = []
        qiniScores = []
        auucScores = []
    
        method = 'CT'
        methods.append(method)
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName +'/' + factor
        opiAuc = getAUUCTopGroup(fullResultFolder, 5, prefileName, postfileName, outcomeName, False)
        auucScores.append(opiAuc) 
        dictVal[method] = opiAuc
        print(method + ": " + str(opiAuc))   
        
        method = 'TOT'
        methods.append(method)
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName +'/' + factor
        opiAuc = getAUUCTopGroup(fullResultFolder, 5, prefileName, postfileName, outcomeName, False)
        auucScores.append(opiAuc) 
        print(method + ": " + str(opiAuc))  
        dictVal[method] = opiAuc

        method = 'TST'
        methods.append(method)
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName +'/' + factor
        opiAuc = getAUUCTopGroup(fullResultFolder, 5, prefileName, postfileName, outcomeName, False)
        auucScores.append(opiAuc) 
        print(method + ": " + str(opiAuc)) 
        dictVal[method] = opiAuc


        method = 'FT'
        methods.append(method)
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName +'/' + factor
        opiAuc = getAUUCTopGroup(fullResultFolder, 5, prefileName, postfileName, outcomeName, False)
        auucScores.append(opiAuc) 
        print(method + ": " + str(opiAuc)) 
        dictVal[method] = opiAuc

        
        method = 'CF'
        methods.append(method)
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName +'/' + factor
        opiAuc = getAUUCTopGroup(fullResultFolder, 5, prefileName, postfileName, outcomeName, False)
        auucScores.append(opiAuc) 
        print(method + ": " + str(opiAuc))  
        dictVal[method] = opiAuc
                
        
        method = 'Xlearner'
        methods.append('XL')
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName +'/' + factor
        opiAuc = getAUUCTopGroup(fullResultFolder, 5, prefileName, postfileName, outcomeName, False)
        auucScores.append(opiAuc) 
        print(method + ": " + str(opiAuc))  
        dictVal['XL'] = opiAuc

        method = 'DRL'
        methods.append(method)
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName +'/' + factor
        opiAuc = getAUUCTopGroup(fullResultFolder, 5, prefileName, postfileName, outcomeName, False)
        auucScores.append(opiAuc) 
        print(method + ": " + str(opiAuc))  
        dictVal['DRL'] = opiAuc

        
        method = 'PIR'
        methods.append('PIR')
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName +'/' + factor
        opiAuc = getAUUCTopGroup(fullResultFolder, 5, prefileName, postfileName, outcomeName, False)
        auucScores.append(opiAuc) 
        dictVal['PIR'] = opiAuc
        
        q0 = pd.DataFrame(dictVal, index =[0]) 
        
        res = pd.concat([q0, res]).reset_index(drop = True)
        
        print(method + ": " + str(opiAuc))
        
        grhData = pd.DataFrame({'CIMP': auucScores, 'Method': methods})
            
        gcount += 1
        
        colors = ['lightsteelblue', 'yellowgreen', 'olive', 'steelblue', 'deeppink', 'darkred', 'saddlebrown', 'tab:orange']
        g = sns.barplot(x='Method', y='CIMP', data= grhData, order = ['CT','FT','TST','TOT','CF', 'XL','DRL','PIR'],
        palette = colors,
        ax = iaxis[gcount])
        if(factor == 'bestFactor'):
            g.patch.set_facecolor('#f0e2a8')
        else:
            g.patch.set_facecolor('w')
        g.set_xlabel('')
        if((gcount % 2) == 0):
            g.set_ylabel('AUUC')
        else:
            g.set_ylabel('')
        ## rename to a friendly ones
        if(factor == 'Grey_wage'):
            factor = 'Grey wage'
            
        if(factor == 'bestFactor'):
            factor = 'pIntervention'
         
        if(factor == 'satisfaction_level'):
            factor = 'Satisfaction'
         
        if(factor == 'average_montly_hours'):
            factor = 'Average monthly hours'
            
        g.set_title(factor +' (' + datasetName + ') ', fontsize=15)
        g.tick_params(axis='both', which='major', labelsize=15)
        g.set_xticklabels(g.get_xticklabels(), rotation=45)
        
        g.spines['top'].set_visible(False)
        g.spines['right'].set_visible(False) 
        g.spines['left'].set_color('black')
        g.spines['bottom'].set_color('black')
        g.spines['left'].set_linewidth(1)
        g.spines['bottom'].set_linewidth(1)
        if(minY != None):
            g.set(ylim=(minY, None))
            
    return res
            
        
        
def generateRank(inputName, factors, outcomeName, datasetName):

    ncauses  = factors.copy()
    perPop = [0.25, 0.5,  0.75, 1.0]
    methods = []
    cfactors = []
    kendals = []
    spears = []
    
    column_names = ['Dataset', 'CT', 'TOT', 'TST',  'FT',  'CF', 'XL', 'DRL', 'PIR']
    res = pd.DataFrame(columns = column_names)

    
    for factor in ncauses:
    
        dictVal = {}
        
        if(inputName == 'adultML'):
            dictVal['Dataset'] = 'ACI'

        if(inputName == 'turnoverML'):
            dictVal['Dataset'] = 'TO'
            
        if(inputName == 'hrML'):
            dictVal['Dataset'] = 'HR'

            
        mtag = ''
        
        if(factor == 'bestFactor'):
            mtag = '_pFactor'
            
        method = 'CT'
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName  + '/' +factor
        kendal, spear = getDiversification(fullResultFolder, 5, prefileName, postfileName, outcomeName, perPop)
        kendals.append(kendal)
        spears.append(spear)
        methods.append(method + mtag)
        cfactors.append(factor)
        dictVal[method] = spear

        method = 'FT'
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName  + '/' +factor
        kendal, spear = getDiversification(fullResultFolder, 5, prefileName, postfileName, outcomeName, perPop)
        kendals.append(kendal)
        spears.append(spear)
        methods.append(method + mtag)
        cfactors.append(factor)
        dictVal[method] = spear

        method = 'TST'
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName  + '/' +factor
        kendal, spear = getDiversification(fullResultFolder, 5, prefileName, postfileName, outcomeName, perPop)
        kendals.append(kendal)
        spears.append(spear)
        methods.append(method + mtag)
        cfactors.append(factor)
        dictVal[method] = spear

        method = 'TOT'
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName  + '/' +factor
        kendal, spear = getDiversification(fullResultFolder, 5, prefileName, postfileName, outcomeName, perPop)
        kendals.append(kendal)
        spears.append(spear)
        methods.append(method + mtag)
        cfactors.append(factor)
        dictVal[method] = spear

        
        method = 'CF'
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName  + '/' +factor
        kendal, spear = getDiversification(fullResultFolder, 5, prefileName, postfileName, outcomeName, perPop)
        kendals.append(kendal)
        spears.append(spear)
        methods.append(method + mtag)
        cfactors.append(factor)
        dictVal[method] = spear


        method = 'Xlearner'
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName  + '/' +factor
        kendal, spear = getDiversification(fullResultFolder, 5, prefileName, postfileName, outcomeName, perPop)
        kendals.append(kendal)
        spears.append(spear)
        method = 'XL'
        methods.append(method + mtag)
        cfactors.append(factor)
        dictVal[method] = spear


        method = 'DRL'
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName  + '/' +factor
        kendal, spear = getDiversification(fullResultFolder, 5, prefileName, postfileName, outcomeName, perPop)
        kendals.append(kendal)
        spears.append(spear)
        methods.append(method + mtag)
        cfactors.append(factor)
        dictVal[method] = spear
        
                    
        method = 'PIR'
        prefileName = inputName + '_' + method + '_' 
        postfileName = ''
        fullResultFolder = baseInFolder + '/'+ method +'/' + inputName  + '/' +factor
        kendal, spear = getDiversification(fullResultFolder, 5, prefileName, postfileName, outcomeName, perPop)
        kendals.append(kendal)
        spears.append(spear)
        method = 'PIR'
        methods.append(method + mtag)
        cfactors.append(factor)
        dictVal[method] = spear
        
        q0 = pd.DataFrame(dictVal, index =[0]) 
        
        res = pd.concat([q0, res]).reset_index(drop = True)        
                
    return res


random.seed(20)

if(plotGBar == True):
    print('plotGBarML')
    plt.clf()    
    figure, allAxis = plt.subplots(4, 2, figsize=(8,9),  sharey=False, dpi=300)    
    allAxis = allAxis.flatten()
    gcount = -1

    column_names = ['Dataset', 'CT', 'FT', 'TST', 'TOT', 'CF', 'XL', 'DRL', 'PIR']
    res = pd.DataFrame(columns = column_names)
    
    
    outcomeName = 'time_spend_company'
    inputName = 'hrML'
    factors = []
    datasetName = 'HR'
    minY = 0 #20
    q0 = plotAUUCBar(inputName, factors, outcomeName, allAxis, datasetName, gcount, minY)
    appResults = os.path.join(baseInFolder, 'PerformanceEval')
    res = pd.concat([q0, res]).reset_index(drop = True)    
    

    outcomeName = 'Long_emp_retention'
    inputName = 'turnoverML'
    factors = [ ]
    datasetName = 'Turnover'
    minY = 0# 2
    q0 = plotAUUCBar(inputName, factors, outcomeName, allAxis, datasetName, gcount, minY)
    appResults = os.path.join(baseInFolder, 'PerformanceEval')
    gcount += len(factors)
    res = pd.concat([q0, res]).reset_index(drop = True)

    
    outcomeName = 'Prof'
    inputName = 'adultML'
    factors = []
    datasetName = 'Adult Income'
    minY = 0#6
    q0 = plotAUUCBar(inputName, factors, outcomeName, allAxis, datasetName, gcount, minY)
    appResults = os.path.join(baseInFolder, 'PerformanceEval')
    gcount += len(factors)
    res = pd.concat([q0, res]).reset_index(drop = True)
    
    

    
    figure.tight_layout(pad=1.0)
    figure.patch.set_facecolor('w')

    res.to_csv(appResults + '/'  + 'AUUC.csv', index=False)  
    
    
if(Rank):

    
    outcomeName = 'Prof'
    inputName = 'adultML'
    factors = ['bestFactor']
    datasetName = 'Adult Income'
    res1 = generateRank(inputName, factors, outcomeName, datasetName)
    
    
    
    outcomeName = 'Long_emp_retention'
    inputName = 'turnoverML'
    factors = ['bestFactor']
    datasetName = 'Turnover'
    res2 = generateRank(inputName, factors, outcomeName, datasetName)

    
    outcomeName = 'time_spend_company'
    inputName = 'hrML'
    factors = ['bestFactor']
    datasetName = 'HR'
    res3 = generateRank(inputName, factors, outcomeName, datasetName)
    
    
    results = pd.concat([res1, res2, res3 ]).reset_index(drop = True)
    
    results.loc['Average'] = results.mean()
    
    results = results[["Dataset", "CT", "TOT", "TST", 
                       "FT", "CF", "XL", "DRL", "PIR"]]
    appResults = os.path.join(baseInFolder, 'PerformanceEval')
    results.to_csv(appResults + '/'  + 'Spearman.csv', index=False) 