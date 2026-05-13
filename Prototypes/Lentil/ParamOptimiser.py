# ---
# jupyter:
#   jupytext:
#     formats: ipynb,py:light
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.18.1
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# +
import datetime as dt
import pandas as pd
import numpy as np 
import matplotlib.pyplot as plt
import APSIMGraphHelpers as AGH
import GraphHelpers as GH
from scipy import stats
import statsmodels.api as sm
from statsmodels.formula.api import ols
import matplotlib.dates as mdates
import MathsUtilities as MUte
import shlex # package to construct the git command to subprocess format
import subprocess 
#import ProcessWheatFiles as pwf
import xmltodict, json
import sqlite3
import scipy.optimize 
from skopt import gp_minimize
from skopt.callbacks import CheckpointSaver
from skopt import load
from skopt.plots import plot_convergence
import matplotlib.gridspec as gridspec

import winsound
frequency = 2500  # Set Frequency To 2500 Hertz
duration = 1000  # Set Duration To 1000 ms == 1 second
# %matplotlib inline

# +
WheatFilePath = r"C:/GitHubRepos/ApsimX/Tests/Validation/Wheat/Wheat.apsimx"
DBFilePath = r"C:/GitHubRepos/ApsimX/Tests/Validation/Wheat/Wheat.db"

BaseLine = pd.read_pickle('./BaselineHarvestObsPred.pkl')

DailyBaseLine = pd.read_pickle('./BaselineDailyObsPred.pkl')

BZOptimised = pd.read_pickle('./CAMP_optimisedBZHarvestObsPred.pkl')

paramNames = ['[Phenology].CAMP.FLNparams.MinLN', 
              '[Phenology].CAMP.FLNparams.PpLN', 
              '[Phenology].CAMP.FLNparams.VrnLN', 
              '[Phenology].CAMP.FLNparams.VxPLN',
              '[Phenology].HeadEmergenceLongDayBase.FixedValue',
              '[Phenology].HeadEmergencePpSensitivity.FixedValue']

FittingVariables = ['Wheat.Phenology.FinalLeafNumber','Wheat.Phenology.FlagLeafDAS',
                    'Wheat.Phenology.HeadingDAS','Wheat.Phenology.FloweringDAS']        

BlankManager = {'$type': 'Models.Manager, Models',
            'Code': '',
            'Parameters': None,
            'Name': 'SetCropParameters',
            'IncludeInDocumentation': False,
            'Enabled': True,
            'ReadOnly': False}

SetCropParams = {
          "$type": "Models.Manager, Models",
          "Code": "using Models.Core;\r\nusing System;\r\nnamespace Models\r\n{\r\n\t[Serializable]\r\n    public class Script : Model\r\n    {\r\n        [Link] Zone zone;\r\n        [EventSubscribe(\"PlantSowing\")]\r\n        private void OnPlantSowing(object sender, EventArgs e)\r\n        {\r\n            object PpFac12 = 0.8;\r\n            zone.Set(\"Wheat.Phenology.CAMP.PpResponse.XYPairs.Y[3]\", PpFac12);  \r\n            object DeVernFac = -.3;\r\n            zone.Set(\"Wheat.Phenology.CAMP.DailyColdVrn1.Response.DeVernalisationRate.FixedValue\", DeVernFac);  \r\n        }\r\n    }\r\n}\r\n                \r\n",
          "Parameters": [],
          "Name": "SetCropParameters",
          "IncludeInDocumentation": False,
          "Enabled": True,
          "ReadOnly": False}

def AppendModeltoModelofTypeAndDeleteOldIfPresent(Parent,TypeToAppendTo,ModelToAppend):
    try:
        for child in Parent['Children']:
            if child['$type'] == TypeToAppendTo:
                pos = 0
                for g in child['Children']:
                    if g['Name'] == ModelToAppend['Name']:
                        del child['Children'][pos]
                        #print('Model ' + ModelToAppend['Name'] + ' found and deleted')
                    pos+=1
                child['Children'].append(ModelToAppend)
                return True
            else:
                Parent = AppendModeltoModelofTypeAndDeleteOldIfPresent(child,TypeToAppendTo,ModelToAppend)
        return Parent
    except:
        return Parent
    
def AppendModeltoModelofType(Parent,TypeToAppendTo,NameToAppendTo,ModelToAppend):
    try:
        for child in Parent['Children']:
            if (child['$type'] == TypeToAppendTo) and (child['Name'] == NameToAppendTo):
                child['Children'].append(ModelToAppend)
                return True
            else:
                Parent = AppendModeltoModelofType(child,TypeToAppendTo,NameToAppendTo,ModelToAppend)
        return Parent
    except:
        return Parent
    
def findNextChild(Parent,ChildName):
    if len(Parent['Children']) >0:
        for child in range(len(Parent['Children'])):
            if Parent['Children'][child]['Name'] == ChildName:
                return Parent['Children'][child]
    else:
        return Parent[ChildName]

def findModel(Parent,PathElements):
    for pe in PathElements:
        Parent = findNextChild(Parent,pe)
    return Parent    

def StopReporting(WheatApsimx,modelPath):
    PathElements = modelPath.split('.')
    report = findModel(WheatApsimx,PathElements)
    report["EventNames"] = []

def removeModel(Parent,modelPath):
    PathElements = modelPath.split('.')
    Parent = findModel(Parent,PathElements[:-1])
    pos = 0
    found = False
    for c in Parent['Children']:
        if c['Name'] == PathElements[-1]:
            del Parent['Children'][pos]
            found = True
            break
        pos += 1
    if found == False:
        print('Failed to find ' + PathElements[-1] + ' to delete')

def ApplyParamReplacementSet(paramValues,paramNames,filePath):
    with open(filePath,'r') as WheatApsimxJSON:
        WheatApsimx = json.load(WheatApsimxJSON)
        WheatApsimxJSON.close()
    ## Remove old prameterSet manager in replacements
    removeModel(WheatApsimx,'Replacements.SetCropParams')

    ## Add crop coefficient overwrite into replacements
    codeString = "using Models.Core;\r\nusing System;\r\nnamespace Models\r\n{\r\n\t[Serializable]\r\n    public class Script : Model\r\n    {\r\n        [Link] Zone zone;\r\n        [EventSubscribe(\"Sowing\")]\r\n        private void OnSowing(object sender, EventArgs e)\r\n     {\r\n        object Pval = 0; \r\n    "
    for p in range(len(paramValues)):
        codeString +=  "         Pval ="
        codeString += str(paramValues[p])
        codeString += ';\r\n         zone.Set(\"'
        codeString += paramNames[p]
        codeString += '\", Pval);  \r\n'
        
    codeString += '\r\n}\r\n}\r\n  }'

    SetCropParams["Code"] = codeString

    AppendModeltoModelofType(WheatApsimx,"Models.Core.Folder, Models","Replacements",SetCropParams)

    with open(filePath,'w') as WheatApsimxJSON:
        json.dump(WheatApsimx,WheatApsimxJSON,indent=2)
        
def makeLongString(SimulationSet):
    longString =  '/SimulationNameRegexPattern:"'
    longString =  longString + '(' + SimulationSet[0]  + ')|' # Add first on on twice as apsim doesn't run the first in the list
    for sim in SimulationSet[:]:
        longString = longString + '(' + sim + ')|'
    longString = longString + '(' + SimulationSet[-1] + ')|' ## Add Last on on twice as apsim doesnt run the last in the list
    longString = longString + '(' + SimulationSet[-1] + ')"'
    return longString

def CalcScaledValue(Value,RMax,RMin):
    return (Value - RMin)/(RMax-RMin)
# +
def Preparefile(filePath,freq):
    ## revert .apximx file to master
    # !del C:\GitHubRepos\ApsimX\Tests\Validation\Wheat\Wheat.db
    #command= "git --git-dir=C:/GitHubRepos/ApsimX/.git --work-tree=C:/GitHubRepos/ApsimX checkout " + filePath 
    #comm=shlex.split(command) # This will convert the command into list format
    #subprocess.run(comm, shell=True) 
    ## Add blank manager into each simulation
    with open(filePath,'r') as WheatApsimxJSON:
        WheatApsimx = json.load(WheatApsimxJSON)
    WheatApsimxJSON.close()
    if freq == 'Harvest':
        #Stop Daily reporting
        StopReporting(WheatApsimx,'Replacements.DailyReport')
        removeModel(WheatApsimx,'DataStore.DailyObsPred')
    else:
        if freq != 'Daily':
            print('Only works with Daily or Harvest frequencies')
    AppendModeltoModelofTypeAndDeleteOldIfPresent(WheatApsimx,'Models.Core.Zone, Models',BlankManager)
    with open(filePath,'w') as WheatApsimxJSON:
        json.dump(WheatApsimx,WheatApsimxJSON,indent=2)
    print('File Prep Complete')
    
def runModelItter(paramNames,paramValues,FittingVariables,Cultivar,DataTable,freq,WheatFilePath,ver):
    paramNames = paramNames + ['[Phenology].CAMP.ColdVrnResponse.Response.DeVernalisationRate.FixedValue']
    paramValues = paramValues + [-5.0]
    ApplyParamReplacementSet(paramValues,paramNames,WheatFilePath)
    OptimisationVariables = ['Observed.'+x for x in FittingVariables]
    DataPresent = pd.Series(index = DataTable.index,dtype=bool)
    DataPresent = False
    for v in OptimisationVariables:
        DataPresent = (DataPresent | ~np.isnan(pd.to_numeric(DataTable.loc[:,v])))
    SetFilter = (DataTable.Cultivar==Cultivar) & DataPresent
    SimulationSet = DataTable.loc[SetFilter,'SimulationName'].values
    SimSet = makeLongString(SimulationSet)
    #print(SimSet)
    start = dt.datetime.now()
    subprocess.run(['C:/GitHubRepos/ApsimX/bin/Debug/netcoreapp3.1/Models.exe',
                    WheatFilePath,
                    SimSet], stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    endrun = dt.datetime.now()
    runtime = (endrun-start).seconds
    con = sqlite3.connect(DBFilePath)
    if freq == 'Harvest':
        ObsPred = pd.read_sql("Select * from HarvestObsPred",con)
    else:
        if freq == 'Daily':
            ObsPred = pd.read_sql("Select * from DailyObsPred",con)
        else:
            print('Only works with Daily or Harvest DataTables')
    con.close()
    n = len(ObsPred.loc[:,'Predicted.Simulation.Name'].drop_duplicates())        
    
    ScObsPre = pd.DataFrame(columns = ['ScObs','ScPred','Var','Predicted.Simulation.Name','Predicted.Experiment','Predicted.Wheat.SowingDate'])
    indloc = 0
    for var in FittingVariables:
        DataPairs = ObsPred.reindex(['Observed.'+var,'Predicted.'+var,'Predicted.Simulation.Name','Predicted.Experiment','Predicted.Wheat.SowingDate'],axis=1).dropna()
        for v in ['Observed.'+var,'Predicted.'+var]:
            DataPairs.loc[:,v] = pd.to_numeric(DataPairs.loc[:,v])
        VarMax = max(DataPairs.loc[:,'Observed.'+var].max(),DataPairs.loc[:,'Predicted.'+var].max())
        VarMin = min(DataPairs.loc[:,'Observed.'+var].min(),DataPairs.loc[:,'Predicted.'+var].min())
        for x in DataPairs.index:
            ScObsPre.loc[indloc,'ScObs'] = CalcScaledValue(DataPairs.loc[x,'Observed.'+var],VarMax,VarMin)
            ScObsPre.loc[indloc,'ScPred'] = CalcScaledValue(DataPairs.loc[x,'Predicted.'+var],VarMax,VarMin)
            ScObsPre.loc[indloc,'Var'] = var
            globals()['IttersObsPred'].loc[(Cultivar,globals()['itter'],indloc),['ScObs','ScPred','Var']] = ScObsPre.loc[indloc,['ScObs','ScPred','Var']]
            globals()['IttersObsPred'].loc[(Cultivar,globals()['itter'],indloc),
                                          ['Predicted.Simulation.Name','Predicted.Experiment','Predicted.Wheat.SowingDate']] = DataPairs.loc[x,['Predicted.Simulation.Name','Predicted.Experiment','Predicted.Wheat.SowingDate']]
            indloc+=1
    RegStats = MUte.MathUtilities.CalcRegressionStats('LN',ScObsPre.loc[:,'ScPred'].values,ScObsPre.loc[:,'ScObs'].values)
    globals()['IttersObsPred'].loc[(Cultivar,globals()['itter']),['NSE','nSims']] = [RegStats.NSE,n]
    globals()['IttersObsPred'].loc[(Cultivar,globals()['itter']),paramNames] = paramValues
    try:
        retVal = max(RegStats.NSE,-2) *-1
        print(str(globals()['itter']) +"  "+ str(paramValues) + " run completed " +str(n) + ' sims in '+ str(runtime) + ' seconds.  NSE = '+str(RegStats.NSE))
    except:
        retVal = 2
        print(str(globals()['itter']) +"  "+ str(paramValues) + " run completed " +str(n) + ' sims in '+ str(runtime) + ' seconds.  NSE = insufficient data')
    globals()['IttersObsPred'].to_pickle("./OptFiles/"+c+"_IttersObsPred"+ver+".pkl")
    globals()['itter'] +=1
    return retVal

def runDevModelItter(paramValues):
    LV = paramValues[0]
    SV = paramValues[0] + paramValues[1]
    SN = paramValues[0] + paramValues[1] + paramValues[2]
    LN = paramValues[0] + paramValues[2] + paramValues[3]
    boundsPass = True
    if (LN > 30):
        boundsPass = False
    if (SV > 25):
        boundsPass = False
    if (SN > 35):
        boundsPass = False
    if (SN < LV):
        boundsPass = False
    if (LN < LV):
        boundsPass = False
    if boundsPass == False:
        print (str(itter) +"  "+ str(paramValues) + " gave out of bounds parameters ")
        retVal = 2.111
        globals()['itter'] +=1
    else:
        retVal = runModelItter(paramNames,paramValues,FittingVariables,c,BaseLine,'Harvest',WheatFilePath,Version)
    return retVal


# -

LaTrobeCampInputs = pd.read_excel('C:\\GitHubRepos\\npi\\Analysis\\Controlled Environment\\CEParamsFLN_Aus.xlsx',sheet_name='ObservedParams',index_col='Cultivar',engine='openpyxl',usecols="A:H")
#LaTrobeCampInputs.columns = ['FLN_LV', 'FLN_LN', 'FLN_SV', 'FLN_SN','VrnTreatTemp','VrnTreatDuration','Expt']
LincolnCampInputs = pd.read_excel('C:\\GitHubRepos\\npi\\Analysis\\Controlled Environment\\CEParamsFLN_NZ96.xlsx',sheet_name='ObservedParams',index_col='Cultivar',engine='openpyxl',usecols="A:H")
#LincolnCampInputs.columns = ['FLN_LV', 'FLN_LN', 'FLN_SV', 'FLN_SN','VrnTreatTemp','VrnTreatDuration','Expt']
CampInputs = pd.concat([LaTrobeCampInputs,LincolnCampInputs])
Aus_FL_ANdata = pd.read_excel('C:\\GitHubRepos\\npi\\Simulation\\ModelFitting\\FinalNPIFitting.xlsx',sheet_name='FL_AN',index_col='Row Labels',engine='openpyxl',usecols="A:C",skiprows=3)
for c in CampInputs.index:
    try:
        CampInputs.loc[c,'[Phenology].HeadEmergenceLongDayBase.FixedValue'] = Aus_FL_ANdata.loc[c,'[Phenology].HeadEmergenceLongDayBase.FixedValue']
    except:
        CampInputs.loc[c,'[Phenology].HeadEmergenceLongDayBase.FixedValue'] =  200
    try:
        CampInputs.loc[c,'[Phenology].HeadEmergencePpSensitivity.FixedValue'] = Aus_FL_ANdata.loc[c,'[Phenology].HeadEmergencePpSensitivity.FixedValue']
    except:
        CampInputs.loc[c,'[Phenology].HeadEmergencePpSensitivity.FixedValue'] = 2
    CampInputs.loc[c,'[Phenology].CAMP.FLNparams.PpLN'] = max(0,CampInputs.loc[c,'[Phenology].CAMP.FLNparams.PpLN'])
    CampInputs.loc[c,'[Phenology].CAMP.FLNparams.VrnLN'] = max(0,CampInputs.loc[c,'[Phenology].CAMP.FLNparams.VrnLN'])


IttersObsPred = pd.DataFrame(columns = ['ScObs','ScPred','Var','Predicted.Simulation.Name','Predicted.Experiment','Predicted.Wheat.SowingDate',
                                                       'NSE','nSims']+paramNames,
                                                        index=pd.MultiIndex.from_arrays([[],[],[]],names=['Cultivar','itter','indloc']))
itter = 1
c='CRW247'
paramValues = [8.0,2,7,0,170,0.0]#[11.361996665701804, 7.722980823327151, 0.3034778864694383, 8.605875140401132, 107, 0.5320729781312123]#[12.749903243908449, 0.40565675723457506, 3.1181189222295616, 3.124163284147423, 58, 6.964840497000481]#[9.032310407820757, 12.0, 5.04128670051436, 2.5544490840721004, 113, 0.9682675298893731] 
# Preparefile(WheatFilePath,'Harvest')
runModelItter(paramNames,paramValues,FittingVariables,c,BaseLine,'Harvest',WheatFilePath,'test')
#Cultivars = [#'Amarok',
 # 'Axe',
 # 'Batavia',
 # 'Battenspring',
 # 'Battenwinter',
 #'Beaufort',
 # 'Bennett',
 # 'Bolac',
 # 'Braewood',
 # 'Calingiri',
 # 'Catalina',
 # 'Claire',
 # 'Condo',
 # 'Crusader',
 # 'Csirow002',
 # 'Csirow003',
 # 'Csirow005',
 # 'Csirow007',
 # 'Csirow011',
 # 'Csirow018',
 # 'Csirow021',
 # 'Csirow023',
 # 'Csirow027',
 # 'Csirow029',
 # 'Csirow073',
 # 'Csirow077',
 # 'Csirow087',
 # 'Csirow102',
 # 'Csirow105',
 # 'Cunningham',
 #'Cutlass',
 #'Derrimut',
# 'Drysdale',
#  'Eaglehawk',
#  'Egret',
#  'Ellison',
# 'Emu_rock',
#  'Forrest',
#  'Gauntlet',
#  'Gregory',
#  'Grenade',
#  'H45',
# 'Hartog',
#  'Hume',
#  'Janz',
#  'Kellalac',
#  'Kittyhawk',
#  'Lancer',
#  'Lang',
 #'Lincoln',
# 'Longsword',
#  'Mace',
#  'Mackellar',
#  'Magenta',
#  'Manning',
# 'Mccubbin',
#  'Merinda',
#  'Mitch',
#  'Otane',
#  'Ouyen',
#  'Peake',
#  'Revenue',
# 'Rongotea',
#  'Rosella',
# 'Scepter',
#  'Scout',
 # 'Scythe',
#  'Spitfire',
#  'Strzelecki',
# 'Sunbee']
#  'Sunbri',
#  'Sunco',
#  'Suneca',
#  'Sunlamb',
#  'Sunstate',
#  'Suntop',
#  'Trojan',
#  'Wakanui',
#  'Wedgetail',
#  'Whistler',
#  'Wills',
#  'Wyalkatchem',
#  'Yecora',
#  'Yitpi',
#  'Young']

Version = '6'
#Kappas=pd.Series(index=[0,1,2],data=[100,10,1])
FailedCultivars = []
rounds = 0
while (rounds<1):
    for c in Cultivars:
        print ('Fitting '+c)
        try:
            checkpoint_saver = CheckpointSaver("./OptFiles/"+c+" FitsCheckpoint"+Version+".pkl", compress=9)
            Preparefile(WheatFilePath,'Harvest')
            RandomCalls = 100
            OptimizerCalls =30
            TotalCalls = RandomCalls + OptimizerCalls
            try:
                x0 = list(CampInputs.loc[c,paramNames].values)
            except:
                x0 = [8.5, 10.2, 7.0, 4.0, 120, 4.5]
            bounds = [(5.0,15.0),
                      (0.0,12.0),
                      (0.0,12.0),
                      (-10.0,10.0),
                      (50,500),
                      (0,8.0)]

            if rounds == 0:
                print ('Creating empty IttersObsPred dataframe')
                globals()['IttersObsPred'] = pd.DataFrame(columns = ['ScObs','ScPred','Var','Predicted.Simulation.Name','Predicted.Experiment','Predicted.Wheat.SowingDate',
                                                       'NSE','nSims']+paramNames,
                                                        index=pd.MultiIndex.from_arrays([[],[],[]],names=['Cultivar','itter','indloc']))
                globals()['itter'] = 0
                ret = gp_minimize(runDevModelItter, bounds, n_calls=TotalCalls,n_initial_points=RandomCalls,
                         initial_point_generator='sobol',callback=[checkpoint_saver],x0=x0)
                currentBest = ret.fun
            else:
                CheckPoint = load("./OptFiles/"+c+" FitsCheckpoint"+Version+".pkl")
                globals()['IttersObsPred'] = pd.read_pickle("./OptFiles/"+c+"_IttersObsPred"+Version+".pkl")
                globals()['itter'] = len(CheckPoint.func_vals)
                x0 = CheckPoint.x_iters
                y0 = CheckPoint.func_vals
                ret = gp_minimize(runDevModelItter, bounds, n_calls=TotalCalls,n_initial_points=RandomCalls,
                         initial_point_generator='sobol',callback=[checkpoint_saver],x0=x0,y0=y0)
                currentBest = ret.fun
            print(c)
            print(str(rounds))
            print(str(currentBest))
        except:
            FailedCultivars.append(c)
    rounds += 1




Cultivars = ['Whistler']

Version = '5'
#Kappas=pd.Series(index=[0,1,2],data=[100,10,1])
FailedCultivars = []
rounds = 0
while (rounds<3):
    for c in Cultivars:
        print ('Fitting '+c)
        try:
            checkpoint_saver = CheckpointSaver("./OptFiles/"+c+" FitsCheckpoint"+Version+".pkl", compress=9)
            Preparefile(WheatFilePath,'Harvest')
            RandomCalls = 50
            OptimizerCalls =15
            TotalCalls = RandomCalls + OptimizerCalls
            try:
                x0 = list(CampInputs.loc[c,paramNames].values)
            except:
                x0 = [8.5, 10.2, 7.0, 4.0, 120, 4.5]
            bounds = [(5.0,15.0),
                      (0.0,12.0),
                      (0.0,12.0),
                      (-10.0,10.0),
                      (50,500),
                      (0,8.0)]

            if rounds == 0:
                print ('Creating empty IttersObsPred dataframe')
                globals()['IttersObsPred'] = pd.DataFrame(columns = ['ScObs','ScPred','Var','Predicted.Simulation.Name','Predicted.Experiment','Predicted.Wheat.SowingDate',
                                                       'NSE','nSims']+paramNames,
                                                        index=pd.MultiIndex.from_arrays([[],[],[]],names=['Cultivar','itter','indloc']))
                globals()['itter'] = 0

                ret = gp_minimize(runDevModelItter, bounds, n_calls=TotalCalls,n_initial_points=RandomCalls,
                         initial_point_generator='sobol',callback=[checkpoint_saver],x0=x0)
                currentBest = ret.fun
            else:
                CheckPoint = load("./OptFiles/"+c+" FitsCheckpoint"+Version+".pkl")
                globals()['IttersObsPred'] = pd.read_pickle("./OptFiles/"+c+"_IttersObsPred"+Version+".pkl")
                globals()['itter'] = len(CheckPoint.func_vals)
                x0 = CheckPoint.x_iters
                y0 = CheckPoint.func_vals
                ret = gp_minimize(runDevModelItter, bounds, n_calls=TotalCalls,n_initial_points=RandomCalls,
                         initial_point_generator='sobol',callback=[checkpoint_saver],x0=x0,y0=y0)
                currentBest = ret.fun
            print(c)
            print(str(rounds))
            print(str(currentBest))
        except:
            FailedCultivars.append(c)
    rounds += 1


def PlotObsPre(obsPreSet,c,ret):
    graph = plt.figure(figsize=(10,20))
    gs = gridspec.GridSpec(6, 6)
    ax = graph.add_subplot(gs[0, 0:2])
    colors = pd.Series(index = ['Wheat.Phenology.FinalLeafNumber','Wheat.Phenology.FlagLeafDAS',
                        'Wheat.Phenology.HeadingDAS','Wheat.Phenology.FloweringDAS'],
                       data = ['r','b','g','k'])
    markers = ['o','s','^','v','1','2','3','4','+','x','d']

    for var in obsPreSet.loc[:,'Var'].drop_duplicates():
        epos = 0
        lablab = var.replace('Wheat.Phenology.','')
        for expt in obsPreSet.loc[:,'Predicted.Experiment'].drop_duplicates():
            filt = ((obsPreSet.loc[:,'Var'] == var) & (obsPreSet.loc[:,'Predicted.Experiment'] == expt))
            plt.plot(obsPreSet.loc[filt,'ScObs'],obsPreSet.loc[filt,'ScPred'],
                     markers[epos],color=colors[var],label=lablab)
            
            epos+=1
            if epos == 11:
                epos= 0
            lablab = None
    plt.plot([0,1],[0,1],'-',color='k')
    plt.ylabel('Scalled Predictions')
    plt.xlabel('Scalled Observations')
    plt.legend(bbox_to_anchor=(1.1, 1.05))
    RegStats = MUte.MathUtilities.CalcRegressionStats('LN',obsPreSet.loc[:,'ScPred'].values,obsPreSet.loc[:,'ScObs'].values)
    plt.text(0.97,0.03,'NSE = '+str(RegStats.NSE)[:4],transform=ax.transAxes,horizontalalignment='right')
    plt.text(0.03,0.97,c,horizontalalignment='left')
    
    ax = graph.add_subplot(gs[0, 3:6])
    plot_convergence(ret);
    plt.ylim(-1,0)
    #plt.plot([20,20,35,35,55,55,70,70,90,90,105,105,125,125,140,140,155,155,175,175,190,190,210,210,225,225]
    
    ShortParams = pd.Series(index=paramNames,data=['MinLN','PpLN','VrnLN','VxPLN','LDB','HPPS'])
    pos = 0
    for p in paramNames:
        ax = graph.add_subplot(gs[1,pos])
        plt.plot(ParamCombs.loc[:,p],-ParamCombs.loc[:,'NSE'],'o',color='k')
        bestFit = ParamCombs.loc[:,'NSE'].idxmin()
        plt.plot(ParamCombs.loc[bestFit,p],-ParamCombs.loc[bestFit,'NSE'],'o',color='cyan',ms=8,mec='k',mew=2)
        plt.plot(ret.x_iters[0][pos],-ret.func_vals[0],'o',color='orange',ms=8,mec='k',mew=2)
        if pos == 0:
            plt.ylabel('NSE')
        else:
            ax.axes.yaxis.set_visible(False)
        pos+=1
        plt.xlabel(ShortParams[p])
        plt.ylim(0,1)
    plt.tight_layout()


c = 'Bennett'
IttersObsPred = pd.read_pickle("./OptFiles/"+c+"_IttersObsPred"+Version+".pkl")
ret = load("./OptFiles/"+c+" FitsCheckpoint"+Version+".pkl")
ParamCombs = pd.DataFrame(ret.x_iters,columns = paramNames)
ParamCombs.loc[:,'NSE'] = ret.func_vals
bestFitItter = ParamCombs.loc[:,'NSE'].idxmin()
bestFitObsPred = IttersObsPred.loc[(c,bestFitItter),:]
PlotObsPre(bestFitObsPred,c,ret)


from skopt.plots import plot_objective
plot_objective(ret)#,minimum='expected_minimum')
