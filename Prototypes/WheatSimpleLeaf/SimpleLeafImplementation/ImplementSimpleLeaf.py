# ---
# jupyter:
#   jupytext:
#     formats: ipynb,py:light
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.15.0
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

import datetime
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import xml.etree.ElementTree as ET
import xmltodict, json
import ast
import numbers
import shlex # package to construct the git command to subprocess format
import subprocess 
import os
# %matplotlib inline

MasterFile = 'C:\GitHubRepos\ApsimX\Tests\Validation\Wheat\Wheat.apsimx'
PrototypeFile = 'C:\GitHubRepos\ApsimX\Prototypes\WheatSimpleLeaf\WheatPrototype.apsimx'
ImplementedFile = 'C:\GitHubRepos\ApsimX\Prototypes\WheatSimpleLeaf\WheatSL.apsimx'
VariableRenamesFile, VRsheetname = 'C:\GitHubRepos\ApsimX\Prototypes\WheatSimpleLeaf\SimpleLeafImplementation\VariableRenames.xlsx', 'SimpleLeafRenames'


# +
def findModel(Parent,modelPath):
    PathElements = modelPath.split('.')
    return findModelFromElements(Parent,PathElements)

def findModelFromElements(Parent,PathElements):
    for pe in PathElements:
        Parent = findNextChild(Parent,pe)
    return Parent

def findNextChild(Parent,ChildName):
    if len(Parent['Children']) >0:
        for child in range(len(Parent['Children'])):
            if Parent['Children'][child]['Name'] == ChildName:
                return Parent['Children'][child]
    else:
        return Parent[ChildName]

def replaceModel(Parent,modelPath,New):
    PathElements = modelPath.split('.')
    try:
        test = findModelFromElements(Parent,PathElements[:-1])[PathElements[-1]]
        findModelFromElements(Parent,PathElements[:-1])[PathElements[-1]] = New
    except:
        try:
            pos = 0
            for kid in findModelFromElements(Parent,PathElements[:-1])['Children']:
                if kid['Name'] == PathElements[-1]:
                    findModelFromElements(Parent,PathElements[:-1])['Children'][pos] = New
                    break
                pos +=1
        except:   
            print('Could not find parent node of model to over write for ' + modelPath)
            raise
            
def addModel(Parent,modelPath,New):
    PathElements = modelPath.split('.')
    Parent = findModelFromElements(Parent,PathElements)
    if Parent == None:
        print('Could not find parent model ' + modelPath + ' to Add new model to.  Dont include the name of the new models name in the path')
    if isinstance(New,dict):
        NewDict = New
    else:
        NewDict = json.loads(New)
    Parent['Children'].append(NewDict)


# +
# command= "git --git-dir=C:/GitHubRepos/ApsimX/.git --work-tree=C:/GitHubRepos/ApsimX checkout upstream/master C:/GitHubRepos/ApsimX/Tests/Validation/Wheat/Wheat.apsimx" 
# #command= "git --git-dir=C:/GitHubRepos/ApsimX/.git --work-tree=C:/GitHubRepos/ApsimX checkout C:/GitHubRepos/ApsimX/Models/Resources/Wheat.json" 
# comm=shlex.split(command) # This will convert the command into list format
# subprocess.run(comm, shell=True) # Run the git command
# -

## Read wheat test file into json object
with open(MasterFile,'r') as MasterJSON:
    Master = json.load(MasterJSON)
    MasterJSON.close()
    ## read prototype wheat file into json object
with open(PrototypeFile,'r') as PrototypeJSON:
    Prototype = json.load(PrototypeJSON)
    PrototypeJSON.close()

#Copy prototype wheat model out of replacements and put it in replacements in test file
NewModel =  findModel(Prototype,'Replacements.Wheat')
addModel(Master,'Replacements',NewModel)
NewModel =  findModel(Prototype,'Replacements.MaxLeafSize')
replaceModel(Master,'Replacements.MaxLeafSize',NewModel)

os.remove(ImplementedFile)
with open(ImplementedFile,'w') as ImplementedJSON:
    json.dump(Master ,ImplementedJSON,indent=2)

# +
replacements = pd.read_excel(VariableRenamesFile,index_col=0,sheet_name = VRsheetname).to_dict()['SimpleLeaf']
with open(ImplementedFile, 'r') as file: 
    data = file.read() 
    for v in replacements.keys():
        data = data.replace(v, replacements[v])
        w = v.replace('Wheat','[Wheat]')
        rw = replacements[v].replace('Wheat','[Wheat]')
        data = data.replace(w, rw)
        
# Opening our text file in write only 
# mode to write the replaced content 
with open(ImplementedFile, 'w') as file: 
  
    # Writing the replaced data in our 
    # text file 
    file.write(data) 

# +
VariableRenames = pd.read_excel(VariableRenamesFile,index_col=0, sheet_name='SimpleLeafRenames').to_dict()['SimpleLeaf']
MaxLeafSizeRenames = pd.read_excel(VariableRenamesFile,index_col=0, sheet_name='MaxLeafSizeRenames').to_dict()['SimpleLeaf']

from pathlib import Path
fileLoc = 'C:\GitHubRepos\ApsimX\Tests\Validation\Wheat\data'
Allcols = []
pathlist = Path(fileLoc).glob('**/*.xlsx')
for path in pathlist:
    # because path is object not string
    obsDat = pd.read_excel(path, engine='openpyxl',sheet_name='Observed')
    newCols = []
    replace = False
    for c in obsDat.columns:
        if c in VariableRenames.keys():
            newCols.append(c.replace(c,VariableRenames[c]))
            replace = True
        else:
            newCols.append(c)
    if replace == True:
        obsDat.columns = newCols
        with pd.ExcelWriter(path, engine='openpyxl', mode='a',if_sheet_exists='replace') as writer: 
            workbook = writer.book
            obsDat.to_excel(writer,index=False,sheet_name='Observed')
    
    obsDat = pd.read_excel(path, engine='openpyxl',sheet_name='MaxLeafSize')
    newCols = []
    replace = False
    for c in obsDat.columns:
        if c in MaxLeafSizeRenames.keys():
            newCols.append(c.replace(c,MaxLeafSizeRenames[c]))
            replace = True
        else:
            newCols.append(c)
    if replace == True:
        obsDat.columns = newCols
        with pd.ExcelWriter(path, engine='openpyxl', mode='a',if_sheet_exists='replace') as writer: 
            workbook = writer.book
            obsDat.to_excel(writer,index=False,sheet_name='MaxLeafSize')
