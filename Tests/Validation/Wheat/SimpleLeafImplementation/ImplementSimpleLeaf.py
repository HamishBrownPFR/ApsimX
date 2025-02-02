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
# %matplotlib inline

# +
def findModel(Parent,PathElements):
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
    
def addModel(Parent,modelPath,New):
    if modelPath != '':
        PathElements = modelPath.split('.')
        Parent = findModel(Parent,PathElements)
    if Parent == None:
        print('Could not find parent model ' + modelPath + ' to Add new model to.  Dont include the name of the new models name in the path')
    if isinstance(New,dict):
        NewDict = New
    else:
        NewDict = json.loads(New)
    Parent['Children'].append(NewDict)
    
def replaceModel(Parent,modelPath,New):
    PathElements = modelPath.split('.')
    try:
        test = findModel(Parent,PathElements[:-1])[PathElements[-1]]
        findModel(Parent,PathElements[:-1])[PathElements[-1]] = New
    except:
        try:
            pos = 0
            for kid in findModel(Parent,PathElements[:-1])['Children']:
                if kid['Name'] == PathElements[-1]:
                    findModel(Parent,PathElements[:-1])['Children'][pos] = New
                    break
                pos +=1
        except:   
            print('Could not find parent node of model to over write for ' + modelPath)
            raise


# -

command= "git --git-dir=C:/GitHubRepos/ApsimX/.git --work-tree=C:/GitHubRepos/ApsimX checkout upstream/master C:/GitHubRepos/ApsimX/Tests/Validation/Wheat/Wheat.apsimx" 
#command= "git --git-dir=C:/GitHubRepos/ApsimX/.git --work-tree=C:/GitHubRepos/ApsimX checkout C:/GitHubRepos/ApsimX/Models/Resources/Wheat.json" 
comm=shlex.split(command) # This will convert the command into list format
subprocess.run(comm, shell=True) # Run the git command

## Read wheat test file into json object
with open('C:\GitHubRepos\ApsimX\Tests\Validation\Wheat\Wheat.apsimx','r') as WheatTestsJSON:
    WheatTests = json.load(WheatTestsJSON)
    WheatTestsJSON.close()
    ## read prototype wheat file into json object
with open('C:\GitHubRepos\ApsimX\Prototypes\WheatSimpleLeaf\WheatFewer.apsimx','r') as WheatPrototypeJSON:
    WheatPrototype = json.load(WheatPrototypeJSON)
    WheatPrototypeJSON.close()

#Copy prototype wheat model out of replacements and put it in replacements in test file
SLWheat =  findModel(WheatPrototype,['Replacements','Wheat'])
addModel(WheatTests,'Replacements',SLWheat)

#Copy prototype wheat model out of replacements and put it in replacements in test file
Replacements =  findModel(WheatPrototype,['Replacements'])
replaceModel(WheatTests,'Replacements',Replacements)

with open('C:\GitHubRepos\ApsimX\Tests\Validation\Wheat\Wheat.apsimx','w') as WheatTestsJSON:
    json.dump(WheatTests ,WheatTestsJSON,indent=2)
