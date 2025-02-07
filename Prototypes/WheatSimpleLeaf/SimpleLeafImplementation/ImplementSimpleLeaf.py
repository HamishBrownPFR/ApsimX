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


# +
# command= "git --git-dir=C:/GitHubRepos/ApsimX/.git --work-tree=C:/GitHubRepos/ApsimX checkout upstream/master C:/GitHubRepos/ApsimX/Tests/Validation/Wheat/Wheat.apsimx" 
# #command= "git --git-dir=C:/GitHubRepos/ApsimX/.git --work-tree=C:/GitHubRepos/ApsimX checkout C:/GitHubRepos/ApsimX/Models/Resources/Wheat.json" 
# comm=shlex.split(command) # This will convert the command into list format
# subprocess.run(comm, shell=True) # Run the git command
# -

## Read wheat test file into json object
with open('C:\GitHubRepos\ApsimX\Tests\Validation\Wheat\Wheat.apsimx','r') as WheatTestsJSON:
    WheatTests = json.load(WheatTestsJSON)
    WheatTestsJSON.close()
    ## read prototype wheat file into json object
with open('C:\GitHubRepos\ApsimX\Prototypes\WheatSimpleLeaf\WheatPrototype.apsimx','r') as WheatPrototypeJSON:
    WheatPrototype = json.load(WheatPrototypeJSON)
    WheatPrototypeJSON.close()

#Copy prototype wheat model out of replacements and put it in replacements in test file
Replacements =  findModel(WheatPrototype,['Replacements'])
replaceModel(WheatTests,'Replacements',Replacements)

with open('C:\GitHubRepos\ApsimX\Prototypes\WheatSimpleLeaf\WheatSL.apsimx','w') as WheatTestsJSON:
    json.dump(WheatTests ,WheatTestsJSON,indent=2)

# +
replacements = pd.read_excel('C:\GitHubRepos\ApsimX\Prototypes\WheatSimpleLeaf\SimpleLeafImplementation\VariableRenames.xlsx',index_col=0).to_dict()['SimpleLeaf']
with open(r'C:\GitHubRepos\ApsimX\Prototypes\WheatSimpleLeaf\WheatSL.apsimx', 'r') as file: 
    data = file.read() 
    for v in replacements.keys():
        data = data.replace(v, replacements[v])
        w = v.replace('Wheat','[Wheat]')
        rw = replacements[v].replace('Wheat','[Wheat]')
        data = data.replace(w, rw)
        
# Opening our text file in write only 
# mode to write the replaced content 
with open(r'C:\GitHubRepos\ApsimX\Prototypes\WheatSimpleLeaf\WheatSL.apsimx', 'w') as file: 
  
    # Writing the replaced data in our 
    # text file 
    file.write(data) 

# +
replacements = pd.read_excel('C:\GitHubRepos\ApsimX\Prototypes\WheatSimpleLeaf\SimpleLeafImplementation\VariableRenames.xlsx',index_col=0, sheet_name='SimpleLeafRenames').to_dict()['SimpleLeaf']
#replacements = pd.read_excel('C:\GitHubRepos\ApsimX\Prototypes\WheatSimpleLeaf\SimpleLeafImplementation\VariableRenames.xlsx',index_col=0,sheet_name='Existing Model renames').to_dict()['SimpleLeaf']

from pathlib import Path
fileLoc = 'C:\GitHubRepos\ApsimX\Tests\Validation\Wheat\data'
Allcols = []
pathlist = Path(fileLoc).glob('**/*.xlsx')
for path in pathlist:
    # because path is object not string
    obsDat = pd.read_excel(path, engine='openpyxl',sheet_name='Observed')
    newCols = []
    for c in obsDat.columns:
        Allcols.append(c)
        if c == 'Wheat.Leaf.Deat.N':
            print(path)
        if c in replacements.keys():
            newCols.append(c.replace(c,replacements[c]))
        else:
            newCols.append(c)
    obsDat.columns = newCols
    with pd.ExcelWriter(path, engine='openpyxl', mode='a',if_sheet_exists='replace') as writer: 
        workbook = writer.book
        obsDat.to_excel(writer,index=False,sheet_name='Observed')

