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
SensibilityReport
            

# -

## Read wheat test file into json object
with open('C:\GitHubRepos\ApsimX\Tests\Validation\Wheat\Wheat.apsimx','r') as WheatTestsJSON:
    WheatTests = json.load(WheatTestsJSON)
    WheatTestsJSON.close()
    ## read prototype wheat file into json object


def StripReports(Parent):
    for model in Parent['Children']:
        #print(model["Name"])
        if model["$type"] == "Models.Report, Models":
            if model["Name"] in ["HarvestReport","DailyReport","MaxLeafSizeReport","SowingReport","NDVIDailyReport"]:
                model["VariableNames"] = None
                model["EventNames"]= None
                #print(model["Name"])
        StripReports(model) 


def StripManagers(Parent):
    for model in Parent['Children']:
        #print(model["Name"])
        if model["$type"] == "Models.Manager, Models":
            if model["Name"] in ["MaxLeafSize","Harvesting"]:
                model["CodeArray"] = []
                model["Parameters"]= []
                #print(model["Name"])
        StripManagers(model) 


StripReports(WheatTests)

StripManagers(WheatTests)

with open('C:\GitHubRepos\ApsimX\Tests\Validation\Wheat\Wheat.apsimx','w') as WheatTestsJSON:
    json.dump(WheatTests ,WheatTestsJSON,indent=2)
