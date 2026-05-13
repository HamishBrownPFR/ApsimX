# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.18.1
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# %% [markdown]
# # Read libraries

# %%
import datetime as dt
import pandas as pd
import numpy as np 
import subprocess 
import os
from pathlib import Path
import json

# %matplotlib inline

# %%
root = "C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil\\NaPA\\New\\Database\\"

# %% [markdown]
# # Read in NaPA_Design.xlsx

# %%
designFP = os.path.join(root,"NaPA_Design.xlsx")
NaPA_Design = pd.read_excel(designFP)

# %%
LentilFilter = (NaPA_Design.loc[:,"Experiments::CropType"]=='Lentil')|(NaPA_Design.loc[:,"MixedCropType"]=='Lentil')

# %%
NaPALentil_Design = NaPA_Design.loc[LentilFilter,:]

# %%
Experiments = NaPALentil_Design.loc[:,"Experiments::SiteKey"].drop_duplicates().tolist()

# %%
NaPALentil_Design.set_index("Experiments::SiteKey",inplace=True)

# %% [markdown]
# # Read in NaPA_Experiment.xlsx

# %%
experimentFP = os.path.join(root,"NaPA_Experiment.xlsx")
NaPA_Experiment = pd.read_excel(experimentFP)

# %%
NaPA_Experiment.set_index("SiteKey",inplace=True)

# %%
ExptInfo = pd.DataFrame(index = Experiments,columns=['Varieties'])

# %%
for e in Experiments:
    ExptInfo.at[e,"Varieties"] = NaPALentil_Design.loc[e,'Variety'].drop_duplicates().to_list()
    ExptInfo.at[e,"SoilName"] = f"{NaPA_Experiment.loc[e,'ExpNo']}_{NaPA_Experiment.loc[e,'SiteName']}"
    
ExptInfo.at['2019_NSW_Greenethorpe_Mixed_Detailed',"SoilName"] = "2022010_Greenethorpe"
ExptInfo.at['2024_NSW_Greenethorpe_Mixed_NFix',"SoilName"] = "2022010_Greenethorpe"
ExptInfo.at['2022_NSW_Methul_Lentil_Satellite',"SoilName"] = "Methul"
ExptInfo.at['2024_Vic_Walpeup_Lentil_Satellite',"SoilName"] = "Walpeup"

# %% [markdown]
# # Read in Management data

# %%
managementFP = os.path.join(root,"NaPA_Management.xlsx")
NaPA_Management = pd.read_excel(managementFP)

# %%
NaPA_Management.set_index("Experiments::SiteKey",inplace=True)

# %% [markdown]
# # read in Irrigation 

# %%
irrigationFP = os.path.join(root,"NaPA_Irrigation.xlsx")
NaPA_Irrigation = pd.read_excel(irrigationFP)
NaPA_Irrigation.set_index("Experiments::SiteKey",inplace=True)

# %%
ExptInfo.loc[:,'IrrigInfo']={}
for e in Experiments:
    irrList = []
    irrigs = {}
    if e in NaPA_Irrigation.index.drop_duplicates():
        trt_irrigs = NaPA_Irrigation.loc[e,"Design::WaterTrt"].drop_duplicates().to_list()
        for i in trt_irrigs:
            irrigTreatFilter = NaPA_Irrigation.loc[:,"Design::WaterTrt"]==i
            irrigData = NaPA_Irrigation.loc[irrigTreatFilter,:].loc[e,["Irrig1_yyyy_mm_dd","Irrig1Amount_mm"]].dropna().drop_duplicates()
            irrigDict = dict(zip(irrigData.Irrig1_yyyy_mm_dd,irrigData.Irrig1Amount_mm))
            formatted_irrigDict = {k.strftime("%d-%m-%Y"): v for k, v in irrigDict.items()}
            irrigs[i] = formatted_irrigDict
    else:
        irrigs["RainFed"] =  {}
    irrList.append(irrigs)
    ExptInfo.at[e,'IrrigInfo'] = irrList[0]

# %%
ExptInfo


# %% [markdown]
# # Extract sowing data

# %%
def formatDateSafe(dt):
    if pd.isna(dt):
        return ""
    return dt.strftime("%d-%m-%Y")


# %%
ExptInfo.loc[:,'SowInfo']={}
for e in Experiments:
    tosData = NaPALentil_Design.loc[e,['TOS', 'TOSDate']].drop_duplicates()
    sowTrts = tosData.TOS.to_list()
    sowTrts.sort()
    tosDic = {}
    for st in sowTrts:
        sdic = {}
        sdic['sowDate'] = tosData.loc[tosData.TOS == st,'TOSDate'].values[0]
        sdic['startDate'] = tosData.loc[tosData.TOS == st,'TOSDate'].values[0]
        if e in NaPA_Management.index.drop_duplicates().to_list():
            sdic['emergeDate'] = formatDateSafe(NaPA_Management.loc[e,'EmergenceDate'].mean())
            sdic['sowDepth'] = NaPA_Management.loc[e,"SowingDepth_mm"].drop_duplicates().values[0]/10
            sdic['rowWidth'] = NaPA_Management.loc[e,"Design::RowSpacing_cm"].drop_duplicates().values[0]
        else:
            print(e)
            sdic['emergeDate'] = ""
            sdic['sowDepth'] = 30
            sdic['rowWidth'] = 400
            
        sdic['endDate'] = "31-dec-"+str(NaPA_Experiment.loc[e,'Year'])
        sdic['popn'] = 100
        
        tosDic[st] = sdic
    ExptInfo.at[e,'SowInfo'] = tosDic


# %% [markdown]
# # Function to apply experiment structure to .apsimx file

# %%
def write_experiment_apply_file(
    tempApplyFile,
    baseFile,
    experimentFile,
    soilName,
    soilLib,
    cultivars,
    irrigations,
    toss
):
    lines = []

    # ------------------------------------------------------------------
    # Load base apsimx
    # ------------------------------------------------------------------
    lines.append(f"load {baseFile}")

    # ------------------------------------------------------------------
    # Add soil
    # ------------------------------------------------------------------
    lines.append(f"add [{soilName}] from {soilLib} to [Zone] name {soilName}")

    # ------------------------------------------------------------------
    # Add varieties
    # ------------------------------------------------------------------
    varietyList = ""
    for c in cultivars:
        cName = c.replace("PBA_", "").replace("GIA_", "")
        varietyList += cName + ", "
    lines.append(
        f"[Factors].Permutation.Variety.specification = "
        f"[Sowing].Script.CultivarName = {varietyList}"
    )

    # ------------------------------------------------------------------
    # Irrigation treatments
    # ------------------------------------------------------------------
    for trt_name, trt_irrigs in irrigations.items():

        # ---- 1. Create the treatment structure via CLI
        lines.append(f"add new CompositeFactor to [WaterTrt] name {trt_name}")
        lines.append(f"[Factors].Permutation.WaterTrt.{trt_name}.Specifications = [IrrigationApplications]")

        # ---- 2. Build an ops-only apsimx file for this treatment
        ops_models = []

        for d, amt in trt_irrigs.items():
            ops_models.append(
                {
                    "$type": "Models.Operation, Models",
                    "Enabled": True,
                    "Date": d,                     # e.g. "2023-08-07"
                    "Action": f"[Irrigation].Apply(amount: {amt})"
                    # Line intentionally omitted – APSIM regenerates it
                }
            )

        ops_apsim = {
            "$type": "Models.Core.Simulations, Models",
            "Name": "Simulations",
            "Children": [
                {
                    "$type": "Models.Core.Simulation, Models",
                    "Name": f"Ops_{trt_name}",
                    "Children": [
                        {
                            "$type": "Models.Operations, Models",
                            "Name": "IrrigationApplications",
                            "OperationsList": ops_models,
                            "Children": [],
                            "Enabled": True,
                            "ReadOnly": False,
                        }
                    ],
                    "Enabled": True,
                    "ReadOnly": False,
                }
            ],
            "Enabled": True,
            "ReadOnly": False,
        }

        # ---- 3. Write temporary ops file
        ops_file = (
            Path(tempApplyFile).with_suffix("")
            .parent
            / f"_ops_{trt_name}.apsimx"
        )

        ops_file.write_text(json.dumps(ops_apsim, indent=2))

        # ---- 4. Copy the Operations model into the experiment via CLI
        lines.append(f"add [IrrigationApplications] from {ops_file} to [Factors].Permutation.WaterTrt.{trt_name} name IrrigationApplications")
    
    # ------------------------------------------------------------------
    # TOS treatments
    # ------------------------------------------------------------------
    for tos_trt, tos_info in toss.items():
        lines.append(f"add new CompositeFactor to [TOS] name TOS{tos_trt}")
        lines.append(f"[TOS].TOS{tos_trt}.Specifications = " +
                     f"[Clock].StartDate = {tos_info['startDate']}," +
                     f"[Clock].EndDate = {tos_info['endDate']},"+
                     f"[Sowing].Script.SowDate = {tos_info['sowDate']},"+
                     f"[Sowing].Script.EmergeDate = {tos_info['emergeDate']},"+
                     f"[Sowing].Script.SowingDepth = {tos_info['sowDepth']},"+
                     f"[Sowing].Script.RowSpacing = {tos_info['rowWidth']},"+
                     f"[Sowing].Script.Population = {tos_info['popn']}")
    
    # ------------------------------------------------------------------
    # Save experiment
    # ------------------------------------------------------------------
    lines.append(f"save {experimentFile}")

    # Write apply file
    tempApplyFile.write_text("\n".join(lines))



# %%
APSIM_EXE = r"C:\GitHubRepos\ApsimX\bin\Debug\net8.0\Models.exe"
workingDir = r"C:\GitHubRepos\ApsimX\Prototypes\Lentil\NaPA\New"
baseFile = os.path.join(workingDir, "newBase.apsimx")
soilLib = r"C:\GitHubRepos\ApsimX\Prototypes\Lentil\NaPA\NaPA_soils.apsimx"

for experimentName in ExptInfo.index:
    print(experimentName)
    experimentFile = os.path.join(workingDir, f"{experimentName}.apsimx")
    soilName = ExptInfo.loc[experimentName,"SoilName"]
    tempApplyFile = os.path.join(workingDir, f"temp{experimentName}CLI.txt")
    cultivars = ExptInfo.loc[experimentName,"Varieties"]
    irrigations = ExptInfo.loc[experimentName,'IrrigInfo']
    toss = ExptInfo.loc[experimentName,'SowInfo']
    write_experiment_apply_file(Path(tempApplyFile), Path(baseFile), Path(experimentFile), soilName, soilLib, cultivars, irrigations, toss)
    result = subprocess.run(
        [
            APSIM_EXE,  #Path to Model.exe
            experimentFile,     #Path to sim.apsimx
            "--apply", tempApplyFile,  #path to apply file with changes to sim.apsimx 
        ],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True
    )
    print(result.stdout)

# %%
ExptInfo

# %%

val = ExptInfo.at[
    "2019_NSW_Greenethorpe_Mixed_Detailed",
    "IrrigInfo"
]

type(val), val


# %%
