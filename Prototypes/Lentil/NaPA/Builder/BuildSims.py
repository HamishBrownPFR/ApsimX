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
root = "C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil\\NaPA\\Builder\\Database\\"

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
ExptInfo.loc[:,"ExptInfo"] = {}
for e in Experiments:
    ExptInfo.at[e,"Varieties"] = NaPALentil_Design.loc[e,'Variety'].drop_duplicates().to_list()
    exptDic = {}
    exptDic["SoilName"] = f"{NaPA_Experiment.loc[e,'ExpNo']}_{NaPA_Experiment.loc[e,'SiteName']}"
    exptDic["SiloMet"] = f"{NaPA_Experiment.loc[e,'State']}_{NaPA_Experiment.loc[e,'SiloWeatherFile']}"
    exptDic["LocalMet"] = NaPA_Experiment.loc[e,'LocalWeatherFile']
    ExptInfo.at[e,"ExptInfo"] = exptDic
    
ExptInfo.at['2022_NSW_WaggaWagga_Lentil_Detailed',"ExptInfo"]["SoilName"] = "2023007_WaggaWagga"
ExptInfo.at['2019_NSW_Greenethorpe_Mixed_Detailed',"ExptInfo"]["SoilName"] = "2022010_Greenethorpe"
ExptInfo.at['2024_NSW_Greenethorpe_Mixed_NFix',"ExptInfo"]["SoilName"] = "2022010_Greenethorpe"
ExptInfo.at['2022_NSW_Methul_Lentil_Satellite',"ExptInfo"]["SoilName"] = "Methul"
ExptInfo.at['2024_Vic_Walpeup_Lentil_Satellite',"ExptInfo"]["SoilName"] = "Walpeup"

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


# %% [markdown]
# # Extract sowing data

# %%
def formatDateSafe(dt):
    if pd.isna(dt):
        return ""
    return dt.strftime("%d-%b")


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
            sdic['emergeDate'] = "1-Jan"
            sdic['sowDepth'] = 30
            sdic['rowWidth'] = 400
            
        sdic['endDate'] = "31-dec-"+str(NaPA_Experiment.loc[e,'Year'])
        sdic['popn'] = 100
        
        tosDic[st] = sdic
    ExptInfo.at[e,'SowInfo'] = tosDic

# %%
ExptInfo.loc["2019_NSW_Greenethorpe_Mixed_Detailed","ExptInfo"]


# %% [markdown]
# # Function to apply experiment structure to .apsimx file

# %%
def write_experiment_apply_file(
    exptName,
    tempApplyFile,
    baseAPSIMFile,
    finalAPSIMFile,
    exptInfo,
    soilLib,
    localWeatherLib,
    cultivars,
    irrigations,
    toss,
    localWeatherName 
):
    lines = []

    # ------------------------------------------------------------------
    # Load base apsimx and name experiment
    # ------------------------------------------------------------------
    lines.append(f"load {baseAPSIMFile}")
    lines.append(f"[BaseExpt].Name = {exptName}")
    
    # ------------------------------------------------------------------
    # Set up the met files
    # ------------------------------------------------------------------
    lines.append(f"[Weather].FileName = Met/{exptInfo['SiloMet']}")
    if localWeatherName is not None:
        lines.append(f"add {localWeatherName} from {localWeatherLib} to [BaseSim]")
    
    # ------------------------------------------------------------------
    # Add soil
    # ------------------------------------------------------------------
    lines.append(f"add [{exptInfo['SoilName']}] from {soilLib} to [Zone] name {exptInfo['SoilName']}")

    # ------------------------------------------------------------------
    # Add varieties
    # ------------------------------------------------------------------
    varietyList = ", ".join(
        c.replace("PBA_", "").replace("GIA_", "")
        for c in cultivars
    )
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
    lines.append(f"save {finalAPSIMFile}")

    # Write apply file
    tempApplyFile.write_text("\n".join(lines))


# %%
ExptInfo.loc['2019_NSW_Greenethorpe_Mixed_Detailed','ExptInfo']


# %%
def makeLocalWeather(template_file, output_dir, experiment_name, local_met):
    """
    Create a LocalWeather apsimx file with patched CodeArray.

    Returns:
        (model_name, file_path) OR (None, None)
    """

    # ----------------------------------------------------------
    # 1. Handle no local weather case
    # ----------------------------------------------------------
    if pd.isna(local_met) or local_met in ["", "nan", None]:
        return None, None

    # ----------------------------------------------------------
    # 2. Load template
    # ----------------------------------------------------------
    data = json.loads(Path(template_file).read_text())

    # ----------------------------------------------------------
    # 3. Find LocalWeather manager
    # ----------------------------------------------------------
    def find_model(node, name):
        if node.get("Name") == name:
            return node
        for child in node.get("Children", []):
            result = find_model(child, name)
            if result:
                return result
        return None

    manager = find_model(data, "LocalWeather")

    if manager is None:
        raise ValueError("LocalWeather model not found in template")

    # ----------------------------------------------------------
    # 4. Replace placeholder in CodeArray
    # ----------------------------------------------------------
    safe_path = str(local_met).replace("\\", "\\\\")

    new_code = []
    for line in manager["CodeArray"]:
        new_code.append(
            line.replace("FindAndReplaceWithScript", safe_path)
        )

    manager["CodeArray"] = new_code

    # ----------------------------------------------------------
    # 5. Write output file
    # ----------------------------------------------------------
    output_file = output_dir / f"_localWeather_{experiment_name}.apsimx"

    output_file.write_text(json.dumps(data, indent=2))

    return "LocalWeather", output_file



# %%
from pathlib import Path

APSIM_EXE = Path(r"C:\GitHubRepos\ApsimX\bin\Debug\net8.0\Models.exe")
workingDir = Path(r"C:\GitHubRepos\ApsimX\Prototypes\Lentil\NaPA\Builder")

applyDir = workingDir / "ApplyFiles"
outputDir = workingDir.parent   # one level up (NaPA)

# Ensure ApplyFiles exists
applyDir.mkdir(exist_ok=True)

baseAPSIMFile = workingDir / "builderBase.apsimx"
soilLib = workingDir / "NaPA_soils.apsimx"
localWeatherLib = workingDir / "localWeather.apsimx"
for experimentName in ExptInfo.index:
    print(experimentName)

    finalAPSIMFile = outputDir / f"{experimentName}.apsimx" 
    tempApplyFile = applyDir / f"temp_{experimentName}CLI.txt" 

    exptInfo = ExptInfo.loc[experimentName, "ExptInfo"]
    cultivars = ExptInfo.loc[experimentName, "Varieties"]
    irrigations = ExptInfo.loc[experimentName, 'IrrigInfo']
    toss = ExptInfo.loc[experimentName, 'SowInfo']
    
    localWeatherName, localWeatherLib = makeLocalWeather(
        localWeatherLib,      # template file
        applyDir,             # where temp files go
        experimentName,
        exptInfo["LocalMet"]
    )

    write_experiment_apply_file(
        experimentName,
        tempApplyFile,
        baseAPSIMFile,
        finalAPSIMFile,
        exptInfo,
        soilLib,
        localWeatherLib,
        cultivars,
        irrigations,
        toss,
        localWeatherName
    )
    
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

