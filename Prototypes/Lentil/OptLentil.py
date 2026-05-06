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

# %%
import datetime as dt
import pandas as pd
import numpy as np 
import matplotlib.pyplot as plt
#import APSIMGraphHelpers as AGH
#import GraphHelpers as GH
#from scipy import stats
#import statsmodels.api as sm
#from statsmodels.formula.api import ols
import matplotlib.dates as mdates
import MathsUtilities as MUte
#import shlex # package to construct the git command to subprocess format
import subprocess 
#import ProcessWheatFiles as pwf
#import xmltodict, json
import sqlite3
import scipy.optimize 
from skopt import gp_minimize
from skopt.callbacks import CheckpointSaver
from skopt import load
from skopt.plots import plot_convergence
import matplotlib.gridspec as gridspec
import os
from pathlib import Path

import winsound
frequency = 2500  # Set Frequency To 2500 Hertz
duration = 1000  # Set Duration To 1000 ms == 1 second
# %matplotlib inline

# %%
def write_cultivar_apply_file(apply_path: Path, apsimx_path: Path, cultivar_name: str, parameters: dict):
    """
    Write an APSIM apply file to select a cultivar and override its parameters.

    parameters: dict mapping APSIM variable paths to values, e.g.
        {
            "[Phenology].JuvenileBase.FixedValue": 30,
            "[Phenology].VernSensitivity.FixedValue": 0.22,
        }
    """

    lines = []

    # Load base apsimx
    lines.append(f"load {apsimx_path}")

    # Select cultivar via playlist
    lines.append(f"[Playlist].Text=*{cultivar_name}*")

    # Build command list
    lines.append(f"[Replacements].Lentil.Cultivars.{cultivar_name}.Command = ")

    for key, value in parameters.items():
        lines.append(f' {key} = {value},')

    # Remove trailing comma on last entry
    lines[-1] = lines[-1].rstrip(",")
    
    # Save and run
    lines.append(f"save {apsimx_path}")
    lines.append("run")
    
    apply_path.write_text("\n".join(lines))


# %%
def calcLoss(fitting_variables, obs_pred, freq):
   # --- collect scaled obs/pred pairs ---
    sc_obs = []
    sc_pred = []

    for var in fitting_variables:
        obs_col = f"Observed.{var}"
        pred_col = f"Predicted.{var}"

        if obs_col not in obs_pred or pred_col not in obs_pred:
            continue

        df = obs_pred[[obs_col, pred_col]].dropna()
        if df.empty:
            continue

        df[obs_col] = pd.to_numeric(df[obs_col])
        df[pred_col] = pd.to_numeric(df[pred_col])

        v_max = max(df[obs_col].max(), df[pred_col].max())
        v_min = min(df[obs_col].min(), df[pred_col].min())

        # scale to 0–1
        obs_scaled = (df[obs_col] - v_min) / (v_max - v_min)
        pred_scaled = (df[pred_col] - v_min) / (v_max - v_min)

        sc_obs.append(obs_scaled.values)
        sc_pred.append(pred_scaled.values)

    # --- guard against insufficient data ---
    if not sc_obs:
        return 2.0  # penalty

    sc_obs = np.concatenate(sc_obs)
    sc_pred = np.concatenate(sc_pred)

    # --- compute NSE ---
    obs_mean = np.mean(sc_obs)
    denominator = np.sum((sc_obs - obs_mean) ** 2)
    if denominator == 0:
        return 2.0  # penalty

    nse = 1.0 - np.sum((sc_obs - sc_pred) ** 2) / denominator

    # --- return loss (minimiser) ---
    return -max(nse, -2.0), len(sc_obs)



# %%
class ResultsStore:
    def __init__(self):
        self.records = []

    def add_result(
        self,
        cultivar,
        parameters,
        loss,
        n_obs=None,
        extra=None
    ):
        record = {
            "cultivar": cultivar,
            "loss": loss,
            "n_obs": n_obs,
            **{f"param_{k}": v for k, v in parameters.items()}
        }

        if extra is not None:
            record.update(extra)

        self.records.append(record)

    def to_dataframe(self):
        return pd.DataFrame(self.records)


# %%
cultivar_params = {
    "[Phenology].JuvenileBase.FixedValue": 40,
    "[Phenology].VernSensitivity.FixedValue": 0.7,
    "[Phenology].InductivePpSensitivity.FixedValue": 0.7
}

fitting_variables = ['Lentil.Phenology.StartBuddingDAS',
                     'Lentil.Phenology.StartFloweringDAS',
                     'Lentil.Phenology.StartPoddingDAS']

APSIM_EXE = r"C:\GitHubRepos\ApsimX\bin\Debug\net8.0\Models.exe"

simulationPath = r"C:\GitHubRepos\ApsimX\Prototypes\Lentil"


# %%
def runModelItter(cultivar_name, paramSets, path, apsimFile, applyFile, playListName, fitting_variables, freq, results_store=None):
    apsimx = os.path.join(path, f"{apsimFile}.apsimx")
    apply  = os.path.join(path, f"{applyFile}.txt")
    db = os.path.join(path, f"{apsimFile}.db")
    db_path = Path(db)
    if db_path.exists():
        db_path.unlink() #this deletes the db file if it exists so we start with a clearn db
    
    write_cultivar_apply_file(Path(apply), apsimx, cultivar_name, paramSets)
    result = subprocess.run(
        [
            APSIM_EXE,
            apsimx,
            "--apply", apply,
            "--playlist", playListName
        ],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True
    )

    print(result.stdout)
    
    
    # --- read APSIM output ---
    con = sqlite3.connect(db_path)

    if freq == "Harvest":
        obs_pred = pd.read_sql("SELECT * FROM HarvestObsPred", con)
    elif freq == "Daily":
        obs_pred = pd.read_sql("SELECT * FROM DailyObsPred", con)
    else:
        con.close()
        raise ValueError("freq must be 'Harvest' or 'Daily'")

    con.close
    
    loss, n_obs = calcLoss(fitting_variables, obs_pred, freq)
    
    if results_store is not None:
        results_store.add_result(
            cultivar=cultivar_name,
            parameters=paramSets,
            loss=loss,
            n_obs=n_obs
        )
    
    return loss

# %%
store = ResultsStore()
runModelItter('Bolt',cultivar_params, simulationPath, "Lentil", "CultivarApply", "ChooseCultivar", fitting_variables, "Harvest", results_store=store)
df = store.to_dataframe()
