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
from skopt.space import Real
from skopt.space import Space
from skopt import Optimizer

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
def calcLoss(fitting_variables, obs_pred):
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

        v_max = df[obs_col].max()
        v_min = df[obs_col].min()

        # scale to 0–1
        obs_scaled = (df[obs_col] - v_min) / (v_max - v_min)
        pred_scaled = (df[pred_col] - v_min) / (v_max - v_min)

        sc_obs.append(obs_scaled.values)
        sc_pred.append(pred_scaled.values)

    # --- guard against insufficient data ---
    if not sc_obs:
        return 2.0, 0, None  # penalty

    sc_obs = np.concatenate(sc_obs)
    sc_pred = np.concatenate(sc_pred)

    # --- compute NSE ---
    obs_mean = np.mean(sc_obs)
    denominator = np.sum((sc_obs - obs_mean) ** 2)
    if denominator == 0:
        return 2.0, 0, None  # penalty

    nse = 1.0 - np.sum((sc_obs - sc_pred) ** 2) / denominator

    # --- return loss (minimiser) ---
    return -max(nse, -2.0), len(sc_obs), sc_obs, sc_pred


# %%
class ResultsStore:
    def __init__(self):
        self.records = []
        self.iteration = 0

    def add_result(
        self,
        cultivar,
        parameters,
        loss,
        n_obs,
        runtime=None,
        obs=None,
        pred=None,
        variable=None
    ):
        """
        Store results from one APSIM evaluation.

        obs, pred: array-like (NumPy arrays or lists)
        variable: name of fitted variable (optional)
        """
        self.iteration += 1

        record = {
            "iteration": self.iteration,
            "cultivar": cultivar,
            "loss": loss,
            "n_obs": n_obs,
            "runtime": runtime,
            "variable": variable,
            **{f"param_{k}": v for k, v in parameters.items()}
        }

        # Store arrays as-is (NumPy arrays are fine)
        record["obs"] = obs
        record["pred"] = pred

        self.records.append(record)

    def to_dataframe(self, drop_arrays=False):
        """
        Convert to DataFrame. Optionally drop obs/pred arrays.
        """
        if drop_arrays:
            return pd.DataFrame([
                {k: v for k, v in r.items() if k not in ("obs", "pred")}
                for r in self.records
            ])
        return pd.DataFrame(self.records)


# %%
fitting_variables = ['Lentil.Phenology.StartBuddingDAS',
                     'Lentil.Phenology.StartFloweringDAS',
                     'Lentil.Phenology.StartPoddingDAS']

APSIM_EXE = r"C:\GitHubRepos\ApsimX\bin\Debug\net8.0\Models.exe"

simulationPath = r"C:\GitHubRepos\ApsimX\Prototypes\Lentil"


# %%
def runModelItter(cultivarName, paramSet, simulationPath, apsimFileName, applyFileName, playListName, fittingVariables, reportName, resultsStore=None):
    apsimx = os.path.join(simulationPath, f"{apsimFileName}.apsimx")
    apply  = os.path.join(simulationPath, f"{applyFileName}.txt")
    db = os.path.join(simulationPath, f"{apsimFileName}.db")
    db_path = Path(db)
    #if db_path.exists():
     #   db_path.unlink() #this deletes the db file if it exists so we start with a clearn db
    
    write_cultivar_apply_file(Path(apply), apsimx, cultivarName, paramSet)
    start = dt.datetime.now()
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
    endrun = dt.datetime.now()
    runtime = (endrun-start).seconds
    
    print(result.stdout)
    
    # Read requested report
    con = sqlite3.connect(db)
    try:
        obs_pred = pd.read_sql(f"SELECT * FROM {reportName}", con)
    finally:
        con.close()
    
    loss, n_obs, obs, pred = calcLoss(fittingVariables, obs_pred)
    
    if resultsStore is not None:
        resultsStore.add_result(
            cultivar=cultivarName,
            parameters=paramSet,
            loss=loss,
            n_obs=n_obs,
            runtime=runtime,
            obs=obs,
            pred=pred,
            variable=None
        )
        
        it = resultsStore.iteration
    else:
        it = "?"


    print(
        f"[{it:03d}] "
        f"{list(paramSet.values())} | "
        f"n={n_obs:4d} | "
        f"t={runtime:4d}s | "
        f"NSE={-loss:6.3f}"
    )

    return loss

# %%
cultivar_params = {
    "[Phenology].JuvenileBase.FixedValue": 40,
    "[Phenology].VernSensitivity.FixedValue": 0.7,
    "[Phenology].InductivePpSensitivity.FixedValue": 0.7
}

store = ResultsStore()
runModelItter(cultivarName='Bolt',
              paramSet=cultivar_params, 
              simulationPath=simulationPath, 
              apsimFileName="Lentil", 
              applyFileName="CultivarApply", 
              playListName="ChooseCultivar", 
              fittingVariables=fitting_variables, 
              reportName="HarvestObsPred", 
              resultsStore=store)
df = store.to_dataframe()

# %%
df


# %%
def objective(x):
    param_dict = dict(zip(paramNames, x))
    
    loss = runModelItter(
        cultivarName="Bolt",
        paramSet=param_dict,
        simulationPath=simulationPath,
        apsimFileName="Lentil",
        applyFileName="CultivarApply",
        playListName="ChooseCultivar",
        fittingVariables=fitting_variables,
        reportName="HarvestObsPred",
        resultsStore=store
    )
    
    return loss


# %%
# ------------------------------------------------------------
# Stagnation detection helpers
# ------------------------------------------------------------

def stagnation_detected(res, window=5, eps=None):
    """
    Detect whether the optimiser has stopped moving in parameter space.
    """
    if len(res.x_iters) < window + 1:
        return False

    recent_moves = np.linalg.norm(
        np.diff(np.array(res.x_iters[-window:]), axis=0),
        axis=1
    )

    return np.max(recent_moves) < eps


def loss_stagnated(res, window=10, tol=0.02):
    """
    Detect whether loss improvement has stalled.
    """
    if len(res.func_vals) < window:
        return False
    best = np.minimum.accumulate(y_hist)
    recent = best[-window:]
    return (recent[0] - recent[-1]) < tol



# %%
# ------------------------------------------------------------
# Parameter definitions
# ------------------------------------------------------------

paramNames = [
    "[Phenology].JuvenileBase.FixedValue",
    "[Phenology].VernSensitivity.FixedValue",
    "[Phenology].InductivePpSensitivity.FixedValue"
]

space_dims = [
    Real(0, 400, name=paramNames[0]),   # JuvenileBase
    Real(0.0, 2.0, name=paramNames[1]), # VernSensitivity
    Real(0.0, 2.0, name=paramNames[2]), # InductivePpSensitivity
]

space = Space(space_dims)

# ------------------------------------------------------------
# Create GP optimiser (ask / tell API)
# ------------------------------------------------------------

opt = Optimizer(
    dimensions=space_dims,
    base_estimator="GP",
    acq_func="EI",
    random_state=42
)

# Initialise results store
store = ResultsStore()

# ------------------------------------------------------------
# Initial design: expert guess + random sampling
# ------------------------------------------------------------

print("=== Initial design: expert + random ===")

# Expert guess (must be list of lists)
expert_guesses = [[100, 0.5, 0.5]]

for x in expert_guesses:
    y = objective(x)
    opt.tell(x, y)

# Random space-filling design
n_initial_random = 29
random_points = space.rvs(n_initial_random, random_state=42)

for x in random_points:
    y = objective(x)
    opt.tell(x, y)

# ------------------------------------------------------------
# Scale-aware stagnation threshold
# ------------------------------------------------------------

param_ranges = np.array([dim.high - dim.low for dim in space_dims])
eps = 0.01 * np.linalg.norm(param_ranges)

# ------------------------------------------------------------
# Staged GP-guided optimisation loop
# ------------------------------------------------------------

stage_size = 5
max_stages = 15

for stage in range(max_stages):
    print(f"\n=== GP optimisation stage {stage + 1} ===")

    # Exactly stage_size new APSIM runs
    for _ in range(stage_size):
        x = opt.ask()          # propose one point
        y = objective(x)       # run APSIM exactly once
        opt.tell(x, y)         # update GP

    # ---- stopping criteria ----
    x_hist = opt.Xi
    y_hist = np.array(opt.yi)

    if stagnation_detected(
        type("Res", (), {"x_iters": x_hist}),
        window=5,
        eps=eps
    ):
        print("Stopping: parameter-space stagnation detected.")
        break

    if loss_stagnated(
        type("Res", (), {"func_vals": y_hist}),
        window=10,
        tol=1e-3
    ):
        print("Stopping: loss improvement stalled.")
        break

# ------------------------------------------------------------
# Final result summary
# ------------------------------------------------------------

X = opt.Xi
Y = opt.yi

res = create_result(
    Xi=opt.Xi,
    yi=np.array(opt.yi),
    space=opt.space,
    specs=None,
    models=opt.models
)

best_idx = int(np.argmin(Y))
best_x = X[best_idx]
best_y = Y[best_idx]

print("\n=== Optimisation complete ===")
print(f"Best loss: {best_y:.4f}")
for name, val in zip(paramNames, best_x):
    print(f"{name}: {val:.4f}")

# %%
df = store.to_dataframe()

# %%
df.loss.plot()

# %%

# Find the best iteration (minimum loss)
best_idx = df["loss"].idxmin()
best_row = df.loc[best_idx]
best_iter = best_row["iteration"]

print(f"Best iteration: {best_iter}, NSE = {-best_row['loss']:.3f}")

# Plot Obs vs Pred for the best iteration
plt.figure()
plt.scatter(best_row["obs"], best_row["pred"], alpha=0.6)
plt.plot(
    [best_row["obs"].min(), best_row["obs"].max()],
    [best_row["obs"].min(), best_row["obs"].max()],
    "k--"
)
plt.xlabel("Observed")
plt.ylabel("Predicted")
plt.title(f"Best iteration {best_iter}, NSE={-best_row['loss']:.3f}")
plt.show()



# %%
from skopt.plots import plot_objective
from skopt.utils import create_result

res = create_result(
    Xi=opt.Xi,
    yi=np.array(opt.yi),
    space=opt.space,
    specs=None,
    models=opt.models
)

plot_objective(res)

# %%
store.to_dataframe()
