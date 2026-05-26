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
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import MathsUtilities as MUte
import subprocess 
import sqlite3
import scipy.optimize 
from skopt import gp_minimize
from skopt.callbacks import CheckpointSaver
from skopt import load
from skopt.plots import plot_convergence
from skopt.space import Real
from skopt.space import Space
from skopt import Optimizer
from skopt.utils import create_result
import matplotlib.gridspec as gridspec
import os
from pathlib import Path

# %matplotlib inline

# %% [markdown]
# # Prepare.apsimx files
# Inject and remove cultivar replacement and playlist to select that cultivar

# %%
def write_cultivar_apply_file(apply_path: Path, apsimx_path: Path, cultivar_name: str, parameters: dict, playListName: str):
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
    lines.append(f"add new Playlist to [Simulations] name {playListName}")
    lines.append(f"[{playListName}].Text=*{cultivar_name}*")

    # Build command list
    lines.append(f"add new Cultivar to [Replacements] name {cultivar_name}")
    lines.append(f"[Replacements].{cultivar_name}.Command = ")
    
    for key, value in parameters.items():
        lines.append(f' {key} = {value},')

    # Remove trailing comma on last entry
    lines[-1] = lines[-1].rstrip(",")
    
    # Save and run
    lines.append(f"save {apsimx_path}")
    lines.append("run")
    
    apply_path.write_text("\n".join(lines))
    
def remove_cultivar_apply_file(apply_path: Path, apsimx_path: Path, cultivar_name: str, playListName: str):
    """
    Write an APSIM apply file to remove the play list and cultivar added to replacements so clean for next run.

    """
    lines = []

    # Load base apsimx
    lines.append(f"load {apsimx_path}")

    # Delete temporary components from fitting
    lines.append(f"delete [Simulations].{playListName}")
    lines.append(f"delete [Replacements].{cultivar_name}")

    # Save and run
    lines.append(f"save {apsimx_path}")
    
    apply_path.write_text("\n".join(lines))


# %% [markdown]
# # Calculate loss 
# Take results from simulation run and calculate a loss value using NSE

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


# %% [markdown]
# # Results Data class
# for results from each parameter combination

# %%
class ResultsStore:
    def __init__(self):
        self.records = []
        self.iteration = 0

    def addResult(
        self,
        cultivar,
        parameters,
        loss,
        nObs,
        runtime=None,
        obs=None,
        pred=None,
        variable=None,
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
            "n_obs": nObs,
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

# %% [markdown]
# # runModelItter
# runs the model with specified parameter set and return loss as a measure of accuracy with that parameter set.

# %%
# def runModelGetStats(runSpec, paramSet, fittingVariables):
#     apsimx = os.path.join(runSpec["simulationPath"], f"{runSpec['apsimFileName']}.apsimx")
#     apply  = os.path.join(runSpec["simulationPath"], f"tempApplyCLI.txt")
    
#     db = os.path.join(runSpec["simulationPath"], f"{runSpec['apsimFileName']}.db")
#     db_path = Path(db)
#     if db_path.exists():
#         db_path.unlink() #this deletes the db file if it exists so we start with a clearn db
#     write_cultivar_apply_file(apply_path=Path(apply), apsimx_path=Path(apsimx), cultivar_name=runSpec["cultivarName"], parameters=paramSet, playListName="tempChooseCultivar")
#     start = dt.datetime.now()
#     result = subprocess.run(
#         [
#             APSIM_EXE,  #Path to Model.exe
#             apsimx,     #Path to sim.apsimx
#             "--apply", apply,  #path to apply file with changes to sim.apsimx 
#             "--playlist", "tempChooseCultivar"  #Intstuction to use playlist
#         ],
#         stdout=subprocess.PIPE,
#         stderr=subprocess.STDOUT,
#         text=True,
#         timeout=300   # 5 minutes safeguard
#     )  
#     if result.stdout and result.stdout.strip():
#         print(result.stdout)

    
#     remove_cultivar_apply_file(apply_path=Path(apply), apsimx_path=Path(apsimx), cultivar_name=runSpec["cultivarName"], playListName="tempChooseCultivar")
#     result = subprocess.run(
#     [
#         APSIM_EXE,
#         apsimx,
#         "--apply", apply
#     ],
#     stdout=subprocess.PIPE,
#     stderr=subprocess.STDOUT,
#     text=True
#     )

#     if result.stdout and result.stdout.strip():
#         print(result.stdout)
#     endrun = dt.datetime.now()
#     runtime = (endrun-start).seconds
    
#     # Read requested report
#     con = sqlite3.connect(db)
#     try:
#         obs_pred = pd.read_sql(f"SELECT * FROM {runSpec['reportName']}", con)
#     finally:
#         con.close()
        
#     return obs_pred, runtime

# %%
def runModelGetStats(runSpec, paramSet, fittingVariables):

    apsimx = os.path.join(runSpec["simulationPath"], f"{runSpec['apsimFileName']}.apsimx")
    apply  = os.path.join(runSpec["simulationPath"], f"tempApplyCLI.txt")
    
    db = os.path.join(runSpec["simulationPath"], f"{runSpec['apsimFileName']}.db")
    db_path = Path(db)

    # Ensure clean DB
    if db_path.exists():
        db_path.unlink()

    # Write apply file
    write_cultivar_apply_file(apply_path=Path(apply),apsimx_path=Path(apsimx),cultivar_name=runSpec["cultivarName"],
        parameters=paramSet,playListName="tempChooseCultivar")

    start = dt.datetime.now()

    # -----------------------------
    # RUN APSIM (main run)
    # -----------------------------
    result = subprocess.run(
        [
            APSIM_EXE,
            apsimx,
            "--apply", apply,
            "--playlist", "tempChooseCultivar"
        ],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        timeout=300
    )

    output = result.stdout or ""

    # --- Detect NO MATCH case ---
    if "Playlist was used but no simulations or experiments match the contents of the list" in output:
        print(f"⚠️ No matching simulations for {runSpec['apsimFileName']} (skipping)")

        # cleanup before exit
        remove_cultivar_apply_file(apply_path=Path(apply),apsimx_path=Path(apsimx),
            cultivar_name=runSpec["cultivarName"],playListName="tempChooseCultivar")

        subprocess.run(
            [APSIM_EXE, apsimx, "--apply", apply],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True
        )

        endrun = dt.datetime.now()
        runtime = (endrun - start).seconds

        return pd.DataFrame(), runtime

    # --- Print only meaningful output ---
    if output.strip():
        print(output)

    # -----------------------------
    # CLEANUP (remove temp nodes)
    # -----------------------------
    remove_cultivar_apply_file(
        apply_path=Path(apply),
        apsimx_path=Path(apsimx),
        cultivar_name=runSpec["cultivarName"],
        playListName="tempChooseCultivar"
    )

    result = subprocess.run(
        [
            APSIM_EXE,
            apsimx,
            "--apply", apply
        ],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True
    )

    if result.stdout and result.stdout.strip():
        print(result.stdout)

    endrun = dt.datetime.now()
    runtime = (endrun - start).seconds

    # -----------------------------
    # SAFE DB READ
    # -----------------------------
    if not os.path.exists(db):
        print(f"⚠️ DB not created for {runSpec['apsimFileName']} (skipping)")
        return pd.DataFrame(), runtime

    con = sqlite3.connect(db)

    try:
        tables = pd.read_sql(
            "SELECT name FROM sqlite_master WHERE type='table';",
            con
        )["name"].tolist()

        if runSpec['reportName'] not in tables:
            print(f"⚠️ Table {runSpec['reportName']} missing in {runSpec['apsimFileName']} (skipping)")
            return pd.DataFrame(), runtime

        obs_pred = pd.read_sql(f"SELECT * FROM {runSpec['reportName']}", con)

    finally:
        con.close()

    return obs_pred, runtime


# %%
def runModelItter(runSpecs, paramSet, fittingVariables, resultsStore=None, printResult=False):
    """
    Run one parameter set across all APSIM files listed in runSpecs and merge results.
    """

    allObsPred = []
    totalRuntime = 0
    itter = 0

    for runSpec in runSpecs:
        obsPred, runtime = runModelGetStats(
            runSpec=runSpec,
            paramSet=paramSet,
            fittingVariables=fittingVariables
        )

        allObsPred.append(obsPred)
        totalRuntime += runtime

    obsPredAll = pd.concat(allObsPred, ignore_index=True)

    # Compute loss and scaled values
    loss, nObs, scObs, scPred = calcLoss(fittingVariables, obsPredAll)

    # Optional: store results
    if resultsStore is not None:
        resultsStore.addResult(
            cultivar=runSpecs[0]["cultivarName"],
            parameters=paramSet,
            loss=loss,
            nObs=nObs,
            runtime=totalRuntime,
            obs=scObs,
            pred=scPred
        )
        
        itter = resultsStore.iteration

    if printResult:
        print(
            f"[{itter:03d}] | "
            f"{list(paramSet.values())} run completed | "
            f"{nObs} obs in {totalRuntime} seconds. | "
            f"NSE = {-loss:.3f}"
        )

    return loss


# %% [markdown]
# # Model fitting settings

# %%
fitting_variables = ['Lentil.Phenology.StartBuddingDAS',
                     'Lentil.Phenology.StartFloweringDAS',
                     'Lentil.Phenology.StartPoddingDAS']

APSIM_EXE = r"C:\GitHubRepos\ApsimX\bin\Debug\net8.0\Models.exe"

# %% [markdown]
# # Test with single file

# %%
cultivar_params = {
                    "[Phenology].JuvenileBase.FixedValue": 96,
                    "[Phenology].VernSensitivity.FixedValue": 0.63,
                    "[Phenology].InductivePpSensitivity.FixedValue": 0.44
                  }

runSpec = {
             "cultivarName":"Bolt",
             "simulationPath":r"C:\GitHubRepos\ApsimX\Prototypes\Lentil\NaPA",
             "apsimFileName":"2023_SA_Pinery_Lentil_Detailed",
             "reportName":"HarvestObsPred"
           }

testStore = ResultsStore()
runSpecs = []
runSpecs.append(runSpec)
runModelItter(runSpecs, cultivar_params, fitting_variables, resultsStore=testStore, printResult=True)
df = testStore.to_dataframe()

# %% [markdown]
# # Test with multi files

# %%
cultivar_params = {
                    "[Phenology].JuvenileBase.FixedValue": 96,
                    "[Phenology].VernSensitivity.FixedValue": 0.63,
                    "[Phenology].InductivePpSensitivity.FixedValue": 0.44
                  }
runSpecs = []
baseRunSpec = {
             "cultivarName":"Bolt",
             "simulationPath":None,
             "apsimFileName":None,
             "reportName":"HarvestObsPred"
           }

filesToRun = [
     {'dir':'C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil','name':'Lentil'},
     #{'dir':'C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil\\NaPA','name':'2019_NSW_Greenethorpe_Mixed_Detailed'},
     {'dir':'C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil\\NaPA','name':'2022_Vic_Kalkee_Lentil_Detailed'},
     {'dir':'C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil\\NaPA','name':'2022_SA_Riverton_Lentil_Detailed'},
     {'dir':'C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil\\NaPA','name':'2022_NSW_WaggaWagga_Lentil_Detailed'},
     #{'dir':'C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil\\NaPA','name':'2022_NSW_Methul_Lentil_Satellite'},
     #{'dir':'C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil\\NaPA','name':'2022_Vic_Ouyen_Lentil_Satellite'},
     #{'dir':'C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil\\NaPA','name':'2022_NSW_RankinsSprings_Lentil_Satellite'},
     #{'dir':'C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil\\NaPA','name':'2022_SA_Warnertown_Lentil_Satellite'},
     {'dir':'C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil\\NaPA','name':'2023_SA_Pinery_Lentil_Detailed'},
     {'dir':'C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil\\NaPA','name':'2023_Vic_Dooen_Lentil_Detailed'},
     #{'dir':'C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil\\NaPA','name':'2023_SA_Warnertown_Lentil_Satellite'},
     #{'dir':'C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil\\NaPA','name':'2023_Vic_Ouyen_Lentil_Satellite'},
     #{'dir':'C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil\\NaPA','name':'2023_Qld_Gatton_Mixed_Light'},
     #{'dir':'C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil\\NaPA','name':'2024_NSW_Greenethorpe_Mixed_NFix'},
     #{'dir':'C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil\\NaPA','name':'2024_SA_Warnertown_Lentil_Satellite'},
     #{'dir':'C:\\GitHubRepos\\ApsimX\\Prototypes\\Lentil\\NaPA','name':'2024_Vic_Walpeup_Lentil_Satellite'}
  ]

for fTR in filesToRun:
    fileRunSpec = baseRunSpec.copy()
    fileRunSpec["apsimFileName"] = fTR['name']
    fileRunSpec["simulationPath"] = fTR['dir']
    runSpecs.append(fileRunSpec)

storeMulti = ResultsStore()

runModelItter(runSpecs, cultivar_params, fitting_variables, resultsStore=storeMulti, printResult=True)
df = testStore.to_dataframe()


# %% [markdown]
# # Objective function
# for optimiser to interface with runModelItter

# %%
def objective(x):
    param_dict = dict(zip(paramNames, x))
   
    loss = runModelItter(runSpecs, param_dict, fitting_variables, resultsStore=storeMulti, printResult=True)
    
    return loss


# %% [markdown]
# # Stagnation functions
# Determine if optimiser is reaching best possible fits.

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

    y_hist = np.array(res.func_vals)

    best = np.minimum.accumulate(y_hist)
    recent = best[-window:]

    return (recent[0] - recent[-1]) < tol




# %% [markdown]
# # Run optimisation

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
expert_guesses = [[96, 0.63, 0.44]]

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
max_stages = 10

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

# %% [markdown]
# # Evolution of loss results

# %%
df = storeMulti.to_dataframe()

# %%
df.loss.plot()

# %% [markdown]
# # Obs vs pred of best fit

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
