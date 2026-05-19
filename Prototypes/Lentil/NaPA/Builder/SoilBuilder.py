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
import pandas as pd
import copy
import json

# =========================================================
# CONSTANTS
# =========================================================
PD = 2.65     # particle density
EPS = 0.005   # small buffer for constraints


# =========================================================
# Converts messy numeric values (strings, lists) → float
# =========================================================
def safe_float(val):
    if pd.isna(val):
        return None

    if isinstance(val, (int, float)):
        return val

    if isinstance(val, str):
        parts = val.strip().split()
        vals = []
        for p in parts:
            try:
                vals.append(float(p))
            except:
                continue
        return sum(vals) / len(vals) if vals else None

    return None


# =========================================================
# Extract soil profile from characterisation data
# (grouped by LayerNo)
# =========================================================
def get_characterisation_profile(char, expno, sitekey):

    g = char[
        (char["ExpNo"] == expno) &
        (char["Experiments::SiteKey"] == sitekey)
    ].copy()

    if g.empty:
        return None

    # ensure numeric
    for col in ["BD_g.cc", "DUL_mm.mm", "LL15_mm.mm"]:
        g[col] = g[col].apply(safe_float)

    g = g.groupby("LayerNo").agg({
        "BD_g.cc": "mean",
        "DUL_mm.mm": "mean",
        "LL15_mm.mm": "mean"
    }).reset_index().sort_values("LayerNo")

    bd = g["BD_g.cc"].tolist()
    dul = g["DUL_mm.mm"].tolist()
    ll15 = g["LL15_mm.mm"].tolist()

    return bd, dul, ll15


# =========================================================
# Merge observed profile onto template
# Keeps template where observed is missing
# Guarantees correct length
# =========================================================
def merge_profiles(template_arr, observed_arr):

    n = len(template_arr)
    observed_arr = list(observed_arr)

    # align lengths
    observed_arr = observed_arr[:n] + [None] * (n - len(observed_arr))

    out = []
    for t, o in zip(template_arr, observed_arr):
        if o is None or pd.isna(o):
            out.append(t)
        else:
            out.append(o)

    return out


# =========================================================
# Enforce soil physics constraints (no NaN allowed)
# =========================================================
def reconstruct_water(th, bd, dul, ll15):

    AirDry, SAT = [], []
    LL, DUL = [], []

    for i in range(len(th)):

        b = bd[i]
        d = dul[i]
        ll = ll15[i]

        # enforce LL < DUL
        if ll >= d:
            ll = d - EPS

        # AirDry rule
        depth = sum(th[:i+1])

        if depth <= 400:
            ad = 0.5 * ll
        else:
            ad = ll

        ad = min(ad, ll - EPS)

        # porosity
        por = 1 - (b / PD)

        # SAT rule
        sat = min(por - EPS, d + 0.05)
        sat = max(sat, d + EPS)

        if d >= sat:
            d = sat - EPS

        AirDry.append(ad)
        LL.append(ll)
        DUL.append(d)
        SAT.append(sat)

    return AirDry, LL, DUL, SAT


# =========================================================
# Ensure APSIM-safe values (no NaN / None)
# =========================================================
def ensure_no_nan(arr):
    return [0.1 if (v is None or pd.isna(v)) else v for v in arr]


# =========================================================
# Replace Physical node using template + observed data
# Template thickness defines layering
# =========================================================
def overwrite_physical(physical, bd_obs, dul_obs, ll15_obs):

    th = physical["Thickness"]

    template_BD = physical["BD"]
    template_LL15 = physical["LL15"]
    template_DUL = physical["DUL"]

    # merge data
    bd = merge_profiles(template_BD, bd_obs)
    dul = merge_profiles(template_DUL, dul_obs)
    ll15 = merge_profiles(template_LL15, ll15_obs)

    # reconstruct
    AirDry, LL15, DUL, SAT = reconstruct_water(th, bd, dul, ll15)

    # clean
    bd = ensure_no_nan(bd)
    LL15 = ensure_no_nan(LL15)
    DUL = ensure_no_nan(DUL)
    SAT = ensure_no_nan(SAT)
    AirDry = ensure_no_nan(AirDry)

    # assign
    physical["BD"] = bd
    physical["LL15"] = LL15
    physical["DUL"] = DUL
    physical["SAT"] = SAT
    physical["AirDry"] = AirDry


# =========================================================
# Ensure SoilCrop LL ≤ DUL
# =========================================================
def fix_soilcrop(physical):

    dul = physical["DUL"]

    for crop in physical["Children"]:
        if "LL" not in crop:
            continue

        ll = crop["LL"]

        for i in range(len(ll)):
            if i < len(dul):
                ll[i] = min(ll[i], dul[i] - EPS)

        crop["LL"] = ll


# =========================================================
# Extract initial soil water (pre-sowing)
# =========================================================
def get_initial_water(water, expno, sitekey):

    g = water[
        (water["ExpNo"] == expno) &
        (water["Experiments::SiteKey"] == sitekey) &
        (water["EventName"] == "PreSowing")
    ]

    if g.empty:
        return None

    g = g.copy()
    g["Vol_mm.mm"] = g["Vol_mm.mm"].apply(safe_float)

    return g.groupby("LayerNo")["Vol_mm.mm"].mean().sort_index().tolist()


# =========================================================
# Extract nitrogen profile
# =========================================================
def get_nitrogen(nitro, expno, sitekey):

    g = nitro[
        (nitro["ExpNo"] == expno) &
        (nitro["Experiments::SiteKey"] == sitekey)
    ]

    if g.empty:
        return None, None

    g = g.copy()
    g["NitrateNitrogen_kg.ha"] = g["NitrateNitrogen_kg.ha"].apply(safe_float)
    g["AmmoniumNitrogen_kg.ha"] = g["AmmoniumNitrogen_kg.ha"].apply(safe_float)

    no3 = g.groupby("LayerNo")["NitrateNitrogen_kg.ha"].mean().tolist()
    nh4 = g.groupby("LayerNo")["AmmoniumNitrogen_kg.ha"].mean().tolist()

    return no3, nh4


# =========================================================
# Write SW into APSIM node
# =========================================================
def set_initial_water(soil, SW):

    if SW is None:
        return

    node = next((c for c in soil["Children"] if "Water" in c["$type"]), None)
    if node:
        node["InitialValues"] = SW


# =========================================================
# Write nitrogen into APSIM nodes
# =========================================================
def set_nitrogen(soil, NO3, NH4):

    for node in soil["Children"]:
        if node.get("Name") == "NO3" and NO3:
            node["InitialValues"] = NO3
        if node.get("Name") == "NH4" and NH4:
            node["InitialValues"] = NH4


# =========================================================
# Select correct template soil (handles proxy cases)
# =========================================================
def get_template_for_experiment(sitekey, apsim, proxy_map):

    if sitekey in proxy_map:
        target = proxy_map[sitekey]
        for s in apsim["Children"]:
            if s["Name"] == target:
                return s

    return apsim["Children"][0]


# =========================================================
# Build one APSIM soil
# =========================================================
def build_apsim_soil(template, sitekey, char, water, nitro, meta):

    expno = meta.loc[sitekey, "ExpNo"]
    sitename = meta.loc[sitekey, "SiteName"]

    result = get_characterisation_profile(char, expno, sitekey)
    if result is None:
        return None

    bd, dul, ll15 = result

    soil = copy.deepcopy(template)
    soil["Name"] = f"{expno}_{sitename}"

    physical = next(c for c in soil["Children"] if "Physical" in c["$type"])

    overwrite_physical(physical, bd, dul, ll15)
    fix_soilcrop(physical)

    SW = get_initial_water(water, expno, sitekey)
    NO3, NH4 = get_nitrogen(nitro, expno, sitekey)

    set_initial_water(soil, SW)
    set_nitrogen(soil, NO3, NH4)

    return soil


# =========================================================
# Build all soils
# =========================================================
def build_all_soils(apsim, char, water, nitro, meta, proxy_map):

    soils = []

    for sitekey in char["Experiments::SiteKey"].unique():

        template = get_template_for_experiment(sitekey, apsim, proxy_map)

        soil = build_apsim_soil(template, sitekey, char, water, nitro, meta)

        if soil:
            soils.append(soil)

    return soils


# =========================================================
# Replace APSIM soils with new ones
# =========================================================
def replace_soils_in_apsim(apsim, soils):

    apsim["Children"] = [
        c for c in apsim["Children"]
        if "Soil" not in c.get("$type", "")
    ] + soils

    return apsim


# %%
proxy_map = {
    "2019_NSW_Greenethorpe_Mixed_Detailed": "2022010_Greenethorpe",
    "2024_NSW_Greenethorpe_Mixed_NFix": "2022010_Greenethorpe",
    "2022_NSW_Methul_Lentil_Satellite": "Methul",
    "2024_Vic_Walpeup_Lentil_Satellite": "Walpeup"
}

# %%
# -----------------------------
# LOAD DATA
# -----------------------------
char = pd.read_csv("Database\\NaPA_SoilCharacterisation.csv")
water = pd.read_csv("Database\\NaPA_SoilWater.csv")
nitro = pd.read_csv("Database\\NaPA_SoilNitrogen.csv")

meta = pd.read_csv("Database\\NaPA_Experiment.csv")
meta.set_index("SiteKey", inplace=True)

char.columns = char.columns.str.strip()
water.columns = water.columns.str.strip()
nitro.columns = nitro.columns.str.strip()

# -----------------------------
# LOAD APSIM LIBRARY
# -----------------------------
with open("NAPA_soils.json") as f:
    apsim = json.load(f)


# -----------------------------
# DEFINE PROXY MAP
# -----------------------------
proxy_map = {
    # example:
    # "2022_NSW_Methul_Lentil_Satellite": "Methul"
}


# -----------------------------
# BUILD SOILS
# -----------------------------
all_soils = build_all_soils(apsim, char, water, nitro, meta, proxy_map)

print(f"Built {len(all_soils)} soils")


# -----------------------------
# WRITE TO APSIM FILE
# -----------------------------
apsim_new = replace_soils_in_apsim(apsim, all_soils)

with open("Rebuilt_Soil_Library.apsimx", "w") as f:
    json.dump(apsim_new, f, indent=2)

print("Saved: Rebuilt_Soil_Library.apsimx")
