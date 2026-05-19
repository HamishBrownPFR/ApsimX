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
inputfile = "NaPA_soils.apsimx"
outputfile = "NaPA_soils_fiexd.apsimx"

# %%
import json

PD = 2.65
EPS = 0.005

def porosity_from_bd(bd):
    if bd is None:
        return None
    return 1.0 - (bd / PD)

def cumulative_depths(thickness):
    depths = []
    total = 0
    for t in thickness:
        total += t
        depths.append(total)
    return depths

def pad_or_trim(arr, target_len):
    if arr is None:
        return [None] * target_len

    # convert "NaN" strings to None while we're here
    cleaned = []
    for v in arr:
        if isinstance(v, str) and v.lower() == "nan":
            cleaned.append(None)
        else:
            cleaned.append(v)

    arr = cleaned

    if len(arr) > target_len:
        return arr[:target_len]

    if len(arr) < target_len:
        last = arr[-1] if len(arr) > 0 else None
        return arr + [last] * (target_len - len(arr))

    return arr

def fix_water_profile(physical):
    th = physical["Thickness"]

    LL15, sources = get_min_crop_LL(physical)
    if LL15 is None:
        return False

    dul = pad_or_trim(physical.get("DUL"), len(th))
    bd  = pad_or_trim(physical.get("BD"), len(th))
    LL15 = pad_or_trim(LL15, len(th))

    depths = cumulative_depths(th)

    AirDry = []
    new_LL15 = []
    new_DUL = []
    SAT = []

    for i in range(len(th)):
        ll = LL15[i]
        d  = dul[i]
        b  = bd[i]

        if None in (ll, d, b):
            # fallback: keep existing values if possible
            AirDry.append(None)
            new_LL15.append(ll)
            new_DUL.append(d)
            SAT.append(None)
            continue

        por = porosity_from_bd(b)

        # enforce LL < DUL
        if ll >= d:
            ll = d - EPS

        # AirDry rule
        if depths[i] <= 400:
            ad = 0.5 * ll
        else:
            ad = ll

        ad = min(ad, ll - EPS)

        # SAT rule
        sat = max(d + EPS, d + 0.05)
        sat = min(sat, por - EPS)

        if sat <= d:
            sat = d + EPS

        if sat >= por:
            sat = por - EPS

        if d >= sat:
            d = sat - EPS

        AirDry.append(ad)
        new_LL15.append(ll)
        new_DUL.append(d)
        SAT.append(sat)

    physical["AirDry"] = AirDry
    physical["LL15"] = new_LL15
    physical["DUL"] = new_DUL
    physical["SAT"] = SAT

    return True

def get_min_crop_LL(physical):
    crop_LLs = []
    crop_names = []

    for crop in physical["Children"]:
        ll = crop.get("LL")
        if ll is not None:
            crop_LLs.append(ll)
            crop_names.append(crop.get("Name", "unknown"))

    if not crop_LLs:
        return None, None

    # find minimum across crops layer-by-layer
    min_LL = []
    num_layers = len(crop_LLs[0])

    for i in range(num_layers):
        vals = []
        for arr in crop_LLs:
            if i < len(arr):
                v = arr[i]

                # clean value
                if isinstance(v, str):
                    try:
                        v = float(v)
                    except:
                        continue

                if v is not None:
                    vals.append(v)

        if vals:
            min_LL.append(min(vals))
        else:
            min_LL.append(None)

    return min_LL, crop_names

def process_soil(soil):
    children = soil["Children"]
    
    physical = next((c for c in children if "Physical" in c["$type"]), None)

    if physical is None:
        return f"{soil.get('Name', 'unknown')}: no Physical node"


    # find lentil
    lentil = None
    for c in physical["Children"]:
        if c["Name"].lower().startswith("lentil"):
            lentil = c
            break

    if lentil is None:
        return f"{soil['Name']}: no lentil data"

    fix_water_profile(physical)
    return None


def process_file(infile, outfile):
    with open(infile) as f:
        data = json.load(f)

    soils = data["Children"]

    issues = []

    for soil in soils:
        msg = process_soil(soil)
        if msg:
            issues.append(msg)

    with open(outfile, "w") as f:
        json.dump(data, f, indent=2)

    return issues



# %%
proxy_map = {
    "2019_NSW_Greenethorpe_Mixed_Detailed": "2022010_Greenethorpe",
    "2024_NSW_Greenethorpe_Mixed_NFix": "2022010_Greenethorpe",
    "2022_NSW_Methul_Lentil_Satellite": "Methul",
    "2024_Vic_Walpeup_Lentil_Satellite": "Walpeup"
}

def get_template_for_experiment(exp_name, apsim, proxy_map):

    # check if proxy applies
    if exp_name in proxy_map:
        target_name = proxy_map[exp_name]
    else:
        return apsim["Children"][0]  # default template

    # find matching soil
    for soil in apsim["Children"]:
        if soil["Name"] == target_name:
            return soil

    # fallback
    return apsim["Children"][0]


# %%

# %%
# ---- RUN ----
issues = process_file(inputfile,outputfile)

for i in issues:
    print(i)
