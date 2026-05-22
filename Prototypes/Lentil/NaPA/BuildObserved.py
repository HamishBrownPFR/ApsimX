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

file_path = "Observed.xlsx"

# Load the Excel file
xls = pd.ExcelFile(file_path)

header_summary = {}

for sheet in xls.sheet_names:
    df = pd.read_excel(xls, sheet_name=sheet, nrows=0)  # read only header
    header_summary[sheet] = list(df.columns)

# Print results cleanly
for sheet, headers in header_summary.items():
    print(f"\nSheet: {sheet}")
    for h in headers:
        print(f"  - {h}")

