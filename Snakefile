# Author: Corrado Caudek
# Project: Groundhog Day


from pathlib import Path
import os

print(f"Current directory: {Path.cwd()}")
print(f"Home directory: {Path.home()}")


## Configuration file
configfile: "config.yaml"


# Run all analyses
rule all:
    input:
        "data/prep/groundhog_raw.RDS",
        "data/prep/groundhog_clean.RDS",
        "data/prep/groundhog_hddmrl_data.csv",
        "results/brms/fitted_models/brms_moodpre_1.RDS",
        "results/brms/tables/brms_moodpre_1.csv",
        "workflows/report/control_report.html",


# include: "workflows/rules/common.smk"


# Read individual PRL files and create a single file
# with all subjects' raw data.
rule read_data:
    input:
        xlsx=config["excel_codes"],
        dir_data=config["data_dir"],
    output:
        rds=config["complete_data_raw"],
    log:
        "logs/read_data.log",
    script:
        "workflows/scripts/import_mpath_data.R"


# Data cleaning.
rule data_wrangling:
    input:
        rds=config["complete_data_raw"],
    output:
        clean=config["cleaned_data"],
    log:
        "logs/data_wrangling.log",
    script:
        "workflows/scripts/data_wrangling.R"


# Effect of control on mood_pre.
rule moodpre_tilda_control:
    input:
        clean=config["cleaned_data"],
    output:
        fit=config["brms_fit_1"],
        csv=config["brms_table_1"],
    log:
        "logs/moodpre_tilda_control.log",
    script:
        "workflows/scripts/brms_moodpre_control.R"


rule control_report:
    input:
        clean=config["cleaned_data"],
    output:
        "workflows/report/control_report.html",
    script:
        "workflows/scripts/control.Rmd"


# Write input file for HDDMrl.
rule data_for_hddmrl:
    input:
        clean=config["cleaned_data"],
    output:
        hddmrl=config["hddmrl_data"],
    log:
        "logs/data_for_hddmrl.log",
    script:
        "workflows/scripts/hddmrl_data.R"


# Success and failure messages
onsuccess:
    print("\n 🎉 Success! The Snakemake workflow is completed.\n")


onerror:
    print("\n ⛔️ Error! The Snakemake workflow aborted.\n")
