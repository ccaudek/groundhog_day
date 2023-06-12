# Important note:
# All paths defined in this configuration file must be either
# absolute or relative to the location of the Snakefile!

import os
from pathlib import Path

# Configuration file
if len(config) == 0:
    if os.path.isfile("config/config.yaml"):

        configfile: "config/config.yaml"

    else:
        sys.exit(
            "".join(
                [
                    "Make sure there is a config.yaml file in ",
                    os.getcwd(),
                    " or specify one with the --configfile commandline parameter.",
                ]
            )
        )


## Sanitize provided input and output directories
import re


def getpath(str):
    if str in ["", ".", "./"]:
        return ""
    if str.startswith("./"):
        regex = re.compile("^\./?")
        str = regex.sub("", str)
    if not str.endswith("/"):
        str += "/"
    return str


print(f"Current directory: {Path.cwd()}")
print(f"Home directory: {Path.home()}")

prepdir = getpath(config["output_prep"])
brmsdir = getpath(config["output_brms"])

Renv = "workflows/envs/environment_R.yaml"


# Run all analyses
rule all:
    input:
        os.path.join(prepdir, "groundhog_raw.RDS"),
        os.path.join(prepdir, "groundhog_clean.RDS"),
        # os.path.join(prepdir, "groundhog_hddmrl_data.csv"),
        # os.path.join(brmsdir, "fitted_models", "brms_moodpre_1.RDS"),
        # os.path.join(brmsdir, "tables", "brms_moodpre_1.csv"),
        # "workflows/report/control_report.html",


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
    conda:
        Renv
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
    conda:
        Renv
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
    conda:
        Renv
    script:
        "workflows/scripts/brms_moodpre_control.R"


rule control_report:
    input:
        clean=config["cleaned_data"],
    output:
        "workflows/report/control_report.html",
    conda:
        Renv
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
    conda:
        Renv
    script:
        "workflows/scripts/hddmrl_data.R"


# Success and failure messages
onsuccess:
    print("\n 🎉 Success! The Snakemake workflow is completed.\n")


onerror:
    print("\n ⛔️ Error! The Snakemake workflow aborted.\n")
