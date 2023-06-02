# Read individual PRL data files and create a single file
# with all the subjects' raw data.
rule read_data:
    input:
        excel_codes=f"{excel_codes_path}",
        data_dir=f"{subj_data_dir}",
    output:
        out=f"{raw_data}",
    script:
        "workflows/scripts/import_mpath_data.R"
