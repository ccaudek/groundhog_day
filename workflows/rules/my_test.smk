# The paths specified within the `smk` file are relative to the project's
# directory (the dir where Snakefile is located).
# In a `smk` file, an Rscript can only be run from a shell directive.
rule the_beautiful_rule:
    input:
        csv="data/raw/data.xlsx",
    output:
        csv="data/my_test.csv",
    shell:
        "Rscript workflows/scripts/my_test.R \
        --input {input.csv} \
        --output {output.csv}"
