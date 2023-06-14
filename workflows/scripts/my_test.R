#!/usr/bin/env Rscript

# This is a test of the smk files. See the following tutorial:
# https://github.com/fritzbayer/snakemake-with-R/blob/main/scr/test-script.R

# libraries
suppressPackageStartupMessages(library("argparse"))
suppressPackageStartupMessages(library("rio"))
suppressPackageStartupMessages(library("tidyverse"))

# parse data
parser <- ArgumentParser(description= 'This progrom does stuff')
parser$add_argument('--input', '-i', help= 'Input file')
parser$add_argument('--output', '-o', help= 'Output file')
# parser$add_argument('--myFactor', '-f', help= 'Imported variable', type= 'double')
xargs <- parser$parse_args()

# Read data from Excel file
dat <- rio::import(xargs$input) 
# Do something with the data.
foo <- data.frame(x = dat$coin)
# Save a csv file in the specified location.
rio::export(foo, xargs$output)
