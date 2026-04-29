#!/bin/bash
# Output dhf_country_name and iso3c columns only
cut -d',' -f1,3 "$(dirname "$0")/../../../data/codebooks/countries_data.csv"
