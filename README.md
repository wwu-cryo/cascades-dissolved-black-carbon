# Cascades snow and meltwater DBC

This is a repository containing data and code used in the manuscript "Dissolved black carbon in Cascade snow, meltwater, and a downstream river" (Vaux et al., _in preparation_).

## Requirements

MATLAB, R, and offline [SNICAR-AD v3](https://github.com/mflanner/SNICARv3) (Flanner et al., 2021) are needed for this analysis. For more information on the BPCA method, including template spreadsheets for BPCA and DBC determination, see [Barton & Wagner, 2022](https://www.protocols.io/view/measuring-dissolved-black-carbon-in-water-via-aque-5qpvoy2b9g4o/v2).

## Usage

- `data` .xlsx and .csv files used for DBC analysis, including:
  - `dbc-data`
    - `dbc-data.csv` contains data used for R analysis, including DBC, BPCAs, broadband albedo (from SNICAR), and instantaneous radiative forcing (IRF). Also includes GPS coordinates and other sampling information.
    - `dbc-master-spreadsheet.xlsx` based on the Barton & Wagner (2022) template for the BPCA method (i.e., SPE) and calculating DBC based on individual BPCA concentrations. Also includes GPS coordinates and other sampling information.
  
  - `irf-data`
    - `dbc-irf-input.xlsx` input spreadsheet to run the `RadForce.m` script to calculate IRF based on SNICAR runs of DBC data (see `dbc-SNICAR-output`).
      - Each tab corresponds to an individual sample. Within each tab, there are 5 important columns: A (wavelength interval in nm), B (clean snow reflectance), and C (BC in snow reflectance), all from the `dbc-SNICAR-output.xlsx` spreadsheet (samples are separated by tab), and E (wavelength interval in nm) and F (spectral irradiance, "global to perpendicular plane" in W/m<sup>2</sup>/nm from the PV Lighthouse Solar Spectrum Calculator as summarized below), both from the `spectral-irradiance-data` files for individual samples.
    - `dbc-irf-output.xlsx` IRF output in W/m<sup>2</sup> for each individual sample. 
  
  - `naaps-data` Navy Aerosol Analysis and Prediction System reanalysis model precipitation data (.xlsx) and daily smoke deposition fluxes (.csv) for summers 2022 and 2023 on Mount Baker.
  
  - `nwac-snow-data` .csv files from the [Northwest Avalanche Center's](https://nwac.us/data-portal/) weather monitoring stations of snow depth on Mount Baker (Heather Meadows) and Mount Rainier (Paradise), from which snow depth was estimated to run the SNICAR model (see `dbc-SNICAR-input`).
    
  - `SNICAR-data`
    - `dbc-SNICAR-input.xlsx` used to run the SNICAR model (`SNICAR_auto.m`).
      - Each sample is an individual tab. Within each tab, column A is the SNICAR parameter name, column B ("default") is clean snow albedo (without BC), and column C uses the same physical snowpack and atmospheric conditions as column B but with BC concentrations to model BC in snow albedo.
      - For more information about the SNICAR input variables, see the `SNICAR_Template.xlsx`.
    - `dbc-SNICAR-output.xlsx` output spreadsheet from the `SNICAR_auto.m` script below.
      - Each tab in the spreadsheet corresponds to an individual sample, and the final tab of the spreadsheet shows broadband albedo for each sample. Within each tab, column A indicates the wavelength interval (nm), column B shows the clean snow reflectance (based on the "default" column in the `dbc-SNICAR-input.xlsx` spreadsheet), and column C shows the BC in snow reflectance (based on column C from the `dbc-SNICAR-input.xlsx` spreadsheet).
    - `SNICAR-plotting-2022.csv` used for R plotting of spectral reflectance.
      - Within the .csv, the Wavelength column (A) is the SNICAR wavelength interval in nm, the Clean Snow column (B) is the averaged clean snow reflectance from each individual tab of the `dbc-SNICAR-output`, and the following columns are the BC in snow reflectance for each individual sample in 2022 from the `dbc-SNICAR-output.xlsx` spreadsheet.
    - `SNICAR-plotting-2023.csv` used for R plotting of spectral reflectance.
      - Within the .csv, the Wavelength column (A) is the SNICAR wavelength interval in nm, the Clean Snow column (B) is the averaged clean snow reflectance from each individual tab of the `dbc-SNICAR-output.xlsx`, and the following columns are the BC in snow reflectance for each individual sample in 2023 from the `dbc-SNICAR-output.xlsx` spreadsheet.
      
  - `spectral-irradiance-data` .xlsx files matching each sampling date/location from the [PV Lighthouse Solar Spectrum Calculator](https://www2.pvlighthouse.com.au/calculators/solar%20spectrum%20calculator/solar%20spectrum%20calculator.aspx). These spectral irradiance data were used to calculate IRF at noon (i.e., the daily maximum) with the `RadForce.m` script.
    - Adjust the minimum wavelength in the "Options" tab to 205 nm to match the SNICAR minimum wavelength, enter the date, time, and coordinate information into the "Calculator" tab, download the .xlsx file, and copy the "Global to perpendicular plane (W/m<sup>2</sup>/nm)" column from the "Spectral irradiance tab" into the `dbc-IRF-input.xlsx` spreadsheet for each sample.  

- `src`- code used for DBC analysis, including:
  - `SNICAR`
    - `SNICAR_Template.xlsx` template spreadsheet to run the offline SNICAR model. See `Metadata` tab for more information and `Location` tabs for examples.
    - `SNICAR_auto.m` code written by Anne Wilce to run the offline SNICAR model.
      - input: `SNICAR_Template.xlsx` (or, for this project, `dbc-SNICAR-input.xlsx`)
      - output: `dbc-SNICAR-output.xlsx`
  - `DBC_Analysis.R` code written by Sally Vaux for figures and statistics in the manuscript based on DBC concentrations, BPCA concentrations, proportions, and ratios, NAAPS deposition fluxes, SNICAR albedo, and river discharge data.
    - inputs: `dbc-data.csv`, `SNICAR-plotting-2022.csv`, `SNICAR-plotting-2023.csv`
  - `RadForce.m` code written by Anne Wilce to calculate IRF from spectral reflectance values.
    - input: `dbc-irf-input.xlsx`
    - output: `dbc-irf-output.xlsx`
