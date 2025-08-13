# ECO-SG-ML_LAI

This is a tool to collocate the LAI data with the land cover data from the ECO-SG-ML map.

## Method:

The tool:
* Reads the compressed global LAI data, combined with the ECO-SG covers
* Reads the ECO-SG-ML cover data
* Clips the area from the LAI data corresponding the ECO-SG-ML map (Europe)
* Interpolates the LAI data on the ECO-SG-ML grid with the nearest neighbour method (simply repeating pixels)
* Extrapolates the LAI data to the areas where a new land appears according to the ECO-SG-ML
* Combines the new LAI with the ECO-SG-ML covers, compresses and writes the new fields.

## Description:

`Env_system` - system configuration file  
`LAI2ML`     - script to run the tool  
`GlobVisu`   - small utility to visualize data with GrADS  
`data`       - directory with links to data  
`src`        - directory with the code  
`util`       - directory with additional code, to run visualization  

## Usage:

0. Clone the project:

   `git clone git@github.com:kourzeneva/ECO-SG-ML_LAI.git`

   ... or download the zip file and unzip it.

2. Make links to files with ECO-SG-ML cover data and LAI data. From `ECO-SG-ML_LAI/data`:
    
   `ln -s .../ecosgml_1.dir . `   
   `ln -s .../LAI_????_c.dir . ` 

Note! 
- The tool works only with ECO-SG-ML cover map in `INTEGER(1)`
- `*.dhr` files are not needed. All parameters are hard-coded (see below where to find them).

2. Create the working directory with the name `wrk`. All code will be copied there, data will be linked there and the tool will run there. Results will be also located there. From `ECO-SG-ML_LAI`:  

   `mkdir wrk`

   This directory should be created only once. If exists, it will be refreshed automatically.
   
3. To save results, it is recommended also to create the directory `res`. You may also creare the directory `scripts`, for some future needs. From `ECO-SG-ML_LAI`:

   `mkdir res`  
   `mkdir scripts`
   
4. Edit the file `Env_system`. Variables to be adjusted to your setup are:

   `Dir` - enter the directory path here  
   `BitmapName1`, `BitmapName2` - file names for the ECO-SG-ML and LAI data 

5. Run the `LAI2ML` script. From `ECO-SG-ML_LAI`:

   `./LAI2ML`

   Processing of one file takes appr. 10 min.
   
   Note! The "endian" problem may appear, since the binary files are processed. The tool was tested on the laptop and ECMWF mashine, but still, the problem may happen.

6. Results can be found in the file `wrk/bitmap_lai_ml_comp`. This file is of appr. 10G. It should be moved to the `res` directory manually and renamed as the used wants. Something like:

   `mv wrk/bitmap_lai_ml_comp res/.`  
   `mv res/bitmap_lai_ml_comp res/LAI_????_c_ml.dir`

   The tool does not create the `*.hdr` files, this should be done manually.
   
7. Main parameters of the fields can be found in `src/Bitmap_ML_LAI.f90`. They can be changed, but one need to be careful, of course.

8. Visualisation tool `GlobVisu` comes without documentation at the moment.  
