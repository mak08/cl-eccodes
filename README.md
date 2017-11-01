# cl-eccoodes
CFFI bindings for GRIB-API functions from the ecCodes library (not complete, read access only)

## Description

cl-eccodes provides bindings to [GRIB API](https://software.ecmwf.int/wiki/display/ECC/ecCodes+Home) functions for reading GRIB2 files.


## Prerequisites

*  Install ecCodes

   ```bash
   $ wget https://software.ecmwf.int/wiki/download/attachments/45757960/eccodes-2.5.0-Source.tar.gz
   $ tar xzf eccodes-2.5.0-Source.tar.gz
   $ mkdir eccodes_build; cd eccodes_build
   $ cmake ../eccodes-2.5.0-Source/ -DENABLE_ECCODES_THREADS=ON -DCMAKE_INSTALL_PREFIX=/usr/local
   $ make
   $ ctest
   $ sudo make install
   ```