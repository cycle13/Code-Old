---
layout:     post
title:      "Python读写netcdf,grib与mat格式文件"
subtitle:   "Read and Write netcdf grib and mat files using Python"
date:       2017-02-02
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
    - NetCDF
---

# Read NetCDF file with scipy.io.netcdf

Scipy.io.netcdf doesn’s support writes at present and can only read NetCDF 3 files. To read a NetCDF file

<pre class="prettyprint lang-py linenums">
from scipy.io import netcdf 
f = netcdf.netcdf_file('test.nc', 'r') 
print f.history 
time = f.variables['time'] 
print time.units 
print time.shape 
print time[:] 
f.close()
</pre>

# Read/Write NetCDF file with Scientific.IO.NetCDF

To create a NetCDF file

<pre class="prettyprint lang-py linenums">
from Scientific.IO.NetCDF import NetCDFFile 
import numpy as np 
f = NetCDFFile('scientificio.nc', 'w') 
# dimension 
f.createDimension('time', 12) 
# variable 
time = f.createVariable('time', 'd', ('time',)) 
# data 
time[:] = np.random.uniform(size=12) 
f.close()
</pre>

To read the file

<pre class="prettyprint lang-py linenums">
from Scientific.IO.NetCDF import NetCDFFile 
import matplotlib.pyplot as plt 
f = NetCDFFile('scientificio.nc') 
fig = plt.figure() 
ax = fig.add_subplot(111) 
ax.plot(f.variables['time']) 
plt.show()
</pre>

# Read/Write NetCDF file with netcdf4-python

To create a NetCDF file

<pre class="prettyprint lang-py linenums">
from netCDF4 import Dataset 
import numpy as np

root_grp = Dataset('test.nc', 'w', format='NETCDF4')
root_grp.description = 'Example temperature data'

# dimensions 
root_grp.createDimension('time', None) 
root_grp.createDimension('lat', 72) 
root_grp.createDimension('lon', 144)

# variables 
times = root_grp.createVariable('time', 'f8', ('time',)) 
latitudes = root_grp.createVariable('latitude', 'f4', ('lat',)) 
longitudes = root_grp.createVariable('longitude', 'f4', ('lon',)) 
temp = root_grp.createVariable('temp', 'f4', ('time', 'lat', 'lon',))

# data 
lats = np.arange(-90, 90, 2.5) 
lons = np.arange(-180, 180, 2.5) 
latitudes[:] = lats 
longitudes[:] = lons

for i in range(5): 
    temp[i,:,:] = np.random.uniform(size=(len(lats), len(lons)))

# group
# my_grp = root_grp.createGroup('my_group')
root_grp.close()
</pre>

To read the file

<pre class="prettyprint lang-py linenums">
from netCDF4 import Dataset 
import pylab as pl 
root_grp = Dataset('test.nc') 
temp = root_grp.variables['temp']

for i in range(len(temp)): 
    pl.clf() 
    pl.contourf(temp[i]) 
    pl.show() 
    raw_input('Press enter.')
</pre>

# Read/Write Grib files with pygrib

To read a Grib file

<pre class="prettyprint lang-py linenums">
import pygrib 
grbs = pygrib.open('sampledata/flux.grb')
grbs.seek(2) 
grbs.tell() 
grb = grbs.read(1)[0] 
print grb 
grb = grbs.select(name='Maximum temperature')[0]
</pre>

To write a Grib file

<pre class="prettyprint lang-py linenums">
import pygrib 
grbout = open('test.grb','wb') 
grbout.write(msg) 
grbout.close() 
print pygrib.open('test.grb').readline()
</pre>

# Read/Write Matlab .mat file

To read a .mat file

<pre class="prettyprint lang-py linenums">
import scipy.io as sio 

mat_contents = sio.loadmat('data.mat') 
print mat_contents
</pre>

To write a .mat file

<pre class="prettyprint lang-py linenums">
import numpy as np
import scipy.io as sio

vect = np.arange(10)
print vect.shape
sio.savemat('data.mat', {'vect':vect})
</pre>
