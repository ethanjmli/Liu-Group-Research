# -*- coding: utf-8 -*-
"""
Created on Sun May 22 13:29:54 2022

@author: yhang2
"""
import pandas as pd
import glob 
import numpy as np
from datetime import datetime, date
from functools import reduce
import datetime
import os
os.environ["PROJ_LIB"] = "C:\\Utilities\\Python\\Anaconda\\Library\\share"; 
from mpl_toolkits.basemap import Basemap
import matplotlib as mpl 
import matplotlib.pyplot as plt
from sklearn.ensemble import RandomForestRegressor
from sklearn.inspection import permutation_importance
import matplotlib.pyplot as plt
import joblib
from scipy import stats
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import cross_val_predict
from sklearn.metrics import r2_score
from sklearn.metrics import mean_squared_error
from sklearn.cluster import KMeans
from sklearn.cluster import DBSCAN
from scipy.cluster.vq import kmeans2, whiten


so4_obs_path = 'C:\\Users\\yhang2\\OneDrive - Emory University\\Desktop\\China_PM_species\\SO4\\Observations\\'
no3_obs_path = 'C:\\Users\\yhang2\\OneDrive - Emory University\\Desktop\\China_PM_species\\NO3\\Observations\\'

predictor_path = 'E:\\Data_Predictors_GridID_Level\\'
save_path = 'C:\\Users\\yhang2\\OneDrive - Emory University\\Desktop\\China_PM_species\\SO4\\Predictions\\'
fig_path = 'C:\\Users\\yhang2\\OneDrive - Emory University\\Desktop\\China_PM_species\\NO3\\Figures\\'
fig_path_save =  'C:\\Users\\yhang2\\OneDrive - Emory University\\Desktop\\China_PM_species\\SO4\\Figures\\'

obs = pd.read_csv('C:\\Users\\yhang2\\OneDrive - Emory University\\Desktop\\China_PM_species\\so4\\Observations\\so4_obs_data_2005_18_06272022.csv')
#21135
obs_grid = obs[['GridID', 'Lat', 'Lon', 'year', 'month', 'day', 'ground_so4']].drop_duplicates().groupby(['GridID', 'Lat', 'Lon', 'year', 'month', 'day']).mean().reset_index()
#18235
obs_grid.to_csv(save_path + 'so4_grid_level_data.csv', index = False)

#obs_grid.GridID.drop_duplicates()
#68

#obs_grid.year.drop_duplicates()
#no data in 2012

#Add predictors
li = []
for year in [2005, 2006, 2007, 2008, 2009, 2010, 2011, 2013, 2014, 2015, 2016, 2017, 2018]:#no data in 2012
    print(year)
    #year = 2013
    data = obs_grid.loc[obs_grid['year'] == year] 
    print(len(data))
    pop=pd.read_csv(predictor_path+'Annual\\Landscan_Population\\Population_' + str(year) + '.csv')
    misr=pd.read_csv(predictor_path+'Annual\\MISR\\MISR_' + str(year) + '.csv').drop('counts', axis=1) 
    pm_martin=pd.read_csv(predictor_path+'Annual\\PM_Martin\\PM25_Martin_' + str(year) + '.csv')
    data_ann = reduce(lambda x, y: pd.merge(x, y, on = ['GridID']), [data,pop,misr,pm_martin])

    cams_emission=pd.read_csv(predictor_path+'Monthly\\CAMS_Emissions\\CAMS_Emission_' + str(year) + '.csv')
    ceres=pd.read_csv(predictor_path+'Monthly\\CERES\\CF_CERES_' + str(year) + '.csv')
    ndvi=pd.read_csv(predictor_path+'Monthly\\NDVI\\NDVI_' + str(year) + '.csv')
    pm_xiao=pd.read_csv(predictor_path+'Monthly\\PM_Xiao\\PM25_Xiao_' + str(year) + '.csv')
    data_ann_mon=reduce(lambda x, y: pd.merge(x, y, on = ['GridID','month']), [data_ann,cams_emission,ceres,ndvi,pm_xiao])
    
    month_day = data_ann_mon[['month','day']].drop_duplicates()
    month = np.array(month_day['month'])
    day = np.array(month_day['day'])
    data_ann_mon_day_li = []
    for i in np.arange(len(month_day)):
        #i = 0
        data_ann_mon_day_select = data_ann_mon.loc[(data_ann_mon['month'] == month[i]) & (data_ann_mon['day'] == day[i])]
        cams_atm=pd.read_csv(predictor_path+'Daily\\CAMS_ATM_Composites\\CAMS_ATM_Composites_' + str(year) + "{0:0=2d}".format(month[i]) + "{0:0=2d}".format(day[i]) +'.csv')
        cmaq=pd.read_csv(predictor_path+'Daily\\CMAQ\\SO4_EC_OC_2005_18\\'+ str(year) + "{0:0=2d}".format(month[i]) + "{0:0=2d}".format(day[i]) +'.csv')
        geos=pd.read_csv(predictor_path+'Daily\\GEOS_5\\GEOS_' + str(year) + "{0:0=2d}".format(month[i]) + "{0:0=2d}".format(day[i]) +'.csv')
        merra=pd.read_csv(predictor_path+'Daily\\MERRA_2\\' + str(year) + "{0:0=2d}".format(month[i]) + "{0:0=2d}".format(day[i]) +'.csv')
        visibility=pd.read_csv(predictor_path+'Daily\\Visibility\\' + str(year) + "{0:0=2d}".format(month[i]) + "{0:0=2d}".format(day[i]) +'.csv')
        data_ann_mon_i=reduce(lambda x, y: pd.merge(x, y, on = ['GridID']), [data_ann_mon_day_select,cams_atm,cmaq,geos,merra,visibility])
        data_ann_mon_day_li.append(data_ann_mon_i)  
    data_ann_mon_day = pd.concat(data_ann_mon_day_li)
    print(len(data_ann_mon_day))
    li.append(data_ann_mon_day)  
    
data_with_predictors = pd.concat(li)
#17490 have some missing data in 2013 and 2014
#add
road = pd.read_csv(predictor_path+'One_Year\\Road_2014.csv')
urban = pd.read_csv(predictor_path+'One_Year\\Urban_Rural_2017.csv')
elevation = pd.read_csv(predictor_path+'One_Year\\Elevation.csv')[['GridID', 'elevation']]
data_with_predictors=reduce(lambda x, y: pd.merge(x, y, on = ['GridID']), [data_with_predictors,road,urban,elevation])
data_with_predictors.to_csv(save_path + 'so4_grid_level_data_with_predictors_all.csv', index = False)
#17838

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#plt study domain with averaged so4 concentration over the entire study period
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
def shiftedColorMap(cmap, start=0, midpoint=0.5, stop=1.0, name='shiftedcmap'):
    '''
    Function to offset the "center" of a colormap. Useful for
    data with a negative min and positive max and you want the
    middle of the colormap's dynamic range to be at zero.

    Input
    -----
      cmap : The matplotlib colormap to be altered
      start : Offset from lowest point in the colormap's range.
          Defaults to 0.0 (no lower offset). Should be between
          0.0 and `midpoint`.
      midpoint : The new center of the colormap. Defaults to 
          0.5 (no shift). Should be between 0.0 and 1.0. In
          general, this should be  1 - vmax / (vmax + abs(vmin))
          For example if your data range from -15.0 to +5.0 and
          you want the center of the colormap at 0.0, `midpoint`
          should be set to  1 - 5/(5 + 15)) or 0.75
      stop : Offset from highest point in the colormap's range.
          Defaults to 1.0 (no upper offset). Should be between
          `midpoint` and 1.0.
    '''
    cdict = {
        'red': [],
        'green': [],
        'blue': [],
        'alpha': []
    }

    # regular index to compute the colors
    reg_index = np.linspace(start, stop, 257)

    # shifted index to match the data
    shift_index = np.hstack([
        np.linspace(0.0, midpoint, 128, endpoint=False), 
        np.linspace(midpoint, 1.0, 129, endpoint=True)
    ])

    for ri, si in zip(reg_index, shift_index):
        r, g, b, a = cmap(ri)

        cdict['red'].append((si, r, r))
        cdict['green'].append((si, g, g))
        cdict['blue'].append((si, b, b))
        cdict['alpha'].append((si, a, a))

    newcmap = matplotlib.colors.LinearSegmentedColormap(name, cdict)
    plt.register_cmap(cmap=newcmap)

    return newcmap
shapefile_path = 'C:\\Users\\yhang2\\OneDrive - Emory University\\Desktop\\China_PM_species\\NO3\\Figures\\Shapefiles\\'
obs_avg = obs[['name_site_checked','ground_so4']].groupby([ 'name_site_checked']).mean().reset_index()
site_lat_lon = obs[['name_site_checked', 'lat_site', 'lon_site']].round(1).drop_duplicates()
so4 = pd.merge(obs_avg, site_lat_lon, on=['name_site_checked'])
so4.to_csv(so4_obs_path + 'averaged_so4_88_ground_sites.csv', index = False)
#88 obs sites, 68 grid ID  

lat = so4['lat_site'].values
lon = so4['lon_site'].values
so4_obs = so4['ground_so4'].values

parallels = np.linspace(0,50,6)
meridians = np.linspace(70,140,8)   

JJJ = pd.read_csv(fig_path + 'JJJ'+'_10km.csv')[['GridID', 'X_Lon', 'Y_Lat']]
PRD = pd.read_csv(fig_path + 'PRD'+'_10km.csv')[['GridID', 'X_Lon', 'Y_Lat']]
YRD = pd.read_csv(fig_path + 'YRD'+'_10km.csv')[['GridID', 'X_Lon', 'Y_Lat']]
CY = pd.read_csv(fig_path + 'CY'+'_10km.csv')[['GridID', 'X_Lon', 'Y_Lat']]

C = np.loadtxt(fig_path + 'blue_white_red.txt',  comments='#', delimiter=None, converters=None, skiprows=0, usecols=None, unpack=False, ndmin=0)
cmap = mpl.colors.ListedColormap(C/255.0)
bounds = np.linspace(0, 20, 21)
norm = mpl.colors.BoundaryNorm(bounds, cmap.N)
orig_cmap = cmap 
shrunk_cmap = shiftedColorMap(orig_cmap, start=0, midpoint=0.25, stop=1, name='shrunk')
parallels = np.linspace(0,50,6)
meridians = np.linspace(70,140,8)  

fig = plt.figure(figsize=(13,10))
m = Basemap(projection='lcc', resolution='l',lat_0=36.5, lon_0=102.5, width=5.5E6, height=4.5E6)
m.shadedrelief()
m.readshapefile(shapefile_path + 'shp\\ChengYu_Dissolve', 'ChengYu_Dissolve',drawbounds=True,linewidth=1.2, default_encoding='iso-8859-15', color ='r', zorder=3)
m.readshapefile(shapefile_path + 'shp\\YangtzeRiverDelta_Dissolve', 'YangtzeRiverDelta_Dissolve',drawbounds=True,linewidth=1.2, default_encoding='iso-8859-15', color ='r', zorder=3)
m.readshapefile(shapefile_path + 'shp\\PearlRiverDelta_Dissolve', 'PearlRiverDelta_Dissolve',drawbounds=True,linewidth=1.2, default_encoding='iso-8859-15', color ='r', zorder=3)
m.readshapefile(shapefile_path + 'shp\\JJJ_Areas_Dissolve', 'JJJ_Areas_Dissolve',drawbounds=True,linewidth=1.2, default_encoding='iso-8859-15', color ='r', zorder=3)
m.readshapefile(shapefile_path + 'province\\province', 'province',drawbounds=True,linewidth=1, default_encoding='iso-8859-15', color ='k')
#m.readshapefile(shapefile_path + 'county_nine_lines\\nine_lines\\Chineseb_9','Chineseb_9',drawbounds=True,linewidth=1, default_encoding='iso-8859-15', color ='k')
m.scatter(lon, lat, latlon=True, c= so4_obs, s = 130,  cmap = shrunk_cmap, edgecolors='k', alpha=0.8, norm=norm, zorder=3) #marker="^" ,alpha - transparent level
m.drawparallels(parallels,labels=[True,False,False,False],linewidth=0.5, size =15)
m.drawmeridians(meridians,labels=[False,False,False,True],linewidth=0.5, size =15)
cbar = plt.colorbar(orientation="horizontal", pad=0.1)#, extend ='max'
cbar.set_ticks(np.arange(0,26,2), )
cbar.ax.tick_params(labelsize=20) 
plt.show()
fig.savefig(fig_path_save +  'study_domain_88_sites.png', transparent=True)


#--------------------------------------------------------------------------------------------------------------
#build RF model
#--------------------------------------------------------------------------------------------------------------
shapefile_path = 'C:\\Users\\yhang2\\OneDrive - Emory University\\Desktop\\China_PM_species\\NO3\\Figures\\Shapefiles\\'
winter = pd.read_csv('E:\\Daily_Predictors\\' + 'predictors_20180105.csv')
summer = pd.read_csv('E:\\Daily_Predictors\\' + 'predictors_20180705.csv')

#--------------------------------------------------------------------------------------------------------------
#map each predictor
#--------------------------------------------------------------------------------------------------------------

fig = plt.figure(figsize=(10,8))
m = Basemap(projection='lcc', resolution='l',lat_0=36.5, lon_0=102.5, width=5.5E6, height=4.5E6)
m.scatter(winter['Lon'].values, winter['Lat'].values, latlon=True, c= winter['elevation'].values, s = 1, marker = 's', cmap='rainbow', alpha=0.8) #alpha - transparent level
plt.colorbar(orientation="horizontal", pad=0.1, extend ='max')
#plt.clim(0,100000)
#plt.show()
fig.savefig(save_path + 'daily_predictors_map\\' +  'elevation.png', transparent=True)


location_info = pd.read_csv(no3_obs_path + 'no3_68_GridID_info.csv')
#68 GridID in total
#new_data = pd.read_csv(save_path + 'so4_grid_level_data_with_predictors_all.csv')
new_data = pd.read_csv(save_path + 'data_GridID_without_extremes.csv')
#new_data_low =  new_data_raw.loc[new_data_raw.ground_so4 >= 0.1]
#17659
#new_data =  new_data_low.loc[new_data_low.ground_so4 < 150]
#17655 
observations = np.array(new_data['ground_so4'])
#'non_abso', 'sph' 
#'PM25_CAMS','year','AOD6',
#strange pattern: 'SO2_Emission_CAMS', 'S04_mixing_ratio_CAMS', 'elevation'
# 'PM25_Martin','l', 'PM25_Xiao',
#decrease r2: 'SO4_AOD_CAMS', 'wind_direction_GEOS','wind_speed_GEOS',
                     
features= new_data[['Visibility', 'SO4_MERRA','SO2_CAMS',  'AOD3','AOD2', 'month',
'm','s','Population','PBLH_GEOS','humidity_GEOS', 'temp_GEOS', 'NDVI', 'CF_CERES']]
features_list = features.columns.tolist()
features_data = np.array(features); 
rf = RandomForestRegressor(n_estimators = 1500, oob_score=True, random_state = 42); rf.fit(features_data, observations)
print(rf.oob_score_) 
#0.6748140020021249
#1500 trees improved model performance, but after that almost remained the same
#0.6748140020021249
#without 'SO2_CAMS': - too many extreme dots
#without 'SO2_Emission_CAMS': - better spatial pattern
    
#joblib.dump(rf, save_path + "./RF_so4_07012022.joblib")
#rf = joblib.load( save_path + "./RF_so4_07012022.joblib")
new_data['predictions'] = rf.oob_prediction_ 
#new_data.to_csv(save_path + 'data_GridID_without_extremes.csv', index = False)
#--------------------------------------------------------------------------------------------------------------
#map RF results
#--------------------------------------------------------------------------------------------------------------
#winter
fig = plt.figure(figsize=(10,8))
m = Basemap(projection='lcc', resolution='l',lat_0=36.5, lon_0=102.5, width=5.5E6, height=4.5E6)
m.scatter(winter['Lon'].values, winter['Lat'].values, latlon=True, c= rf.predict(winter[features.columns]), s = 1, marker = 's', cmap='rainbow', alpha=0.8) #alpha - transparent level
m.readshapefile(shapefile_path + 'province\\province', 'province',drawbounds=True,linewidth=0.2, default_encoding='iso-8859-15')
plt.colorbar(orientation="horizontal", pad=0.1, extend ='max')
plt.clim(0,30)
plt.show()

#summer
fig = plt.figure(figsize=(10,8))
#plt.box(False)
m = Basemap(projection='lcc', resolution='l',lat_0=36.5, lon_0=102.5, width=5.5E6, height=4.5E6)
m.scatter(summer['Lon'].values, summer['Lat'].values, latlon=True, c= rf.predict(summer[features.columns]), s = 1, marker = 's', cmap='rainbow', alpha=0.8) #alpha - transparent level
m.readshapefile(shapefile_path + 'province\\province', 'province',drawbounds=True,linewidth=0.2, default_encoding='iso-8859-15')
plt.colorbar(orientation="horizontal", pad=0.1, extend ='max')
plt.clim(0,30)
plt.show()


#plot predictors' rankings
result = permutation_importance(rf, features_data, observations)
importances = list(result.importances_mean)
feature_importances = [(feature, round(importance, 4)) for feature, importance in zip(features_list, importances)]
feature_importances = sorted(feature_importances, key = lambda x: x[1], reverse = True)
[print('Variable: {:20} Importance: {}'.format(*pair)) for pair in feature_importances];


df = pd.DataFrame({'Variable':features_list, 'Importance':result.importances_mean})
ordered_df = df.sort_values(by='Importance')
my_range=range(1,len(df.index)+1)
fig = plt.figure(figsize=(10,10))
ax = plt.gca();plt.hlines(y=my_range, xmin=0, xmax=ordered_df['Importance'], color='k')
plt.plot(ordered_df['Importance'], my_range, "o", markersize=5, color='r')#
plt.yticks(my_range, ordered_df['Variable'])
plt.xlabel('Premutation feature importance', fontsize=18)
ax.grid(False);ax.tick_params(axis='both', which='major', labelsize=15);plt.show()
fig.savefig(fig_path_save + 'predictor_importance_ranking.png', transparent=True)

#--------------------------------------------------------------------------------------------------------------
#clean ground obs of each gridid
#--------------------------------------------------------------------------------------------------------------
data = new_data
GridID_li = data.GridID.drop_duplicates().tolist()
#68

new_data_GridID_li = []
data_GridID_remove_li = []
#n = 0
for i in GridID_li:
    
    #i = GridID_li[n]
    data_GridID = data.loc[data['GridID'] == 25462] #  GridID_li[n]
    fig = plt.figure(figsize=(9,8))
    ax = plt.axes()
    ax.set_xlabel('Prediction', fontsize=18)
    ax.set_ylabel('Observation', fontsize=18)
    ax.scatter(data_GridID['predictions'], data_GridID['ground_so4'], edgecolors='b', linewidth=2,  marker=r'o', alpha=1, s = 50)
    plt.text(0.12, 0.95,'$R^{2}$ = '+ str(round( r2_score(data_GridID['ground_so4'], data_GridID['predictions'])  ,2)), ha='center', va='center', transform=ax.transAxes , fontsize=20)
    plt.show()
    
    print(data_GridID.ground_so4.nlargest(n=10))
    print(data_GridID.ground_so4.nsmallest(n=10))
    
    new_data_GridID = data_GridID[(data_GridID.ground_so4 >= 0) & (data_GridID.ground_so4 <= 100)]
    fig = plt.figure(figsize=(9,8))  
    ax = plt.axes()
    ax.set_xlabel('Prediction', fontsize=18)
    ax.set_ylabel('Observation', fontsize=18)
    ax.scatter(new_data_GridID['predictions'], new_data_GridID['ground_so4'], edgecolors='b', linewidth=2,  marker=r'o', alpha=1, s = 50)
    plt.text(0.12, 0.95,'$R^{2}$ = '+ str(round(r2_score(new_data_GridID['ground_so4'], new_data_GridID['predictions'])  ,2)), ha='center', va='center', transform=ax.transAxes , fontsize=20)
    plt.show()

    new_data_GridID_li.append(new_data_GridID)  
    data_GridID_remove = pd.concat([data_GridID,new_data_GridID]).drop_duplicates(keep=False)
    data_GridID_remove_li.append(data_GridID_remove)  
    print(len(new_data_GridID_li))
    print(len(data_GridID_remove_li))
    n = len(new_data_GridID_li)
    
data_GridID_remove_li_all = pd.concat(data_GridID_remove_li)
data_GridID_remove_li_all.drop_duplicates().to_csv(save_path + 'data_GridID_remove_li_'+ str(n)+'_more.csv', index = False)
#73

new_data_GridID_li_all = pd.concat(new_data_GridID_li)
new_data_GridID_li_all.to_csv(save_path + 'new_data_GridID_li_'+ str(n)+'_more.csv', index = False)
#17765

data_GridID_remove_li_all.drop_duplicates()
new_data_GridID_li_all_final_2 = pd.concat([data_GridID_remove_li_all.drop_duplicates(),data]).drop_duplicates(keep=False)
#17765
#new_data_GridID_li_all_final_2.to_csv(save_path + 'data_GridID_without_extremes_2.csv', index = False)
#17284
#data_GridID_remove_li_all_final = pd.read_csv(save_path + 'data_GridID_remove_li.csv')#.drop(['predictions'], axis=1) 


#--------------------------------------------------------------------------------------------------------------
#daily OOB performance
#--------------------------------------------------------------------------------------------------------------
daily_mean =  pd.merge(new_data, location_info, on=['GridID'])[['GridID', 'Lat', 'Lon', 'year', 'month', 'day', 'ground_so4','predictions', 'region', 'province', 'city', 'location']] 
daily_mean['GridID-year-month']=daily_mean["GridID"].astype(str)+'-' +daily_mean["year"].astype(str)+'-'  + daily_mean["month"].astype(str) 
df = daily_mean.groupby(['GridID-year-month']).count().reset_index()[['GridID-year-month','GridID']].rename(columns={"GridID": "Count"})
daily_mean_merge = pd.merge(daily_mean, df, on=['GridID-year-month'])
daily_mean_merge.to_csv(save_path + 'OOB_daily_mean.csv', index = False)
#17652

daily_mean = pd.read_csv(save_path + 'OOB_daily_mean.csv').drop_duplicates().dropna()
#17659

obs= np.array(daily_mean['ground_so4']);pre= np.array(daily_mean['predictions'])
fig = plt.figure(figsize=(9,8))
ax = plt.axes()
ax.scatter(pre, obs, c="k", alpha=1, marker=r'o', s= 10)
ax.set_xlabel('Predicted daily SO$_{4}$- (\u03bcg m$^{-3}$)', fontsize=22)
ax.set_ylabel('Observed daily SO$_{4}$- (\u03bcg m$^{-3}$)', fontsize=22)
ax.axis('tight')
r_2 = r2_score(obs, pre)  
m, b = np.polyfit(pre, obs, 1)
plt.plot([0, 200], [0, 200], ls="--", c=".3")
plt.xlim(-3,200)
plt.ylim(-3,200)
ax.set_facecolor('xkcd:white')
ax.grid(False)
xlims = ax.get_xlim()
new_x = np.arange(xlims[0], xlims[1],(xlims[1]-xlims[0])/250.)
ax.plot(new_x, b + m*new_x, color='r', linestyle='-', lw = 2.5 )
ax.tick_params(axis='both', which='major', labelsize=20)
plt.xticks(np.arange(0, 250, 50))
plt.yticks(np.arange(0, 250, 50))
plt.show()

rmse = mean_squared_error(obs, pre)
print ('y = {:.2f} + {:.2f}x'.format(b, m) )
print ('R_2 = '+ str(round(r_2,2)))
print ('rmse = ' + str(round(rmse,2)) )
print ('N = ' + str(len(obs)))

fig.savefig(fig_path_save +  'OOB_daily_mean_performance.png', transparent=True)


#check model performance
for i in daily_mean.GridID.drop_duplicates().tolist():
    #(1) check R_2 of daily predictions of each gridid
    #i=location_info.GridID.tolist()[0]
    #i = 65279
    data_GridID = daily_mean.loc[daily_mean['GridID'] == i]
    fig = plt.figure(figsize=(9,8))
    ax = plt.axes()
    ax.set_xlabel('Prediction', fontsize=18)
    ax.set_ylabel('Observation', fontsize=18)
    ax.scatter(data_GridID['predictions'], data_GridID['ground_so4'], edgecolors='b', linewidth=2,  marker=r'o', alpha=1, s = 50)
    plt.text(0.12, 0.95,'$R^{2}$ = '+ str(round( r2_score(data_GridID['ground_so4'], data_GridID['predictions'])  ,2)), ha='center', va='center', transform=ax.transAxes , fontsize=20)
    plt.show()
    fig.savefig(save_path +'GridID_R_2\\' + location_info.loc[location_info['GridID'] == i].location.tolist()[0]+ '_' + str(i) +'.png', transparent=True)
    
    #(2) check temporal trend of monthly predictions (include all counts)
    data_GridID = daily_mean.loc[daily_mean['GridID'] == i] [['year', 'month', 'ground_so4', 'predictions']].groupby([ 'year','month']).mean().reset_index()
    plt.figure(figsize=(25,6))
    data_GridID['year-month']=data_GridID["year"].astype(str)+'-'  + data_GridID["month"].astype(str) 
    plt.plot(data_GridID['year-month'].tolist(), data_GridID['ground_so4'].tolist(),  'r', label = 'Observations', linewidth=3)
    plt.plot(data_GridID['year-month'].tolist(), data_GridID['predictions'].tolist(),  'k', label = 'Predictions', linewidth=3)
    #plt.xlabel('Year-Month', fontsize=30)
    plt.ylabel('SO$_{4}$- (\u03bcg m$^{-3}$)', fontsize=25) #()
    #plt.ylim(0,50)
    plt.tick_params(axis='both', which='major', labelsize=25)
    df = data_GridID.loc[(data_GridID['month'] == 1) | (data_GridID['month'] == 7)]
    plt.xticks(df.index.tolist(), df['year-month'].tolist(), rotation=20)  # Set text labels and properties
    #plt.show()
    plt.savefig(save_path + 'GridID_temporal_trend\\'+ location_info.loc[location_info['GridID'] == i].location.tolist()[0] + '_'+ str(i) +'.png', bbox_inches='tight', transparent=True)



#--------------------------------------------------------------------------------------------------------------
#monthly OOB performance
#--------------------------------------------------------------------------------------------------------------
daily_mean_select = daily_mean.loc[(daily_mean['Count'] >  6)]

for i in daily_mean_select.GridID.drop_duplicates().tolist():
    data_GridID = daily_mean_select.loc[daily_mean_select['GridID'] == i] [['year', 'month', 'ground_so4', 'predictions']].groupby([ 'year','month']).mean().reset_index()
    plt.figure(figsize=(25,6))
    data_GridID['year-month']=data_GridID["year"].astype(str)+'-'  + data_GridID["month"].astype(str) 
    plt.plot(data_GridID['year-month'].tolist(), data_GridID['ground_so4'].tolist(),  'r', label = 'Observations', linewidth=3)
    plt.plot(data_GridID['year-month'].tolist(), data_GridID['predictions'].tolist(),  'k', label = 'Predictions', linewidth=3)
    #plt.xlabel('Year-Month', fontsize=30)
    plt.ylabel('SO$_{4}$- (\u03bcg m$^{-3}$)', fontsize=25) #()
    #plt.ylim(0,50)
    plt.tick_params(axis='both', which='major', labelsize=25)
    df = data_GridID.loc[(data_GridID['month'] == 1) | (data_GridID['month'] == 7)]
    plt.xticks(df.index.tolist(), df['year-month'].tolist(), rotation=20)  # Set text labels and properties
    #plt.show()
    plt.savefig(save_path + 'GridID_temporal_trend_count\\'+ location_info.loc[location_info['GridID'] == i].location.tolist()[0] + '_'+ str(i) +'.png', bbox_inches='tight', transparent=True)

monthly = daily_mean_select[['GridID', 'Lat', 'Lon', 'year', 'month', 'ground_so4', 'predictions']]
monthly_mean = monthly.groupby(['GridID', 'Lat', 'Lon', 'year', 'month']).mean().reset_index()
monthly_mean.to_csv(save_path + 'OOB_monthly_mean_count.csv', index =False)
#1166

#plot predictions over six selected sites
for i in [39616,65279,1649,89324,26935,25462]:
#for i in []:
    #i = 89324
    data_GridID = monthly_mean.loc[monthly_mean['GridID'] == i] [['year', 'month', 'ground_so4', 'predictions']].groupby([ 'year','month']).mean().reset_index()
    plt.figure(figsize=(20,6))
    data_GridID['year-month']= data_GridID["year"].astype(str) +'-' +  data_GridID["month"].astype(str)     
    #.str[-2:] 
    plt.plot(data_GridID['year-month'].tolist(), data_GridID['ground_so4'].tolist(),  'r', label = 'Observations', linewidth=4.5)
    plt.plot(data_GridID['year-month'].tolist(), data_GridID['predictions'].tolist(),  'k', label = 'Predictions', linewidth=4.5)
    #plt.xlabel('Year-Month', fontsize=30)
    plt.ylabel('SO$_{4}$- (\u03bcg m$^{-3}$)', fontsize=25)
    plt.ylim(0,72)
    #plt.ylim(0,70)
    plt.tick_params(axis='both', which='major', labelsize=25)
    #plt.xticks(data_GridID.index.tolist(), data_GridID['year-month'].tolist())  #, rotation=30, rotation=20 Set text labels and properties.

    df = data_GridID.loc[(data_GridID['month'] == 1) | (data_GridID['month'] == 7)]
    plt.xticks(df.index.tolist(), df['year-month'].tolist(), rotation=30)  #, rotation=20 Set text labels and properties.
    #plt.legend(loc="best",prop={'size': 35})
    #Chengdu: plt.xticks(df.index.tolist()[1:], df['year-month'].tolist()[1:])  #, rotation=20 Set text labels and properties.
    #Beijing: plt.xticks(df.index.tolist()[0:7], df['year-month'].tolist()[0:7])  #, rotation=20 Set text labels and properties.
    plt.show()
    plt.savefig(fig_path_save + 'selected_six_sites\\'+  location_info.loc[location_info['GridID'] == i].location.tolist()[0] + '_'+ str(i) +'.png', bbox_inches='tight', transparent=True)



obs_mon = np.array(monthly_mean['ground_so4']);pre_mon = np.array(monthly_mean['predictions'])
fig = plt.figure(figsize=(9,8))
ax = plt.axes()
ax.scatter(pre_mon, obs_mon, c="k", alpha=1, marker=r'o', s= 10, label="EC concentration" )
ax.set_xlabel('Predicted monthly SO$_{4}$- (\u03bcg m$^{-3}$)', fontsize=22)
ax.set_ylabel('Observed monthly SO$_{4}$- (\u03bcg m$^{-3}$)', fontsize=22)
ax.axis('tight')
r_2 = r2_score(obs_mon, pre_mon)  
m, b = np.polyfit(pre_mon, obs_mon, 1)
plt.plot([0, 90], [0, 90], ls="--", c=".3")
plt.xlim(-1,75)
plt.ylim(-1,75)
ax.set_facecolor('xkcd:white')
ax.grid(False)
xlims = ax.get_xlim()
new_x = np.arange(xlims[0], xlims[1],(xlims[1]-xlims[0])/250.)
ax.plot(new_x, b + m*new_x, color='r', linestyle='-', lw = 2.5 )
ax.tick_params(axis='both', which='major', labelsize=20)
#plt.text(0.12, 0.95,'$R^{2}$ = '+ str(round( r2_score(monthly_mean['ground_so4'], monthly_mean['predictions'])  ,2)), ha='center', va='center', transform=ax.transAxes , fontsize=20)
plt.show()

rmse = mean_squared_error(obs_mon, pre_mon)
print ('y = {:.2f} + {:.2f}x'.format(b, m) )
print ('R_2 = '+ str(round(r_2,2)))
print ('rmse = ' + str(round(rmse,2)) )
print ('N = ' + str(len(obs_mon)))

#fig.savefig(fig_path_save +  'monthly_gridID\\OOB_monthly_mean_performance_'+str(i)+'.png', transparent=True)
fig.savefig(fig_path_save +  'OOB_monthly_mean_performance.png', transparent=True)

#--------------------------------------------------------------------------------------------------------------
#20-fold random CV
#--------------------------------------------------------------------------------------------------------------

new_data = pd.read_csv(save_path + 'data_GridID_without_extremes_w_counts.csv')
#17663
#new_data['GridID-year-month']=new_data["GridID"].astype(str)+'-' +new_data["year"].astype(str)+'-'  + new_data["month"].astype(str) 
#df = new_data.groupby(['GridID-year-month']).count().reset_index()[['GridID-year-month','GridID']].rename(columns={"GridID": "Count"})
#new_data_merge = pd.merge(new_data, df, on=['GridID-year-month'])
#new_data_merge.to_csv(save_path + 'data_GridID_without_extremes_w_counts.csv', index =False)

from sklearn.utils import shuffle
shuffled = shuffle(shuffled, random_state=1)
new_data = shuffled

observations = np.array(new_data['ground_so4'])
features= new_data[['Visibility', 'SO4_MERRA','SO2_CAMS',  'AOD3','AOD2', 'month','m','s','Population','PBLH_GEOS','humidity_GEOS', 'temp_GEOS', 'NDVI', 'CF_CERES']]
features_list = features.columns.tolist()
features_data = np.array(features); 
model = RandomForestRegressor(n_estimators = 1500, oob_score=True, random_state = 42)
x = features_data
y = observations 
y_pre_cv = cross_val_predict(model, x, y, cv = 20)
r_2_cv = r2_score(y, y_pre_cv)  
print(r_2_cv)
#0.6751047267847373

joblib.dump(model, save_path + "./random_20_fold_so4_07032022.joblib")
#rf = joblib.load( save_path + "./random_20_fold_so4_07032022.joblib")

new_data['predictions_20_fold'] = y_pre_cv
new_data.to_csv(save_path + 'data_GridID_without_extremes_w_counts_random_CV.csv', index =False)

fig = plt.figure(figsize=(9,8))
ax = plt.axes()
ax.scatter(new_data['predictions_20_fold'], new_data['ground_so4'], c="k", alpha=1, marker=r'o', s= 10)
ax.set_xlabel('Predicted daily SO$_{4}$- (\u03bcg m$^{-3}$)', fontsize=22)
ax.set_ylabel('Observed daily SO$_{4}$- (\u03bcg m$^{-3}$)', fontsize=22)
ax.axis('tight')
r_2 = r2_score(new_data['ground_so4'], new_data['predictions_20_fold'])  
m, b = np.polyfit(new_data['predictions_20_fold'], new_data['ground_so4'], 1)
plt.plot([0, 200], [0, 200], ls="--", c=".3")
plt.xlim(-1,200)
plt.ylim(-1,200)
ax.set_facecolor('xkcd:white')
ax.grid(False)
xlims = ax.get_xlim()
new_x = np.arange(xlims[0], xlims[1],(xlims[1]-xlims[0])/250.)
ax.plot(new_x, b + m*new_x, color='r', linestyle='-', lw = 2.5 )
ax.tick_params(axis='both', which='major', labelsize=20)
plt.xticks(np.arange(0, 250, 50))
plt.yticks(np.arange(0, 250, 50))
plt.show()
fig.savefig(fig_path_save +  'random_20_fold_daily_mean_performance.png', transparent=True)

rmse = mean_squared_error(new_data['ground_so4'], new_data['predictions_20_fold'])
print ('y = {:.2f} + {:.2f}x'.format(b, m) )
print ('R_2 = '+ str(round(r_2,2)))
print ('rmse = ' + str(round(rmse,2)) )
print ('N = ' + str(len(new_data['ground_so4'])) )

#monthly
df = new_data.groupby(['GridID-year-month']).count().reset_index()[['GridID-year-month','GridID']].rename(columns={"GridID": "Count"})
new_data_merge = pd.merge(new_data, df, on=['GridID-year-month'])
new_data_merge.to_csv(save_path + 'data_GridID_without_extremes_w_counts_random_CV.csv', index =False)

#17621
new_data_select = new_data_merge.loc[(new_data_merge['Count'] >  6)]
#16676

monthly = new_data_select[['GridID', 'Lat', 'Lon', 'year', 'month', 'ground_so4', 'predictions_20_fold']]
monthly_mean = monthly.groupby(['GridID', 'Lat', 'Lon', 'year', 'month']).mean().reset_index()
#monthly_mean.to_csv(save_path + 'OOB_monthly_mean_count.csv', index =False)
#1166

obs_mon = np.array(monthly_mean['ground_so4']);pre_mon = np.array(monthly_mean['predictions_20_fold'])
fig = plt.figure(figsize=(9,8))
ax = plt.axes()
ax.scatter(pre_mon, obs_mon, c="k", alpha=1, marker=r'o', s= 10)
ax.set_xlabel('Predicted monthly SO$_{4}$- (\u03bcg m$^{-3}$)', fontsize=22)
ax.set_ylabel('Observed monthly SO$_{4}$- (\u03bcg m$^{-3}$)', fontsize=22)
ax.axis('tight')
r_2 = r2_score(obs_mon, pre_mon)  
m, b = np.polyfit(pre_mon, obs_mon, 1)
plt.plot([0, 90], [0, 90], ls="--", c=".3")
plt.xlim(-1,75)
plt.ylim(-1,75)
ax.set_facecolor('xkcd:white')
ax.grid(False)
xlims = ax.get_xlim()
new_x = np.arange(xlims[0], xlims[1],(xlims[1]-xlims[0])/250.)
ax.plot(new_x, b + m*new_x, color='r', linestyle='-', lw = 2.5 )
ax.tick_params(axis='both', which='major', labelsize=20)
#plt.text(0.12, 0.95,'$R^{2}$ = '+ str(round( r2_score(monthly_mean['ground_so4'], monthly_mean['predictions'])  ,2)), ha='center', va='center', transform=ax.transAxes , fontsize=20)
plt.show()
fig.savefig(fig_path_save +  'random_20_fold_monthly_mean_performance.png', transparent=True)

rmse = mean_squared_error(obs_mon, pre_mon)
print ('y = {:.2f} + {:.2f}x'.format(b, m) )
print ('R_2 = '+ str(round(r_2,2)))
print ('rmse = ' + str(round(rmse,2)) )
print ('N = ' + str(len(obs_mon)))


#--------------------------------------------------------------------------------------------------------------
#20-fold spatial CV
#--------------------------------------------------------------------------------------------------------------
#draw 20 clusters map
new_data = pd.read_csv(save_path + 'data_GridID_without_extremes_w_counts_random_CV.csv')

new_data['GridID'].drop_duplicates()
coordinates = np.array(new_data[['Lon', 'Lat']].drop_duplicates())#;rng.shuffle(coordinates)
#'GridID'- 68
centroid, label = kmeans2(coordinates, 20, minit='points')
colors=["#0000FF", "#00FF00", "#FF0066","#FFFF00", "#FF0000", "#C0FF3E","#FF00FF", "#FFEC8B", "#BFEFFF","#ADFF2F", "#FF6103", "#00FFFF","#458B00", "#E3CF57", "#6495ED","#BF3EFF", "#FF1493", "#00BFFF","#C1CDC1", "#98FB98"]
fig = plt.figure(figsize=(13,10)); plt.box(False)
m = Basemap(projection='lcc', resolution='l',lat_0=36.5, lon_0=102.5, width=5.5E6, height=4.5E6)
m.readshapefile(shapefile_path + 'province\\province', 'province',drawbounds=True,linewidth=1, default_encoding='iso-8859-15', color ='#C1CDCD')
for i in np.arange(20):
    w = coordinates[label == i]
    m.scatter(w[:, 0], w[:, 1], latlon=True,  s = 150, edgecolors='k', alpha=0.8,  zorder=3, c =  colors[i]) #marker="^" ,alpha - transparent level
m.scatter(centroid[:, 0], centroid[:, 1], latlon=True,s = 150, edgecolors='k', c = 'w', marker='*',  zorder=3) #marker="^" ,alpha - transparent level
plt.show(); fig.savefig(fig_path_save +  'spatial_cv_map.png', transparent=True)

df = pd.DataFrame(coordinates, columns=['Lon', 'Lat'])
df["cluster"] = label
df.to_csv(save_path +  'spatial_cv_cluster.csv', index =False)
new_data = pd.merge(new_data, df, on=['Lon', 'Lat'])
#17659

pre_li = []
for i in np.arange(20):
    #i = 0
    data_cluster = new_data.loc[(new_data['cluster'] == i)]
    data_cluster_removed = new_data.loc[~(new_data['cluster'] == i)]

    obs_cluster = np.array(data_cluster_removed['ground_so4'])
    features_cluster = data_cluster_removed[['SO4_MERRA','SO2_CAMS','Visibility', 'AOD3','AOD2', 'month','m','s','Population','PBLH_GEOS','humidity_GEOS', 'temp_GEOS', 'NDVI', 'CF_CERES']]
    features_list = features_cluster.columns.tolist()
    features_data = np.array(features_cluster); 
    rf_cluster = RandomForestRegressor(n_estimators = 500, oob_score=True, random_state = 42); rf_cluster.fit(features_data, obs_cluster)
    data_cluster['predictions_spatial'] = rf_cluster.predict(pd.get_dummies(data_cluster[features_cluster.columns]))
    pre_li.append(data_cluster)
    
pre_li_total = pd.concat(pre_li)
pre_li_total.to_csv(save_path +  'spatial_cv_cluster_pre.csv', index =False)

new_data = pre_li_total
fig = plt.figure(figsize=(9,8))
ax = plt.axes()
ax.scatter(new_data['predictions_spatial'], new_data['ground_so4'], c="k", alpha=1, marker=r'o', s= 10)
ax.set_xlabel('Predicted daily SO$_{4}$- (\u03bcg m$^{-3}$)', fontsize=22)
ax.set_ylabel('Observed daily SO$_{4}$- (\u03bcg m$^{-3}$)', fontsize=22)
ax.axis('tight')
r_2 = r2_score(new_data['ground_so4'], new_data['predictions_spatial'])  
m, b = np.polyfit(new_data['predictions_spatial'], new_data['ground_so4'], 1)
plt.plot([0, 200], [0, 200], ls="--", c=".3")
plt.xlim(-1,200)
plt.ylim(-1,200)
ax.set_facecolor('xkcd:white')
ax.grid(False)
xlims = ax.get_xlim()
new_x = np.arange(xlims[0], xlims[1],(xlims[1]-xlims[0])/250.)
ax.plot(new_x, b + m*new_x, color='r', linestyle='-', lw = 2.5 )
ax.tick_params(axis='both', which='major', labelsize=20)
plt.xticks(np.arange(0, 250, 50))
plt.yticks(np.arange(0, 250, 50))
plt.show()
fig.savefig(fig_path_save +  'spatial_20_fold_daily_mean_performance.png', transparent=True)

rmse = mean_squared_error(new_data['ground_so4'], new_data['predictions_20_fold'])
print ('y = {:.2f} + {:.2f}x'.format(b, m) )
print ('R_2 = '+ str(round(r_2,2)))
print ('rmse = ' + str(round(rmse,2)) )
print ('N = ' + str(len(new_data['ground_so4'])) )


#monthly spatial CV performance

test = pre_li_total
test['GridID-year-month']=test["GridID"].astype(str)+'-' +test["year"].astype(str)+'-'  + test["month"].astype(str) 


test_group = test.groupby(['GridID-year-month']).count().reset_index()[['GridID-year-month','GridID']].rename(columns={"GridID": "Count"})

test_group_select = test_group.loc[(test_group['Count'] >  6)]
daily_mean_7 = pd.merge(monthly_mean, test_group_select, on=['GridID-year-month'])

monthly = test[['GridID-year-month', 'ground_so4', 'predictions_spatial']]
monthly_mean= monthly.groupby(['GridID-year-month']).mean()
monthly_mean =  pd.merge(monthly_mean, location_info, on=['GridID']) 
monthly_mean.to_csv(save_path + 'spatial_cv_monthly_mean.csv')

#monthly_mean = monthly_mean.loc[(monthly_mean['ground_so4'] < 30)]
#monthly_mean = monthly_mean.loc[(monthly_mean['predictions_spatial'] < 30)]
monthly_mean = daily_mean_7
#1369
obs_mon = np.array(monthly_mean['ground_so4']);pre_mon = np.array(monthly_mean['predictions_spatial'])

fig = plt.figure(figsize=(9,8))
ax = plt.axes()
ax.scatter(pre_mon, obs_mon, c="k", alpha=1, marker=r'o', s= 10, label="EC sconcentration" )
ax.set_xlabel('Predictions', fontsize=18)
ax.set_ylabel('Observations', fontsize=18)
ax.axis('tight')
r_2 = r2_score(obs_mon, pre_mon)  
m, b = np.polyfit(pre_mon, obs_mon, 1)
plt.plot([0, 90], [0, 90], ls="--", c=".3")
plt.xlim(-1,90)
plt.ylim(-1,90)
ax.set_facecolor('xkcd:white')
ax.grid(False)
xlims = ax.get_xlim()
new_x = np.arange(xlims[0], xlims[1],(xlims[1]-xlims[0])/250.)
ax.plot(new_x, b + m*new_x, color='r', linestyle='-', lw = 2.5 )
ax.tick_params(axis='both', which='major', labelsize=15)
plt.show()
fig.savefig(fig_path_save +  'spatial_monthly_mean_performance.png', transparent=True)

rmse = mean_squared_error(obs_mon, pre_mon)
print ('y = {:.2f} + {:.2f}x'.format(b, m) )
print ('R_2 = '+ str(round(r_2,2)))
print ('rmse = ' + str(round(rmse,2)) )
print ('N = ' + str(len(obs_mon)))

#-----------------------------------------------------------------------------------------------------------
#20-fold temporal CV
#-----------------------------------------------------------------------------------------------------------
temporal_split = []
for year in np.arange(2005,2019):
    #year = 2005
    data_year = new_data.loc[(new_data['year'] == year)]
    shuffled = data_year.sample(frac = 1)
    temporal_split_year = np.array_split(shuffled, 20)  
    temporal_split.append(temporal_split_year)

pre_li=[]
for j in np.arange(20):
    #j = 0
    data_fold_i = []
    for i in np.arange(14):
        data_temporal = temporal_split[i][j]
        data_fold_i.append(data_temporal)
    data_cluster = pd.concat(data_fold_i)
    data_cluster_removed = pd.concat([new_data,data_cluster]).drop_duplicates(keep=False)
    obs_cluster = np.array(data_cluster_removed['ground_so4'])
    features_cluster = data_cluster_removed[['so4_CAMS','AOD19','humidity_GEOS','Total_Road_Length','NO2_CAMS', 'NO_CAMS','month','Hso4_CAMS',
    'NDVI','NH3_Emission_CAMS' ,'AOD2', 's','PBLH_GEOS','temp_GEOS','PM25_Xiao','AOD3','AOD6','l']]
    features_list = features_cluster.columns.tolist()
    features_data = np.array(features_cluster); 
    rf_cluster = RandomForestRegressor(n_estimators = 1000, oob_score=True, random_state = 42); rf_cluster.fit(features_data, obs_cluster)
    data_cluster['predictions_temporal'] = rf_cluster.predict(pd.get_dummies(data_cluster[features_cluster.columns]))
    pre_li.append(data_cluster)
pre_li_total = pd.concat(pre_li)
pre_li_total.to_csv(save_path + 'temporal_cv_cluster_pre.csv', index =False)

#monthly temporal CV performance
monthly = pre_li_total[['GridID', 'Lat', 'Lon', 'year', 'month', 'ground_so4', 'predictions_temporal']]
monthly_mean= monthly.groupby(['GridID', 'Lat', 'Lon', 'year', 'month']).mean()
#1369
#monthly_mean =  pd.merge(monthly_mean, location_info, on=['GridID'])#.drop_duplicates() 
#1393
monthly_mean.to_csv(save_path + 'temporal_CV_monthly_mean.csv')

obs_mon = np.array(monthly_mean['ground_so4']);pre_mon = np.array(monthly_mean['predictions_temporal'])
fig = plt.figure(figsize=(9,8))
ax = plt.axes()
ax.scatter(pre_mon, obs_mon, c="k", alpha=1, marker=r'o', s= 10, label="EC concentration" )
ax.set_xlabel('Predictions', fontsize=18)
ax.set_ylabel('Observations', fontsize=18)
ax.axis('tight')
r_2 = r2_score(obs_mon, pre_mon)  
m, b = np.polyfit(pre_mon, obs_mon, 1)
plt.plot([0, 90], [0, 90], ls="--", c=".3")
plt.xlim(-1,90)
plt.ylim(-1,90)
ax.set_facecolor('xkcd:white')
ax.grid(False)
xlims = ax.get_xlim()
new_x = np.arange(xlims[0], xlims[1],(xlims[1]-xlims[0])/250.)
ax.plot(new_x, b + m*new_x, color='r', linestyle='-', lw = 2.5 )
ax.tick_params(axis='both', which='major', labelsize=15)
plt.show()
fig.savefig(fig_path_save +  'temporal_monthly_mean_performance.png', transparent=True)

rmse = mean_squared_error(obs_mon, pre_mon)
print ('y = {:.2f} + {:.2f}x'.format(b, m) )
print ('R_2 = '+ str(round(r_2,2)))
print ('rmse = ' + str(round(rmse,2)) )
print ('N = ' + str(len(obs_mon)))


'''
#data = pd.read_csv(save_path + 'so4_grid_level_data_with_predictors_all.csv').dropna()
#17490
#location_info = pd.read_csv(so4_obs_path + 'so4_obs_data_2005_18_05232022.csv')[['GridID','province','city']].drop_duplicates()
#location_info['location']=location_info['region'] +'_'+ location_info['province'] + '_'+ location_info['city']
#location_info.to_csv(so4_obs_path + 'so4_68_GridID_info.csv', index = False)

observations = np.array(data['ground_so4'])
features= data[['so4_CAMS','AOD19','humidity_GEOS','Total_Road_Length','NO2_CAMS', 'NO_CAMS','month','Hso4_CAMS',
'NDVI','NH3_Emission_CAMS' ,'AOD2', 's','PBLH_GEOS','temp_GEOS','PM25_Xiao','AOD3','AOD6','l']]
features_list = features.columns.tolist()
features_data = np.array(features); 
rf_original = RandomForestRegressor(n_estimators = 1000, oob_score=True, random_state = 42); rf_original.fit(features_data, observations)
print(rf_original.oob_score_)
#use all data - 0.6588435760622142
data['predictions'] = rf_original.oob_prediction_ 






#performance of monthly predictions
monthly = new_data[['GridID', 'Lat', 'Lon', 'year', 'month', 'ground_so4', 'predictions', 'predictions_10_fold']]
monthly_mean= monthly.groupby(['GridID', 'Lat', 'Lon', 'year', 'month']).mean()
monthly_mean =  pd.merge(monthly_mean, location_info, on=['GridID']) 
monthly_mean.to_csv(save_path + 'OOB_monthly_mean.csv')

obs_mon = np.array(monthly_mean['ground_so4'])
pre_mon = np.array(monthly_mean['predictions'])
pre_mon_10_fold = np.array(monthly_mean['predictions_10_fold'])

fig = plt.figure(figsize=(9,8))
ax = plt.axes()
ax.scatter(pre_mon, obs_mon, c="k", alpha=1, marker=r'o', s= 10, label="EC concentration" )
ax.set_xlabel('Predictions', fontsize=18)
ax.set_ylabel('Observations', fontsize=18)
ax.axis('tight')
r_2 = r2_score(obs_mon, pre_mon)  
m, b = np.polyfit(pre_mon, obs_mon, 1)
plt.plot([0, 90], [0, 90], ls="--", c=".3")
plt.xlim(-1,90)
plt.ylim(-1,90)
ax.set_facecolor('xkcd:white')
ax.grid(False)
xlims = ax.get_xlim()
new_x = np.arange(xlims[0], xlims[1],(xlims[1]-xlims[0])/250.)
ax.plot(new_x, b + m*new_x, color='r', linestyle='-', lw = 2.5 )
ax.tick_params(axis='both', which='major', labelsize=15)
plt.show()

rmse = mean_squared_error(obs_mon, pre_mon)
print ('y = {:.2f} + {:.2f}x'.format(b, m) )
print ('R_2 = '+ str(round(r_2,2)))
print ('rmse = ' + str(round(rmse,2)) )
print ('N = ' + str(len(obs_mon)) )

fig = plt.figure(figsize=(9,8))
ax = plt.axes()
ax.scatter(pre_mon_10_fold, obs_mon, c="k", alpha=1, marker=r'o', s= 10, label="EC concentration" )
ax.set_xlabel('10-fold CV Predictions', fontsize=18)
ax.set_ylabel('Observations', fontsize=18)
ax.axis('tight')
r_2 = r2_score(obs_mon, pre_mon_10_fold)  
m, b = np.polyfit(pre_mon_10_fold, obs_mon, 1)
plt.plot([0, 90], [0, 90], ls="--", c=".3")
plt.xlim(-1,90)
plt.ylim(-1,90)
ax.set_facecolor('xkcd:white')
ax.grid(False)
xlims = ax.get_xlim()
new_x = np.arange(xlims[0], xlims[1],(xlims[1]-xlims[0])/250.)
ax.plot(new_x, b + m*new_x, color='r', linestyle='-', lw = 2.5 )
ax.tick_params(axis='both', which='major', labelsize=15)
plt.show()

rmse = mean_squared_error(obs_mon, pre_mon_10_fold)
print ('y = {:.2f} + {:.2f}x'.format(b, m) )
print ('R_2 = '+ str(round(r_2,2)))
print ('rmse = ' + str(round(rmse,2)) )
print ('N = ' + str(len(obs_mon)) )


r_2_li = [] 
for i in GridID_li:
    #i=GridID_li[0]
    data_GridID = new_data.loc[new_data['GridID'] == i]
    r_2 = round( r2_score(data_GridID['ground_so4'], data_GridID['predictions']) ,2)
    r_2_li.append(r_2)  
    
df = pd.DataFrame(list(zip(GridID_li,r_2_li)),columns =['GridID','r_2'])
df = pd.merge(df, location_info, on=['GridID']) 
df.to_csv(save_path + 'GridID_r_2_province_city.csv', index = False)

#remove some GridID to test model performance
low_r_2 = df.loc[df['r_2'] <= 0.7]
remove_li = low_r_2.GridID.drop_duplicates().tolist()
for i in remove_li:
    print('------------')
    print(i)
    #i = remove_li[0]
    new_data_select = new_data[new_data.GridID != i]
    observations = np.array(new_data_select['ground_so4'])
    features= new_data_select[['so4_CAMS','AOD19','humidity_GEOS','Total_Road_Length','NO2_CAMS', 'NO_CAMS','month','Hso4_CAMS',
    'NDVI','NH3_Emission_CAMS' ,'AOD2', 's','PBLH_GEOS','temp_GEOS','PM25_Xiao','AOD3','AOD6','l']]
    features_list = features.columns.tolist()
    features_data = np.array(features)
    rf_remove_li = RandomForestRegressor(n_estimators = 1000, oob_score=True, random_state = 42); rf_remove_li.fit(features_data, observations)
    print(rf_remove_li.oob_score_)

#remove_li = [39616,50533]
#0.7130487038384892

#remove_li = [39616,50533,26063,31689,25462]
#0.7196284234380799

remove_li = [39616,50533,26063,31689,25462,36776,31252,22701,26935,35942,29965,32127,21067,24188]
#0.7318138023458401
#0.7353667538030586
new_data_select = new_data[~new_data['GridID'].isin(remove_li)]
observations = np.array(new_data_select['ground_so4'])
features= new_data_select[['so4_CAMS','humidity_GEOS','Total_Road_Length','NO2_CAMS', 'NO_CAMS','month','Hso4_CAMS',
'NDVI','NH3_Emission_CAMS' ,'PBLH_GEOS','temp_GEOS','PM25_Xiao','AOD19','AOD2','AOD3','AOD6','s','l']]
#
features_list = features.columns.tolist()
features_data = np.array(features)
rf_remove_li = RandomForestRegressor(n_estimators = 1000, oob_score=True, random_state = 42); rf_remove_li.fit(features_data, observations)
print(rf_remove_li.oob_score_)
new_data_select['predictions'] = rf_remove_li.oob_prediction_ 

#check temporal trend of predictions
for GridID in np.array(new_data_select['GridID'].drop_duplicates()):
    #GridID = np.array(new_data_select['GridID'].drop_duplicates())[0]
    data_GridID = new_data_select.loc[new_data_select['GridID'] == GridID] [['year', 'month', 'ground_so4', 'predictions']].groupby([ 'year','month']).mean().reset_index()
    plt.figure(figsize=(30,6))
    data_GridID['year-month']=data_GridID["year"].astype(str)  + data_GridID["month"].astype(str)     
    plt.plot(data_GridID['year-month'].tolist(), data_GridID['ground_so4'].tolist(),  'r', label = 'ground_so4', linewidth=3)
    plt.plot(data_GridID['year-month'].tolist(), data_GridID['predictions'].tolist(),  'k', label = 'predictions', linewidth=3)
    #plt.show()
    plt.savefig(save_path + 'prediction_quality_control\\'+ str(GridID) + '_'+ location_info.loc[location_info['GridID'] == GridID].location.tolist()[0] +'_modified.png', bbox_inches='tight')




#1384,2845,3467,1242,1244,1367,1368,23774,29965,32127,38451,56145
#len(remove_li)

#remain_li = [x for x in GridID_li if x not in remove_li]
#len(remain_li)
new_data_select = new_data[~new_data['GridID'].isin(remove_li)]
#16628
observations = np.array(new_data_select['ground_so4'])
features= new_data_select[['so4_CAMS','AOD19','humidity_GEOS','Total_Road_Length','NO2_CAMS', 'NO_CAMS','month','Hso4_CAMS',
'NDVI','NH3_Emission_CAMS' ,'AOD2', 's','PBLH_GEOS','temp_GEOS','PM25_Xiao','AOD3','AOD6','l']]
features_list = features.columns.tolist()
features_data = np.array(features); 
rf_remove_li = RandomForestRegressor(n_estimators = 1000, oob_score=True, random_state = 42); rf_remove_li.fit(features_data, observations)
print(rf_remove_li.oob_score_)
new_data_select['predictions'] = rf_remove_li.oob_prediction_

#10-fold 
model = RandomForestRegressor(n_estimators = 1000, oob_score=True, random_state = 42)
x = features_data
y = observations 
y_pre = cross_val_predict(model, x, y, cv = 10)
r_2 = r2_score(observations, y_pre)  
print(r_2)
#0.42
#0.4724382297589931
#0.513541706098249
new_data_select['predictions_10_fold'] = y_pre

#performance of daily predictions
fig = plt.figure(figsize=(9,8))
ax = plt.axes()
ax.scatter(new_data['predictions_10_fold'], new_data['ground_so4'], c="k", alpha=1, marker=r'o', s= 10)
ax.set_xlabel('10-fold CV Predictions', fontsize=18)
ax.set_ylabel('Observations', fontsize=18)
ax.axis('tight')
r_2 = r2_score(new_data['ground_so4'], new_data['predictions_10_fold'])  
m, b = np.polyfit(new_data['predictions_10_fold'], new_data['ground_so4'], 1)
plt.plot([0, 160], [0, 160], ls="--", c=".3")
plt.xlim(-1,160)
plt.ylim(-1,160)
ax.set_facecolor('xkcd:white')
ax.grid(False)
xlims = ax.get_xlim()
new_x = np.arange(xlims[0], xlims[1],(xlims[1]-xlims[0])/250.)
ax.plot(new_x, b + m*new_x, color='r', linestyle='-', lw = 2.5 )
ax.tick_params(axis='both', which='major', labelsize=15)
plt.show()
rmse = mean_squared_error(new_data['ground_so4'], new_data['predictions_10_fold'])
print ('y = {:.2f} + {:.2f}x'.format(b, m) )
print ('R_2 = '+ str(round(r_2,2)))
print ('rmse = ' + str(round(rmse,2)) )
print ('N = ' + str(len(new_data['ground_so4'])) )

#performance of monthly predictions
monthly = new_data_select[['GridID', 'Lat', 'Lon', 'year', 'month', 'ground_so4', 'predictions', 'predictions_10_fold']]
monthly_mean= monthly.groupby(['GridID', 'Lat', 'Lon', 'year', 'month']).mean()
obs_mon = np.array(monthly_mean['ground_so4'])
pre_mon = np.array(monthly_mean['predictions'])
pre_mon_10_fold = np.array(monthly_mean['predictions_10_fold'])

fig = plt.figure(figsize=(9,8))
ax = plt.axes()
ax.scatter(pre_mon, obs_mon, c="k", alpha=1, marker=r'o', s= 10, label="EC concentration" )
ax.set_xlabel('Predictions', fontsize=18)
ax.set_ylabel('Observations', fontsize=18)
ax.axis('tight')
r_2 = r2_score(obs_mon, pre_mon)  
m, b = np.polyfit(pre_mon, obs_mon, 1)
plt.plot([0, 90], [0, 90], ls="--", c=".3")
plt.xlim(-1,90)
plt.ylim(-1,90)
ax.set_facecolor('xkcd:white')
ax.grid(False)
xlims = ax.get_xlim()
new_x = np.arange(xlims[0], xlims[1],(xlims[1]-xlims[0])/250.)
ax.plot(new_x, b + m*new_x, color='r', linestyle='-', lw = 2.5 )
ax.tick_params(axis='both', which='major', labelsize=15)
plt.show()

rmse = mean_squared_error(obs_mon, pre_mon)
print ('y = {:.2f} + {:.2f}x'.format(b, m) )
print ('R_2 = '+ str(round(r_2,2)))
print ('rmse = ' + str(round(rmse,2)) )
print ('N = ' + str(len(obs_mon)) )

fig = plt.figure(figsize=(9,8))
ax = plt.axes()
ax.scatter(pre_mon_10_fold, obs_mon, c="k", alpha=1, marker=r'o', s= 10, label="EC concentration" )
ax.set_xlabel('10-fold CV Predictions', fontsize=18)
ax.set_ylabel('Observations', fontsize=18)
ax.axis('tight')
r_2 = r2_score(obs_mon, pre_mon_10_fold)  
m, b = np.polyfit(pre_mon_10_fold, obs_mon, 1)
plt.plot([0, 90], [0, 90], ls="--", c=".3")
plt.xlim(-1,90)
plt.ylim(-1,90)
ax.set_facecolor('xkcd:white')
ax.grid(False)
xlims = ax.get_xlim()
new_x = np.arange(xlims[0], xlims[1],(xlims[1]-xlims[0])/250.)
ax.plot(new_x, b + m*new_x, color='r', linestyle='-', lw = 2.5 )
ax.tick_params(axis='both', which='major', labelsize=15)
plt.show()

rmse = mean_squared_error(obs_mon, pre_mon_10_fold)
print ('y = {:.2f} + {:.2f}x'.format(b, m) )
print ('R_2 = '+ str(round(r_2,2)))
print ('rmse = ' + str(round(rmse,2)) )
print ('N = ' + str(len(obs_mon)) )



data = data[data['ground_so4'] <= 160]
#17489
#total_li = data.GridID.drop_duplicates().tolist()
#68
remove_li = [36776,35942,2677,1640,1637,2485,1240,2673,3467   ,32127,29965,23774]
#,1368,1367,32127,29965
#23774
#li = [38451,23774,32127,57414, 61038,36776,35942,31689,31252,29965,2485,1367]
#12
#remain_li = list(set(total_li) - set(li))
#56
#data_select = data[~data['GridID'].isin(remove_li)]
#for i in remain_li:
#print (i)
#i = li[0]
#data_select = data.loc[~(data['GridID'] == i)]
#print(len(data_select))
data_select = data[~data['GridID'].isin(remove_li)]
data_select = data

#15927
data_select.to_csv(save_path + 'obs_data_select_06062022_original.csv', index = False)

#remove extreme points
data_select = pd.read_csv(save_path + 'obs_data_select_06062022.csv')
#15775

#li_remove_2 = [38451, 56145]
#[1242, 1244, 1367, 1368, ]
#data_select = data_select[~data_select['GridID'].isin(li_remove_2)]
#15203


data_select['predictions'] = rf.oob_prediction_ 
data_select_test = data_select[['GridID', 'Lat', 'Lon', 'year', 'month', 'day', 'ground_so4','predictions']]
data_select_test.to_csv(save_path + 'obs_data_select_test_06062022.csv', index = False)

#joblib.dump(rf, save_path + "./RF_so4_06062022.joblib")
#rf = joblib.load( save_path + "./RF_so4_06062022.joblib")
li = data_select.GridID.drop_duplicates().tolist()

for i in li:
    #print(i)
    GridID = data_select[data_select['GridID']==i]
    y_pre = np.array(GridID['predictions'])
    y = np.array(GridID['ground_so4'])
    r_2 = r2_score(y, y_pre)  
    #print(r_2)

    fig = plt.figure(figsize=(9,8))
    ax = plt.axes()
    #ax.scatter(y_pre, y, c="k", alpha=1, marker=r'o', s = 10, label="EC concentration" )
    ax.set_xlabel('Predicted concentration ($μg \ m^{-3}$)', fontsize=18)
    ax.set_ylabel('Observed concentration ($μg \ m^{-3}$)', fontsize=18)
    ax.axis('tight')
    m, b = np.polyfit(y_pre, y, 1)
    plt.plot([-10, 120], [-10, 120], ls="--", c=".3")
    plt.xlim(-10,120)
    plt.ylim(-10,120)
    ax.set_facecolor('xkcd:white')
    ax.grid(False)
    xlims = ax.get_xlim()
    new_x = np.arange(xlims[0], xlims[1],(xlims[1]-xlims[0])/250.)
    ax.plot(new_x, b + m*new_x, color='r', linestyle='-', lw = 2 )
    ax.scatter(GridID['predictions'], GridID['ground_so4'], edgecolors='b', linewidth=2,  marker=r'o', alpha=1, s = 50)#, facecolors='none' 
    plt.legend(loc='upper left', fontsize=18)
    plt.text(0.12, 0.95,'$R^{2}$ = '+ str(round(r_2,2)), ha='center', va='center', transform=ax.transAxes , fontsize=20)
    #plt.text(0.20, 0.9,'y = {:.2f} + {:.2f}x'.format(b, m) , ha='center', va='center', transform=ax.transAxes , fontsize=20)
    #plt.text(0.15, 0.85,'$RMSE$ = ' + str(round(rmse,2)), ha='center', va='center', transform=ax.transAxes , fontsize=20)
    ax.tick_params(axis='both', which='major', labelsize=15)
    plt.show()
    
    fig.savefig(save_path+'prediction_quality_control_regression\\' + str(i) +'_'+ location_info.loc[location_info['GridID'] == i].location.tolist()[0] +'.png', transparent=True)


#improve ground data quality
data_select['predictions'] = rf.oob_prediction_ 
for GridID in np.array(data_select['GridID'].drop_duplicates()):
    #GridID = np.array(data_select['GridID'].drop_duplicates())[0]
    data_GridID = data_select.loc[data_select['GridID'] == GridID] [['year', 'month', 'ground_so4', 'predictions']].groupby([ 'year','month']).mean().reset_index()
    plt.figure(figsize=(30,6))
    data_GridID['year-month']=data_GridID["year"].astype(str)  + data_GridID["month"].astype(str) 
                                              
    plt.plot(data_GridID['year-month'].tolist(), data_GridID['ground_so4'].tolist(),  'r', label = 'ground_so4', linewidth=3)
    plt.plot(data_GridID['year-month'].tolist(), data_GridID['predictions'].tolist(),  'k', label = 'predictions', linewidth=3)
    #plt.show()
    plt.savefig(save_path + 'prediction_quality_control\\'+ str(GridID) + '_'+ location_info.loc[location_info['GridID'] == GridID].location.tolist()[0] +'.png', bbox_inches='tight')
    
    
'''
