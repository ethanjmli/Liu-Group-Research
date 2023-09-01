# -*- coding: utf-8 -*-
"""
Created on Wed May 18 17:12:23 2022

@author: yhang2
"""
import pandas as pd
import datetime
import numpy as np

#get ground measurements of so4 
so4_obs_path = 'C:\\Users\\yhang2\\OneDrive - Emory University\\Desktop\\China_PM_species\\SO4\\Observations\\'
obs_data_path = 'C:\\Users\\yhang2\\OneDrive - Emory University\\Desktop\\China_PM_species\\China_PM2.5_consitituents_ground_obs\\'
GridID = pd.read_csv('C:\\Users\\yhang2\\OneDrive - Emory University\\Desktop\\China_PM_species\\GridID.csv')

#2005-17 ---------------------------------------------------------------------------------------------------------------------
data_2005_2017_raw = pd.read_csv(obs_data_path + "2005_17\\ModelData_2005-2017_IncludeAdded_convl_OMI_update201912.csv" )[['GridID', 'Year', 'Month', 'DOY', 'SITE_NAME', 'LON_site', 'LAT_site', 'SO4', 'X_Lon_x', 'Y_Lat_x']].dropna().drop_duplicates()
#drop 1258.98
data_2005_2017_raw = data_2005_2017_raw[data_2005_2017_raw.SO4 != 1258.98]
#drop repeated 0.01 in LS
data_2005_2017_raw = data_2005_2017_raw[data_2005_2017_raw.SO4 != 0.01]
#16410 obs

data_2005_2017_raw = data_2005_2017_raw.rename(columns={"Year": "year", "Month": "month", "LON_site": "lon_site", "LAT_site": "lat_site", "X_Lon_x": "Lon", "Y_Lat_x": "Lat", "SITE_NAME": "name_site", "SO4": "ground_so4"})
day = []
for i in np.arange(len(data_2005_2017_raw)):
    #i = 0
    r = datetime.datetime(int(data_2005_2017_raw['year'].iloc[i]), 1, 1) + datetime.timedelta(int(data_2005_2017_raw['DOY'].iloc[i]) - 1)  
    day_i = r.day
    day.append(day_i)  
data_2005_2017_raw['day'] = day    
data_2005_2017 = data_2005_2017_raw[['GridID', 'Lat', 'Lon', 'year', 'month', 'day', 'ground_so4', 'name_site', 'lat_site', 'lon_site']]
data_2005_2017.to_csv( so4_obs_path + 'data_2005_2017.csv',index=False)  
#16415 
#NOTE: some repeated value over BJ_* sites, need to calculate average when aggregate to gridid level

data_2005_2017_site = data_2005_2017[['name_site', 'lat_site', 'lon_site']].drop_duplicates()
data_2005_2017_site.to_csv( so4_obs_path + 'data_2005_2017_site_info.csv',index=False)  
  

#2019 added data ---------------------------------------------------------------------------------------------------------------------
data_2019_add_raw = pd.read_csv(obs_data_path + "new_data_added_in_2019\\PM_Constituents_CDC_updated201912.csv")[['Province ', 'City ', 'SITE_NAME', 'year', 'month', 'day','(SO42-)(/m3)']].dropna()#
#2913
#remove repeated 0.0535 and 0.01 in LS
data_2019_add_raw = data_2019_add_raw[data_2019_add_raw['(SO42-)(/m3)'] != 0.0535 ]
data_2019_add_raw = data_2019_add_raw[data_2019_add_raw['(SO42-)(/m3)'] != 0.01 ]
data_2019_add_raw = data_2019_add_raw[data_2019_add_raw['(SO42-)(/m3)'] != 1258.98]
#2890

data_2019_add_raw = data_2019_add_raw.rename(columns={'Province ': "province", 'City ': "city", "SITE_NAME": "name_site", "(SO42-)(/m3)": "ground_so4"})
#['province', 'city', 'name_site', 'year', 'month', 'day', 'ground_so4']

#data_2019_add_site_province_city = data_2019_add[['name_site', 'province', 'city']].drop_duplicates()
#21
GridID_2018 = pd.read_csv(obs_data_path + "new_data_added_in_2019\\address_201912updated_10kmGridID.csv").rename(columns={'Station.Name': 'info_site', 'SITE_NAME': 'name_site', "lon": "lon_site", "lat": "lat_site"})
#['info_site', 'name_site', 'lat_site', 'lon_site', 'GridID']
#23
data_2019_add = pd.merge(data_2019_add_raw, GridID_2018[['name_site', 'lat_site', 'lon_site', 'GridID']], on=['name_site']) 
data_2019_add = pd.merge(data_2019_add, GridID, on=['GridID'])[['GridID', 'Lat', 'Lon', 'year', 'month', 'day', 'ground_so4', 'name_site', 'lat_site', 'lon_site', 'province', 'city']]  
data_2019_add.to_csv( so4_obs_path + 'data_2019_add.csv',index=False)  

#data_2019_add_site_PC = data_2019_add[['name_site', 'province', 'city']].drop_duplicates()
data_2019_add_site = data_2019_add[['name_site', 'lat_site', 'lon_site', 'province', 'city']].drop_duplicates()
data_2019_add_site.to_csv( so4_obs_path + 'data_2019_add_site_info.csv',index=False)  

#2021 added data ---------------------------------------------------------------------------------------------------------------------
data_2021_add_raw = pd.read_csv(obs_data_path + "new_data_added_in_2021\\data_2018.csv")[['code', '采样日期', '省', '市', '区', '站点名称', 'SO42.1']].dropna().drop_duplicates()
#drop 0.0535
data_2021_add_raw = data_2021_add_raw[data_2021_add_raw['SO42.1'] != 0.0535]
#3517
#NOTE: repeated value over the same city, need to average at gridid level

data_2021_add_location = pd.read_csv(obs_data_path + "new_data_added_in_2021\\data_2018_location.csv")[['code', 'latitude', 'longitude']]
data_2021_add_merge = pd.merge(data_2021_add_location, data_2021_add_raw, on=['code']).rename(columns={'省': 'province', '市': 'city', "longitude": "lon_site", "latitude": "lat_site", '站点名称': 'name_site', "SO42.1": "ground_so4"}).drop(['code', '区'], axis=1) 
#3517
province_list = data_2021_add_merge['province'].drop_duplicates().tolist()
#8 
df = data_2021_add_merge
df.province.replace(['河南省', '江苏省', '浙江省', '四川省', '广东省', '广西', '河北省', '黑龙江省'], ['Henan', 'Jiangsu', 'Zhejiang', 'Sichuan', 'Guangdong', 'Guangxi', 'Hebei', 'Heilongjiang'], inplace=True)
city_list = data_2021_add_merge['city'].drop_duplicates().tolist()
#28
li_1 = ['安阳市','常州市', '杭州市', '成都市', '镇江市', '广州市', '南京市', '南通市', '南宁市', '石家庄市', '哈尔滨市', '淮安市', '佛山市', '金华市', '廊坊市', '丽水市', '连云港市', '深圳市', '宁波市', '苏州市', '台州市', '泰州市', '宿迁市', '徐州市', '盐城市', '扬州市', '郑州市','珠海市']
li_2 = ['Anyang','Changzhou', 'Hangzhou', 'Chengdu', 'Zhengjiang', 'Guangzhou', 'Nanjing', 'Nantong', 'Nanning', 'Shijiazhuang', 'Harbin', 'Huaian', 'Foshan', 'Jinhua', 'Langfang', 'Lishui', 'Lianyungang', 'Shenzhen', 'Ningbo', 'Suzhou', 'Taizhou', 'Taizhou', 'Suqian', 'Xuzhou', 'Yancheng', 'Yangzhou', 'Zhengzhou','Zhuhai']
df.city.replace(li_1, li_2, inplace=True)
data_2021_add_merge = df
df = data_2021_add_merge['采样日期'].str.split('/', 2, expand=True)
data_2021_add_merge['month'] = df[0]
data_2021_add_merge['day'] = df[1]
data_2021_add_merge['year'] = df[2]

def function(x):
    num = round(x/0.05)
    if (x - num*0.05) < 0:
        num = num-1
    else:
        num = num  
    if (num % 2) == 0:
        num =round((num+1)*0.05,2)
    else: 
        num =round((num*0.05),2)     
    return(num)

data_2021_add_merge['Lat'] = data_2021_add_merge['lat_site'].apply(function)
data_2021_add_merge['Lon'] = data_2021_add_merge['lon_site'].apply(function)
data_2021_add = pd.merge(data_2021_add_merge, GridID, on=['Lat','Lon'])[['GridID', 'Lat', 'Lon', 'year', 'month', 'day', 'ground_so4', 'name_site', 'lat_site', 'lon_site', 'province', 'city']]  
#3617
data_2021_add.to_csv( so4_obs_path + 'data_2021_add.csv',index=False, encoding="utf_8_sig")  


data_2021_add_site = data_2021_add[['name_site', 'lat_site', 'lon_site', 'province', 'city']].drop_duplicates()
data_2021_add_site.to_csv( so4_obs_path + 'data_2021_add_site_info.csv',index=False, encoding="utf_8_sig" )  


#merge all so4 data ---------------------------------------------------------------------------------------------------------------------
data_a = data_2005_2017[['GridID', 'Lat', 'Lon', 'year', 'month', 'day', 'ground_so4', 'lat_site', 'lon_site']]#
data_b = data_2019_add[['GridID', 'Lat', 'Lon', 'year', 'month', 'day', 'ground_so4', 'lat_site', 'lon_site']]#
data_c = data_2021_add[['GridID', 'Lat', 'Lon', 'year', 'month', 'day', 'ground_so4', 'lat_site', 'lon_site']]#

data = pd.concat([data_a, data_b, data_c]).drop_duplicates().dropna()
#21135
data.to_csv(so4_obs_path + 'so4_obs_data_2005_18_06272022.csv',index=False)
#, encoding="utf_8_sig" 
#save chinese

#add site information 
obs = pd.read_csv(so4_obs_path+ 'so4_obs_data_2005_18_06272022.csv')
path = 'C:\\Users\\yhang2\\OneDrive - Emory University\\Desktop\\China_PM_species\\NO3\\Observations\\site_info\\'
a_info=pd.read_csv(path +'data_2005_2017_site_info.csv')[['name_site_checked', 'lat_site', 'lon_site', 'province','city']]
b_info =pd.read_csv(path+'data_2019_add_site_info.csv')[['name_site_checked', 'lat_site', 'lon_site', 'province','city']]
c_info =pd.read_csv(path+'2021.csv')[['name_site_checked', 'lat_site', 'lon_site', 'province','city']]
all_info = pd.concat([a_info, b_info, c_info]).drop_duplicates()
all_info.to_csv(so4_obs_path+ 'so4_site_info_06272022.csv',index=False)

obs_add_site = pd.merge(obs, all_info, on=['lat_site', 'lon_site'])
#21135
obs_add_site.to_csv(so4_obs_path + 'so4_obs_data_2005_18_06272022.csv',index=False)

#site province city
site_number = all_info[['name_site_checked','province', 'city']].drop_duplicates()
#88 sites
site_number.groupby('province').count()
province_city_number = site_number.drop(['name_site_checked'], axis=1).drop_duplicates().groupby('province').count()
'''
len(site_number['province'].drop_duplicates()): 16
len(site_number['city'].drop_duplicates()): 45
Beijing          1
Guangdong        4
Guangxi          1
Hebei            2
Heilongjiang     1
Henan            2
HongKong         5
Jiangsu         13
Shandong         1
Shanghai         1
Shanxi           2
Sichuan          1
Taiwan           4
Tianjin          1
Xinjiang         1
Zhejiang         6
'''
obs_info = obs_add_site[['name_site_checked','province', 'city', 'year', 'month', 'day']]
obs_info_group = obs_info.groupby(['province', 'city','name_site_checked', 'year']).count()
obs_info_group.to_csv( so4_obs_path + 'so4_obs_info_analysis_06272022.csv')


