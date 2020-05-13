---
layout:     post
title:      "如何用Python绘制JS地图？"
subtitle:   "How to plot JS map using Python"
date:       2017-10-19
author:     "QQF"
header-img: "img/home-bg.png"
catalog: true
tags:
    - Python
---

Folium是建立在Python生态系统的数据整理（Datawrangling）能力和Leaflet.js库的映射能力之上的开源库。用Python处理数据，然后用Folium将它在Leaflet地图上进行可视化。

# 概念

Folium能够将通过Python处理后的数据轻松地在交互式的Leaflet地图上进行可视化展示。它不单单可以在地图上展示数据的分布图，还可以使用Vincent/Vega在地图上加以标记。

这个开源库中有许多来自OpenStreetMap、MapQuest Open、MapQuestOpen Aerial、Mapbox和Stamen的内建地图元件，而且支持使用Mapbox或Cloudmade的API密钥来定制个性化的地图元件。Folium支持GeoJSON和TopoJSON两种文件格式的叠加，也可以将数据连接到这两种文件格式的叠加层，最后可使用color-brewer配色方案创建分布图。

# 安装

安装folium包

# 开始创建地图

创建底图，传入起始坐标到Folium地图中：

```
import folium
map_osm= folium.Map(location=[45.5236, -122.6750]) #输入坐标
map_osm.create_map(path='osm.html')
```

![img](/img/in-post/2017-10-19-python-JS-map/01.png)

Folium默认使用OpenStreetMap元件，但是Stamen Terrain, Stamen Toner, Mapbox Bright 和MapboxControl空间元件是内置的：

```
#输入位置，tiles，缩放比例
stamen =folium.Map(location=[45.5236, -122.6750], tiles='Stamen Toner',zoom_start=13)

stamen.create_map(path='stamen_toner.html')#保存图片
```

![img](/img/in-post/2017-10-19-python-JS-map/02.png)

Folium也支持Cloudmade 和 Mapbox的个性化定制地图元件，只需简单地传入API_key ：

```
custom =folium.Map(location=[45.5236, -122.6750], tiles='Mapbox',
API_key='wrobstory.map-12345678')
```

最后，Folium支持传入任何与Leaflet.js兼容的个性化地图元件：

```
tileset= r'http://{s}.tiles.yourtiles.com/{z}/{x}/{y}.png'
map =folium.Map(location=[45.372, -121.6972], zoom_start=12,
tiles=tileset, attr='My DataAttribution')
```

# 地图标记

Folium支持多种标记类型的绘制，下面从一个简单的Leaflet类型的位置标记弹出文本开始：

```
map_1 =folium.Map(location=[45.372, -121.6972], zoom_start=12,
tiles='Stamen Terrain')
map_1.simple_marker([45.3288,-121.6625], popup='Mt. Hood Meadows')#文字标记
map_1.simple_marker([45.3311,-121.7113], popup='Timberline Lodge')

map_1.create_map(path='mthood.html')
```

![img](/img/in-post/2017-10-19-python-JS-map/03.png)

Folium支持多种颜色和标记图标类型：

```
map_1 =folium.Map(location=[45.372, -121.6972], zoom_start=12,tiles='Stamen Terrain')
map_1.simple_marker([45.3288,-121.6625], popup='Mt. Hood Meadows',marker_icon='cloud') #标记图标类型为云
map_1.simple_marker([45.3311,-121.7113], popup='Timberline Lodge',marker_color='green') #标记颜色为绿色
map_1.simple_marker([45.3300,-121.6823], popup='Some OtherLocation',marker_color='red',marker_icon='info-sign')
#标记颜色为红色，标记图标为“info-sign”)
map_1.create_map(path='iconTest.html')
```

Folium也支持使用个性化的尺寸和颜色进行圆形标记：

```
map_2 =folium.Map(location=[45.5236, -122.6750], tiles='Stamen Toner',
zoom_start=13)
map_2.simple_marker(location=[45.5244,-122.6699], popup='The Waterfront')
```

# 简单树叶类型标记

```
map_2.circle_marker(location=[45.5215,-122.6261], radius=500,
popup='Laurelhurst Park',line_color='#3186cc',
fill_color='#3186cc')#圆形标记
map_2.create_map(path='portland.html')
```

![img](/img/in-post/2017-10-19-python-JS-map/04.png)

Folium有一个简便的功能可以使经/纬度悬浮于地图上：

```
map_3 =folium.Map(location=[46.1991, -122.1889], tiles='Stamen Terrain',zoom_start=13)
map_3.lat_lng_popover()

map_3.create_map(path='sthelens.html')
```

![img](/img/in-post/2017-10-19-python-JS-map/05.png)

Click-for-marker功能允许标记动态放置：

```
map_4 =folium.Map(location=[46.8527, -121.7649], tiles='Stamen Terrain',zoom_start=13)
map_4.simple_marker(location=[46.8354,-121.7325], popup='Camp Muir')
map_4.click_for_marker(popup='Waypoint')

map_4.create_map(path='mtrainier.html')
```

![img](/img/in-post/2017-10-19-python-JS-map/06.png)

Folium也支持来自Leaflet-DVF的Polygon（多边形）标记集：

```
map_5 =folium.Map(location=[45.5236, -122.6750], zoom_start=13)
map_5.polygon_marker(location=[45.5012,-122.6655], popup='Ross Island Bridge',fill_color='#132b5e', num_sides=3,radius=10)#三边形标记
map_5.polygon_marker(location=[45.5132,-122.6708], popup='Hawthorne Bridge',fill_color='#45647d', num_sides=4,radius=10)#四边形标记
map_5.polygon_marker(location=[45.5275,-122.6692], popup='Steel Bridge',fill_color='#769d96', num_sides=6, radius=10)#四边形标记
map_5.polygon_marker(location=[45.5318,-122.6745], popup='Broadway Bridge',fill_color='#769d96', num_sides=8,radius=10) #八边形标记

map_5.create_map(path='bridges.html')
```

![img](/img/in-post/2017-10-19-python-JS-map/07.png)

# Vincent/Vega标记

Folium能够使用vincent 进行任何类型标记，并悬浮在地图上。

```
buoy_map= folium.Map(location=[46.3014, -123.7390], zoom_start=7,
tiles='StamenTerrain')
buoy_map.polygon_marker(location=[47.3489,-124.708], fill_color='#43d9de',radius=12, popup=(vis1, 'vis1.json'))
buoy_map.polygon_marker(location=[44.639,-124.5339], fill_color='#43d9de',radius=12, popup=(vis2, 'vis2.json'))

buoy_map.polygon_marker(location=[46.216,-124.1280], fill_color='#43d9de',radius=12, popup=(vis3, 'vis3.json'))
```

![img](/img/in-post/2017-10-19-python-JS-map/08.png)

# GeoJSON/TopoJSON层叠加

GeoJSON 和TopoJSON层都可以导入到地图，不同的层可以在同一张地图上可视化出来：

```
geo_path= r'data/antarctic_ice_edge.json'
topo_path= r'data/antarctic_ice_shelf_topo.json'
ice_map= folium.Map(location=[-59.1759, -11.6016],tiles='Mapbox Bright', zoom_start=2)
ice_map.geo_json(geo_path=geo_path)#导入geoJson层
ice_map.geo_json(geo_path=topo_path,topojson='objects.antarctic_ice_shelf')#导入Toposon层
ice_map.create_map(path='ice_map.html')
```

![img](/img/in-post/2017-10-19-python-JS-map/09.png)

# 分布图

Folium允许PandasDataFrames/Series类型和Geo/TopoJSON类型之间数据转换。Color Brewer 颜色方案也是内建在这个库，可以直接导入快速可视化不同的组合：

```
importfolium
importpandas as pd
state_geo= r'data/us-states.json'#地理位置文件
state_unemployment= r'data/US_Unemployment_Oct2012.csv'#美国失业率文件
state_data= pd.read_csv(state_unemployment)
#LetFolium determine the scale
map =folium.Map(location=[48, -102], zoom_start=3)
map.geo_json(geo_path=state_geo,data=state_data,
columns=['State', 'Unemployment'],
key_on='feature.id',
fill_color='YlGn',fill_opacity=0.7, line_opacity=0.2,
legend_name='Unemployment Rate(%)')
map.create_map(path='us_states.html')
```

![img](/img/in-post/2017-10-19-python-JS-map/10.png)

基于D3阈值尺度，Folium在右上方创建图例，通过分位数创建最佳猜测值，导入设定的阈值很简单：

```
map.geo_json(geo_path=state_geo,data=state_data,
columns=['State', 'Unemployment'],
threshold_scale=[5, 6, 7, 8, 9,10],
key_on='feature.id',
fill_color='BuPu',fill_opacity=0.7, line_opacity=0.5,
legend_name='Unemployment Rate(%)',
reset=True)
map.create_map(path='us_states.html')
```

![img](/img/in-post/2017-10-19-python-JS-map/11.png)

通过Pandas DataFrame进行数据处理，可以快速可视化不同的数据集。下面的例子中，df DataFrame包含6列不同的经济数据，我们将在下面可视化一部分数据：

# 2011年就业率分布图

```
map_1 =folium.Map(location=[48, -102], zoom_start=3)
map_1.geo_json(geo_path=county_geo,data_out='data1.json', data=df,
columns=['GEO_ID','Employed_2011'],key_on='feature.id',
fill_color='YlOrRd',fill_opacity=0.7, line_opacity=0.3,
topojson='objects.us_counties_20m')#2011就业率分布图
map_1.create_map(path='map_1.html')
```

![img](/img/in-post/2017-10-19-python-JS-map/12.png)

# 2011年失业率分布图

```
map_2 =folium.Map(location=[40, -99], zoom_start=4)
map_2.geo_json(geo_path=county_geo,data_out='data2.json', data=df,
columns=['GEO_ID','Unemployment_rate_2011'],
key_on='feature.id',
threshold_scale=[0, 5, 7, 9, 11,13],
fill_color='YlGnBu', line_opacity=0.3,
legend_name='Unemployment Rate2011 (%)',
topojson='objects.us_counties_20m')#2011失业率分布图

map_2.create_map(path='map_2.html')
```

![img](/img/in-post/2017-10-19-python-JS-map/13.png)

# 2011年中等家庭收入分布图

```
map_3 =folium.Map(location=[40, -99], zoom_start=4)
map_3.geo_json(geo_path=county_geo,data_out='data3.json', data=df,
columns=['GEO_ID','Median_Household_Income_2011'],
key_on='feature.id',
fill_color='PuRd',line_opacity=0.3,
legend_name='Median Household Income2011 ($)',
topojson='objects.us_counties_20m')#2011中等家庭收入分布图
map_3.create_map(path='map_3.html')
```

![img](/img/in-post/2017-10-19-python-JS-map/14.png)
