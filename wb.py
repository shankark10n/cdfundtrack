import pandas as pd
from pandas import DataFrame as df
from pandas import Series as ss
import json

data_path = '/Users/shankar/work/box/projects/'
data = pd.read_csv(data_path + 'fec/allstates_agg_house_races.csv')
data_dic = dict(list(data.groupby('state_name')))
json_dic = {}
for state_name in data_dic:
	json_dic[state_name] = [i[1].to_dict() for i in data_dic[state_name].iterrows()]

uss = json.load(open(data_path + 'fec/data/geojson/us-states.json'))
# add funding trends
for i in xrange(0, 52):
    state=uss['features'][i]['properties']['name']
    uss['features'][i]['properties']['fundingtrends']=json_dic[state]
mean_frac_inc_wins = data.groupby('state_name').frac_incumbent_winners.mean().to_dict()
total_indiv_cont = data.groupby('state_name').total_indiv_cont.sum().to_dict()
total_inc_indiv_cont = data.groupby('state_name').total_incumbent_indiv_cont.sum().to_dict()
for i in xrange(0, 52):
    state=uss['features'][i]['properties']['name']
    uss['features'][i]['properties']['total_indiv_cont']=total_indiv_cont[state]
    uss['features'][i]['properties']['mean']=mean[state]
json.dump(uss, open('us-states-housefunding.json', 'w'))
