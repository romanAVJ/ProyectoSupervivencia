#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 15 13:10:53 2020

@author: roman_avj
KPI system for sellers, embassadors and clients
"""

import sys
sys.path.insert(1, '/Users/roman_avj/Scripts/PythonScripts/DbbConectors')
import conn_test
import googleDriveAPI as gdapi
import pandas as pd
import os
import time
from datetime import date
from datetime import datetime

today = date.today() #date of today
#==============================================================================
####                                FUNCTIONS
#==============================================================================

def fconditional_sum(s, value):
    """
    

    Parameters
    ----------
    s : Pandas Series
        Serie to compare values
    value : List
        List of the values to compare

    Returns
    -------
    Sum of the occurences in s that have that value

    """
    return s.isin(value).sum()


def diff_time(t1,t2):
    """
    t2 - t1

    Parameters
    ----------
    t1 : timestamp
        DESCRIPTION.
    t2 : timestamp
        DESCRIPTION.

    Returns
    -------
    Days as int.

    """
    #time difference
    t = t2 - t1
    # parse to days
    t = t.days
    
    return(t)



def censor_time(df,time_column,time_category = ''):
    now = datetime.now() #date of today as datetime (compatible with timestamp)
    #making censoring
    df['censor_' + time_category] = df[time_column].\
        apply(lambda x: 0 if(pd.isna(x)) else 1)
        
    #apply today date to time_column
    df[time_column] = df[time_column].apply(lambda x: now if(pd.isna(x)) else x)
    
    return(df)

def tidy_df(df, cols):
    df_main = df[cols]
    # keeping rows with embassadors who have clients
    df_main = df_main[ pd.notna( df_main['client_email'])]
    
    # keeping rows with clients in Enso
    df_main = df_main[ pd.notna( df_main['client_phone'])]

def pivot_table(df, group_bys, dic_func):
    #check if the dic keys are in the columns
    flag = True
    for k in dic_func.keys():
        flag = flag and (k in df.columns)
        
    if flag:
        df = df.groupby(group_bys).agg(dic_func)
        #drop first multiindex of columns
        df.columns = df.columns.droplevel(level = 0)
    else: 
        print("There is a key in the dictionary that it isnt in the dataframe")
        df = None
        
    return(df)
      



#==============================================================================
#####                                MODULES
#==============================================================================

def googleAPI_credsANDclient():
    # json_kpi_api is only the path to the json
    global json_key_api
    #credentials and client API
    client,creds = gdapi.gAPIclient(json_key_api)
    return (client, creds)
    


def google_df(client, name_sheet, cols_to_string):
    # google sheet dataframe
    df = gdapi.gspread2dataFrame(client, name_sheet)
    
    #modify columns to string
    df[cols_to_string] = df[cols_to_string].astype(dtype = str)
    return(df)

    
def joins_df(list_df, list_on, join_type = 'left'):
    """
    Generates a dataframe with a set of dataframes.
    len(list_df) == len(list_on) + 1 !

    Parameters
    ----------
    list_df : list
        List with all the dataframes that should be joined.
    list_on : list
        The columns names in comun that should be merged.
    join_type : str, optional
        How the join should be. SQL type. The default is 'left'.

    Returns
    -------
    Pandas DataFrame

    """
    #initialize dataframe with the first one
    df = list_df[0] 
    n = len(list_df)
    for i in range(n-1): #n-1 because len(list_df) == len(list_on) + 1
        df = pd.merge(left = df,
                      right = list_df[i+1],
                      how = join_type ,
                      on = list_on[i])
    return(df)

def agg_censor(df):
    #censoring
    df = censor_time(df, time_column = 'time_user_activated', time_category = 'activated')
    df = censor_time(df, time_column = 'time_user_complete',time_category = 'cmplt')
    df = censor_time(df, time_column = 'first_transcard',time_category = 'transcard')
    return(df)
    


def agg_times(df):
    #time for activation in days
    df['diff_time_activation'] = \
        df[['time_register_client','time_user_activated']].\
            apply(lambda x: diff_time(x['time_register_client'], x['time_user_activated']),
                  axis = 'columns')
            
    #time for completeness 
    df['diff_time_complete'] = \
        df[['time_register_client','time_user_complete']].\
            apply(lambda x: diff_time(x['time_register_client'], x['time_user_complete']),
                  axis = 'columns')
            
    #time for first use of card
    df['diff_time_carduse'] = \
        df[['time_register_client','first_transcard']].\
            apply(lambda x: diff_time(x['time_register_client'], x['first_transcard']),
                  axis = 'columns')
    
    #negative diff (this is because there were people who already were in enso)
    df[['diff_time_activation','diff_time_complete','diff_time_carduse']] = \
        df[['diff_time_activation',
            'diff_time_complete','diff_time_carduse']].applymap(lambda x: 0 if(x < 0) else x)
    return(df)
    

def clean_df(df):
    #agg if is in enso or not the user
    df['enso_complete'] = df['client_phone'].apply(lambda x: 1 if(pd.notna(x))else 0)
    
    # filling na's
    df['client_amount_transcard'].fillna(value = 0, inplace = True)
    
    #censoring
    df = agg_censor(df)
    #time aggregates
    df = agg_times(df)
    
    #drop clients that arent even in minimalist
    df.dropna(subset = ['client_email'], inplace = True)
    
    # settle columns
    cols = ['client_email','phone_seller_enso','phone_embassador_enso','enso_complete','user_complete',
            'card_status', 'card_activated','client_num_transcard','client_amount_transcard','num_operations', 'num_deposits', 'num_withdrawals', 'amount_deposits',
       'amount_operations', 'amount_withdrawals', 'diff_time_activation','censor_activated',
       'diff_time_complete','censor_cmplt', 'diff_time_carduse', 'censor_transcard','time_user_complete']
    df = df[cols]
    df.sort_values('enso_complete', ascending = False,inplace = True)
    return(df)


