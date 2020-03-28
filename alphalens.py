#!/usr/bin/env python
# coding: utf-8

# In[5]:


get_ipython().run_line_magic('pylab', 'inline --no-import-all')
import alphalens
import pandas as pd
import numpy as np


# In[6]:


import warnings
warnings.filterwarnings('ignore')


# In[82]:


tickers = [ 'ACN', 'ATVI', 'ADBE', 'AMD', 'AKAM', 'ADS', 'GOOGL', 'GOOG', 'APH', 'ADI', 'ANSS', 'AAPL',
'AVGO', 'CA', 'CDNS', 'CSCO', 'CTXS', 'CTSH', 'GLW', 'CSRA', 'DXC', 'EBAY', 'EA', 'FFIV', 'FB',
'FLIR', 'IT', 'GPN', 'HRS', 'HPE', 'HPQ', 'INTC', 'IBM', 'INTU', 'JNPR', 'KLAC', 'LRCX', 'MA', 'MCHP',
'MSFT', 'MSI', 'NTAP', 'NFLX', 'NVDA', 'ORCL', 'PAYX', 'PYPL', 'QRVO', 'QCOM', 'RHT', 'CRM', 'STX',
'AMG', 'AFL', 'ALL', 'AXP', 'AIG', 'AMP', 'AON', 'AJG', 'AIZ', 'BAC', 'BK', 'BBT', 'BRK.B', 'BLK', 'HRB',
'BHF', 'COF', 'CBOE', 'SCHW', 'CB', 'CINF', 'C', 'CFG', 'CME', 'CMA', 'DFS', 'ETFC', 'RE', 'FITB', 'BEN',
'GS', 'HIG', 'HBAN', 'ICE', 'IVZ', 'JPM', 'KEY', 'LUK', 'LNC', 'L', 'MTB', 'MMC', 'MET', 'MCO', 'MS',
'NDAQ', 'NAVI', 'NTRS', 'PBCT', 'PNC', 'PFG', 'PGR', 'PRU', 'RJF', 'RF', 'SPGI', 'STT', 'STI', 'SYF', 'TROW',
'ABT', 'ABBV', 'AET', 'A', 'ALXN', 'ALGN', 'AGN', 'ABC', 'AMGN', 'ANTM', 'BCR', 'BAX', 'BDX', 'BIIB', 'BSX',
'BMY', 'CAH', 'CELG', 'CNC', 'CERN', 'CI', 'COO', 'DHR', 'DVA', 'XRAY', 'EW', 'EVHC', 'ESRX', 'GILD', 'HCA',
'HSIC', 'HOLX', 'HUM', 'IDXX', 'ILMN', 'INCY', 'ISRG', 'IQV', 'JNJ', 'LH', 'LLY', 'MCK', 'MDT', 'MRK', 'MTD',
'MYL', 'PDCO', 'PKI', 'PRGO', 'PFE', 'DGX', 'REGN', 'RMD', 'SYK', 'TMO', 'UNH', 'UHS', 'VAR', 'VRTX', 'WAT',
'MMM', 'AYI', 'ALK', 'ALLE', 'AAL', 'AME', 'AOS', 'ARNC', 'BA', 'CHRW', 'CAT', 'CTAS', 'CSX', 'CMI', 'DE',
'DAL', 'DOV', 'ETN', 'EMR', 'EFX', 'EXPD', 'FAST', 'FDX', 'FLS', 'FLR', 'FTV', 'FBHS', 'GD', 'GE', 'GWW',
'HON', 'INFO', 'ITW', 'IR', 'JEC', 'JBHT', 'JCI', 'KSU', 'LLL', 'LMT', 'MAS', 'NLSN', 'NSC', 'NOC', 'PCAR',
'PH', 'PNR', 'PWR', 'RTN', 'RSG', 'RHI', 'ROK', 'COL', 'ROP', 'LUV', 'SRCL', 'TXT', 'TDG', 'UNP', 'UAL',
'AES', 'LNT', 'AEE', 'AEP', 'AWK', 'CNP', 'CMS', 'ED', 'D', 'DTE', 'DUK', 'EIX', 'ETR', 'ES', 'EXC']


# In[83]:


import pandas_datareader.data as web
import datetime
import yfinance as yf
yf.pdr_override()


# In[84]:


start=datetime.datetime(2017, 1, 1)

end=datetime.datetime(2019, 2, 1)

pan=web.get_data_yahoo(tickers, start, end)


# In[85]:


pan.head()


# In[115]:


today_price = pan['Open']


# In[116]:


yesterday_price = today_price.shift(1)


# In[163]:


events = today_price[ (today_price-yesterday_price)/yesterday_price <0.01 ]


# In[164]:


events = events.stack()


# In[165]:


events.index = events.index.set_names(['date', 'asset'])


# In[166]:


events = events.astype(float)


# In[167]:


events


# In[168]:


pricing = pan['Open'].iloc[1:]


# In[169]:


pricing.head()


# In[170]:


filter_zscore = None


# In[171]:


quantiles = None
bins      = [-1000000,1000000]


# In[172]:


long_short = True


# In[173]:


factor_data = alphalens.utils.get_clean_factor_and_forward_returns(events, 
                                                                   pricing, 
                                                                   quantiles=5,
                                                                   periods=(1, 2, 3, 4, 5, 6, 10))


# In[174]:


alphalens.tears.create_event_study_tear_sheet(factor_data, pricing, avgretplot=(5, 10))


# In[175]:


alphalens.tears.create_returns_tear_sheet(factor_data,long_short=True, group_neutral=False, by_group=False)


# In[180]:


alphalens.tears.create_information_tear_sheet(factor_data, group_neutral=False, by_group=False)


# In[181]:


alphalens.tears.create_turnover_tear_sheet(factor_data)


# In[ ]:




