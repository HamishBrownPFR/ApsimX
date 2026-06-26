# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.19.3
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# %%
import datetime as dt
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import APSIMGraphHelpers as AGH
import GraphHelpers as GH
from scipy import stats
import sqlite3

# %%
np.power(2,6)

# %%
Roberts = pd.read_excel('CEData.xlsx',sheet_name='Roberts_etal_1988')
Roberts.set_index(['Cultivar','FinalT','InitPp','FinalPp','InitT','Experiment.Script.TransitionDay'],drop=False,inplace=True)
Roberts.sort_index(axis=0,inplace=True)
Roberts.sort_index(axis=1,inplace=True)

# %%
RobCults = Roberts.index.get_level_values(0).drop_duplicates()
RobInitTemps = Roberts.index.get_level_values(4).drop_duplicates()
RobInitPps = Roberts.index.get_level_values(2).drop_duplicates()
RobFinalTemps = Roberts.index.get_level_values(1).drop_duplicates()
RobFinalPps = Roberts.index.get_level_values(3).drop_duplicates()

# %%
RobCults

# %%
RobInitTemps

# %%
RobInitPps

# %%
RobFinalTemps

# %%
RobFinalPps

# %%
cols = dict(zip(RobInitTemps.values,['k','goldenrod','lightskyblue']))
fill = dict(zip(RobInitPps.values,[False,True]))
lins = dict(zip(RobInitPps.values,['D--','o-']))
ylims = dict(zip(RobCults.values,[(500,2000),(300,1000)]))

# %%
Graph = plt.figure(figsize=(10,20))
pos = 1
for c in RobCults:
    for ft in RobFinalTemps:
        for fp in RobFinalPps:
            ax = Graph.add_subplot(4,2,pos)
            plt.text(0.95,0.95,c+" "+str(ft)+"oC "+str(fp)+"h",horizontalalignment='right',verticalalignment='top', transform=ax.transAxes)
            for it in RobInitTemps:
                for ip in RobInitPps:
                    datfilter = (Roberts.Cultivar==c)&(Roberts.FinalT==ft)&(Roberts.FinalPp==fp)&(Roberts.InitT==it)&(Roberts.InitPp==ip)
                    dat = Roberts.loc[datfilter,:]
                    fc = cols[it]
                    ls = lins[ip]
                    if ip == 8:
                        fc = 'w'
                    plt.plot(dat.loc[:,"Experiment.Script.TransitionDay"],dat.loc[:,"Lentil.Phenology.StartFloweringDAS"],ls,color = cols[it],mfc=fc,label=' temp = '+str(it)+'oC '+'Pp = '+str(ip))
            #plt.ylim(ylims[c][0],ylims[c][1])
            plt.ylim(0,160)
            if pos in [4,8]:
                plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
            pos+=1

# %%
Graph = plt.figure(figsize=(10,5))
pos = 1
for c in RobCults:
    for ft in RobFinalTemps:
        for fp in RobFinalPps:
            ax = Graph.add_subplot(2,4,pos)
            plt.text(0.95,0.95,c+" "+str(ft)+"oC "+str(fp)+"h",horizontalalignment='right',verticalalignment='top', transform=ax.transAxes)
            for it in RobInitTemps:
                for ip in RobInitPps:
                    datfilter = (Roberts.Cultivar==c)&(Roberts.FinalT==ft)&(Roberts.FinalPp==fp)&(Roberts.InitT==it)&(Roberts.InitPp==ip)
                    dat = Roberts.loc[datfilter,:]
                    fc = cols[it]
                    ls = lins[ip]
                    if ip == 8:
                        fc = 'w'
                    plt.plot(dat.loc[:,"Experiment.Script.TransitionDay"],dat.TtFirstFlower,ls,color = cols[it],mfc=fc, label=' temp = '+str(it)+'oC '+'Pp = '+str(ip) )
            plt.ylim(ylims[c][0],ylims[c][1])
            if pos in [4,8]:
                plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
            pos+=1

# %%
Summerfield = pd.read_excel('CEData.xlsx',sheet_name='Summerfield_etal_1985')
Summerfield.set_index(['Cultivar','FinalT','InitPp','FinalPp','InitT','Experiment.Script.TransitionDay'],drop=False,inplace=True)
Summerfield.sort_index(axis=0,inplace=True)
Summerfield.sort_index(axis=1,inplace=True)

# %%
SumCults = Summerfield.index.get_level_values(0).drop_duplicates()
SumInitTemps = Summerfield.index.get_level_values(4).drop_duplicates()
SumInitPps = Summerfield.index.get_level_values(2).drop_duplicates()
SumFinalTemps = Summerfield.index.get_level_values(1).drop_duplicates()
SumFinalPps = Summerfield.index.get_level_values(3).drop_duplicates()
SumInitDurats = Summerfield.index.get_level_values(5).drop_duplicates()

# %%
SumCults

# %%
SumInitTemps

# %%
SumInitPps

# %%
SumFinalTemps

# %%
SumFinalPps

# %%
SumInitDurats

# %%
cols = dict(zip(SumFinalPps.values,['grey','darkgoldenrod','darkorange']))
fill = dict(zip(SumInitDurats.values,[False,True]))
lins = dict(zip(SumInitDurats.values,['o--','o-']))
#ylims = dict(zip(RobCults.values,[(500,2000),(300,1000)]))
verns = dict(zip(SumInitDurats.values,['Nil','Chill']))

# %%
Graph = plt.figure(figsize=(5,10))
pos = 1
for c in SumCults:
    ax = Graph.add_subplot(3,1,pos)
    plt.text(0.95,0.95,c,horizontalalignment='right',verticalalignment='top', transform=ax.transAxes)
    for id in SumInitDurats:
        for fp in SumFinalPps:
            datfilter = (Summerfield.Cultivar==c)&(Summerfield.loc[:,"Experiment.Script.TransitionDay"]==id)&(Summerfield.FinalPp==fp)
            dat = Summerfield.loc[datfilter,:]
            fc = cols[fp]
            ls = lins[id]
            if id == 0:
                fc = 'w'
            plt.plot(dat.FinalT,dat.loc[:,"Lentil.Phenology.StartFloweringDAS"],ls,color = cols[fp],mfc=fc,label = 'Vern = '+str(verns[id])+' Pp = '+str(fp))
    #plt.ylim(ylims[c][0],ylims[c][1])
    plt.ylim(0,130)
    plt.xlim(5,25)
    plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
    pos+=1

# %%
Graph = plt.figure(figsize=(5,10))
pos = 1
for c in SumCults:
    ax = Graph.add_subplot(3,1,pos)
    plt.text(0.95,0.95,c,horizontalalignment='right',verticalalignment='top', transform=ax.transAxes)
    for id in SumInitDurats:
        for fp in SumFinalPps:
            datfilter = (Summerfield.Cultivar==c)&(Summerfield.loc[:,"Experiment.Script.TransitionDay"]==id)&(Summerfield.FinalPp==fp)
            dat = Summerfield.loc[datfilter,:]
            fc = cols[fp]
            ls = lins[id]
            if id == 0:
                fc = 'w'
            plt.plot(dat.FinalT,dat.TtFirstFlower,ls,color = cols[fp],mfc=fc,label = 'Vern = '+str(verns[id])+' Pp = '+str(fp))
    #plt.ylim(ylims[c][0],ylims[c][1])
    plt.ylim(0,2000)
    plt.xlim(5,25)
    plt.legend(loc='center left', bbox_to_anchor=(1, 0.5))
    pos+=1

# %%
Roberts86 = pd.read_excel('CEData.xlsx',sheet_name='Roberts_etal_1986')
Roberts86.set_index(['Cultivar','FinalT','InitPp','FinalPp','InitT','Experiment.Script.TransitionDay'],drop=False,inplace=True)
Roberts86.sort_index(axis=0,inplace=True)
Roberts86.sort_index(axis=1,inplace=True)

# %%
Rob86Cults = Roberts86.index.get_level_values(0).drop_duplicates()
Rob86InitTemps = Roberts86.index.get_level_values(4).drop_duplicates()
Rob86InitPps = Roberts86.index.get_level_values(2).drop_duplicates()
Rob86FinalTemps = Roberts86.index.get_level_values(1).drop_duplicates()
Rob86FinalPps = Roberts86.index.get_level_values(3).drop_duplicates()
Rob86InitDurats = Roberts86.index.get_level_values(5).drop_duplicates()

# %%
Rob86Cults

# %%
Rob86InitTemps

# %%
Rob86InitPps

# %%
Rob86FinalTemps

# %%
Rob86FinalPps

# %%
Rob86InitDurats

# %%
Graph = plt.figure(figsize=(5,10))
pos = 1
for c in Rob86Cults:
    ax = Graph.add_subplot(3,1,pos)
    plt.text(0.05,0.95,c,horizontalalignment='left',verticalalignment='top', transform=ax.transAxes)
    for ip in Rob86InitPps:
        for fp in Rob86FinalPps:
            #if (ip != fp):
            datfilter = (Roberts86.Cultivar==c)&(Roberts86.InitPp==ip)&(Roberts86.FinalPp==fp)
            dat = Roberts86.loc[datfilter,:]
            if (len(dat.Cultivar)>0):
                if ip == 8:
                    ec = 'b'
                    fc = 'w'
                if ip == 10:
                    ec = 'r'
                    fc = 'w'
                if ip == 16:
                    if fp == 8:
                        ec = 'b'
                        fc = 'b'
                    if fp == 10:
                        ec = 'r'
                        fc = 'r'
                plt.plot(dat.loc[:,"Experiment.Script.TransitionDay"],dat.loc[:,"Lentil.Phenology.StartFloweringDAS"],'o',mfc = fc, mec = ec,label = str(ip)+' -> '+str(fp))
    #plt.ylim(ylims[c][0],ylims[c][1])
    plt.ylim(0,150)
    #plt.xlim(5,25)
    plt.legend()
    pos+=1


# %%
Graph = plt.figure(figsize=(5,10))
pos = 1
for c in Rob86Cults:
    ax = Graph.add_subplot(3,1,pos)
    plt.text(0.05,0.95,c,horizontalalignment='left',verticalalignment='top', transform=ax.transAxes)
    for ip in Rob86InitPps:
        for fp in Rob86FinalPps:
            #if (ip != fp):
            datfilter = (Roberts86.Cultivar==c)&(Roberts86.InitPp==ip)&(Roberts86.FinalPp==fp)
            dat = Roberts86.loc[datfilter,:]
            if (len(dat.Cultivar)>0):
                if ip == 8:
                    ec = 'b'
                    fc = 'w'
                if ip == 10:
                    ec = 'r'
                    fc = 'w'
                if ip == 16:
                    if fp == 8:
                        ec = 'b'
                        fc = 'b'
                    if fp == 10:
                        ec = 'r'
                        fc = 'r'
                plt.plot(dat.loc[:,"Experiment.Script.TransitionDay"],dat.TtFirstBud,'o',mfc = fc, mec = ec,label = str(ip)+' -> '+str(fp))
    #plt.ylim(ylims[c][0],ylims[c][1])
    plt.ylim(0,2400)
    #plt.xlim(5,25)
    plt.legend()
    pos+=1


# %%
Graph = plt.figure(figsize=(5,10))
pos = 1
for c in Rob86Cults:
    ax = Graph.add_subplot(3,1,pos)
    plt.text(0.05,0.95,c,horizontalalignment='left',verticalalignment='top', transform=ax.transAxes)
    for ip in Rob86InitPps:
        for fp in Rob86FinalPps:
            #if (ip != fp):
            datfilter = (Roberts86.Cultivar==c)&(Roberts86.InitPp==ip)&(Roberts86.FinalPp==fp)
            dat = Roberts86.loc[datfilter,:]
            if (len(dat.Cultivar)>0):
                if ip == 8:
                    ec = 'b'
                    fc = 'w'
                if ip == 10:
                    ec = 'r'
                    fc = 'w'
                if ip == 16:
                    if fp == 8:
                        ec = 'b'
                        fc = 'b'
                    if fp == 10:
                        ec = 'r'
                        fc = 'r'
                plt.plot(dat.loc[:,"Experiment.Script.TransitionDay"],dat.TtFirstFlower-dat.TtFirstBud,'o',mfc = fc, mec = ec,label = str(ip)+' -> '+str(fp))
    #plt.ylim(ylims[c][0],ylims[c][1])
    plt.ylim(0,1500)
    #plt.xlim(5,25)
    plt.legend()
    pos+=1

