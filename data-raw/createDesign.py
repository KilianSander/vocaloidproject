#!/usr/bin/env python
# -*- coding: utf8 -*-


import numpy  
import sys 
import os
import random


#OUTPUT:
# 0 trial (1..90) --> (1..120)
# 1 session (1..2) --> (1..2)
# 2 blocknr (1..10) [1-5 session 1, 6-10 session 2] --> (1..12) [1-6 sess 1, 7-12 sess 2]
# 3 withinblock trialnr (1..9) * 10 repetitions, one for each block --> (1..10 ) * 12 repetitions (6 per sess)
# 4 stimulus excerpt nr (1..30) #serial order randomized first, then balanced and arranged in blocks of 15 for first and second session
# 5 condition (1=Instrumental only, 2=FULL_HS_h, 3=FULL_HS_a, FULL_VC) #serial order systematically balanced, based on #6 coding 4 serial orders
# 6 coding type of serial order of conditions: first 15 trials condition serpos: 1-2-4-3; second: 2-3-1-4; third: 3-4-2-1, fourth: 4-1-3-2, same in second session
# 7 counter for stimuli: 1..15 in first session, 16-30 in second session

#15 stimuli, first session, in 4 versions (=60 Stimuli) - 15 stimuli, second session, in 4 versions (=60 stimuli)

DataPath = os.getcwd() #application folder! #bug.. change by hand
pathname = '/Users/elke.lange/Documents/sun/Projekte_eigene/Vocaloid/Exp4-Prolific' #directory name
os.chdir(pathname) #change path
DataPath = pathname + os.sep #add seperator

print('create design A for 1/4 of participants ')
total_trials = 120
stim = numpy.arange(30) + 1 #counter for 30 musical excerpts
random.seed(4)
random.shuffle(stim) #1..30 in random order
print(stim)


seq = numpy.repeat([1, 2, 3, 4], 15) #4 large blocks per sess, 1-15 each
seq_c = numpy.concatenate((seq,seq)) #sequence for condition
seq1 = numpy.arange(start=1, stop=16, step=1) #stim 1-15 in sess1
seq2 = numpy.arange(start=16, stop=31, step=1) #stim 16-30 in sess2
seq_s = numpy.concatenate((seq1, seq1, seq1, seq1, seq2, seq2, seq2, seq2)) #sequence for stimuli

design = numpy.zeros((total_trials, 8))  #numpy array 90 trials x 6 variables
for i in range(len(design)): #120 mal
    design[i, 0] = (i+1) #trialnr 1..120
    if i < int(len(design)/2):  #sessionnr
        design[i, 1] = 1
    else: design[i, 1] = 2 
    design[i, 2] = (i//10) + 1 #blocknr (1..10) #//=floor division
    design[i, 3] = (i%10) + 1 #withinblock trialnr (1..10) * 12 blocks, 6 per sess
    design[i, 6] = seq_c[i] #1=con serial order 1-2-4-3; 2= 2-3-1-4; 3= 3-4-2-1, 4=4-1-3-2
    design[i, 7] = seq_s[i] #1=stimuli serial order 1..15; 1..15, 1..15, 1..15, 16..30, 16..30, 16..30, 16..30
    #conditions in fixed serial order 1=I, 2=FULL_HS_h, 3=FULL_HS_a, 4=FULL_VC trialnr 
    #(1,2,4,3) for the first 15, (2,3,1,4) for the second 15, (3,4,2,1) for the third, (4,1,3,2) for the forth 
    #BUT: we dont have 16 stimuli, but 15, that is, for the last repeat, the series is incomplete
    #second session the same.
    if design[i,6] == 1:
        con = numpy.array([1, 2, 4, 3])
        design[i, 5] = con[((i)%4)]
    elif design[i,6] == 2:
        con = numpy.array([2, 3, 1, 4])
        design[i, 5] = con[((i+1)%4)]
    elif design[i,6] == 3: 
        con = numpy.array([3, 4, 2, 1])
        design[i, 5] = con[((i+2)%4)]
    elif design[i,6] == 4: 
        con = numpy.array([4, 1, 3, 2])
        design[i, 5] = con[((i+3)%4)]
    #serial order of stimuli in packages of 15 based on seq_s
    design[i,4] = stim[int(design[i,7]-1)]
    
print(design)
        
DesignPath = DataPath + 'designA.txt'
DesignDataFile = open(DesignPath, 'w')  #w = existing file will be erased; a = append text
DesignText = str(design[:, :]).replace('[','').replace(']','') .replace('.','').replace('   ',' ').replace('  ',' ')
DesignText = DesignText + '\n'
DesignDataFile.write(DesignText)
DesignDataFile.close()
designA = design.copy()
print('end of designA creation')

#designB: create designB by reversing the serial order of stimuli 
#across the two sessions: 1..120 --> 120..1
stimB = stim[::-1] #reversing the serial order of stimuli
print(stimB)
#repeat loop 
design = numpy.zeros((total_trials, 8))  #numpy array 90 trials x 6 variables
for i in range(len(design)): #120 mal
    design[i, 0] = (i+1) #trialnr 1..120
    if i < int(len(design)/2):  #sessionnr
        design[i, 1] = 1
    else: design[i, 1] = 2 
    design[i, 2] = (i//10) + 1 #blocknr (1..10) #//=floor division
    design[i, 3] = (i%10) + 1 #withinblock trialnr (1..10) * 12 blocks, 6 per sess
    design[i, 6] = seq_c[i] #1=con serial order 1-2-4-3; 2= 2-3-1-4; 3= 3-4-2-1, 4=4-1-3-2
    design[i, 7] = seq_s[i] #1=stimuli serial order 1..15; 1..15, 1..15, 1..15, 16..30, 16..30, 16..30, 16..30
    #conditions in fixed serial order 1=I, 2=FULL_HS_h, 3=FULL_HS_a, 4=FULL_VC trialnr 
    #(1,2,4,3) for the first 15, (2,3,1,4) for the second 15, (3,4,2,1) for the third, (4,1,3,2) for the forth 
    #BUT: we dont have 16 stimuli, but 15, that is, for the last repeat, the series is incomplete
    #second session the same.
    if design[i,6] == 1:
        con = numpy.array([1, 2, 4, 3])
        design[i, 5] = con[((i)%4)]
    elif design[i,6] == 2:
        con = numpy.array([2, 3, 1, 4])
        design[i, 5] = con[((i+1)%4)]
    elif design[i,6] == 3: 
        con = numpy.array([3, 4, 2, 1])
        design[i, 5] = con[((i+2)%4)]
    elif design[i,6] == 4: 
        con = numpy.array([4, 1, 3, 2])
        design[i, 5] = con[((i+3)%4)]
    #serial order of stimuli in packages of 15 based on seq_s
    design[i,4] = stimB[int(design[i,7]-1)] #!!DIFFERENT FROM BEFORE

#print(design)

DesignPath = DataPath + 'designB.txt'
DesignDataFile = open(DesignPath, 'w')  #w = existing file will be erased; a = append text
DesignText = str(design[:, :]).replace('[','').replace(']','') .replace('.','').replace('   ',' ').replace('  ',' ')
DesignText = DesignText + '\n'
DesignDataFile.write(DesignText)
DesignDataFile.close()
designB = design.copy() #save as name designB

###############
#DesignC = Design A but changing the sessions 
half = int(design.shape[0]/2)
designC = designA.copy()
for i in range(half): #60 mal
    designC[i, 4] = designA[i+half,4] 
    designC[i+half, 4] = designA[i,4] 
    
    
DesignPath = DataPath + 'designC.txt'
DesignDataFile = open(DesignPath, 'w')  #w = existing file will be erased; a = append text
DesignText = str(designC[:, :]).replace('[','').replace(']','') .replace('.','').replace('   ',' ').replace('  ',' ')
DesignText = DesignText + '\n'
DesignDataFile.write(DesignText)
DesignDataFile.close()

#DesignD = Design B but changing the sessions 
designD = designB.copy()
#designD = numpy.zeros((total_trials, 8)) 
for i in range(half): #60 mal
    designD[i, 4] = designB[i+half,4] 
    designD[i+half, 4] = designB[i,4] 

DesignPath = DataPath + 'designD.txt'
DesignDataFile = open(DesignPath, 'w')  #w = existing file will be erased; a = append text
DesignText = str(designD[:, :]).replace('[','').replace(']','') .replace('.','').replace('   ',' ').replace('  ',' ')
DesignText = DesignText + '\n'
DesignDataFile.write(DesignText)
DesignDataFile.close()
