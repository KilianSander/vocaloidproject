#!/usr/bin/env python
# -*- coding: utf8 -*-


import numpy  
import sys 
import os
import random


#OUTPUT:
# 0 trial (1..90) --> (1..120)
# 1 session (1..2..3) --> (1..2..3)
# 2 blocknr (1..12) [1-4 session 1, 5-8 session 2, 9-12 session 3] 
# 3 withinblock trialnr (1..10) * 12 repetitions, one for each block (4 per sess)
# 4 stimulus excerpt nr (1..30) #serial order randomized first, then balanced and arranged in blocks of 10 for first, second, third session
# 5 condition (1=Instrumental only, 2=FULL_HS_h, 3=FULL_HS_a, 4=FULL_VC) #serial order systematically balanced, based on #6 coding 4 serial orders
# 6 coding type of serial order of conditions: first 10 trials condition serpos: 1-2-4-3; second: 2-3-1-4; third: 4-1-3-2, fourth: 3-4-2-1,  same in second session
# 7 counter for stimuli: 1..10 in first session, 11-20 in second session, 21-30 in third session

#10 stimuli, first-third session, in 4 versions (=40 Stimuli) 

######
#design: 3 blocks, one per session; but balancing partially kept from 2 sessions design
#A 1AB, 2AB, 3AB
#B: 3BA (backwards), 2BA (backwards), 1BA (backwards)
#C: 2B, 3AB, 1AB, 2A (as in A forwards)
#D: 2A (backwards), 1BA (backwards), 3BA (backwards), 2B (backwards)

DataPath = os.getcwd() #application folder! #bug.. change by hand
pathname = '/Users/elke.lange/Documents/sun/Projekte_eigene/Vocaloid/Exp4-Prolific/Designs' #directory name
os.chdir(pathname) #change path
DataPath = pathname + os.sep #add seperator

print('create design A for 1/4 of participants ')
total_trials = 120
stim = numpy.arange(30) + 1 #counter for 30 musical excerpts
random.seed(4)
random.shuffle(stim) #1..30 in random order
print(stim)

seq = numpy.repeat([1, 2, 3, 4], 10) #4 large blocks per sess, 1-10 each
seq_c = numpy.concatenate((seq,seq,seq)) #sequence for condition
seq1 = numpy.arange(start=1, stop=11, step=1) #stim 1-10 in sess1
seq2 = numpy.arange(start=11, stop=21, step=1) #stim 11-20 in sess2
seq3 = numpy.arange(start=21, stop=31, step=1) #stim 21-30 in sess2
seq_s = numpy.concatenate((seq1, seq1, seq1, seq1, seq2, seq2, seq2, seq2, seq3, seq3, seq3, seq3)) #sequence for stimuli

design = numpy.zeros((total_trials, 8))  #numpy array 90 trials x 6 variables
for i in range(len(design)): #120 mal
    design[i, 0] = (i+1) #trialnr 1..120
    if i < int(len(design)/3):  #sessionnr 1
        design[i, 1] = 1
    elif i < int((len(design)/3)*2):  #sessionnr 2
        design[i, 1] = 2
    else: design[i, 1] = 3 #sessionnr 3
    design[i, 2] = (i//10) + 1 #blocknr (1..10) #//=floor division
    design[i, 3] = (i%10) + 1 #withinblock trialnr (1..10) * 12 blocks, 6 per sess
    design[i, 6] = seq_c[i] #1=con serial order 1-2-4-3; 2=2-3-1-4; 3=4-1-3-2, 4=3-4-2-1 in the first session of 40 trials, then 1=2-3-1-4, 2=4-1-3-2, in last session 1=4-1-3-2 etc. 
    design[i, 7] = seq_s[i] #1=stimuli serial order 4 x 1..10; 4 x 11..20, 4 x 21-30 
    #conditions in fixed serial order 1=I, 2=FULL_HS_h, 3=FULL_HS_a, 4=FULL_VC trialnr 
    #(1,2,4,3) for the first 10, (2,3,1,4) for the second 10, (4,1,3,2) for the third, (3,4,2,1) for the forth 
    #BUT: we dont have 16 stimuli, but 10 per session, that is, for the repeats, the series is incomplete
    #second session the same.
    if design[i,6] == 1 and design[i,1] == 1:
        con = numpy.array([1, 2, 4, 3])
        design[i, 5] = con[((i)%4)]
    elif design[i,6] == 2 and design[i,1] == 1:
        con = numpy.array([2, 3, 1, 4])
        design[i, 5] = con[((i+2)%4)]
    elif design[i,6] == 3 and design[i,1] == 1: 
        con = numpy.array([4, 1, 3, 2])
        design[i, 5] = con[((i)%4)]
    elif design[i,6] == 4 and design[i,1] == 1: 
        con = numpy.array([3, 4, 2, 1]) 
        design[i, 5] = con[((i+2)%4)]
    elif design[i,6] == 1 and design[i,1] == 2: #start with second condition array
        con = numpy.array([2, 3, 1, 4])
        design[i, 5] = con[((i)%4)]
    elif design[i,6] == 2 and design[i,1] == 2:
        con = numpy.array([4, 1, 3, 2])
        design[i, 5] = con[((i+2)%4)]
    elif design[i,6] == 3 and design[i,1] == 2: 
        con = numpy.array([3, 4, 2, 1])
        design[i, 5] = con[((i)%4)]
    elif design[i,6] == 4 and design[i,1] == 2: 
        con = numpy.array([1, 2, 4, 3]) 
        design[i, 5] = con[((i+2)%4)] 
    elif design[i,6] == 1 and design[i,1] == 3:
        con = numpy.array([4, 1, 3, 2]) #starts with third entry of conditions
        design[i, 5] = con[((i)%4)]
    elif design[i,6] == 2 and design[i,1] == 3:
        con = numpy.array([3, 4, 2, 1]) 
        design[i, 5] = con[((i+2)%4)]
    elif design[i,6] == 3 and design[i,1] == 3: 
        con = numpy.array([1, 2, 4, 3]) 
        design[i, 5] = con[((i)%4)]
    elif design[i,6] == 4 and design[i,1] == 3: 
        con = numpy.array([2, 3, 1, 4]) 
        design[i, 5] = con[((i+2)%4)]
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

##################
#designB: create designB by reversing the serial order of stimuli 
#across the three sessions: 1..120 --> 120..1
stimB = stim[::-1] #reversing the serial order of stimuli, but the 10 stimuli of the second session stay in the second session? 
print(stimB)
#repeat loop 
design = numpy.zeros((total_trials, 8))  #numpy array 90 trials x 6 variables
for i in range(len(design)): #120 mal
    design[i, 0] = (i+1) #trialnr 1..120
    if i < int(len(design)/3):  #sessionnr 1
        design[i, 1] = 1
    elif i < int((len(design)/3)*2):  #sessionnr 2
        design[i, 1] = 2
    else: design[i, 1] = 3 #sessionnr 3
    design[i, 2] = (i//10) + 1 #blocknr (1..10) #//=floor division
    design[i, 3] = (i%10) + 1 #withinblock trialnr (1..10) * 12 blocks, 6 per sess
    design[i, 6] = seq_c[i] #1=con serial order 1-2-4-3; 2=2-3-1-4; 3=4-1-3-2, 4=3-4-2-1 in the first session of 40 trials, then 1=2-3-1-4, 2=4-1-3-2, in last session 1=4-1-3-2 etc. 
    design[i, 7] = seq_s[i] #1=stimuli serial order 4 x 1..10; 4 x 11..20, 4 x 21-30 
    #conditions in fixed serial order 1=I, 2=FULL_HS_h, 3=FULL_HS_a, 4=FULL_VC trialnr 
    #(1,2,4,3) for the first 10, (2,3,1,4) for the second 10, (4,1,3,2) for the third, (3,4,2,1) for the forth 
    #BUT: we dont have 16 stimuli, but 10 per session, that is, for the repeats, the series is incomplete
    #second session the same.
    if design[i,6] == 1 and design[i,1] == 1:
        con = numpy.array([1, 2, 4, 3])
        design[i, 5] = con[((i)%4)]
    elif design[i,6] == 2 and design[i,1] == 1:
        con = numpy.array([2, 3, 1, 4])
        design[i, 5] = con[((i+2)%4)]
    elif design[i,6] == 3 and design[i,1] == 1: 
        con = numpy.array([4, 1, 3, 2])
        design[i, 5] = con[((i)%4)]
    elif design[i,6] == 4 and design[i,1] == 1: 
        con = numpy.array([3, 4, 2, 1]) 
        design[i, 5] = con[((i+2)%4)]
    elif design[i,6] == 1 and design[i,1] == 2: #start with second condition array
        con = numpy.array([2, 3, 1, 4])
        design[i, 5] = con[((i)%4)]
    elif design[i,6] == 2 and design[i,1] == 2:
        con = numpy.array([4, 1, 3, 2])
        design[i, 5] = con[((i+2)%4)]
    elif design[i,6] == 3 and design[i,1] == 2: 
        con = numpy.array([3, 4, 2, 1])
        design[i, 5] = con[((i)%4)]
    elif design[i,6] == 4 and design[i,1] == 2: 
        con = numpy.array([1, 2, 4, 3]) 
        design[i, 5] = con[((i+2)%4)] 
    elif design[i,6] == 1 and design[i,1] == 3:
        con = numpy.array([4, 1, 3, 2]) #starts with third entry of conditions
        design[i, 5] = con[((i)%4)]
    elif design[i,6] == 2 and design[i,1] == 3:
        con = numpy.array([3, 4, 2, 1]) 
        design[i, 5] = con[((i+2)%4)]
    elif design[i,6] == 3 and design[i,1] == 3: 
        con = numpy.array([1, 2, 4, 3]) 
        design[i, 5] = con[((i)%4)]
    elif design[i,6] == 4 and design[i,1] == 3: 
        con = numpy.array([2, 3, 1, 4]) 
        design[i, 5] = con[((i+2)%4)]
    #serial order of stimuli in packages of 15 based on seq_s
    design[i,4] = stimB[int(design[i,7]-1)] #!!DIFFERENT FROM BEFORE, backwards

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



