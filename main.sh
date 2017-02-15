#!/bin/bash

echo """PhD Metacontrol: Leiden University
Year 1, study 1: VisCogSearchRATAUT_Study1.
Last adjustment on: 2017-02-14
r.van.dooren@fsw.leidenuniv.nl
"""

expStartTime=$(date +%Y%m%d%H%M%S)
echo "Current Time:" $expStartTime

echo -n "Enter subject ID: "
read subject_ID
echo -n "Choose condition: diffuse (d), clustered (c), or random (r): "
read condition
echo -n "Debug (f = false, t = true): "
read debug

# Scrabble pretest
./scrabble_practice.py
./scrabble_pretest.py $expStartTime $subject_ID $condition $debug

# Foraging task
./visual_foraging_practice.py $expStartTime $subject_ID $condition $debug
./visual_foraging.py $expStartTime $subject_ID $condition $debug

# Scrabble posttest
./scrabble_posttest.py $expStartTime $subject_ID $condition $debug

# Alternate Uses Task (AUT) and Remote Association Task (RAT)
if (($subject_ID % 4 == 1 || $subject_ID % 4 == 2))
then
# Alternate Uses Task first
./alternate_uses_task.py $expStartTime $subject_ID $condition $debug
./remote_association_task.py $expStartTime $subject_ID $condition $debug
else
# Remote Association Task first
./remote_association_task.py $expStartTime $subject_ID $condition $debug
./alternate_uses_task.py $expStartTime $subject_ID $condition $debug
fi

