@echo off
set PATH=C:\Program Files\PsychoPy2;%PATH%

echo PhD Metacontrol: Leiden University
echo Year 1, study 1: VisCogSearchRATAUT_Study1.
echo Last adjustment on: 2017-02-14
echo r.van.dooren@fsw.leidenuniv.nl

set expStartTime=%DATE:~9,4%%DATE:~6,2%%DATE:~2,3% %TIME:~0,2%%TIME:~3,2%%TIME:~6,2%
set expStartTime=%expStartTime: =%
echo.
echo Current Time: %expStartTime%

set /p subject_ID= "Enter subject ID: "
set /p condition= "Choose condition: diffuse (d), clustered (c), or random (r): "
set /p debug= "Debug (f = false, t = true): "
set /a modulus=%subject_ID% %% 4
IF %modulus% == 1 set result=true
IF %modulus% == 2 set result=true

:: Scrabble pretest
python ./scrabble_practice.py
python ./scrabble_pretest.py %1 %expStartTime% %subject_ID% %condition% %debug%

:: Foraging task
python ./visual_foraging_practice.py %1 %expStartTime% %subject_ID% %condition% %debug%
python ./visual_foraging.py %1 %expStartTime% %subject_ID% %condition% %debug%

:: Scrabble posttest
python ./scrabble_posttest.py %1 %expStartTime% %subject_ID% %condition% %debug%

:: Alternate Uses Task (AUT) and Remote Association Task (RAT)
IF "%result%" == "true" (
	:: Alternate Uses Task first
	python ./alternate_uses_task.py %1 %expStartTime% %subject_ID% %condition% %debug%
	python ./remote_association_task.py %1 %expStartTime% %subject_ID% %condition% %debug%
) ELSE (
	:: Remote Association Task first
	python ./remote_association_task.py %1 %expStartTime% %subject_ID% %condition% %debug%
	python ./alternate_uses_task.py %1 %expStartTime% %subject_ID% %condition% %debug%
)
