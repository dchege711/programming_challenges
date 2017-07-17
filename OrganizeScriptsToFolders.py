import shutil
import os
from os.path import basename
import sys
import glob

# Open each of the scripts and move to new folders
sourcePath = "/Users/dchege711/Reddit_Daily_Programmer/Timeless_Discussions"

for scriptPath in glob.glob(os.path.join(sourcePath, '*.md')):
    fileName = basename(scriptPath).split('.')[0]
    destination = sourcePath + "/" + fileName + "/"
    os.mkdir(destination)
    shutil.move(scriptPath, destination)
