#!/usr/bin/env python
from os.path import *
import sys
import subprocess
if exists(sys.argv[1]):
    if "bin/" in sys.argv[1]:
        print (subprocess.check_output(sys.argv[1]))
    else:
        with file(sys.argv[1]) as inf:
            print (inf.readline().strip())
