#!/usr/bin/env python
from os.path import *
import sys
if exists(sys.argv[1]):
    with file(sys.argv[1]) as inf:
        print inf.readline().strip()
