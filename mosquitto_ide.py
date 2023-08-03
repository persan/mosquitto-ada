import GPS
import autoformat
from os.path import *


def initialize_project_plugin():
    if exists("case_exceptions.xml"):
        with open("case_exceptions.xml") as inf:
            GPS.parse_xml(inf.read())


def finalize_project_plugin():
    pass

