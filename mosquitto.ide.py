import GPS
from os.path import *


def case_exceptions() -> []:
    return ["case_exceptions.xml"]


def initialize_project_plugin():
    for i in case_exceptions():
        if exists(i):
            with open(i) as inf:
                GPS.parse_xml(inf.read())


def finalize_project_plugin():
    pass

