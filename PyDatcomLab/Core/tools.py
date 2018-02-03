#!/usr/bin/env python


loggerChannel = r'Datcomlogger'

import logging

#定义日志函数
def logInfo(strInfo):
    logging.getLogger(loggerChannel).info(strInfo)
    
def logDebug(strinfo):
    logging.getLogger(loggerChannel).debug(strinfo)
    
def logError(strinfo):
    logging.getLogger(loggerChannel).error(strinfo)

#定义XML的修饰函数
def xml_Indent(elem, level=0):
    """
    修饰xml，使其符合人类阅读习惯
    """
    i = "\n" + level*"  "
    if len(elem):
        if not elem.text or not elem.text.strip():
            elem.text = i + "  "
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
        for elem in elem:
            xml_Indent(elem, level+1)
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
    else:
        if level and (not elem.tail or not elem.tail.strip()):
            elem.tail = i
