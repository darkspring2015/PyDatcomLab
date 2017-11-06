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
