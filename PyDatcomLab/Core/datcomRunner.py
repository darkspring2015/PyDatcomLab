#!/usr/bin/env python

import os

class runner(object):
    def __init__(self, problemDir=r'.', execDatcomPath=r'./DATCOM.exe'):
        #检查输入目录是否有效
        if not os.path.exists(problemDir):
            print(r'输入目录%s 是一个无效目录'%problemDir)
            os.makedirs(problemDir)
        self.problemDir = os.path.abspath(problemDir)
        
        #检查可执行文件是否有效
        if not os.path.isfile(execDatcomPath):
            print(r'Datcom.Exe的路径不正确：%s '%execDatcomPath)
        self.execDatcomPath = os.path.abspath(execDatcomPath)
        
    def running(self, exmpleFile=r'forDat.06'):
        
        if not os.path.isfile(exmpleFile):
            print(r'exmpleFile的路径不正确：%s '%exmpleFile)
        self.exmpleFile = os.path.abspath(exmpleFile)
            
        #开始执行
        curdirOld = os.getcwd()
        os.chdir(self.problemDir)
        tempFile = 'temp.txt'
        f=open(tempFile,'w')    # r只读，w可写，a追加
        f.write(self.exmpleFile )
        f.close()
        #调用Datcom
        command = r'%s < %s'%( self.execDatcomPath, tempFile)
        os.system(command)        
        #移除临时文件
        os.remove(tempFile)
        os.chdir(curdirOld)
            
        return '成功执行'
        
    def runningPopen(self, tcwd='.', exePath='datcom.exe', problemFile='EX1.INP'):     
        ''' Popen方法编写的
        '''
        
        if not os.path.isfile(exePath):
            if  not os.path.isfile(self.execDatcomPath):
               return "Datcom.exe配置错误！"
            exePath = self.execDatcomPath
            print('Datcom 执行文件不存在,使用默认配置： %s'%exePath)
            
        if not os.path.isfile(problemFile):
            print('算例文件不存在 %s 不存在'%problemFile)
            return '算例文件不存在 %s 不存在'%problemFile
        
        #检查输入目录是否有效
        if tcwd is None:
            tcwd = os.path.absPath(os.path.dirname(problemFile))
        if not os.path.exists(tcwd):
            print(r'输入目录%s 是一个无效目录'%tcwd)
            try:
                os.makedirs(tcwd)
            except IOError:
                print("IOError")
                
        from subprocess import  Popen, PIPE
        command = os.path.abspath(exePath)
        p = Popen(command, stdin=PIPE, stdout=PIPE, stderr=PIPE,  cwd=os.path.abspath(tcwd))

        (outStr, errStr)= p.communicate(bytes(os.path.abspath(problemFile) , encoding='utf8' ))
        returnCode = p.wait()
        print( "returnCode:",returnCode)
        print(outStr)
        print(errStr)
        
        return '成功执行'
        
        
            
    def runningFiles(self, files, prjDir, exePath):
        '''        
        '''
        if not os.path.isfile(exePath):
            print('Datcom 执行文件不存在,使用默认配置： %s'%exePath)
            return "Datcom.exe配置错误！"
        if not os.path.exists(prjDir):
            print(r'输入目录%s 是一个无效目录'%prjDir)
            try:
                os.makedirs(prjDir)
            except IOError:
                print("IOError")
                
        from subprocess import  Popen, PIPE
        for tfile in files:
            if not os.path.isfile(tfile):
                continue
            (exmpleName,extension) = os.path.splitext(os.path.basename(tfile))
            exmpleDir = os.path.join(prjDir, exmpleName)
            try:
                os.makedirs(exmpleDir)
            except IOError:
                print("IOError")
                continue
            
            command = os.path.abspath(exePath)
            p = Popen(command, stdin=PIPE, stdout=PIPE, stderr=PIPE,  cwd=os.path.abspath(exmpleDir))
            (outStr, errStr)= p.communicate(bytes(os.path.abspath(tfile) , encoding='utf8' ))
            returnCode = p.wait()
            print('%s 执行结果 . %s'%(tfile, returnCode))

def getFiles(dir, ext ='.py'):
    files=[]
    dirB = os.path.abspath(dir)
    for file in os.listdir(dirB):
        if file.endswith(ext):
            files.append(os.path.join(dirB, file))
    return files
 

if __name__ == '__main__':
    pDir = os.path.join(r'extras', r'PyDatcomProjects', r'tests', 'example')
    fDatcomexxc = os.path.join('PyDatcomLab', 'Bin', 'datcom.exe')
    exampleFile =  os.path.join(r'tests', r'data', r'exwin', r'EX1.INP')
    runner = runner(pDir, fDatcomexxc)
    #print(runner.running(exampleFile))
    print(runner.runningPopen(problemFile=exampleFile, tcwd=pDir))
    
    
    #导入
    files = getFiles(os.path.join(r'tests', r'data', r'exwin'), '.INP')
    runner.runningFiles(files,pDir, fDatcomexxc)
    
    
