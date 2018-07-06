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
        ''' 
        Popen方法编写的执行器函数
        方法将触发异常 raise  UserWarning('')
        
        '''
        
        if not os.path.isfile(exePath):
            if  not os.path.isfile(self.execDatcomPath):
                raise  UserWarning('Datcom.exe配置错误！无法推定默认配置')
            exePath = self.execDatcomPath
            print('Datcom 执行文件不存在,使用默认配置： %s'%exePath)            
            
        if not os.path.isfile(problemFile):
            print('算例文件不存在 %s 不存在'%problemFile)
            raise  UserWarning('算例文件不存在 %s 不存在'%problemFile)
        
        #检查输入目录是否有效
        if tcwd is None:
            tcwd = os.path.absPath(os.path.dirname(problemFile))
        if not os.path.exists(tcwd):
            print(r'输入目录%s 是一个无效目录'%tcwd)
            try:
                os.makedirs(tcwd)
            except IOError:
                raise  IOError('输入目录不存在（%s），并且无法创建！'%problemFile)

        #执行解算逻辑
        from subprocess import  Popen, PIPE
        command = os.path.abspath(exePath)
        p = Popen(command, stdin=PIPE, stdout=PIPE, stderr=PIPE,  cwd=os.path.abspath(tcwd))

        (outStr, errStr)= p.communicate(bytes(os.path.abspath(problemFile) , encoding='utf8' ))
        returnCode = p.wait()
        #结果转换：从字节流到字符串
        if str(returnCode) in poenReturnCode.keys():
            returnCode = str(returnCode) +": "+  poenReturnCode[str(returnCode) ]
        return returnCode, str(outStr, encoding = "utf8")  , str(errStr, encoding = "utf8")   
        
        
            
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
 
poenReturnCode ={'0': '正确', '1': 'Operation not permitted', '2': 'No such file or directory', '3': 'No such process',
'4': 'Interrupted system call', '5': 'Input/output error', '6': 'No such device or address', '7': 'Argument list too long',
'8': 'Exec format error', '9': 'Bad file descriptor', '10': 'No child processes', '11': 'Resource temporarily unavailable', 
'12': 'Cannot allocate memory', '13': 'Permission denied', '14': 'Bad address', '15': 'Block device required', 
'16': 'Device or resource busy', '17': 'File exists', '18': 'Invalid cross-device link', '19': 'No such device',
'20': 'Not a directory', '21': 'Is a directory', '22': 'Invalid argument', '23': 'Too many open files in system',
'24': 'Too many open files', '25': 'Inappropriate ioctl for device', '26': 'Text file busy', '27': 'File too large', 
'28': 'No space left on device', '29': 'Illegal seek', '30': 'Read-only file system', '31': 'Too many links',
'32': 'Broken pipe', '33': 'Numerical argument out of domain', '34': 'Numerical result out of range',
'35': 'Resource deadlock avoided', '36': 'File name too long', '37': 'No locks available', '38': 'Function not implemented', 
'39': 'Directory not empty', '40': 'Too many levels of symbolic links', '42': 'No message of desired type', 
'43': 'Identifier removed', '44': 'Channel number out of range', '45': 'Level 2 not synchronized', '46': 'Level 3 halted',
'47': 'Level 3 reset', '48': 'Link number out of range', '49': 'Protocol driver not attached', '50': 'No CSI structure available',
'51': 'Level 2 halted', '52': 'Invalid exchange', '53': 'Invalid request descriptor', '54': 'Exchange full', '55': 'No anode', 
'56': 'Invalid request code', '57': 'Invalid slot', '59': 'Bad font file format', '60': 'Device not a stream', 
'61': 'No data available', '62': 'Timer expired', '63': 'Out of streams resources', '64': 'Machine is not on the network',
'65': 'Package not installed', '66': 'Object is remote', '67': 'Link has been severed', '68': 'Advertise error', 
'69': 'Srmount error', '70': 'Communication error on send', '71': 'Protocol error', '72': 'Multihop attempted',
'73': 'RFS specific error', '74': 'Bad message', '75': 'Value too large for defined data type', '76': 'Name not unique on network', 
'77': 'File descriptor in bad state', '78': 'Remote address changed', '79': 'Can not access a needed shared library',
'80': 'Accessing a corrupted shared library', '81': '.lib section in a.out corrupted', '82': 'Attempting to link in too many shared libraries',
'83': 'Cannot exec a shared library directly', '84': 'Invalid or incomplete multibyte or wide character',
'85': 'Interrupted system call should be restarted', '86': 'Streams pipe error', '87': 'Too many users', '88': 'Socket operation on non-socket',
'89': 'Destination address required', '90': 'Message too long', '91': 'Protocol wrong type for socket', '92': 'Protocol not available',
'93': 'Protocol not supported', '94': 'Socket type not supported', '95': 'Operation not supported', '96': 'Protocol family not supported', 
'97': 'Address family not supported by protocol', '98': 'Address already in use', '99': 'Cannot assign requested address', 
'100': 'Network is down', '101': 'Network is unreachable', '102': 'Network dropped connection on reset', 
'103': 'Software caused connection abort', '104': 'Connection reset by peer', '105': 'No buffer space available', 
'106': 'Transport endpoint is already connected', '107': 'Transport endpoint is not connected', 
'108': 'Cannot send after transport endpoint shutdown', '110': 'Connection timed out', 
'111': 'Connection refused', '112': 'Host is down', '113': 'No route to host', 
'114': 'Operation already in progress', '115': 'Operation now in progress', 
'116': 'Stale NFS file handle', '117': 'Structure needs cleaning',
'118': 'Not a XENIX named type file', '119': 'No XENIX semaphores available', '120': 'Is a named type file', 
'121': 'Remote I/O error', '122': 'Disk quota exceeded', '123': 'No medium found', '124': 'Wrong medium type',
'125': 'Operation canceled', '126': 'Required key not available', '127': 'Key has expired', 
'128': 'Key has been revoked', '129': 'Key was rejected by service', '130': 'Owner died', '131': 'State not recoverable'}





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
    
    
