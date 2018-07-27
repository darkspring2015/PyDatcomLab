#！
import zipfile, os

def zip_ya( startdir = ".PyDatomLab/3rdParty"):
    """
    startdir =  ".PyDatomLab/3rdParty"  #要压缩的文件夹路径
    """
    tDirName = os.path.split(startdir)[-1]
    file_news = os.path.join(startdir, tDirName +'.zip') # 压缩后文件夹的名字
    z = zipfile.ZipFile(file_news,'w',zipfile.ZIP_DEFLATED) #参数一：文件夹名
    for dirpath, dirnames, filenames in os.walk(startdir):
        fpath = dirpath.replace(startdir,'') #这一句很重要，不replace的话，就从根目录开始复制
        fpath = fpath and fpath + os.sep or ''#这句话理解我也点郁闷，实现当前文件夹以及包含的所有文件的压缩
        for filename in filenames:
            z.write(os.path.join(dirpath, filename),fpath+filename)
            print ('压缩成功')
    z.close()

import shutil #, os,
#打包3rdParty
mainPath = os.path.split(os.path.realpath(__file__))[0]
#for root, dirs, files in os.walk(os.path.join(mainPath,'3rdParty'), topdown=False):
t3rdRoot = os.path.join(mainPath,'PyDatcomLab','3rdParty')
for iD in os.listdir(t3rdRoot):
    iD = os.path.join(t3rdRoot, iD)
    if os.path.isdir(iD) :
        tPath = os.path.join(t3rdRoot, os.path.split(iD)[-1])
        shutil.make_archive(tPath, 'zip', iD)


if __name__=="__main__":
    startdir = ".\\123"  #要压缩的文件夹路径
    file_news = startdir +'.zip' # 压缩后文件夹的名字
    zip_ya(startdir, file_news)
