
import os, logging, shutil #, sys
import chardet , codecs
import re  , tempfile
import markdown
from markdown.extensions.wikilinks import WikiLinkExtension
 
# 获取文件编码类型
def get_encoding(file):
    # 二进制方式读取，获取字节数据，检测类型
    with open(file, 'rb') as f:
        return chardet.detect(f.read())['encoding']
        


class markdownEngine(object):
    """
    编写一个Markdown的引擎，用来将md文件装换为Html文件
    """
    def __init__(self , iProperties ={}):
        """
        构造函数
        """
        super(markdownEngine, self).__init__()
        #日志
        self.logger = logging.getLogger(r'Datcomlogger')       
        #定义配置参数
        self.Properties = {'extFilter': ["*.md"], 
                                'docDirectory':os.path.join(os.path.expanduser('~'), '.PyDatcomLab', 'wiki'), 
                                'MathJaxPath':r'http://cdn.mathjax.org/mathjax/latest/MathJax.js', 
        }
        if iProperties is not None and type(iProperties) is dict:
            self.Properties.update(iProperties)    
        #定义CSS
        self.cssSet = {'Default':os.path.join('css', 'diy1.css') , 
                            'Github': os.path.join('css', 'typora_defualt.css'),         
        }
        #MathJax支持
        if 'MathJaxOnLinePath' in self.Properties:
            tUrl = self.Properties['MathJaxOnLinePath']
        else:
            tUrl = r'http://cdn.mathjax.org/mathjax/latest/MathJax.js'
        self.mathJaxScriptOnline = """
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
        <script type="text/javascript" src="%s?config=TeX-AMS-MML_HTMLorMML">
</script>
        """%(tUrl)
        #配置HTML的头和CSS信息
        self.defaultCSS = self._loadCSS()
        if os.path.isfile( self.Properties['MathJaxPath']):
            self.mathJaxInstall = self.Properties['MathJaxPath']
            
            self.mathJaxHeader ="""
            <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
            <script type="text/javascript" src="%s?config=TeX-AMS-MML_HTMLorMML"">
              MathJax.Hub.Config({
        extensions: ["tex2jax.js"],
        jax: ["input/TeX","output/HTML-CSS"],
        tex2jax: {
        inlineMath: [['$','$'],['\\(','\\)']],
        displayMath: [ ['$$','$$'], ["\\[","\\]"] ]}
      });
            </script>

            """%(self.mathJaxInstall)
        else:
            self.mathJaxHeader = """%s"""%(self.mathJaxScriptOnline)           
        #定义图像的正则表达式
        self.regImage = re.compile(r"""<img\s.*?\s?src\s*=\s*['|"]?([^\s'"]+).*?>""",re.I)  
        
        #创建临时目录，执行初始化
        self.tempDirectory = os.path.join(os.path.expanduser('~'), '.PyDatcomLab', 'wiki')
        if 'PyDatcomWorkspace' in self.Properties:
            self.tempDirectory =  os.path.join(os.path.realpath(self.Properties['PyDatcomWorkspace']), '.PyDatcomLab', 'wiki')
        try:
            self.tempDirectory= tempfile.mkdtemp(suffix='',prefix='Datcom')
        except Exception as e:
            self.logger.warning("markdownEngine()无法创建临时目录!")
            
    
    def _loadCSS(self, iPath = None):
        """
        加载配置文件，iPath为输入文件的路径
        """
        if iPath is None or not os.path.isfile(iPath):
            #iPath = os.path.join(os.path.split(os.path.realpath(__file__))[0], 'css', 'typora_defualt.css')
            iPath = os.path.join(os.path.split(os.path.realpath(__file__))[0], self.cssSet['Default'] )
        try:            
            with open(iPath, 'rb') as f:
                tFile = f.read()
                tCodecs = chardet.detect(tFile)['encoding']
                tText = tFile.decode(tCodecs) 
                return tText           
        except Exception as e:
            self.logger.warning("_loadCSS():%s"%e)
         
       
    def setCSS(self, iKey, iCss):
        """
        增加一个CSS
        iKey是索引名称
        iCSS是CSS文件的路径
        """
        self.cssSet.update({ iKey:iCss})
    
    def getCSS(self, iKey='Default'):
        """
        查询现有的CSS
        如果iKey默认为'Default'；
        如果iKey不存在，则返回'Default'的CSS的路径
        """
        return self.cssSet.get(iKey, self.cssSet['Default'])
    
    def getAllCSSKeys(self):
        """
        返回所有CSS的Key
        """
        return self.cssSet.keys().copy()
        
    def _readFile(self, iPath):
        """
        读取文件编码 # 获取文件编码类型
        """
        # 二进制方式读取，获取字节数据，检测类型
        with open(iPath, 'rb') as f:
            tFile = f.read()
            return tFile, chardet.detect(tFile)['encoding']
            
        
    def md2Html(self, iPath, iStyle ='Default'):
        """
        将Markdown文件装换为Html文件输出
        函数行为：
        1.使用临时文件目录输出
        """

        #tSysCodecs = sys.getdefaultencoding()
        #print("System Codes:%s"%tSysCodecs)        
        if os.path.isfile(iPath):
            try:
                #打开读取MD文件
                #获取文件的编码
                tEncode = get_encoding(iPath)
                print(tEncode)
                input_file = codecs.open(iPath, mode="r", encoding=tEncode)  #"utf-8"
                text = input_file.read()
                input_file.close()     
                #转换HTML
                html = markdown.markdown(text, output_format='html5', \
                    extensions=['markdown.extensions.toc',\
                    WikiLinkExtension(base_url='https://en.wikipedia.org/wiki/',\
                        end_url='#Hyperlinks_in_wikis'),\
                    'markdown.extensions.sane_lists',\
                    'markdown.extensions.codehilite',\
                    'markdown.extensions.abbr',\
                    'markdown.extensions.attr_list',\
                    'markdown.extensions.def_list',\
                    'markdown.extensions.fenced_code',\
                    'markdown.extensions.footnotes',\
                    'markdown.extensions.smart_strong',\
                    'markdown.extensions.meta',\
                    'markdown.extensions.nl2br',\
                    'markdown.extensions.tables', 
                    'mdx_math'])

                #写到HTML                
                #创建运行目录
                tFn, tExt = os.path.splitext(os.path.basename(iPath))
                tDir = os.path.dirname(os.path.realpath(iPath))
                #分析临时目录
                tOutPath = os.path.join(self.tempDirectory , '%s.html'%(tFn))
                if iStyle in self.cssSet:
                    tSt = self._loadCSS(self.cssSet[iStyle])                    
                else:
                    tSt = self._loadCSS(self.cssSet['Default'])
                tHtml =  self.mathJaxHeader + tSt + html
                #分析图像的路径，复制图像文件
                tFinds = self.regImage.findall(html)
                if tFinds is not None:
                    #迭代添加
                    for iM in tFinds :
                        if os.path.isabs(iM):
                            self.logger.warning("Img Tag使用了绝对路径，不复制！")
                            continue
                        #处理相对路径
                        tFileSrc  = os.path.join(tDir, iM)
                        tFileObj  = os.path.join(self.tempDirectory , iM) 
                        tObjDir   = os.path.dirname(tFileObj)
                        #检查并创建目标目录
                        if not os.path.exists(tObjDir):
                            os.makedirs(tObjDir)
                            self.logger.info("创建了%s"%tObjDir)
                        #复制替换对应文件
                        if os.path.isfile(tFileSrc):
                            if  os.path.isfile(tFileObj):
                                os.remove(tFileObj)
                            shutil.copy(tFileSrc, tObjDir)
                            self.logger.info("复制%s To %s"%(tFileSrc, tFileObj))
                        else:
                            self.logger.warning("md2Html无法处理的<img>tag！")
                #写入到文件系统
                output_file = codecs.open(tOutPath, "w", 
                          encoding="utf-8",   #"utf-8"
                          errors="xmlcharrefreplace")
                output_file.write(tHtml)
                output_file.close()
                #更新右侧显示
                return tOutPath, tHtml
            except Exception as e:
                self.logger.warning("md2Html():%s"%e)
