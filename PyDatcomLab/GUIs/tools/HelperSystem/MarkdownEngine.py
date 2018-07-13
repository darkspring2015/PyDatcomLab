
import os, logging, sys, shutil
import chardet , codecs
import re  
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
    def __init__(self):
        """
        构造函数
        """
        super(markdownEngine, self).__init__()
        #日志
        self.logger = logging.getLogger(r'Datcomlogger')       
        #定义CSS
        self.cssSet = {'Default': os.path.join('css', 'typora_defualt.css'), 
        'DIY': os.path.join('css', 'diy1.css'), 
        
        }
        #MathJax支持
        self.mathJaxScriptOnline = """
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
        <script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>
        """
        #
        self.mathJaxInstall = os.path.join(os.path.realpath("E:\Solution\WebDevelopment\MathJax"), "MathJax.js")
        self.defaultCSS = self._loadCSS()
        self.mathJaxScriptLocal ="""
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
        <script type="text/javascript" src="%s?config=default">
          MathJax.Hub.Config({
    extensions: ["tex2jax.js"],
    jax: ["input/TeX","output/HTML-CSS"],
    tex2jax: {
    inlineMath: [['$','$'],['\\(','\\)']],
    displayMath: [ ['$$','$$'], ["\\[","\\]"] ]}
  });
        </script>
%s
        """%(self.mathJaxInstall, self.defaultCSS)
        
        self.regImage = re.compile(r"""<img\s.*?\s?src\s*=\s*['|"]?([^\s'"]+).*?>""",re.I)  
        


    
    def _loadCSS(self, iPath = None):
        """
        加载配置文件
        """
        if iPath is None or not os.path.isfile(iPath):
            #iPath = os.path.join(os.path.split(os.path.realpath(__file__))[0], 'css', 'typora_defualt.css')
            iPath = os.path.join(os.path.split(os.path.realpath(__file__))[0], 'css', 'diy1.css')
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
        iCSS是CSS的定义，用三引号定义
        """
        self.cssSet.update({ iKey:iCss})
    
    def getCSS(self, iKey='Default'):
        """
        查询现有的CSS
        如果iKey默认为'Default'；
        如果iKey不存在，则返回'Default'的CSS
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

        tSysCodecs = sys.getdefaultencoding()
        print("System Codes:%s"%tSysCodecs)
        
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
                #创建临时目录
                import tempfile
                tmpPath= tempfile.mkdtemp(suffix='',prefix='Datcom')
                tOutPath = os.path.join(tmpPath, '%s.html'%(tFn))
                if iStyle in self.cssSet:
                    tSt = self._loadCSS(self.cssSet[iStyle])                    
                else:
                    tSt = self._loadCSS(self.cssSet['Default'])
                tHtml =  self.mathJaxScriptLocal + tSt + html
                #分析图像的路径
                tFinds = self.regImage.findall(html)
                if tFinds is not None:
                    #迭代添加
                    for iM in tFinds :
                        tFileSrc = os.path.join(tDir, iM)
                        tFileObj  = os.path.join(tmpPath, iM) 
                        tObjDir = os.path.dirname(tFileObj)
                        if not os.path.exists(tObjDir):
                            os.makedirs(tObjDir)
                            self.logger.info("创建了%s"%tObjDir)
                        if os.path.isfile(tFileSrc):
                            shutil.copy(tFileSrc, tObjDir)
                            self.logger.info("复制%s To %s"%(tFileSrc, tFileObj))
                        #print(iM,tFileSrc, tFileObj )                    
                    
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
