#!/usr/bin/env python
import os , sys, re
import ply.yacc as yacc
import ply.lex as lex
from ply.lex import TOKEN


class Parser(object):
    """
    Base class for a lexer/parser that has the rules defined as methods
    """
    tokens = ()
    precedence = ()

    def __init__(self, file_name=None, debug=0,
                keep_parse_tab=False):
        self.debug = debug
        self.file_name = file_name
        self.keep_parse_tab = keep_parse_tab
        self.cases = []
        self.common_dicts = []
        try:
            modname = os.path.split(
                os.path.splitext(__file__)[0])[1] + \
            "_" + self.__class__.__name__
        except:
            modname = "parser" + "_" + self.__class__.__name__
        self.debugfile = modname + ".dbg"
        self.tabmodule = modname + "_" + "parsetab"
        #print self.debugfile, self.tabmodule

        # Build the lexer and parser
        self.lex = lex.lex(module=self,
            debug=self.debug)
        self.yacc = yacc.yacc(module=self,
            debug=self.debug,
            debugfile=self.debugfile,
            tabmodule=self.tabmodule)

        if self.file_name == None:
            while 1:
                try:
                    s = input('> ')
                except EOFError:
                    break
                if not s:
                    continue
                yacc.parse(s)
        else:
            with open(self.file_name) as f:
                file_data = f.read()
            yacc.parse(file_data)

        if not self.keep_parse_tab:
            parse_tab_file = self.tabmodule +'.py'
            if os.path.exists(parse_tab_file):
                os.remove(parse_tab_file)


class DatcomParserTalbe(Parser):
    """
    Parses a datcom output file.
    """
#    def __init__(self):
#        self.cases = {}  #记录遇到了多少个One
    
    re_float = \
        r'(\+|-)?((\d*\.\d+)|(\d+\.))(E(\+|-)?\d+)?'

    re_namelist = \
        r'(\$(?<namelist>.*)\$)'
        
    states = (        
        ('CASE', 'exclusive'),
        ('INPUT', 'exclusive'),
        ('TalbeHeader', 'exclusive'))
    
    reserved_INPUT = {}
    reserved_NAMELISTS = []
    
    tokens = (
        'FLOAT',      #匹配浮点数
        'WORD',       #任意单词
        'UNITWORD', 
        #'BLACKSPACE', #空白
        'NEWLINE',    #\n
        'ZEROBEGIN',  #0开头的行
        'ONEBEGIN'    #1开头的行
        'BALCKBEGIN', #空白开头的行   
        'SHROTBAR',   #---
        'ASTERRISK'   #* 匹配星号
        'LPAREN',     #（
        'RPAREN',     #）
        'DOTPOINT' ,  #,   
        'ENDTALBEDEC',  #匹配结果约束说明  
        'BEFOREONESTART',  #匹配1开头前一行的结尾 
        'BEFOREZEROSTART', #匹配0开头的上一行的结尾
        )\
        +tuple(reserved_INPUT.values())
        
 
 
    # Tokens
    t_ANY_WORD   = r'\b\w+[-]?\w+\b' 
    #t_ANY_BLACKSPACE = r' '
    t_ANY_NEWLINE = r'\n'  #\r\n
    t_ANY_ZEROBEGIN = r'^0\s'
    t_ANY_ONEBEGIN = r'^1\s'
    t_ANY_BALCKBEGIN = r'^ '
    t_ANY_SHORTBAR = r'-{3,:}'  #匹配---出现3次以上 
    t_ANY_UNITWORD = r'\b\w+([*/]+\w+)+\b'  #匹配单位描述 
    t_ANY_ASTERRISK = r'\*'    #匹配 *
    t_ANY_LPAREN = r'\('
    t_ANY_RPAREN = r'\)'
    t_ANY_DOTPOINT = r','
    t_ANY_ENDTALBEDEC = r'\r\n \r\n'  #针对 windows
    #需要多定义几种结束符然后将这些结束符都进行转换
    t_ANY_BEFOREONESTART = r'(?P<ENDNODE>\r\n)1\ +.*\r\n'
    t_ANY_BEFOREZEROSTART = r'(?P<ENDSUBNODE>\n)0\ +.*\r\n' #\n0\ |\n0\r|\n0*+\r
    #t_ANY_CRCR = r'\n{2}'
    
    
    def t_INPUT_end_INPUT(self, t):
        r'1.*AUTOMATED\ STABILITY\ AND\ CONTROL\ METHODS.*'
        #print 'end input'
        t.lexer.pop_state()
    
    @TOKEN(re_float)
    def t_INPUT_FLOAT(self, t):
        try:
            t.value = float(t.value)
        except ValueError:
            p_error(t)
            t.value = 0
        #print t
        return t
    
    
    
    ########################################################
    # Parsing rules

    precedence = ()
    
    # first rule is top-level rule
    
    def p_NODE_BEFOREONESTART(self, p):
        '''
        statememts : NODE BEFOREONESTART
        '''
        #当遇到NODE闭合操作，让解释器重新开始
        if self.NODEFlag == 'RESULT':
            self.NODEFlag = 'NONE'
        
    
    def p_NODE_ONEBEGIN(self, p):
        '''
        NODE : ONEBEGIN
        '''
        if self.NODEFlag == 'NONE':
            self.cases.append({'NAME':'newNODE', 'INPUT':{}, 'RESULT':{}})
            #向结果中添加一个字典
            self.NODEFlag = 'INPUT'
            p[0] = self.cases[-1]('INPUT')
        if self.NODEFlag == 'INPUT':
            self.NODEFlag = 'RESULT'
            p[0] = self.cases[-1]('RESULT')
        #if self.NODEFlag == 'RESULT': #重置操作 在解析器中施行 一定需要重置为 NONE

               
    def p_NODE_LINEOBJS(self, p):
        '''
        NODE : NODE LINEOBJS  ENDTALBEDEC       
        '''
        if self.stautsFLAG == 'TALBEDEC':
            p[1]['TABLEDEC'] =p[2]
            p[0] = p[1]
            self.stauts = 'TABLEHEADER'
        if self.stautsFLAG == 'TABLEHEADER':
            p[1]['TABLEHEADER'] =p[2]
            p[0] = p[1]
            self.stauts = 'TABLE'
        if self.stautsFLAG == 'TABLE':
            p[1]['TABLE'] =p[2]
            p[0] = p[1]
            self.stauts = 'TALBEDEC'
    
        
#    def p_word_join_word(self, p):
#        '''
#        WORD:WORD BLACKSPACE WORD
#        '''
#        p[0]=p[1]+p[2]+p[3]

    def p_word_join_word(self, p):
        '''
        WORD : WORD  WORD
        '''
        p[0]=p[1]+' '+p[2]
        
    def p_word_endline(self, p):
        '''
        LINEOBJ : WORD NEWLINE
        '''
        p[0] = {'linePos':self.lineCount, 'Value':p[1]}
        

    def p_word_dotpoint(self, p):   #需要调整优先级 
        '''
        PATCHOBJ : WORD DOTPOINT  
        '''
        p[0] = [p[1], ]
        
    def p_PATCHOBJ_word(self, p):
        '''
        PATCHOBJ : PATCHOBJ WORD
        '''
        p[0] = p[1].append(p[2])
        
    def p_PATCHOBJ_NEWLINE(self, p):
        '''
        LINEOBJ : PATCHOBJ NEWLINE
        '''
        p[0] = {'linePos':self.lineCount, 'Value':p[1]}
 
     
    #追加模式 
    def p_lineobj_lineobj(self, p):
        '''
        LINEOBJS :LINEOBJ LINEOBJ
        '''
        p[0] = [p[1], p[2]]
        
#    def p_BLACKSPACE_BLACKSPACE(self, p):
#        '''
#        BLACKSPACE : BLACKSPACE BLACKSPACE
#        '''
#        p[0] = p[1] + p[2]
#    def p_BLACKSPACE_WORD(self, p):    
#        '''
#        WORD : BLACKSPACE WORD
#        '''
#        p[0] = p[2]
        
    def get_common(self):
        """
        get a dictionary of common information,
        use get_cases to get more information from
        each case
        """
        # find case

        # fill dict
        return {}

    def get_cases(self):
        return self.cases
    
        

if __name__ == '__main__':
    str='''

    '''
    str = os.path.join('extras','PyDatcomProjects', '1', 'myex1.out')
    parser = DatcomParserTalbe(str, debug=1, keep_parse_tab=True)        
    commonDict = parser.get_common()    
    mycases = parser.get_cases()

    
    

    
    
    
    
    
    
