#!/usr/bin/env python
import sys
import os
import re
#import string

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

class DatcomParser(Parser):
    """
    Parses a datcom output file.USAF
    """

    re_float = \
        r'(\+|-)?((\d*\.\d+)|(\d+\.))(E(\+|-)?\d+)?'

    re_namelist = \
        r'(\$(?<namelist>.*)\$)'
        
    

    states = (
        ('CASE', 'exclusive'),
        ('INPUT', 'exclusive'),
    )

    reserved_INPUT = {
#        'TRIM': 'TRIM',
#        'DIM': 'DIM',
#        'DAMP': 'DAMP',
#        'PART': 'PART',
#        'DERIV': 'DERIV',
#        'DUMP': 'DUMP',
#        'SAVE':'SAVE', 
    }

    reserved_NAMELISTS = [
#        'FLTCON',#
#        'SYNTHS',#
#        'BODY',#
#        'SYMFLP',#
#        'ASYFLP', #
#        'OPTINS',#
#        'WGPLNF', #    
#        'HTPLNF',#
#        'VTPLNF',#
#        'VFPLNF',#
#        'WGSCHR',#
#        'HTSCHR',#
#        'VTSCHR',#
#        'VFSCHR',#
#        'PROPWR',#linger
#        'JETPWR', #
#        'CONTAB', #
#        'EXPR', #
#        'GRNDEF', #
#        'HYPEFF', #
#        'LARWB', #
#        'TRNJET', #
#        'TVTPAN', #    
        ]

    tokens = (
        'WORD',       #任意单词
        'BLACKSPACE', #空白
        'NEWLINE',    #\n
        'ZEROBEGIN',  #0开头的行
        'ONEBEGIN'    #1开头的行
        'BALCKBEGIN', #空白开头的行   
        'SHROTBAR',   #-
        'ASTERRISK'   #* 匹配星号
        
#        'NEWCASE',
#        'DYNAMICTABLE',
#        'STATICTABLE',
#        'ASYFLPTABLE',
#        'SYMFLPTABLE',
#        'FLOAT',
#        'FLOATVECTOR',
#        'INTEGER',
#        'EQUALS',
#        'ENDNAMELIST',  # delimeter
#        'COMMA',
#        'BOOL',
#        'CASEID',
#        'NAMELIST',          
        )\
        +tuple(reserved_INPUT.values())


    # Tokens
    t_ANY_WORD   = r'\b\w+[-]?\w\b'
    t_ANY_LPAREN = r'\('
    t_ANY_RPAREN = r'\)'
    t_ANY_ASTERRISK= r'\*'
    t_ANY_ZEROBEGIN = r'^0\s'
    t_ANY_ONEBEGIN = r'^1\s'
    t_ANY_BALCKBEGIN = r'^\w'
    r'(\b[a-zA-Z]+\*{1,2}\w+\b)|(\b\w+/\w+\b)'

    t_INITIAL_ignore = ' \t'

    t_ANY_TAB = ' \t'

    t_INPUT_EQUALS = r'='

    t_INPUT_ignore = ' \r\n\t'

    def t_INPUT_BOOL(self, t):
        r'(\.TRUE\.|\.FALSE\.)'
        return t



    def t_ANY_newline(self, t):
        r'\n'
        t.lexer.lineno += t.value.count("\n")
        #print 'newline'

    def t_ANY_error(self, t):
        print("Illegal character '%s' at line" % t.value[0], t.lexer.lineno)
        t.lexer.skip(1)
        sys.exit(1)




    @TOKEN(re_float)
    def t_INPUT_FLOAT(self, t):
        try:
            t.value = float(t.value)
        except ValueError:
            p_error(t)
            t.value = 0
        #print t
        return t

    def t_INPUT_ERROR(self, t):
        r'0.*ERROR.*\n(.+\n)'
        print ('error: %s' ,  t.value)

    def t_INPUT_ZEROLINE(self, t):
        r'0.*'
        #print 'zeroline'

    def t_ANY_INTEGER(self, t):
        r'\d+'
        try:
            t.value = int(t.value)
        except ValueError:
            p_error(t)
            t.value = 0
        #print t
        return t

    # Parsing rules

    precedence = ()

    # first rule is top-level rule
    #linger 分析这条rule是用来产生case集合的
    def p_file_statement_append(self, p):
        """
        file : statement file
        """
        if p[2] == None:
            p[2] = [p[1]]
        else:
            p[0] = p[2].append(p[1])

    def p_file_from_statement(self, p):
        """
        file : statement
        """
        p[0] = [p[1]]

    def p_newcase(self, p):
        """
        statement : NEWCASE
        """
        self.cases.append({})
        #print '\n'

    def p_caseid(self, p):
        """
        statement : CASEID
        """
        self.cases[-1]['ID'] = p[1]

    def parse_vector(self, data):
        vector = []
        for val in data.split():
            vector.append(float(val))
        return vector

    def parse_table1d(self, cols, data):
        table = {}
        colLastOld = -1
        lines = data.split('\n')
        for i in range(len(cols)):
            colLast = colLastOld + cols[i][1]
            valList = []
            for j in range(len(lines) - 1):
            # -1 to skip last newline
                line = lines[j]
                col = line[colLastOld + 1:colLast].strip()
                if col == '' or  \
                        col == 'NDM' or \
                        col == 'NA':
                    pass
                else:
                    #print 'raw: %11s' % col
                    valList.append(float(col))
                    #print 'float: %11f\n' % val
            table[cols[i][0]] = valList
            colLastOld = colLast
        return  table

    def p_dynamictable(self, p):
        'statement : DYNAMICTABLE'
        self.cases[-1]['DYNAMIC'] = self.parse_table1d(
            [['ALPHA', 10], ['CLQ', 13], ['CMQ', 13],
            ['CLAD', 15], ['CMAD', 13], ['CLP', 14],
            ['CYP', 13], ['CNP', 13], ['CNR', 13],
            ['CLR', 14]],
            p[1]['deriv_table'])

    def p_statictable(self, p):
        'statement : STATICTABLE'
        self.cases[-1]['STATIC'] = self.parse_table1d(
            [['ALPHA', 8], ['CD', 9], ['CL', 9],
            ['CM', 10], ['CN', 8], ['CA', 9],
            ['XCP', 9], ['CLA', 13], ['CMA', 13],
            ['CYB', 13], ['CNB', 14], ['CLB', 13]],
            p[1]['deriv_table'])

    def p_symflptable(self, p):
        'statement : SYMFLPTABLE'
        data = {}
        data['DERIV'] = \
                self.parse_table1d(
            [['DELTA', 12], ['D(CL)', 10],
             ['D(CM)', 11], ['D(CL MAX)', 10],
             ['D(CD MIN)', 13], ['(CLA)D', 25],
             ['(CH)A', 12], ['(CH)D', 11]],
             p[1]['deriv_table'])
        (data['ALPHA'],
         data['CD']) = \
                self.parse_table2d(9,
            [15, 10, 10, 10, 10, 10, 10, 10, 10],
            p[1]['drag_table'])
        data['DELTA'] = \
            self.parse_vector(p[1]['deflection'])
        #print data['CNTRL_DEFLECT']
        #print self.cases[-1]['CNTRL_DRAG']
        self.cases[-1]['SYMFLP'] = data

    def p_asymflptable(self, p):
        'statement : ASYFLPTABLE'
        data = {}
        (data['ALPHA'],
         data['CN']) = \
                self.parse_table2d(7,
            [18, 12, 12, 12, 12, 12, 12, 12, 12],
            p[1]['yaw_table'])
        data['ROLL'] = \
                self.parse_table1d(
           [['DELTAL', 51], ['DELTAR', 16],
            ['CL(ROLL)', 22]],
            p[1]['roll_table'])
        data['DELTA'] = \
            self.parse_vector(p[1]['deflection'])
        self.cases[-1]['ASYFLP'] = data

    def parse_table2d(self, rowWidth, colWidths, data):
        colLastOld = -1
        dataArray = []
        rows = []
        lines = data.split('\n')

        for i in range(len(lines) - 1):
        # -1 to skip last newline
            line = lines[i]
            rows.append(float(line[0:rowWidth - 1]))
            colLastOld = rowWidth
            dataArray.append([])
            for j in range(len(colWidths)):
                colLast = colLastOld + colWidths[j]
                col = line[colLastOld + 1:colLast].strip()
                if col == '':
                    val = 0
                elif col == 'NDM':
                    val = 'NDM'
                elif col == 'NA':
                    val = 'NA'
                else:
                    #print 'raw: %11s' % col
                    val = float(col)
                    #print 'float: %11f\n' % val
                dataArray[-1].append(val)
                colLastOld = colLast

        return (rows, dataArray)

    def p_error(self, p):
        if p:
            print("Syntax error '%s' at line: %d, state: %s" \
                  % (p.value, self.lex.lineno, self.lex.lexstate))
        else:
            print("Syntax error at EOF")
        sys.exit(1)

    def p_namelist(self, p):
        'statement : NAMELIST terms ENDNAMELIST'
        self.cases[-1][p[1]] = p[2]

    def p_scalar_term(self, p):
        """
        term : NAME EQUALS FLOAT
        | NAME EQUALS INTEGER
        """
        p[0] = {p[1]: p[3]}
        #print 'term'

    def p_bool_term(self, p):
        'term : NAME EQUALS BOOL'
        if p[3] == ".TRUE.":
            p[0] = {p[1]: True}
        elif p[3] == ".FALSE.":
            p[0] = {p[1]: False}
        else:
            self.p_error(p[3])

    def p_airfoil(self, p):
        'statement : AIRFOIL'
        self.cases[-1]['AIRFOIL'] = p[1]

    def p_trim(self, p):
        'statement : TRIM'

    def p_dim(self, p):
        'statement : DIM NAME'
        self.cases[-1][p[1]] = p[2]

    def p_damp(self, p):
        'statement : DAMP'
        self.cases[-1][p[1]] = True

    def p_part(self, p):
        'statement : PART'
        self.cases[-1][p[1]] = True

    def p_deriv(self, p):
        'statement : DERIV NAME'
        self.cases[-1][p[1]] = p[2]

    def p_dump(self, p):
        'statement : DUMP NAME'

    def p_term_terms(self, p):
        'terms : term'
        p[0] = p[1]

    def p_terms(self, p):
        'terms : terms COMMA term'
        p[0] = p[1]
        for key in p[3].keys():
            if key in p[0]:
                print( 'WARNING: duplicate key %s' % key)
            else:
                p[0][key] = p[3][key]

    def p_array_term(self, p):
        """
        term : NAME LPAREN INTEGER RPAREN EQUALS FLOATVECTOR
        | NAME LPAREN INTEGER RPAREN EQUALS FLOAT
        | NAME LPAREN INTEGER RPAREN EQUALS INTEGER
        """
        p[0] = {p[1]: p[6]}

    def get_common(self):
        """
        get a dictionary of common information,
        use get_cases to get more information from
        each case
        """
        # find cases
        re_aileron = re.compile('.*aileron.*', re.I)
        re_flap = re.compile('.*flap.*', re.I)
        re_total = re.compile('.*total.*', re.I)
        cases = {}
        for case in self.cases:
            name = case['ID']
            if re_aileron.match(name):
                cases['aileron'] = case
            elif re_flap.match(name):
                cases['flap'] = case
            elif re_total.match(name):
                cases['total'] = case
        for key in ['aileron', 'flap', 'total']:
            if key not in cases:
                #raise IOError('%s case not found' % key)
                pass

        # extract some need dictionaries
        dFlap = cases['flap']['SYMFLP']
        dAileron = cases['aileron']['ASYFLP']
        dElevator = cases['total']['SYMFLP']
        dDynamic = cases['total']['DYNAMIC']
        dStatic = cases['total']['STATIC']

        # create model name
        if self.file_name == None:
            model_name = 'name'
        else:
            model_name = os.path.split(os.path.splitext(self.file_name)[0])[1]

        # fill dict
        return {
          'name': model_name,
          # lift
          'CL_Basic': dStatic['CL'],
          'dCL_Flap': dFlap['DERIV']['D(CL)'],
          'dCL_Elevator': dElevator['DERIV']['D(CL)'],
          'dCL_PitchRate': dDynamic['CLQ'],
          'dCL_AlphaDot': dDynamic['CLAD'],

          # drag
          'CD_Basic': dStatic['CD'],
          'dCD_Flap': dFlap['CD'],
          'dCD_Elevator': dElevator['CD'],

          # side force
          'dCY_Beta': dStatic['CYB'],
          'dCY_RollRate': dDynamic['CYP'],

          # roll moment
          'dCl_Aileron': dAileron['ROLL']['CL(ROLL)'],
          'dCl_Beta': dStatic['CLB'],
          'dCl_RollRate': dDynamic['CLP'],
          'dCl_YawRate': dDynamic['CLR'],

          # pitch moment
          'Cm_Basic': dStatic['CM'],
          'dCm_Flap': dFlap['DERIV']['D(CM)'],
          'dCm_Elevator': dElevator['DERIV']['D(CM)'],
          'dCm_PitchRate': dDynamic['CMQ'],
          'dCm_AlphaDot': dDynamic['CMAD'],

          # yaw moment
          'dCn_Aileron': dAileron['CN'],
          'dCn_Beta': dStatic['CNB'],
          'dCn_RollRate': dDynamic['CNP'],
          'dCn_YawRate': dDynamic['CNR'],

          # surfaces/ wind angles
          'flap': dFlap['DELTA'],
          'alrn': dAileron['DELTA'],
          'elev': dElevator['DELTA'],
          'alpha': dStatic['ALPHA'],
        }

    def get_cases(self):
        return self.cases
