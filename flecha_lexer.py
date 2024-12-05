#!/usr/bin/python3
# -*- coding: utf-8 -*-

import sys
sys.path.insert(0, '../..')

from flecha_parser import Parser
from ast_representation.program import Program
from ast_representation.parameters import Parameters
from ast_representation.apply_atomic_expression import ApplyAtomicExpression
from ast_representation.case_branch import CaseBranch
from ast_representation.case_expression import CaseExpression
from ast_representation.definition import Definition
from ast_representation.atomic_expression import AtomicExpression
from ast_representation.lambda_expression import LambdaExpression
from ast_representation.let_expression import LetExpression

class Flecha(Parser):

    reserved = {
        'if': 'IF',
        'else': 'ELSE',
        'elif': 'ELIF',
        'then': 'THEN',
        'def': 'DEF',
        'case': 'CASE',
        'def': 'DEF',
        'let': 'LET',
        'in': 'IN'
        }

    #literals = ['\n']

    tokens = list(reserved.values()) + [
        'DEFEQ','SEMICOLON','LPAREN','RPAREN','LAMBDA','PIPE','ARROW',
        'LOWERID','UPPERID',
  		'NUMBER',
  		'STRING', 'CHAR',
        'AND','OR','NOT',
        'EQ','NE','GE','LE','GT','LT',
        'PLUS','MINUS','TIMES','DIV','MOD'
        ]

    t_DEFEQ 	= r'='
    t_SEMICOLON = r';'
    t_LPAREN 	= r'\('
    t_RPAREN 	= r'\)'
    t_LAMBDA 	= r'\\'
    t_PIPE 		= r'\|'
    t_ARROW 	= r'->'

    t_AND 		= r'&&'

    t_OR        = r'\|\|'

    t_NOT 		= r'!'

    t_EQ 		= r'=='
    t_NE 		= r'!='
    t_GE 		= r'>='
    t_LE 		= r'<='
    t_GT 		= r'>'
    t_LT 		= r'<'

    t_PLUS 		= r'\+'
    t_MINUS 	= r'-'
    t_TIMES 	= r'\*'
    t_DIV 		= r'/'
    t_MOD 		= r'%'

    def t_LOWERID(self, t):
        r'''[a-z][_a-zA-Z_0-9]*'''

        t.type = Flecha.reserved.get(t.value,'LOWERID')    # Chequeo de palabras reservadas

        return t

    def t_UPPERID(self, t):
        r'''[A-Z][_a-zA-Z_0-9]*'''

        t.type = Flecha.reserved.get(t.value, 'UPPERID')  # Chequeo de palabras reservadas
        return t

    def t_CHAR(self, t):
        r'''(\'[^\'\\]*(?:\\.[^\'\\]*)*\')'''

        t.value = t.value[1:-1]
        return t

    def t_STRING(self, t):
        r'''(\"[^\"\\]*(?:\\.[^\"\\]*)*\")'''

        t.value = t.value[1:-1]
        return t

    def t_NUMBER(self, t):
        r'''\d+'''

        try:
            t.value = int(t.value)
        except ValueError:
            print ('Integer value too large', t.value)
            t.value = 0
        return t

    t_ignore_COMMENT = r'--.*'
    t_ignore = ' \t\r'

    precedence = (
    	('left','SEMICOLON'),
        ('left','IF', 'CASE', 'LET', 'LAMBDA'),
        ('left','THEN', 'ELSE', 'ELIF'),
    	('left','OR'),
    	('left','AND'),
    	('right','NOT'),
        ('nonassoc','EQ','NE','GE','LE','GT','LT'),
    	('left','PLUS', 'MINUS'),
    	('left','TIMES'),
    	('left','DIV', 'MOD'),
    	('right', 'UMINUS'),
    )

    # ******************* Program *******************
    def p_program(self, p):
    	''' program : empty_program
    				| not_empty_program '''
    	p[0] = p[1]

    def p_empty(self, p):
    	''' empty : '''
    	pass

    def p_empty_program(self, p):
    	''' empty_program : empty '''
    	p[0] = Program(children=[])

    def p_not_empty_program(self, p):
        ''' not_empty_program : program definition_with_params
                              | program definition_without_params '''
        p[0] = p[1].push(p[2])

    # ******************* Definition *******************

    def p_definition_without_params(self, p):
        ''' definition_without_params : DEF LOWERID empty_params DEFEQ expression '''
        p[0] = Definition(children=[p[2]] + [p[5]])

    def p_definition_with_params(self, p):
        ''' definition_with_params : DEF LOWERID params DEFEQ expression '''
        p[0] = Definition(children=[p[2]] + [LambdaExpression(children=[p[3],p[5]])])

    # ******************* Params *******************

    def p_empty_params(self, p):
        ''' empty_params : '''
        p[0] = []

    def p_params(self, p):
        ''' params : empty_params
                   | not_empty_params
        '''
        p[0] = p[1]

    def p_not_empty_params(self, p):
        ''' not_empty_params : LOWERID params '''
        p[0] = [p[1]] + p[2]

    # ******************* Expression *******************

    def p_expression(self, p):
        ''' expression : outer_expression
                       | secuence_expression '''
        p[0] = p[1]

    # ******************* SecuenceExpression *******************

    def p_secuence_expression(self, p):
        ''' secuence_expression : outer_expression SEMICOLON expression '''
        p[0] = LetExpression(children=["_", p[1], p[3]])

    def p_outer_expression(self, p):
        ''' outer_expression : inner_expression
                             | case_expression
                             | if_expression
                             | lambda_expression
                             | let_expression '''
        p[0] = p[1]


    def p_if_expression(self, p):
        ''' if_expression : IF inner_expression THEN inner_expression branch_else '''
        p[0] = CaseExpression(children=[p[2]] + [[CaseBranch(children = ["True"] + [[]] + [p[4]])]+[p[5]]])

    def p_branch_else(self, p):
        ''' branch_else : elif_expression
                        | else_expression '''
        p[0] = p[1]

    def p_elif_expression(self, p):
        '''elif_expression : ELIF inner_expression THEN inner_expression branch_else'''
        p[0] = CaseBranch(children = ["False"] + [[]] + [ CaseExpression(children=[p[2]] + [[CaseBranch(children = ["True"] + [[]] + [p[4]])]+[p[5]]]) ])

        CaseExpression(children=[p[2]] + [[CaseBranch(children = ["False"] + [[]] + [p[4]])]])

    def p_else_expression(self, p):
        '''else_expression : ELSE inner_expression'''
        p[0] = CaseBranch(children = ["False"] + [[]] + [p[2]])

    def p_case_expression(self, p):
        ''' case_expression : CASE inner_expression branches_case '''
        p[0] = CaseExpression(children = [p[2]] + [p[3]])

    def p_branches_case(self, p):
        '''branches_case : empty_branch
                         | non_empty_branch
        '''
        p[0] = p[1]

    def p_empty_branch(self, p):
        '''empty_branch : '''
        p[0] = []

    def p_non_empty_branch(self, p):
        '''non_empty_branch : branch_case branches_case '''
        p[0] = [p[1]] + p[2]

    def p_branch_case(self, p):
        '''branch_case : PIPE UPPERID params ARROW inner_expression '''
        p[0] = CaseBranch(children = [p[2]] + [p[3]] + [p[5]])

    def p_let_expression(self, p):
        ''' let_expression : LET LOWERID params DEFEQ inner_expression IN outer_expression '''
        count_params = len(p[3])
        if count_params >= 1:
            lamdaParameter = LambdaExpression(children=[p[3], p[5]])
        elif count_params == 0:
            lamdaParameter = p[5]
        p[0] = LetExpression(children=[p[2], lamdaParameter, p[7]])

    def p_lambda_expression(self, p):
        ''' lambda_expression : LAMBDA params ARROW outer_expression
        '''
        count_params = len(p[2])
        if count_params >= 1:
            p[0] = LambdaExpression(children=[p[2], p[4]])
        elif count_params == 0:
            p[0] = p[4]
        return p[0]

    def p_inner_expression(self, p):
        ''' inner_expression : apply_expression
                             | binary_expression
                             | unary_expression '''
        p[0] = p[1]

    def p_apply_expression(self, p):
        ''' apply_expression : atomic_expression
                             | apply_atomic_expression '''
        p[0] = p[1]

    def p_atomic_expression(self, p):
        ''' atomic_expression : non_paren_atomic
                              | paren_atomic '''
        p[0] = p[1]

    def p_non_paren_atomic(self, p):
        ''' non_paren_atomic : char_expression
                             | string_expression
                             | number_expression
                             | lower_id_expression
                             | upper_id_expression '''
        p[0] = p[1]

    def get_char_ord(self, char):
        if char != '':
            if char[0] == "\\":
                char = bytes(char, encoding='ascii')
                char = char.decode('unicode-escape')
            char = str(ord(char))
        return char

    def p_char_expression(self, p):
        ''' char_expression : CHAR
        '''
        p[1] = self.get_char_ord(p[1])

        p[0] = AtomicExpression("ExprChar", p[1])

    def p_string_expression(self, p):
        ''' string_expression : STRING
        '''
        if p[1]:
            index_char = 1
            next_char = p[1][0]
            if next_char == "\\":
                index_char = 2
                next_char = p[1][0:index_char]
            sub = ApplyAtomicExpression(children=[AtomicExpression("ExprConstructor", "Cons"), AtomicExpression("ExprChar", self.get_char_ord(next_char))])
            p[1] = p[1][index_char:]
            p[0] = ApplyAtomicExpression(children=[sub,self.p_string_expression(p)])
        else:
            p[0] = AtomicExpression("ExprConstructor", "Nil")
        return p[0]

    def p_number_expression(self, p):
        ''' number_expression : NUMBER
        '''
        p[0] = AtomicExpression("ExprNumber", str(p[1]))

    def p_lower_id_expression(self, p):
        ''' lower_id_expression : LOWERID
        '''
        p[0] = AtomicExpression("ExprVar", p[1])

    def p_upper_id_expression(self, p):
        ''' upper_id_expression : UPPERID
        '''
        p[0] = AtomicExpression("ExprConstructor", p[1])

    def p_paren_atomic(self, p):
        ''' paren_atomic : LPAREN expression RPAREN'''
        p[0] = p[2]

    def p_apply_atomic_expression(self, p):
        ''' apply_atomic_expression : apply_expression atomic_expression '''
        p[0] = ApplyAtomicExpression(children=[p[1], p[2]])

    def p_binary_expression(self, p):
        ''' binary_expression : inner_expression AND inner_expression
                              | inner_expression OR inner_expression
                              | inner_expression EQ inner_expression
                              | inner_expression NE inner_expression
                              | inner_expression GE inner_expression
                              | inner_expression LE inner_expression
                              | inner_expression GT inner_expression
                              | inner_expression LT inner_expression
                              | inner_expression PLUS inner_expression
                              | inner_expression MINUS inner_expression
                              | inner_expression TIMES inner_expression
                              | inner_expression DIV inner_expression
                              | inner_expression MOD inner_expression '''
        typeBinaryOp = None
        if p[2] == '&&' : typeBinaryOp = "AND"
        elif p[2] == '||': typeBinaryOp = "OR"
        elif p[2] == '==': typeBinaryOp = "EQ"
        elif p[2] == '!=': typeBinaryOp = "NE"
        elif p[2] == '>=': typeBinaryOp = "GE"
        elif p[2] == '<=': typeBinaryOp = "LE"
        elif p[2] == '>': typeBinaryOp = "GT"
        elif p[2] == '<': typeBinaryOp = "LT"
        elif p[2] == '+': typeBinaryOp = "ADD"
        elif p[2] == '-': typeBinaryOp = "SUB"
        elif p[2] == '*': typeBinaryOp = "MUL"
        elif p[2] == '/': typeBinaryOp = "DIV"
        elif p[2] == '%': typeBinaryOp = "MOD"
        subExpr = ApplyAtomicExpression(children=[AtomicExpression("ExprVar", typeBinaryOp), p[1]])
        p[0] = ApplyAtomicExpression(children=[subExpr, p[3]])

    def p_unary_expression(self, p):
        ''' unary_expression : NOT inner_expression
                             | MINUS inner_expression %prec UMINUS '''
        typeUnaryOp = None
        if p[1] == '!' : typeUnaryOp = "NOT"
        elif p[1] == '-': typeUnaryOp = "UMINUS"
        p[0] = ApplyAtomicExpression(children=[AtomicExpression("ExprVar", typeUnaryOp), p[2]])

    def t_newline(self, t):
        r'''\n+'''
        t.lexer.lineno += t.value.count('\n')

    def t_error(self, t):
        print ("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)

    def p_error(self, p):
        print (p)
        if p:
            print ('Syntax error at token', p.type)

            # Just discard the token and tell the parser it's okay.

            self.yacc.errok()
        else:
            print ('Syntax error at EOF')