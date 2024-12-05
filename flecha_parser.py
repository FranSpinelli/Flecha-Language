import sys, os
sys.path.insert(0,"../..")

from ply import yacc as yacc
from ply import lex as lex

class Parser:

  tokens = ()
  precedence = ()

  def __init__(self, **kw):
      self.debug = kw.get('debug', 0)
      self.names = { }
      try:
         modname = os.path.split(os.path.splitext(__file__)[0])[1] + "_" + self.__class__.__name__
      except:
         modname = "parser"+"_"+self.__class__.__name__
      self.debugfile = modname + ".dbg"
      self.tabmodule = modname + "_" + "parsetab"
      #print self.debugfile, self.tabmodule

      # Build the lexer and parser
      self.lexer = lex.lex(module=self, debug=self.debug)
      self.yacc = yacc.yacc(module=self, debug=self.debug, debugfile=self.debugfile)

  def run(self):
    while 1:
      try:
        s = raw_input('flecha > ')
      except EOFError:
        break
      if not s: continue
      yacc.parse(s)