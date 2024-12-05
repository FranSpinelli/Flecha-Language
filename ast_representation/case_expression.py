import sys, os
sys.path.insert(0,"../..")

from .node import Node

class CaseExpression(Node):

	def __init__(self, children=[], leaf=None):
		Node.__init__(self, 'ExprCase', children, leaf)

	def __str__(self, level=0):
		ret = " "*level+ "[\"" +str(self.typeNode) + "\"\n"
		ret += self.children[0].__str__(level+1) + ",\n"
		ret += " "*(level+1)+ "["
		if self.children[1]:
			ret += "\n" + self.children[1][0].__str__(level+2)
			for child in self.children[1][1:]:
				ret += ",\n" + child.__str__(level+2)
			ret += "\n" + " "*(level+1)+ "]" + "\n"
		else:
			ret += "]\n"
		ret += " "*level+ "]"
		return ret