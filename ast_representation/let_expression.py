import sys, os
sys.path.insert(0,"../..")

from .node import Node

class LetExpression(Node):
	def __init__(self, children=[], leaf=None):
		Node.__init__(self, 'ExprLet', children, leaf)

	def __str__(self, level=0):
		ret = " "*level+ "[\"" +str(self.typeNode)+"\", \""+self.children[0]+"\",\n"
		if self.children[1]:
			ret += self.children[1].__str__(level+1) + ",\n"
		ret += self.children[2].__str__(level+1) + "\n"
		ret += " "*level+ "]"
		return ret