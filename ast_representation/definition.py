import sys, os
sys.path.insert(0,"../..")

from .node import Node

class Definition(Node):
	
	def __init__(self, children=[], leaf=None):
		Node.__init__(self, 'Def', children, leaf)

	def __str__(self, level=0):
		ret = " "*level+ "[\"" +str(self.typeNode) + "\", \"" + self.children[0] + "\", \n"
		ret += self.children[1].__str__(level+1)
		ret += "\n" + " "*level+ "]"
		return ret