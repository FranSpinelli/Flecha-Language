import sys, os
sys.path.insert(0,"../..")

from .node import Node

class CaseBranch(Node):

	def __init__(self, children=[], leaf=None):
		Node.__init__(self, 'CaseBranch', children, leaf)

	def __str__(self, level=0):
		ret = " "*level+ "[\"" +str(self.typeNode) + "\", "
		ret += "\"" + self.children[0] + "\", ["
		if self.children[1]:
			ret += "\""+self.children[1][0]+"\""
			for child in self.children[1][1:]:
				ret += ", " + "\""+child+"\""
		ret += "],\n"
		ret += self.children[2].__str__(level+1) + "\n"
		ret += " "*level+ "]"
		return ret