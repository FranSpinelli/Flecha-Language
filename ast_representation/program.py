import sys, os
sys.path.insert(0,"../..")

from .node import Node

class Program(Node):
	
	def __init__(self,children=[],leaf=None):
		Node.__init__(self, '', children, leaf)

	def push(self,child):
		self.children = self.children + [child]
		return self

	def __str__(self, level=0):
		ret = " "*level+ "["
		if self.children:
			ret += "\n" + self.children[0].__str__(level+1)
			for child in self.children[1:]:
				ret += ",\n" + child.__str__(level+1)
			ret += " "*level+ "\n]"
		else:
			ret += " "*level+ "]"
		return ret