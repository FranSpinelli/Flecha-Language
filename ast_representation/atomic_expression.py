import sys, os
sys.path.insert(0,"../..")

from .node import Node

class AtomicExpression(Node):
	
	def __init__(self, name, leaf):
		Node.__init__(self, name, [], leaf)

	def __str__(self, level=0):
		ret = " "*level+ "[\"" +str(self.typeNode) + "\", "
		ret = ret + (self.leaf if self.leaf.isnumeric() else "\"" + self.leaf + "\"") + "]"
		return ret