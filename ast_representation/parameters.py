import sys, os
sys.path.insert(0,"../..")

from .node import Node

class Parameters(Node):

	def __init__(self, children=[]):
		Node.__init__(self, '', children, None)

	def __str__(self, level=0):
		ret = " "*level+ "[\""
		for p in self.children:
			ret += p	
		ret += "]"
		return ret