import unittest
import numpy
import node

class TestNode(unittest.TestCase):

    def setUp(self):
        x = numpy.random.rand(50).astype(numpy.float32)
        y = numpy.random.rand(50).astype(numpy.float32)

        self.clx = node.Data(x,"x")
        self.cly = node.Data(y,"y")

    def test_make(self):
        clx = self.clx
        keys = [i for i in clx.params.keys()]
        #check that x is in the keys for clx
        self.assertEqual("x",keys[0])
        #check that x is the ONLY key in clx
        self.assertEqual("x",keys[-1])
        #check that the correct text has been generated
        self.assertEqual("x[gid]",clx.text)    

    def test_node(self):
        n = node.Data(3.5)
        self.assertEqual(n.text,"3.5")
        self.assertEqual(n.params,{})

if __name__ == '__main__':
    unittest.main()
