import unittest
from day01 import day_1a,day_1b

class Day01Test(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_day01_a_example_1_test(self):
        input = [1,-2,3,1]
        result = day_1a(input)
        self.assertEquals(3, result)
    
    def test_day01_b_example_1_test(self):
        input = [3,3,4,-2,-4]
        result = day_1b(input)
        self.assertEquals(10, result)
    
    def test_day01_b_example_2_test(self):
        input = [-6,3,8,5,-6]
        result = day_1b(input)
        self.assertEquals(5, result)
    
    def test_day01_b_example_3_test(self):
        input = [7,7,-2,-7,-4]
        result = day_1b(input)
        self.assertEquals(14, result)

    def test_day01_b_example_4_test(self):
        input = [1,-1]
        result = day_1b(input)
        self.assertEquals(0, result)