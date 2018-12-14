import unittest
from day02 import day_2a, day_2b, parse_input

class Day02Test(unittest.TestCase):

    def test_parse_input_return_correct_number_of_elements(self):
        self.assertEqual(250,len(parse_input('2.txt')))

    def test_day02_a_example_test(self):
        ids = ['abcdef','bababc','abbcde','abcccd','aabcdd','abcdee','a babab']
        self.assertEqual(12,day_2a(ids))

    def test_day02_b_example_test(self):
        ids = ['abcde','fghij','klmno','pqrst','fguij','axcye','wvxyz']
        self.assertEqual('fgij', day_2b(ids))        
