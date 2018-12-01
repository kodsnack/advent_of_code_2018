from py2bf import *

init(['c','b','a','s','t','f','i','D','P','M','N','O'],[0, 0, 0, 0, 0, 0, 0, 10, ord('+'), ord('-'), ord(' '), 0])

"""
PSEUDO:

read char 'c'
loop on 'c'

 set 'b' = 'c' == '+'
 if 'b'
  set 's' = '1'

 set 'b' = 'c' == '-'
 if 'b'
  set 's' = '-1'

 set 'b' = 'c' == 'N'
 if 'b'
  multiply 'f' by 's'
  set array 'i' to 'f'
  add 1 to 'i'
 else
  multiply 'f' by 10
  set 't' to 'c'
  convert 't' to integer
  add 't' to 'f'

 read char 'c'
end loop 'c'

reset 'i'
reset 's'
set 'f' array 'i'
loop on 'f'
 add 'f' to 's'
 add 1 to 'i'
 set 'f' array 'i'
end loop 'f'
"""


#Assemble code
code = read('c')+\
       startloop('c')+\
        setreg('b','c')+\
        isequal('b','P')+\
        ifx('b',\
            setreg('s','P')
        )+\
        ''+\
        setreg('b','c')+\
        isequal('b','M')+\
        ifx('b',\
            setreg('s','M')
        )+\
        ''+\
        setreg('b','c')+\
        isequal('b','N')+\
        ifx('b',\
            setreg('b','s')+\
            isequal('b','M')+\
            ifxelse('b',
                    sub('a','f'),
                    add('a','f')
            )+\
            clear('f')
        )+\
        ''+\
        setreg('b','c')+\
        isnotequal('b','P')+\
        ifx('b',\
            setreg('b','c')+\
            isnotequal('b','M')
        )+\
        ifx('b',\
            setreg('b','c')+\
            isnotequal('b','N')
        )+\
        ifx('b',\
            mul('f','D')+\
            setreg('t','c')+\
            integer('t')+\
            add('f','t')
        )+\
        read('c')+\
       endloop('c')+\
       setreg('b','s')+\
       isequal('b','M')+\
       ifxelse('b',
               sub('a','f'),
               add('a','f')
       )+\
       display('a')

#print "CODE"
#print code

#print "Brainfuck"
generate(code)
