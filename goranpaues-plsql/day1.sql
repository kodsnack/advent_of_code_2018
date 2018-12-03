create table input
( 
  id number generated always as identity,
  message varchar2(200)
)
/

create table output
(   
  id number generated always as identity,
  message varchar2(200)
)
/

create or replace package pkg_advcode
is
    TYPE numbers_aat IS TABLE OF NUMBER
    INDEX BY PLS_INTEGER; 
        
    procedure doit;
end;
/

create or replace package body pkg_advcode
is
    procedure log(p_message in varchar)
    is
    begin
        insert into output(message) values (p_message);
    end;

    function part1
    return number
    is
        l_result number :=0; 
    begin
        select sum(
            case 
                when substr(message,1,1) = '+' 
                then substr(message,2,length(message)) 
                else message
                end
        )
        into l_result
        from input;

        return l_result;
    end;

    function part2
    return number
    is
        l_numbers numbers_aat; 
        l_reached numbers_aat;
        l_frequency number := 0;
        l_index PLS_INTEGER;
    begin
        select frequency_change
        bulk collect into l_numbers
        from (select id,
                    case 
                        when substr(message,1,1) = '+' 
                        then substr(message,2,length(message)) 
                        else message
                        end frequency_change
                   from input
                   order by id);

        l_index := l_numbers.FIRST;
        
        while not l_reached.exists(l_frequency)
        loop
            l_reached(l_frequency) := l_frequency;
            l_frequency := l_frequency + l_numbers(l_index);
            l_index := l_numbers.next(l_index);
            if l_index is null then 
              l_index := l_numbers.first;
            end if;
        end loop;

        return l_frequency;
    end;

    procedure doit
    is    
    begin
        log('Part 1: ' || part1);
        log('Part 2: ' || part2);
    end;

end;
/

insert into input (message) values ('+11')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('+1')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('+20')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('+1')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('-22')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('+21')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('-27')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('-5')
/
insert into input (message) values ('+1')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('-5')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('+3')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('+1')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('-21')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('+6')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('+6')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('+1')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('+3')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('+21')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('+6')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('+1')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('+3')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('+3')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('+21')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('-7')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('+29')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('+34')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('+23')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('+6')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('-7')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('+22')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('+1')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('+1')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('-5')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('-23')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('+22')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('+3')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('+6')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('+39')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('+32')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('-41')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('+24')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('-38')
/
insert into input (message) values ('-59')
/
insert into input (message) values ('-22')
/
insert into input (message) values ('-35')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('-22')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('+3')
/
insert into input (message) values ('+6')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('+21')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('-23')
/
insert into input (message) values ('-5')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('+6')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('-7')
/
insert into input (message) values ('+26')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('-48')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('-21')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('-5')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('+3')
/
insert into input (message) values ('-7')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('+6')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('-29')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('-21')
/
insert into input (message) values ('-23')
/
insert into input (message) values ('-25')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('+100')
/
insert into input (message) values ('+61')
/
insert into input (message) values ('+1')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('+108')
/
insert into input (message) values ('-60')
/
insert into input (message) values ('+291')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('+118')
/
insert into input (message) values ('-1098')
/
insert into input (message) values ('-56117')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('+3')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('-7')
/
insert into input (message) values ('-21')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('-7')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('+3')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('-7')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('-7')
/
insert into input (message) values ('+6')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('-7')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('-5')
/
insert into input (message) values ('+22')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('-5')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('+23')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('-5')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('+3')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('+20')
/
insert into input (message) values ('+22')
/
insert into input (message) values ('-43')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('+22')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('-23')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('+20')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('+3')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('-48')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-28')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-20')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('+6')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('+3')
/
insert into input (message) values ('-27')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('-30')
/
insert into input (message) values ('+6')
/
insert into input (message) values ('-22')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('-20')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('+65')
/
insert into input (message) values ('+20')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('-93')
/
insert into input (message) values ('-22')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('-71')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('+47')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('-23')
/
insert into input (message) values ('-21')
/
insert into input (message) values ('-23')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('-5')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('-30')
/
insert into input (message) values ('-25')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('+6')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('-29')
/
insert into input (message) values ('+1')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('+22')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('-5')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('+6')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('+3')
/
insert into input (message) values ('-23')
/
insert into input (message) values ('-46')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('-21')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('-20')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('+6')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('+26')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('-34')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('-88')
/
insert into input (message) values ('+1')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('+20')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('-20')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('-5')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('-5')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('+6')
/
insert into input (message) values ('-20')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('+6')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('-35')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('+111')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('+1')
/
insert into input (message) values ('+23')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('+24')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('+3')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('-5')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-24')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('+1')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('-5')
/
insert into input (message) values ('+24')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('-24')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('+159')
/
insert into input (message) values ('-27')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('+3')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('-25')
/
insert into input (message) values ('-23')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('-24')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('-70')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('-32')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('+103')
/
insert into input (message) values ('+162')
/
insert into input (message) values ('+41')
/
insert into input (message) values ('-401')
/
insert into input (message) values ('-56221')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('-21')
/
insert into input (message) values ('-7')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('+27')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('+23')
/
insert into input (message) values ('-54')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('+21')
/
insert into input (message) values ('-65')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('-20')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('-7')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('+20')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('+20')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('-20')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('+3')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('+1')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('+21')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('-18')
/
insert into input (message) values ('-8')
/
insert into input (message) values ('-5')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('+23')
/
insert into input (message) values ('-7')
/
insert into input (message) values ('-28')
/
insert into input (message) values ('+18')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('+1')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('+15')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('-34')
/
insert into input (message) values ('+8')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('-35')
/
insert into input (message) values ('-9')
/
insert into input (message) values ('-17')
/
insert into input (message) values ('-5')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('-27')
/
insert into input (message) values ('+1')
/
insert into input (message) values ('+21')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('+3')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('+45')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('+24')
/
insert into input (message) values ('+91')
/
insert into input (message) values ('+20')
/
insert into input (message) values ('+23')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('+6')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('+7')
/
insert into input (message) values ('-12')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('-2')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('-4')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('+21')
/
insert into input (message) values ('+11')
/
insert into input (message) values ('+1')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('-5')
/
insert into input (message) values ('+14')
/
insert into input (message) values ('+16')
/
insert into input (message) values ('-10')
/
insert into input (message) values ('+4')
/
insert into input (message) values ('+17')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('-7')
/
insert into input (message) values ('-13')
/
insert into input (message) values ('-7')
/
insert into input (message) values ('-19')
/
insert into input (message) values ('+6')
/
insert into input (message) values ('+24')
/
insert into input (message) values ('+5')
/
insert into input (message) values ('-6')
/
insert into input (message) values ('+9')
/
insert into input (message) values ('-11')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('+12')
/
insert into input (message) values ('+19')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('-14')
/
insert into input (message) values ('+2')
/
insert into input (message) values ('-1')
/
insert into input (message) values ('-16')
/
insert into input (message) values ('+10')
/
insert into input (message) values ('-5')
/
insert into input (message) values ('-32')
/
insert into input (message) values ('+13')
/
insert into input (message) values ('-3')
/
insert into input (message) values ('-15')
/
insert into input (message) values ('+113294')
/

-- Execution
begin
    pkg_advcode.doit;
end;
/

-- Print result
select message
from   output
order by id
/

rollback;
