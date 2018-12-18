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
    TYPE x_aat IS TABLE OF NUMBER
    INDEX BY PLS_INTEGER; 
    TYPE grid_aat is table of x_aat
    INDEX BY PLS_INTEGER;     
    procedure doit(p_serial_number in number);
end;
/

create or replace package body pkg_advcode
is
    procedure log(p_message in varchar)
    is
    begin
        insert into output(message) values (p_message);
    end;

    function part1(p_serial_number in number)
    return varchar2
    is
        l_grid grid_aat;
        l_rack_id PLS_INTEGER;
        l_max_x pls_integer;
        l_max_y pls_integer;
        l_area pls_integer := 0;
        l_max_area pls_integer := 0;
    begin
        for x in 1..300 loop
            for y in 1..300 loop
                l_grid(x)(y) := mod(floor(((x+10)*y+p_serial_number)*(x+10)/100),10)-5;
            end loop;
        end loop;
        
        for xstart in 1..297 loop
            for ystart in 1..297 loop
                l_area := 0;
                for x in xstart..xstart+2 loop
                    for y in ystart..ystart+2 loop
                    l_area := l_area + l_grid(x)(y);
                    end loop;
                end loop;
                if l_area > l_max_area then
                    l_max_x := xstart;
                    l_max_y := ystart;
                    l_max_area := l_area;
                end if;
            end loop;
        end loop;

        return l_max_x || ',' || l_max_y;
    end;
    
    --Part 2 took hours to finish, optimization needed!
    function part2(p_serial_number in number)
    return varchar2
    is
        l_grid grid_aat;
        l_rack_id PLS_INTEGER;
        l_area pls_integer := 0;
        l_max_x pls_integer;
        l_max_y pls_integer;
        l_max_area pls_integer := 0;
        l_max_size pls_integer := 0;
    begin
        for x in 1..300 loop
            for y in 1..300 loop
                l_grid(x)(y) := mod(floor(((x+10)*y+p_serial_number)*(x+10)/100),10)-5;
            end loop;
        end loop;
        for c_size in 1..300 loop
            for xstart in 1..(300-c_size+1) loop
                for ystart in 1..(300-c_size+1) loop
                    l_area := 0;
                    for x in xstart..xstart+c_size-1 loop
                        for y in ystart..ystart+c_size-1 loop
                        l_area := l_area + l_grid(x)(y);
                        end loop;
                    end loop;
                    if l_area > l_max_area then
                        l_max_x := xstart;
                        l_max_y := ystart;
                        l_max_size := c_size;
                        l_max_area := l_area;
                    end if;
                end loop;
            end loop;
        end loop;
        return l_max_x || ',' || l_max_y || ',' || l_max_size;
    end;

    procedure doit(p_serial_number in number)
    is    
    begin
        log('Part 1: ' || part1(p_serial_number));
        log('Part 2: ' || part2(p_serial_number));
    end;

end;
/

-- Execution
begin
    pkg_advcode.doit(7403);
end;
/

-- Print result
select message
from   output
order by id
/

drop table input purge
/

drop table output purge
/
