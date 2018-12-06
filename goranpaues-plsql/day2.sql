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
    TYPE strings_aat IS TABLE OF varchar2(200)
    INDEX BY PLS_INTEGER; 
    
    TYPE count_aat IS TABLE OF NUMBER
    INDEX BY VARCHAR2(1); 
        
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
        l_charcount count_aat;
        l_current_char varchar2(1);
        l_2exists boolean;
        l_3exists boolean;
        l_count2 number := 0;
        l_count3 number := 0;
        l_charidx varchar2(1);
    begin
        for c in (select id, message from input) loop
            l_2exists := false;
            l_3exists := false;
            l_charcount.delete;
            for indx in 1 .. length(c.message) loop
                l_current_char := substr(c.message,indx,1);
                if not l_charcount.exists(l_current_char) then l_charcount(l_current_char) := 0;
                end if;
                l_charcount(l_current_char) := l_charcount(l_current_char) + 1;              
            end loop;
            l_charidx := l_charcount.first;
            while (l_charidx is not null)
            loop
                if l_charcount(l_charidx) = 2 then l_2exists := true;
                elsif l_charcount(l_charidx) = 3 then l_3exists := true;
                end if;
                l_charidx := l_charcount.next(l_charidx);
            end loop;
            if l_2exists then l_count2 := l_count2 + 1; end if;
            if l_3exists then l_count3 := l_count3 + 1; end if;
        end loop;

        return l_count2*l_count3;
    end;

    function part2
    return varchar2
    is
        l_strings strings_aat;
        l_difference number;
        l_result varchar2(100);        
    begin
        select message bulk collect into l_strings from input;
        <<outer_loop>>
        for indx in l_strings.first..(l_strings.last-1) loop
            for indx2 in indx+1..l_strings.last loop
                l_result := null;
                l_difference := 0;
                for indx3 in 1..length(l_strings(indx)) loop
                    if ( substr(l_strings(indx),indx3,1) = substr(l_strings(indx2),indx3,1)) then
                        l_result := l_result || substr(l_strings(indx),indx3,1);
                    else 
                        l_difference := l_difference + 1;
                    end if;
                end loop;
                exit outer_loop when l_difference = 1;
            end loop;
        end loop outer_loop;
        return l_result;
    end;

    procedure doit
    is    
    begin
        log('Part 1: ' || part1);
        log('Part 2: ' || part2);
    end;

end;
/

insert into input (message) values ('ovfclbidieyujztrswxmckgnaw')
/
insert into input (message) values ('pmfqlbimheyujztrswxmckgnap')
/
insert into input (message) values ('ovfqlbidhetujztrswxmcfgnas')
/
insert into input (message) values ('gvfqebddheyujztrswxmckgnap')
/
insert into input (message) values ('ovfqlbidheyejztrswxqekgnap')
/
insert into input (message) values ('ovzqlbiqheyujztsswxmckgnap')
/
insert into input (message) values ('oofqlbidhoyujztoswxmckgnap')
/
insert into input (message) values ('ovfqlbicqeyujztrswxmckgncp')
/
insert into input (message) values ('ovfelbidheyujltrswxmcwgnap')
/
insert into input (message) values ('ovfqlbidheyujzerswxmchgnaf')
/
insert into input (message) values ('bvfqlbidheyxjztnswxmckgnap')
/
insert into input (message) values ('ovfqlbidheyugztrswamnkgnap')
/
insert into input (message) values ('ovfqxbidheyujrtrswxmckgbap')
/
insert into input (message) values ('ovfqlbidheyujztrdwxqckgjap')
/
insert into input (message) values ('ovfqebiqheyujztrscxmckgnap')
/
insert into input (message) values ('avfqlbidheyvjztkswxmckgnap')
/
insert into input (message) values ('ovfqlbidheyujktrswxvskgnap')
/
insert into input (message) values ('ovfqlbidheeujztrswrmckgnae')
/
insert into input (message) values ('ovaqlbidheydjztrswxmchgnap')
/
insert into input (message) values ('ovfqlbodweyujztpswxmckgnap')
/
insert into input (message) values ('xvfqlbidhmyujztrswxmykgnap')
/
insert into input (message) values ('ovfqlnidheyujztxswumckgnap')
/
insert into input (message) values ('ovfqlbidhexujztrswxyckgeap')
/
insert into input (message) values ('ovfqlkidhekubztrswxmckgnap')
/
insert into input (message) values ('ovfqlbidheysjzkrsxxmckgnap')
/
insert into input (message) values ('oxfqebidheyujzprswxmckgnap')
/
insert into input (message) values ('ovfqlbidhetujztrswmmckghap')
/
insert into input (message) values ('ovfclbidhuyujztrswrmckgnap')
/
insert into input (message) values ('ovfqlbijhdyujztrswxmcvgnap')
/
insert into input (message) values ('ovfqlkidhyyujztrswxvckgnap')
/
insert into input (message) values ('ovfqlbiehlyujztrswxhckgnap')
/
insert into input (message) values ('ovfqlbidheyxjjtrsdxmckgnap')
/
insert into input (message) values ('jvfqlbidheyujztrvwxmckcnap')
/
insert into input (message) values ('ovfvlbidheyujzhrswxmckgnzp')
/
insert into input (message) values ('ovfqnbidhuyujztrswfmckgnap')
/
insert into input (message) values ('ovfrlbidheyujztpswxmckgnat')
/
insert into input (message) values ('ovfqpbidheyujztrywxmcngnap')
/
insert into input (message) values ('ovfqlbidheyumrtrswpmckgnap')
/
insert into input (message) values ('ovfqlbidhoyzjztrswxmckgkap')
/
insert into input (message) values ('ovfqlbibheyuhztrswxmskgnap')
/
insert into input (message) values ('ovfqlbidheybjzfrswxkckgnap')
/
insert into input (message) values ('ovfqnbinheyujztrawxmckgnap')
/
insert into input (message) values ('ovfqlbidheyujztryxxmckgnao')
/
insert into input (message) values ('ovfqzbidheyujztrsuxmckgnpp')
/
insert into input (message) values ('ovfqlbidherujztrswxmckgjsp')
/
insert into input (message) values ('ovfqlbidheyujhtrywxmckgtap')
/
insert into input (message) values ('oofmlbidheyujftrswxmckgnap')
/
insert into input (message) values ('ovfqlbidhhyujztrawxmckgnbp')
/
insert into input (message) values ('ovfqlbidheyujztrswxeckmnae')
/
insert into input (message) values ('lvfqlbidheyujztrswxzckvnap')
/
insert into input (message) values ('ovfqlbidheyujztrswxmckqnfr')
/
insert into input (message) values ('offqlbidheyrjztrswxmwkgnap')
/
insert into input (message) values ('ovnqlbidzeyujztmswxmckgnap')
/
insert into input (message) values ('ovfxlbxdheyujztrawxmckgnap')
/
insert into input (message) values ('ovfqmbidheyujztrsaxwckgnap')
/
insert into input (message) values ('ovfqlbidhryzjztrswxmckgcap')
/
insert into input (message) values ('offqlbidheyujnthswxmckgnap')
/
insert into input (message) values ('ogmqlbimheyujztrswxmckgnap')
/
insert into input (message) values ('ovfqlbidheyulztkswxockgnap')
/
insert into input (message) values ('ovfqlbidheyujjtrswxmckypap')
/
insert into input (message) values ('ovfqibidheypjztrswxmskgnap')
/
insert into input (message) values ('ovfqlbwdhyyujztrswxmckgnnp')
/
insert into input (message) values ('ovfqlbidheyujztsvwxmckgkap')
/
insert into input (message) values ('odfqlbidoeyujztrswxjckgnap')
/
insert into input (message) values ('ovfqlbodpeyujztrswxmcggnap')
/
insert into input (message) values ('ovfqlbicheyujztrhwxmcagnap')
/
insert into input (message) values ('ovfqlbidheyuaztrgwxmckhnap')
/
insert into input (message) values ('ovfwlbidhyyujztrswtmckgnap')
/
insert into input (message) values ('ovfqlbidgzyujztrswxmckgcap')
/
insert into input (message) values ('ovnqlbcdheyujztrswxmckgnup')
/
insert into input (message) values ('ovfqlbieheyujrtrsdxmckgnap')
/
insert into input (message) values ('ovfqlbidkeyujztrswfmckgnqp')
/
insert into input (message) values ('ovfqlbidtekujztrswxsckgnap')
/
insert into input (message) values ('ovfklbedheyujztrscxmckgnap')
/
insert into input (message) values ('ovfqltivhnyujztrswxmckgnap')
/
insert into input (message) values ('ovfqlbidheyuvuyrswxmckgnap')
/
insert into input (message) values ('ovfqlbidheyjjrtrcwxmckgnap')
/
insert into input (message) values ('ojfqlbidheyujztrswxmckguvp')
/
insert into input (message) values ('ovfqlbidheiqjqtrswxmckgnap')
/
insert into input (message) values ('ivfqlfidheyujatrswxmckgnap')
/
insert into input (message) values ('cvfqlbidheyujgtrswxmckgnrp')
/
insert into input (message) values ('ovfclbidheeujztrswxmckgnaw')
/
insert into input (message) values ('ovfqlbhdheyujztrswvmcklnap')
/
insert into input (message) values ('ovfqcbidheyvjztaswxmckgnap')
/
insert into input (message) values ('ovgqlbijheyujztrswxvckgnap')
/
insert into input (message) values ('gvfqlbidheyujvtrswxmckgnaj')
/
insert into input (message) values ('ovfqlbidheyujztrdwxmcggnvp')
/
insert into input (message) values ('cvfqlbidheyujgtrswxmckqnap')
/
insert into input (message) values ('ovfqlbrdheyqjztrswxmckgnaj')
/
insert into input (message) values ('ovfqlbidheyujzjrswbmcrgnap')
/
insert into input (message) values ('ovfqlbvdheyujxtrswxvckgnap')
/
insert into input (message) values ('ovaqlbidheyujctrswxmbkgnap')
/
insert into input (message) values ('ovfqlbidheyujztgdwxvckgnap')
/
insert into input (message) values ('ovfqlbidhevujztrssxmwkgnap')
/
insert into input (message) values ('rvfqlbidheyujztrzwxmckhnap')
/
insert into input (message) values ('ovfqmbidheysjztrswxmikgnap')
/
insert into input (message) values ('ovfqlbidheiujztrsdxuckgnap')
/
insert into input (message) values ('ovfqlbidheyveztrswxmckgnah')
/
insert into input (message) values ('ovfqnbiaheytjztrswxmckgnap')
/
insert into input (message) values ('ovfqlbidnayujhtrswxmckgnap')
/
insert into input (message) values ('ovfqlbidheyujztnswxdckgnag')
/
insert into input (message) values ('ovfqlbidheyuyztrswxmzzgnap')
/
insert into input (message) values ('ovfqlbiohexujzthswxmckgnap')
/
insert into input (message) values ('lvfqlbidheyujztcswxxckgnap')
/
insert into input (message) values ('ovuqlbidhfxujztrswxmckgnap')
/
insert into input (message) values ('ovfqluidheyujotrswxmrkgnap')
/
insert into input (message) values ('ovfalbidheyujztrswxhckgngp')
/
insert into input (message) values ('ohjqlbidheyujztrswumckgnap')
/
insert into input (message) values ('ovfqxbidhecujztrspxmckgnap')
/
insert into input (message) values ('ovfqcbidheyusztrpwxmckgnap')
/
insert into input (message) values ('fvfwlbidheyujztrswxmcxgnap')
/
insert into input (message) values ('ovfqlbidhxyplztrswxmckgnap')
/
insert into input (message) values ('ovfqlbidheyujftrswxdckgrap')
/
insert into input (message) values ('ovfqlepdheyujztrswxmckgnjp')
/
insert into input (message) values ('ojjqlbidhuyujztrswxmckgnap')
/
insert into input (message) values ('ovfqlbwdheyujztrswxmcggeap')
/
insert into input (message) values ('ovfqlbidheyujltrscxkckgnap')
/
insert into input (message) values ('oifqibidheyujztrswxjckgnap')
/
insert into input (message) values ('ovfqlbigheyujztrswdmcqgnap')
/
insert into input (message) values ('ovfqlbieheyujztrswxzzkgnap')
/
insert into input (message) values ('ovfqlbidheyujztrswmmcgbnap')
/
insert into input (message) values ('ovfqlbidhnyujzerswxmkkgnap')
/
insert into input (message) values ('ovfqdbinheyujztrswxeckgnap')
/
insert into input (message) values ('oveqlbidheyujztrswhmckgnab')
/
insert into input (message) values ('ovfqkbytheyujztrswxmckgnap')
/
insert into input (message) values ('ovfqlbidheyujstsswxmcklnap')
/
insert into input (message) values ('ovfimbidheyujztrewxmckgnap')
/
insert into input (message) values ('ovfqebidhequjztrnwxmckgnap')
/
insert into input (message) values ('ovfqlbidheyukztrswxmckunwp')
/
insert into input (message) values ('oifqlbidheyuwztrswxmckgnao')
/
insert into input (message) values ('ovfqlbidweyufztrswxmckgtap')
/
insert into input (message) values ('evfqlbidheyujztrswxsckvnap')
/
insert into input (message) values ('svbqlbidheyujztrsaxmckgnap')
/
insert into input (message) values ('ovfqlbidheyaoztrswxmckjnap')
/
insert into input (message) values ('ovfqllidheyujztrswxmckynhp')
/
insert into input (message) values ('ohfqlbidheyujatrswtmckgnap')
/
insert into input (message) values ('omfjlfidheyujztrswxmckgnap')
/
insert into input (message) values ('xvfqlbidheyujutrswxvckgnap')
/
insert into input (message) values ('ovfqlbidheyukztsswxmzkgnap')
/
insert into input (message) values ('ovfqibidhehujztrswxeckgnap')
/
insert into input (message) values ('ovfqlbydheyuoztrswxmcygnap')
/
insert into input (message) values ('ovfqlbidheyufztrmwxmckvnap')
/
insert into input (message) values ('ovfqrbkdheyujztrswxmckgnaq')
/
insert into input (message) values ('ovfqlbigheyuyztrswxmckgzap')
/
insert into input (message) values ('ovfqlbidheyujztrsjxmcnnnap')
/
insert into input (message) values ('uvfqlbipheyujztrswxmckgnay')
/
insert into input (message) values ('ovfqlbddneyujbtrswxmckgnap')
/
insert into input (message) values ('tvfqlbidheyujztrswxpckgeap')
/
insert into input (message) values ('ovfqlbidheyuiztrhwxmckznap')
/
insert into input (message) values ('ovfqlbidheyujzteswxvckgvap')
/
insert into input (message) values ('avfqlbidheyijzlrswxmckgnap')
/
insert into input (message) values ('oqfqmbidheyujvtrswxmckgnap')
/
insert into input (message) values ('ovnqlbidneyujztrswxmckxnap')
/
insert into input (message) values ('ovfqlbidfeyujztrswxqckgncp')
/
insert into input (message) values ('ovfaybidheyujztrswxrckgnap')
/
insert into input (message) values ('ovfqlbidhemujzarnwxmckgnap')
/
insert into input (message) values ('ovfqlwidheyujctrsfxmckgnap')
/
insert into input (message) values ('ovtelbidheysjztrswxmckgnap')
/
insert into input (message) values ('ovfqlbidheyujztrswsmchunap')
/
insert into input (message) values ('pvfqlbidheyulztrswxmckynap')
/
insert into input (message) values ('ovfqlbzdhezujztfswxmckgnap')
/
insert into input (message) values ('ozfqibidheyujztrhwxmckgnap')
/
insert into input (message) values ('ovfqlbioheycjztmswxmckgnap')
/
insert into input (message) values ('ovfqleidheyujztoswxmckgnhp')
/
insert into input (message) values ('ovfqlcidhejujztrswnmckgnap')
/
insert into input (message) values ('eqfqlbidheyujztrswxmrkgnap')
/
insert into input (message) values ('ovfqlbitheywjztrmwxmckgnap')
/
insert into input (message) values ('ovfqlbidheyujptrswnmcggnap')
/
insert into input (message) values ('oofqlbidhjyujztrswxmcegnap')
/
insert into input (message) values ('ovfqlbidmeyujztrswxmcxgnxp')
/
insert into input (message) values ('ovjhlbidhefujztrswxmckgnap')
/
insert into input (message) values ('ovfqlbidkeyujzarswxmcugnap')
/
insert into input (message) values ('hvyqlridheyujztrswxmckgnap')
/
insert into input (message) values ('ovfqlbidheyujxtrswqmckgnpp')
/
insert into input (message) values ('ovfqlbidheyuiztrtsxmckgnap')
/
insert into input (message) values ('ovfqlbidqeyuuztrbwxmckgnap')
/
insert into input (message) values ('ovfqpbidheyujztrswxwekgnap')
/
insert into input (message) values ('ovfqltidheyujztrbwxmckgnab')
/
insert into input (message) values ('okfxluidheyujztrswxmckgnap')
/
insert into input (message) values ('ovfplbidheyujztrsaxmckgnax')
/
insert into input (message) values ('wvfqlbidheiujztrswxjckgnap')
/
insert into input (message) values ('ovfqlbidheyqjzlrsqxmckgnap')
/
insert into input (message) values ('ovfqlbadheyujztrsxxmckgnop')
/
insert into input (message) values ('ovfqliidheyujzerswvmckgnap')
/
insert into input (message) values ('ovlrlbidheyujztaswxmckgnap')
/
insert into input (message) values ('cvzqlbidheyujgtrswxmckqnap')
/
insert into input (message) values ('ovfqlbidheyujzuqswxmckgnnp')
/
insert into input (message) values ('ovfqlsjdheyujztrswxmcklnap')
/
insert into input (message) values ('ovrqlbidheyujztrssrmckgnap')
/
insert into input (message) values ('ovcqlbidheyujztrsmxmcognap')
/
insert into input (message) values ('ovcqlbidheyjjztrswxmckunap')
/
insert into input (message) values ('ovfilbrdhnyujztrswxmckgnap')
/
insert into input (message) values ('ovfqlbodheyujztrswxeckqnap')
/
insert into input (message) values ('ovfqlbidhuyujqtrswxdckgnap')
/
insert into input (message) values ('ovmqlbidheyujderswxmckgnap')
/
insert into input (message) values ('ovfylbidheyajzrrswxmckgnap')
/
insert into input (message) values ('ovfklbidhtyujzjrswxmckgnap')
/
insert into input (message) values ('rvfqlbidheyujztcswxcckgnap')
/
insert into input (message) values ('ovfnlyidheyuwztrswxmckgnap')
/
insert into input (message) values ('ovfqlbidhexujztrswxmpktnap')
/
insert into input (message) values ('ovfplbidheyfjztrswhmckgnap')
/
insert into input (message) values ('oyfqlbidmexujztrswxmckgnap')
/
insert into input (message) values ('mvfqlbidhcyujztrawxmckgnap')
/
insert into input (message) values ('ovfqlbidhqyujdtrswxmcbgnap')
/
insert into input (message) values ('ovfqjbidheybjrtrswxmckgnap')
/
insert into input (message) values ('ozfqlbidhbyujztrswxmckgpap')
/
insert into input (message) values ('okfqlbidheyujztrmwxmckhnap')
/
insert into input (message) values ('ovfqlbydheyujzrrswxnckgnap')
/
insert into input (message) values ('ovfqtbidheycjztrswxmckgnah')
/
insert into input (message) values ('avfqloidheyujztrswxyckgnap')
/
insert into input (message) values ('ovfqlbldteyujzyrswxmckgnap')
/
insert into input (message) values ('ovfqlbpdhedujztpswxmckgnap')
/
insert into input (message) values ('ovfqlbidheyujztrswxucrvnap')
/
insert into input (message) values ('ocnqlbidheyujztrswxmwkgnap')
/
insert into input (message) values ('ovfqlvidheyujztrswkmckgnlp')
/
insert into input (message) values ('evfqlbidheyujzmrswqmckgnap')
/
insert into input (message) values ('ovfqlbidhryujztrcwxmekgnap')
/
insert into input (message) values ('ovfqlbvdheyujztrzwxmcxgnap')
/
insert into input (message) values ('ovfqlridgeyujztrswxmkkgnap')
/
insert into input (message) values ('yvfqlbidheyujzthswzmckgnap')
/
insert into input (message) values ('ovfqlbidheyujmtrswxyukgnap')
/
insert into input (message) values ('ovfqlbidheqgjztrswdmckgnap')
/
insert into input (message) values ('dvfzlcidheyujztrswxmckgnap')
/
insert into input (message) values ('jvfqlbidheyujztrswxmczgnae')
/
insert into input (message) values ('ovfqlbzdheyujztrswxyckgnjp')
/
insert into input (message) values ('ovfqlbxdheyujatrswxmcqgnap')
/
insert into input (message) values ('ovftlbldheyujztrewxmckgnap')
/
insert into input (message) values ('owfqlbitheyujzyrswxmckgnap')
/
insert into input (message) values ('ovfqlbidheyujztrswxmchgtvp')
/
insert into input (message) values ('ovfqibidheyujzttswxmkkgnap')
/
insert into input (message) values ('ovkqlbodheyujztvswxmckgnap')
/
insert into input (message) values ('onfqlbbdheyujztrxwxmckgnap')
/
insert into input (message) values ('ovfqlbidyeysgztrswxmckgnap')
/
insert into input (message) values ('ovfqlbixherujztrswxmcngnap')
/
insert into input (message) values ('ovlqlbidfeyujztrswxgckgnap')
/
insert into input (message) values ('ovfqlbfdheyujztwswumckgnap')
/
insert into input (message) values ('ovfqlbidheeujztrswxmckgkah')
/
insert into input (message) values ('ovfqtbqdheyujztrswmmckgnap')
/
insert into input (message) values ('bbfqlbigheyujztrswxmckgnap')
/
insert into input (message) values ('ovfqljidheyujztrswxmwkgnip')
/
insert into input (message) values ('ovfqobidheyujztrsvxmmkgnap')
/
insert into input (message) values ('ovfqlbidheydjvtrswxmckanap')
/
insert into input (message) values ('ovfqlxidheyujztrswgmckgnep')
/
insert into input (message) values ('jvfqlbidhzyujztrswxmckgnay')
/
insert into input (message) values ('ovfqlbidhkyujztrswxmlkenap')
/
insert into input (message) values ('ovfqobidheyujztrswxmckjnaf')
/
insert into input (message) values ('ovfxlbidheyujztrswxmcbgnac')
/
insert into input (message) values ('ovfqcbidhtyujztrswxmckqnap')
/
insert into input (message) values ('ozfglbidheyujzvrswxmckgnap')
/
insert into input (message) values ('ovfqlbidheyujztoswxyckcnap')
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

drop table input purge
/

drop table output purge
/
