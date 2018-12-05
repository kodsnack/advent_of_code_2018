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

   TYPE array_t IS TABLE OF NUMBER 
      INDEX BY PLS_INTEGER;

   TYPE matrix_t IS TABLE OF array_t 
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
        l_strings strings_aat;
        l_fabric matrix_t;
        l_start_x number;
        l_start_y number;
        l_size_x number;
        l_size_y number;
        l_result number := 0;
    begin
        select message bulk collect into l_strings from input;
        for indx in l_strings.first..l_strings.last loop
            l_start_x := substr(l_strings(indx),instr(l_strings(indx),' ',1,2)+1,instr(l_strings(indx),',',1,1)-(instr(l_strings(indx),' ',1,2)+1));
            l_start_y := substr(l_strings(indx),instr(l_strings(indx),',',1,1)+1,instr(l_strings(indx),':',1,1)-(instr(l_strings(indx),',',1,1)+1));
            l_size_x := substr(l_strings(indx),instr(l_strings(indx),' ',1,3)+1,instr(l_strings(indx),'x',1,1)-(instr(l_strings(indx),' ',1,3)+1));
            l_size_y := substr(l_strings(indx),instr(l_strings(indx),'x',1,1)+1,length(l_strings(indx))+1-(instr(l_strings(indx),'x',1,1)+1));
            for indx_x in l_start_x+1..l_start_x+l_size_x loop
                for indx_y in l_start_y+1..l_start_y+l_size_y loop
                    begin 
                        --will throw exception of empty
                        l_fabric(indx_x)(indx_y) := l_fabric(indx_x)(indx_y);
                    exception when no_data_found then 
                        l_fabric(indx_x)(indx_y) := 0;
                    end;
                    l_fabric(indx_x)(indx_y) := l_fabric(indx_x)(indx_y) + 1;
                end loop;            
            end loop;
        end loop;
        for indx_x in l_fabric.first..l_fabric.last loop
            for indx_y in l_fabric(indx_x).first..l_fabric(indx_x).last loop
                begin l_fabric(indx_x)(indx_y) := l_fabric(indx_x)(indx_y);
                exception when no_data_found then l_fabric(indx_x)(indx_y) := 0;
                end;
                if l_fabric(indx_x)(indx_y) > 1 then l_result := l_result +1; end if;
            end loop;            
        end loop;
        
        return l_result;
    end;

    function part2
    return number
    is
        l_strings strings_aat;
        l_fabric matrix_t;
        l_start_x number;
        l_start_y number;
        l_size_x number;
        l_size_y number;
        l_claim_id number;
        l_claim_overlap array_t;
    begin
        select message bulk collect into l_strings from input;
        for indx in l_strings.first..l_strings.last loop
            l_claim_id := substr(l_strings(indx),2,instr(l_strings(indx),' ',1,1)-2);
            l_start_x := substr(l_strings(indx),instr(l_strings(indx),' ',1,2)+1,instr(l_strings(indx),',',1,1)-(instr(l_strings(indx),' ',1,2)+1));
            l_start_y := substr(l_strings(indx),instr(l_strings(indx),',',1,1)+1,instr(l_strings(indx),':',1,1)-(instr(l_strings(indx),',',1,1)+1));
            l_size_x := substr(l_strings(indx),instr(l_strings(indx),' ',1,3)+1,instr(l_strings(indx),'x',1,1)-(instr(l_strings(indx),' ',1,3)+1));
            l_size_y := substr(l_strings(indx),instr(l_strings(indx),'x',1,1)+1,length(l_strings(indx))+1-(instr(l_strings(indx),'x',1,1)+1));
            l_claim_overlap(l_claim_id) := 0;
            for indx_x in l_start_x+1..l_start_x+l_size_x loop
                for indx_y in l_start_y+1..l_start_y+l_size_y loop
                    begin
                        --will throw exception of empty
                        l_fabric(indx_x)(indx_y) := l_fabric(indx_x)(indx_y);
                        --this part only runs if overlapped
                        l_claim_overlap(l_claim_id) := 1;
                        l_claim_overlap(l_fabric(indx_x)(indx_y)) := 1;
                    exception when no_data_found then 
                        -- no overlap
                        l_fabric(indx_x)(indx_y) := l_claim_id;
                    end;
                end loop;            
            end loop;
        end loop;
        l_claim_id := l_claim_overlap.first;
        loop
            exit when l_claim_overlap(l_claim_id) = 0;
            l_claim_id := l_claim_overlap.next(l_claim_id);
        end loop;
        
        return l_claim_id;
    end;

    procedure doit
    is    
    begin
        log('Part 1: ' || part1);
        log('Part 2: ' || part2);
    end;

end;
/

insert into input (message) values ('#1 @ 935,649: 22x22')
/
insert into input (message) values ('#2 @ 346,47: 19x26')
/
insert into input (message) values ('#3 @ 218,455: 25x17')
/
insert into input (message) values ('#4 @ 451,711: 10x20')
/
insert into input (message) values ('#5 @ 797,342: 28x27')
/
insert into input (message) values ('#6 @ 97,281: 23x13')
/
insert into input (message) values ('#7 @ 752,418: 25x26')
/
insert into input (message) values ('#8 @ 181,975: 11x20')
/
insert into input (message) values ('#9 @ 986,373: 13x18')
/
insert into input (message) values ('#10 @ 734,260: 21x18')
/
insert into input (message) values ('#11 @ 174,822: 15x14')
/
insert into input (message) values ('#12 @ 339,138: 20x28')
/
insert into input (message) values ('#13 @ 190,315: 20x14')
/
insert into input (message) values ('#14 @ 890,631: 26x29')
/
insert into input (message) values ('#15 @ 967,390: 16x10')
/
insert into input (message) values ('#16 @ 314,205: 10x23')
/
insert into input (message) values ('#17 @ 199,93: 14x20')
/
insert into input (message) values ('#18 @ 965,143: 19x29')
/
insert into input (message) values ('#19 @ 876,70: 15x18')
/
insert into input (message) values ('#20 @ 671,254: 11x25')
/
insert into input (message) values ('#21 @ 755,229: 14x18')
/
insert into input (message) values ('#22 @ 578,727: 24x14')
/
insert into input (message) values ('#23 @ 298,459: 11x25')
/
insert into input (message) values ('#24 @ 593,793: 25x28')
/
insert into input (message) values ('#25 @ 377,777: 15x26')
/
insert into input (message) values ('#26 @ 710,224: 3x18')
/
insert into input (message) values ('#27 @ 836,814: 18x16')
/
insert into input (message) values ('#28 @ 10,413: 21x15')
/
insert into input (message) values ('#29 @ 961,500: 27x21')
/
insert into input (message) values ('#30 @ 714,515: 25x13')
/
insert into input (message) values ('#31 @ 47,240: 21x25')
/
insert into input (message) values ('#32 @ 503,203: 27x29')
/
insert into input (message) values ('#33 @ 488,463: 25x15')
/
insert into input (message) values ('#34 @ 139,935: 11x16')
/
insert into input (message) values ('#35 @ 936,573: 16x25')
/
insert into input (message) values ('#36 @ 510,958: 28x18')
/
insert into input (message) values ('#37 @ 818,447: 27x21')
/
insert into input (message) values ('#38 @ 440,606: 27x25')
/
insert into input (message) values ('#39 @ 738,548: 21x21')
/
insert into input (message) values ('#40 @ 251,176: 14x25')
/
insert into input (message) values ('#41 @ 916,835: 25x13')
/
insert into input (message) values ('#42 @ 241,462: 21x18')
/
insert into input (message) values ('#43 @ 694,184: 28x22')
/
insert into input (message) values ('#44 @ 26,363: 13x28')
/
insert into input (message) values ('#45 @ 754,356: 13x12')
/
insert into input (message) values ('#46 @ 647,256: 14x14')
/
insert into input (message) values ('#47 @ 273,259: 17x25')
/
insert into input (message) values ('#48 @ 274,299: 15x17')
/
insert into input (message) values ('#49 @ 775,108: 25x22')
/
insert into input (message) values ('#50 @ 313,73: 17x21')
/
insert into input (message) values ('#51 @ 914,790: 19x18')
/
insert into input (message) values ('#52 @ 699,965: 23x13')
/
insert into input (message) values ('#53 @ 480,607: 16x27')
/
insert into input (message) values ('#54 @ 493,134: 10x26')
/
insert into input (message) values ('#55 @ 868,661: 16x19')
/
insert into input (message) values ('#56 @ 402,792: 11x14')
/
insert into input (message) values ('#57 @ 129,606: 12x12')
/
insert into input (message) values ('#58 @ 190,654: 11x17')
/
insert into input (message) values ('#59 @ 32,628: 23x17')
/
insert into input (message) values ('#60 @ 875,227: 20x21')
/
insert into input (message) values ('#61 @ 28,937: 29x14')
/
insert into input (message) values ('#62 @ 416,864: 27x18')
/
insert into input (message) values ('#63 @ 94,378: 15x13')
/
insert into input (message) values ('#64 @ 40,774: 18x11')
/
insert into input (message) values ('#65 @ 283,547: 21x16')
/
insert into input (message) values ('#66 @ 245,31: 29x12')
/
insert into input (message) values ('#67 @ 268,618: 12x26')
/
insert into input (message) values ('#68 @ 973,980: 20x16')
/
insert into input (message) values ('#69 @ 492,240: 10x18')
/
insert into input (message) values ('#70 @ 898,385: 12x22')
/
insert into input (message) values ('#71 @ 465,782: 14x24')
/
insert into input (message) values ('#72 @ 350,985: 14x15')
/
insert into input (message) values ('#73 @ 804,941: 15x24')
/
insert into input (message) values ('#74 @ 618,624: 14x13')
/
insert into input (message) values ('#75 @ 269,540: 27x19')
/
insert into input (message) values ('#76 @ 220,949: 10x18')
/
insert into input (message) values ('#77 @ 577,299: 19x18')
/
insert into input (message) values ('#78 @ 311,132: 26x17')
/
insert into input (message) values ('#79 @ 202,330: 26x11')
/
insert into input (message) values ('#80 @ 564,413: 19x21')
/
insert into input (message) values ('#81 @ 38,472: 19x28')
/
insert into input (message) values ('#82 @ 352,670: 15x11')
/
insert into input (message) values ('#83 @ 39,532: 11x28')
/
insert into input (message) values ('#84 @ 375,832: 24x16')
/
insert into input (message) values ('#85 @ 453,232: 25x17')
/
insert into input (message) values ('#86 @ 649,571: 14x12')
/
insert into input (message) values ('#87 @ 876,324: 28x28')
/
insert into input (message) values ('#88 @ 932,177: 11x20')
/
insert into input (message) values ('#89 @ 709,241: 24x10')
/
insert into input (message) values ('#90 @ 698,546: 12x21')
/
insert into input (message) values ('#91 @ 773,51: 15x17')
/
insert into input (message) values ('#92 @ 113,436: 16x12')
/
insert into input (message) values ('#93 @ 598,794: 11x27')
/
insert into input (message) values ('#94 @ 581,437: 14x14')
/
insert into input (message) values ('#95 @ 787,620: 29x15')
/
insert into input (message) values ('#96 @ 8,593: 10x21')
/
insert into input (message) values ('#97 @ 845,607: 19x22')
/
insert into input (message) values ('#98 @ 189,672: 16x15')
/
insert into input (message) values ('#99 @ 335,901: 28x17')
/
insert into input (message) values ('#100 @ 52,312: 14x16')
/
insert into input (message) values ('#101 @ 13,487: 21x13')
/
insert into input (message) values ('#102 @ 937,624: 15x23')
/
insert into input (message) values ('#103 @ 148,804: 21x18')
/
insert into input (message) values ('#104 @ 707,146: 21x18')
/
insert into input (message) values ('#105 @ 188,507: 27x28')
/
insert into input (message) values ('#106 @ 450,647: 12x14')
/
insert into input (message) values ('#107 @ 430,250: 11x27')
/
insert into input (message) values ('#108 @ 279,519: 26x28')
/
insert into input (message) values ('#109 @ 736,495: 24x12')
/
insert into input (message) values ('#110 @ 693,454: 13x12')
/
insert into input (message) values ('#111 @ 982,624: 12x12')
/
insert into input (message) values ('#112 @ 409,713: 22x26')
/
insert into input (message) values ('#113 @ 644,311: 13x14')
/
insert into input (message) values ('#114 @ 106,418: 20x16')
/
insert into input (message) values ('#115 @ 714,740: 19x18')
/
insert into input (message) values ('#116 @ 732,814: 13x15')
/
insert into input (message) values ('#117 @ 756,213: 10x27')
/
insert into input (message) values ('#118 @ 336,230: 27x18')
/
insert into input (message) values ('#119 @ 817,358: 17x20')
/
insert into input (message) values ('#120 @ 913,779: 29x28')
/
insert into input (message) values ('#121 @ 351,913: 29x23')
/
insert into input (message) values ('#122 @ 707,408: 19x10')
/
insert into input (message) values ('#123 @ 195,666: 12x19')
/
insert into input (message) values ('#124 @ 688,451: 14x25')
/
insert into input (message) values ('#125 @ 851,822: 23x26')
/
insert into input (message) values ('#126 @ 832,839: 25x12')
/
insert into input (message) values ('#127 @ 434,38: 12x21')
/
insert into input (message) values ('#128 @ 627,42: 29x29')
/
insert into input (message) values ('#129 @ 340,78: 20x10')
/
insert into input (message) values ('#130 @ 402,328: 26x24')
/
insert into input (message) values ('#131 @ 257,741: 21x16')
/
insert into input (message) values ('#132 @ 677,67: 12x13')
/
insert into input (message) values ('#133 @ 140,703: 12x21')
/
insert into input (message) values ('#134 @ 522,165: 11x24')
/
insert into input (message) values ('#135 @ 407,840: 18x16')
/
insert into input (message) values ('#136 @ 625,712: 21x14')
/
insert into input (message) values ('#137 @ 850,727: 26x11')
/
insert into input (message) values ('#138 @ 428,851: 24x20')
/
insert into input (message) values ('#139 @ 351,537: 11x26')
/
insert into input (message) values ('#140 @ 830,896: 16x14')
/
insert into input (message) values ('#141 @ 837,324: 29x22')
/
insert into input (message) values ('#142 @ 485,26: 21x17')
/
insert into input (message) values ('#143 @ 121,861: 24x15')
/
insert into input (message) values ('#144 @ 220,960: 29x12')
/
insert into input (message) values ('#145 @ 74,235: 14x23')
/
insert into input (message) values ('#146 @ 354,728: 22x28')
/
insert into input (message) values ('#147 @ 351,776: 21x18')
/
insert into input (message) values ('#148 @ 508,64: 18x26')
/
insert into input (message) values ('#149 @ 795,45: 10x19')
/
insert into input (message) values ('#150 @ 436,400: 28x22')
/
insert into input (message) values ('#151 @ 958,248: 24x26')
/
insert into input (message) values ('#152 @ 388,818: 14x24')
/
insert into input (message) values ('#153 @ 957,901: 19x21')
/
insert into input (message) values ('#154 @ 952,755: 12x21')
/
insert into input (message) values ('#155 @ 763,242: 14x16')
/
insert into input (message) values ('#156 @ 63,483: 24x11')
/
insert into input (message) values ('#157 @ 319,572: 13x16')
/
insert into input (message) values ('#158 @ 477,701: 10x25')
/
insert into input (message) values ('#159 @ 557,537: 20x15')
/
insert into input (message) values ('#160 @ 331,915: 24x16')
/
insert into input (message) values ('#161 @ 877,956: 17x14')
/
insert into input (message) values ('#162 @ 956,570: 29x16')
/
insert into input (message) values ('#163 @ 490,200: 18x29')
/
insert into input (message) values ('#164 @ 596,525: 22x22')
/
insert into input (message) values ('#165 @ 311,935: 27x11')
/
insert into input (message) values ('#166 @ 218,318: 22x12')
/
insert into input (message) values ('#167 @ 533,766: 27x25')
/
insert into input (message) values ('#168 @ 788,881: 10x10')
/
insert into input (message) values ('#169 @ 36,179: 26x29')
/
insert into input (message) values ('#170 @ 122,316: 28x20')
/
insert into input (message) values ('#171 @ 153,55: 29x21')
/
insert into input (message) values ('#172 @ 388,618: 29x24')
/
insert into input (message) values ('#173 @ 749,344: 25x15')
/
insert into input (message) values ('#174 @ 466,726: 23x16')
/
insert into input (message) values ('#175 @ 437,461: 22x26')
/
insert into input (message) values ('#176 @ 762,263: 15x18')
/
insert into input (message) values ('#177 @ 810,883: 27x14')
/
insert into input (message) values ('#178 @ 400,490: 12x25')
/
insert into input (message) values ('#179 @ 429,295: 23x10')
/
insert into input (message) values ('#180 @ 542,364: 18x17')
/
insert into input (message) values ('#181 @ 807,445: 24x26')
/
insert into input (message) values ('#182 @ 271,252: 13x21')
/
insert into input (message) values ('#183 @ 439,682: 29x21')
/
insert into input (message) values ('#184 @ 335,725: 20x27')
/
insert into input (message) values ('#185 @ 158,58: 11x11')
/
insert into input (message) values ('#186 @ 167,546: 22x11')
/
insert into input (message) values ('#187 @ 13,803: 19x28')
/
insert into input (message) values ('#188 @ 775,527: 23x24')
/
insert into input (message) values ('#189 @ 920,169: 28x19')
/
insert into input (message) values ('#190 @ 908,823: 28x20')
/
insert into input (message) values ('#191 @ 391,264: 18x20')
/
insert into input (message) values ('#192 @ 550,919: 20x17')
/
insert into input (message) values ('#193 @ 788,392: 26x13')
/
insert into input (message) values ('#194 @ 160,393: 13x20')
/
insert into input (message) values ('#195 @ 226,638: 12x18')
/
insert into input (message) values ('#196 @ 809,602: 16x18')
/
insert into input (message) values ('#197 @ 33,372: 18x21')
/
insert into input (message) values ('#198 @ 591,865: 23x15')
/
insert into input (message) values ('#199 @ 796,765: 25x19')
/
insert into input (message) values ('#200 @ 429,599: 24x13')
/
insert into input (message) values ('#201 @ 584,391: 21x17')
/
insert into input (message) values ('#202 @ 336,192: 18x13')
/
insert into input (message) values ('#203 @ 883,30: 24x28')
/
insert into input (message) values ('#204 @ 790,195: 27x12')
/
insert into input (message) values ('#205 @ 342,304: 16x19')
/
insert into input (message) values ('#206 @ 348,758: 22x27')
/
insert into input (message) values ('#207 @ 336,133: 11x13')
/
insert into input (message) values ('#208 @ 313,96: 11x17')
/
insert into input (message) values ('#209 @ 315,922: 27x20')
/
insert into input (message) values ('#210 @ 802,266: 14x23')
/
insert into input (message) values ('#211 @ 415,178: 25x20')
/
insert into input (message) values ('#212 @ 43,945: 16x29')
/
insert into input (message) values ('#213 @ 647,70: 29x12')
/
insert into input (message) values ('#214 @ 800,590: 18x14')
/
insert into input (message) values ('#215 @ 362,907: 17x17')
/
insert into input (message) values ('#216 @ 586,773: 24x17')
/
insert into input (message) values ('#217 @ 125,913: 26x24')
/
insert into input (message) values ('#218 @ 363,757: 22x19')
/
insert into input (message) values ('#219 @ 530,368: 16x13')
/
insert into input (message) values ('#220 @ 366,455: 16x17')
/
insert into input (message) values ('#221 @ 331,134: 10x18')
/
insert into input (message) values ('#222 @ 552,307: 18x27')
/
insert into input (message) values ('#223 @ 972,366: 15x27')
/
insert into input (message) values ('#224 @ 252,150: 20x11')
/
insert into input (message) values ('#225 @ 41,111: 12x13')
/
insert into input (message) values ('#226 @ 364,457: 29x11')
/
insert into input (message) values ('#227 @ 16,166: 16x10')
/
insert into input (message) values ('#228 @ 965,784: 19x18')
/
insert into input (message) values ('#229 @ 682,625: 17x13')
/
insert into input (message) values ('#230 @ 872,213: 28x28')
/
insert into input (message) values ('#231 @ 38,985: 26x10')
/
insert into input (message) values ('#232 @ 276,335: 12x24')
/
insert into input (message) values ('#233 @ 863,940: 28x20')
/
insert into input (message) values ('#234 @ 321,797: 19x16')
/
insert into input (message) values ('#235 @ 124,222: 22x10')
/
insert into input (message) values ('#236 @ 731,31: 28x19')
/
insert into input (message) values ('#237 @ 185,799: 19x14')
/
insert into input (message) values ('#238 @ 166,434: 24x10')
/
insert into input (message) values ('#239 @ 913,805: 29x14')
/
insert into input (message) values ('#240 @ 355,617: 18x25')
/
insert into input (message) values ('#241 @ 361,976: 29x23')
/
insert into input (message) values ('#242 @ 188,219: 15x17')
/
insert into input (message) values ('#243 @ 63,460: 24x28')
/
insert into input (message) values ('#244 @ 288,953: 20x15')
/
insert into input (message) values ('#245 @ 913,922: 14x14')
/
insert into input (message) values ('#246 @ 22,167: 21x15')
/
insert into input (message) values ('#247 @ 755,554: 28x18')
/
insert into input (message) values ('#248 @ 560,542: 9x6')
/
insert into input (message) values ('#249 @ 59,287: 22x16')
/
insert into input (message) values ('#250 @ 953,249: 11x16')
/
insert into input (message) values ('#251 @ 564,433: 21x11')
/
insert into input (message) values ('#252 @ 506,356: 15x23')
/
insert into input (message) values ('#253 @ 328,197: 22x24')
/
insert into input (message) values ('#254 @ 404,378: 28x15')
/
insert into input (message) values ('#255 @ 39,385: 10x16')
/
insert into input (message) values ('#256 @ 619,580: 14x26')
/
insert into input (message) values ('#257 @ 818,825: 28x15')
/
insert into input (message) values ('#258 @ 893,810: 16x21')
/
insert into input (message) values ('#259 @ 127,161: 21x23')
/
insert into input (message) values ('#260 @ 959,973: 21x19')
/
insert into input (message) values ('#261 @ 299,124: 24x19')
/
insert into input (message) values ('#262 @ 91,602: 18x25')
/
insert into input (message) values ('#263 @ 502,614: 3x22')
/
insert into input (message) values ('#264 @ 770,852: 20x19')
/
insert into input (message) values ('#265 @ 191,544: 24x28')
/
insert into input (message) values ('#266 @ 78,714: 12x27')
/
insert into input (message) values ('#267 @ 865,161: 15x14')
/
insert into input (message) values ('#268 @ 577,861: 29x12')
/
insert into input (message) values ('#269 @ 265,710: 19x10')
/
insert into input (message) values ('#270 @ 252,982: 23x12')
/
insert into input (message) values ('#271 @ 792,29: 11x29')
/
insert into input (message) values ('#272 @ 845,696: 13x13')
/
insert into input (message) values ('#273 @ 712,974: 26x25')
/
insert into input (message) values ('#274 @ 893,231: 23x29')
/
insert into input (message) values ('#275 @ 170,689: 21x11')
/
insert into input (message) values ('#276 @ 520,905: 16x22')
/
insert into input (message) values ('#277 @ 103,603: 11x19')
/
insert into input (message) values ('#278 @ 45,954: 28x23')
/
insert into input (message) values ('#279 @ 111,699: 22x29')
/
insert into input (message) values ('#280 @ 122,43: 13x14')
/
insert into input (message) values ('#281 @ 438,579: 13x13')
/
insert into input (message) values ('#282 @ 725,767: 21x13')
/
insert into input (message) values ('#283 @ 63,811: 22x22')
/
insert into input (message) values ('#284 @ 492,286: 10x19')
/
insert into input (message) values ('#285 @ 432,541: 16x26')
/
insert into input (message) values ('#286 @ 171,39: 21x21')
/
insert into input (message) values ('#287 @ 620,623: 24x27')
/
insert into input (message) values ('#288 @ 33,810: 12x28')
/
insert into input (message) values ('#289 @ 577,736: 17x16')
/
insert into input (message) values ('#290 @ 332,276: 24x18')
/
insert into input (message) values ('#291 @ 951,387: 26x21')
/
insert into input (message) values ('#292 @ 302,683: 13x28')
/
insert into input (message) values ('#293 @ 392,795: 14x16')
/
insert into input (message) values ('#294 @ 965,780: 25x17')
/
insert into input (message) values ('#295 @ 47,566: 17x10')
/
insert into input (message) values ('#296 @ 311,859: 24x18')
/
insert into input (message) values ('#297 @ 170,776: 16x29')
/
insert into input (message) values ('#298 @ 780,941: 25x18')
/
insert into input (message) values ('#299 @ 121,59: 22x11')
/
insert into input (message) values ('#300 @ 125,748: 23x23')
/
insert into input (message) values ('#301 @ 573,48: 12x28')
/
insert into input (message) values ('#302 @ 88,300: 22x14')
/
insert into input (message) values ('#303 @ 77,602: 26x20')
/
insert into input (message) values ('#304 @ 264,553: 24x24')
/
insert into input (message) values ('#305 @ 66,424: 24x27')
/
insert into input (message) values ('#306 @ 802,659: 15x22')
/
insert into input (message) values ('#307 @ 187,924: 20x13')
/
insert into input (message) values ('#308 @ 381,252: 21x25')
/
insert into input (message) values ('#309 @ 780,258: 19x26')
/
insert into input (message) values ('#310 @ 75,914: 29x11')
/
insert into input (message) values ('#311 @ 381,978: 13x12')
/
insert into input (message) values ('#312 @ 721,175: 21x15')
/
insert into input (message) values ('#313 @ 821,445: 29x20')
/
insert into input (message) values ('#314 @ 702,41: 20x23')
/
insert into input (message) values ('#315 @ 888,370: 22x20')
/
insert into input (message) values ('#316 @ 528,393: 27x21')
/
insert into input (message) values ('#317 @ 984,555: 4x4')
/
insert into input (message) values ('#318 @ 983,632: 16x13')
/
insert into input (message) values ('#319 @ 644,36: 23x21')
/
insert into input (message) values ('#320 @ 525,925: 14x14')
/
insert into input (message) values ('#321 @ 165,701: 23x27')
/
insert into input (message) values ('#322 @ 427,858: 24x16')
/
insert into input (message) values ('#323 @ 549,276: 11x23')
/
insert into input (message) values ('#324 @ 821,274: 16x17')
/
insert into input (message) values ('#325 @ 28,625: 17x22')
/
insert into input (message) values ('#326 @ 581,878: 25x10')
/
insert into input (message) values ('#327 @ 897,632: 19x28')
/
insert into input (message) values ('#328 @ 308,114: 24x24')
/
insert into input (message) values ('#329 @ 201,762: 18x20')
/
insert into input (message) values ('#330 @ 883,693: 23x25')
/
insert into input (message) values ('#331 @ 657,878: 19x19')
/
insert into input (message) values ('#332 @ 426,866: 28x15')
/
insert into input (message) values ('#333 @ 319,305: 27x14')
/
insert into input (message) values ('#334 @ 327,918: 17x10')
/
insert into input (message) values ('#335 @ 327,317: 25x19')
/
insert into input (message) values ('#336 @ 960,362: 19x18')
/
insert into input (message) values ('#337 @ 572,404: 14x21')
/
insert into input (message) values ('#338 @ 45,287: 22x18')
/
insert into input (message) values ('#339 @ 552,489: 26x28')
/
insert into input (message) values ('#340 @ 328,670: 10x20')
/
insert into input (message) values ('#341 @ 559,634: 29x28')
/
insert into input (message) values ('#342 @ 174,409: 19x28')
/
insert into input (message) values ('#343 @ 873,136: 23x10')
/
insert into input (message) values ('#344 @ 261,70: 18x14')
/
insert into input (message) values ('#345 @ 702,524: 23x18')
/
insert into input (message) values ('#346 @ 295,647: 25x20')
/
insert into input (message) values ('#347 @ 65,433: 19x27')
/
insert into input (message) values ('#348 @ 704,723: 28x21')
/
insert into input (message) values ('#349 @ 115,916: 20x19')
/
insert into input (message) values ('#350 @ 418,640: 22x24')
/
insert into input (message) values ('#351 @ 263,351: 28x24')
/
insert into input (message) values ('#352 @ 931,293: 18x18')
/
insert into input (message) values ('#353 @ 19,873: 19x28')
/
insert into input (message) values ('#354 @ 436,194: 18x12')
/
insert into input (message) values ('#355 @ 112,783: 23x24')
/
insert into input (message) values ('#356 @ 412,488: 16x12')
/
insert into input (message) values ('#357 @ 260,453: 29x13')
/
insert into input (message) values ('#358 @ 412,259: 21x18')
/
insert into input (message) values ('#359 @ 482,912: 15x20')
/
insert into input (message) values ('#360 @ 957,173: 10x22')
/
insert into input (message) values ('#361 @ 109,441: 24x13')
/
insert into input (message) values ('#362 @ 805,826: 10x24')
/
insert into input (message) values ('#363 @ 680,619: 10x21')
/
insert into input (message) values ('#364 @ 154,804: 29x27')
/
insert into input (message) values ('#365 @ 670,299: 23x24')
/
insert into input (message) values ('#366 @ 33,964: 17x24')
/
insert into input (message) values ('#367 @ 771,320: 28x27')
/
insert into input (message) values ('#368 @ 780,195: 18x11')
/
insert into input (message) values ('#369 @ 828,622: 24x16')
/
insert into input (message) values ('#370 @ 311,786: 15x15')
/
insert into input (message) values ('#371 @ 107,671: 28x14')
/
insert into input (message) values ('#372 @ 958,438: 29x16')
/
insert into input (message) values ('#373 @ 932,472: 24x20')
/
insert into input (message) values ('#374 @ 569,534: 28x11')
/
insert into input (message) values ('#375 @ 692,571: 9x14')
/
insert into input (message) values ('#376 @ 322,646: 12x29')
/
insert into input (message) values ('#377 @ 285,458: 26x12')
/
insert into input (message) values ('#378 @ 636,707: 28x14')
/
insert into input (message) values ('#379 @ 484,602: 13x28')
/
insert into input (message) values ('#380 @ 458,655: 14x24')
/
insert into input (message) values ('#381 @ 547,203: 25x25')
/
insert into input (message) values ('#382 @ 359,567: 18x26')
/
insert into input (message) values ('#383 @ 880,312: 25x22')
/
insert into input (message) values ('#384 @ 489,330: 19x20')
/
insert into input (message) values ('#385 @ 760,708: 16x20')
/
insert into input (message) values ('#386 @ 506,58: 13x26')
/
insert into input (message) values ('#387 @ 705,231: 12x22')
/
insert into input (message) values ('#388 @ 274,351: 13x17')
/
insert into input (message) values ('#389 @ 868,519: 13x29')
/
insert into input (message) values ('#390 @ 806,655: 29x28')
/
insert into input (message) values ('#391 @ 858,726: 16x10')
/
insert into input (message) values ('#392 @ 427,438: 26x27')
/
insert into input (message) values ('#393 @ 542,806: 11x13')
/
insert into input (message) values ('#394 @ 119,663: 20x24')
/
insert into input (message) values ('#395 @ 158,690: 17x27')
/
insert into input (message) values ('#396 @ 961,199: 10x23')
/
insert into input (message) values ('#397 @ 524,303: 19x14')
/
insert into input (message) values ('#398 @ 913,246: 11x29')
/
insert into input (message) values ('#399 @ 696,311: 27x14')
/
insert into input (message) values ('#400 @ 921,639: 20x27')
/
insert into input (message) values ('#401 @ 526,70: 13x18')
/
insert into input (message) values ('#402 @ 824,446: 12x25')
/
insert into input (message) values ('#403 @ 201,97: 6x9')
/
insert into input (message) values ('#404 @ 516,176: 22x22')
/
insert into input (message) values ('#405 @ 652,267: 17x20')
/
insert into input (message) values ('#406 @ 396,249: 16x29')
/
insert into input (message) values ('#407 @ 962,641: 11x24')
/
insert into input (message) values ('#408 @ 430,15: 26x21')
/
insert into input (message) values ('#409 @ 480,35: 23x24')
/
insert into input (message) values ('#410 @ 430,610: 21x15')
/
insert into input (message) values ('#411 @ 975,935: 11x16')
/
insert into input (message) values ('#412 @ 705,395: 14x26')
/
insert into input (message) values ('#413 @ 338,792: 15x25')
/
insert into input (message) values ('#414 @ 801,345: 6x20')
/
insert into input (message) values ('#415 @ 29,601: 13x12')
/
insert into input (message) values ('#416 @ 772,388: 23x14')
/
insert into input (message) values ('#417 @ 507,307: 15x21')
/
insert into input (message) values ('#418 @ 55,683: 20x15')
/
insert into input (message) values ('#419 @ 837,593: 27x12')
/
insert into input (message) values ('#420 @ 10,402: 17x14')
/
insert into input (message) values ('#421 @ 273,260: 10x17')
/
insert into input (message) values ('#422 @ 21,938: 10x18')
/
insert into input (message) values ('#423 @ 951,768: 10x20')
/
insert into input (message) values ('#424 @ 681,830: 13x26')
/
insert into input (message) values ('#425 @ 558,417: 24x17')
/
insert into input (message) values ('#426 @ 766,391: 23x24')
/
insert into input (message) values ('#427 @ 693,410: 18x13')
/
insert into input (message) values ('#428 @ 67,733: 14x16')
/
insert into input (message) values ('#429 @ 950,373: 23x11')
/
insert into input (message) values ('#430 @ 470,504: 15x15')
/
insert into input (message) values ('#431 @ 851,800: 15x16')
/
insert into input (message) values ('#432 @ 596,267: 11x25')
/
insert into input (message) values ('#433 @ 794,511: 15x17')
/
insert into input (message) values ('#434 @ 134,532: 11x22')
/
insert into input (message) values ('#435 @ 360,489: 13x19')
/
insert into input (message) values ('#436 @ 757,314: 22x13')
/
insert into input (message) values ('#437 @ 456,493: 12x20')
/
insert into input (message) values ('#438 @ 414,721: 10x11')
/
insert into input (message) values ('#439 @ 392,851: 23x16')
/
insert into input (message) values ('#440 @ 744,161: 19x18')
/
insert into input (message) values ('#441 @ 411,130: 22x17')
/
insert into input (message) values ('#442 @ 385,481: 16x24')
/
insert into input (message) values ('#443 @ 187,644: 5x8')
/
insert into input (message) values ('#444 @ 459,548: 19x12')
/
insert into input (message) values ('#445 @ 919,442: 17x18')
/
insert into input (message) values ('#446 @ 967,734: 27x23')
/
insert into input (message) values ('#447 @ 159,80: 25x20')
/
insert into input (message) values ('#448 @ 20,160: 19x16')
/
insert into input (message) values ('#449 @ 4,829: 20x11')
/
insert into input (message) values ('#450 @ 116,627: 14x12')
/
insert into input (message) values ('#451 @ 262,813: 14x29')
/
insert into input (message) values ('#452 @ 259,66: 28x22')
/
insert into input (message) values ('#453 @ 730,170: 19x23')
/
insert into input (message) values ('#454 @ 53,257: 15x21')
/
insert into input (message) values ('#455 @ 688,326: 17x20')
/
insert into input (message) values ('#456 @ 865,201: 16x20')
/
insert into input (message) values ('#457 @ 311,137: 13x29')
/
insert into input (message) values ('#458 @ 516,776: 22x23')
/
insert into input (message) values ('#459 @ 681,309: 23x18')
/
insert into input (message) values ('#460 @ 694,252: 18x15')
/
insert into input (message) values ('#461 @ 647,701: 18x14')
/
insert into input (message) values ('#462 @ 29,977: 13x11')
/
insert into input (message) values ('#463 @ 881,876: 25x29')
/
insert into input (message) values ('#464 @ 942,533: 23x24')
/
insert into input (message) values ('#465 @ 679,147: 17x22')
/
insert into input (message) values ('#466 @ 331,706: 21x11')
/
insert into input (message) values ('#467 @ 89,806: 17x27')
/
insert into input (message) values ('#468 @ 878,650: 21x15')
/
insert into input (message) values ('#469 @ 980,22: 10x17')
/
insert into input (message) values ('#470 @ 955,872: 16x19')
/
insert into input (message) values ('#471 @ 177,973: 21x26')
/
insert into input (message) values ('#472 @ 0,497: 18x10')
/
insert into input (message) values ('#473 @ 598,15: 14x22')
/
insert into input (message) values ('#474 @ 703,256: 26x14')
/
insert into input (message) values ('#475 @ 609,906: 16x27')
/
insert into input (message) values ('#476 @ 212,756: 17x27')
/
insert into input (message) values ('#477 @ 143,322: 16x17')
/
insert into input (message) values ('#478 @ 114,108: 11x11')
/
insert into input (message) values ('#479 @ 214,140: 24x18')
/
insert into input (message) values ('#480 @ 275,3: 21x25')
/
insert into input (message) values ('#481 @ 744,128: 10x10')
/
insert into input (message) values ('#482 @ 537,146: 17x14')
/
insert into input (message) values ('#483 @ 270,59: 20x25')
/
insert into input (message) values ('#484 @ 63,229: 13x18')
/
insert into input (message) values ('#485 @ 819,402: 22x27')
/
insert into input (message) values ('#486 @ 874,231: 21x14')
/
insert into input (message) values ('#487 @ 915,580: 28x27')
/
insert into input (message) values ('#488 @ 2,812: 15x23')
/
insert into input (message) values ('#489 @ 338,827: 23x16')
/
insert into input (message) values ('#490 @ 370,987: 28x11')
/
insert into input (message) values ('#491 @ 704,147: 28x12')
/
insert into input (message) values ('#492 @ 484,246: 16x19')
/
insert into input (message) values ('#493 @ 428,67: 14x14')
/
insert into input (message) values ('#494 @ 324,901: 12x26')
/
insert into input (message) values ('#495 @ 221,656: 17x27')
/
insert into input (message) values ('#496 @ 354,758: 24x23')
/
insert into input (message) values ('#497 @ 80,613: 25x23')
/
insert into input (message) values ('#498 @ 810,907: 26x12')
/
insert into input (message) values ('#499 @ 821,846: 14x20')
/
insert into input (message) values ('#500 @ 694,344: 10x22')
/
insert into input (message) values ('#501 @ 605,588: 12x26')
/
insert into input (message) values ('#502 @ 804,117: 15x11')
/
insert into input (message) values ('#503 @ 973,256: 15x25')
/
insert into input (message) values ('#504 @ 862,370: 20x23')
/
insert into input (message) values ('#505 @ 337,823: 19x26')
/
insert into input (message) values ('#506 @ 560,584: 29x21')
/
insert into input (message) values ('#507 @ 108,276: 20x18')
/
insert into input (message) values ('#508 @ 46,200: 21x16')
/
insert into input (message) values ('#509 @ 452,412: 29x24')
/
insert into input (message) values ('#510 @ 438,254: 18x21')
/
insert into input (message) values ('#511 @ 686,292: 19x24')
/
insert into input (message) values ('#512 @ 122,382: 29x22')
/
insert into input (message) values ('#513 @ 95,936: 16x23')
/
insert into input (message) values ('#514 @ 783,471: 11x25')
/
insert into input (message) values ('#515 @ 74,183: 22x28')
/
insert into input (message) values ('#516 @ 24,949: 13x10')
/
insert into input (message) values ('#517 @ 49,819: 22x11')
/
insert into input (message) values ('#518 @ 863,973: 22x22')
/
insert into input (message) values ('#519 @ 792,700: 10x26')
/
insert into input (message) values ('#520 @ 243,974: 21x21')
/
insert into input (message) values ('#521 @ 283,63: 19x15')
/
insert into input (message) values ('#522 @ 284,0: 13x22')
/
insert into input (message) values ('#523 @ 975,5: 13x26')
/
insert into input (message) values ('#524 @ 347,326: 17x15')
/
insert into input (message) values ('#525 @ 754,292: 25x29')
/
insert into input (message) values ('#526 @ 352,279: 23x17')
/
insert into input (message) values ('#527 @ 125,705: 17x20')
/
insert into input (message) values ('#528 @ 497,375: 22x17')
/
insert into input (message) values ('#529 @ 302,497: 16x28')
/
insert into input (message) values ('#530 @ 251,302: 11x11')
/
insert into input (message) values ('#531 @ 400,181: 27x23')
/
insert into input (message) values ('#532 @ 373,463: 14x17')
/
insert into input (message) values ('#533 @ 12,707: 22x10')
/
insert into input (message) values ('#534 @ 605,764: 14x21')
/
insert into input (message) values ('#535 @ 99,280: 16x17')
/
insert into input (message) values ('#536 @ 620,616: 23x12')
/
insert into input (message) values ('#537 @ 732,485: 17x10')
/
insert into input (message) values ('#538 @ 166,905: 29x19')
/
insert into input (message) values ('#539 @ 61,484: 24x22')
/
insert into input (message) values ('#540 @ 656,444: 20x17')
/
insert into input (message) values ('#541 @ 531,954: 11x21')
/
insert into input (message) values ('#542 @ 214,181: 15x20')
/
insert into input (message) values ('#543 @ 877,200: 26x12')
/
insert into input (message) values ('#544 @ 539,756: 17x16')
/
insert into input (message) values ('#545 @ 819,677: 18x25')
/
insert into input (message) values ('#546 @ 909,638: 18x11')
/
insert into input (message) values ('#547 @ 957,692: 25x27')
/
insert into input (message) values ('#548 @ 780,614: 22x15')
/
insert into input (message) values ('#549 @ 320,125: 7x4')
/
insert into input (message) values ('#550 @ 748,277: 10x19')
/
insert into input (message) values ('#551 @ 332,19: 23x22')
/
insert into input (message) values ('#552 @ 218,135: 26x27')
/
insert into input (message) values ('#553 @ 404,762: 27x22')
/
insert into input (message) values ('#554 @ 36,451: 24x12')
/
insert into input (message) values ('#555 @ 410,641: 13x26')
/
insert into input (message) values ('#556 @ 457,415: 12x10')
/
insert into input (message) values ('#557 @ 668,549: 15x16')
/
insert into input (message) values ('#558 @ 450,246: 15x24')
/
insert into input (message) values ('#559 @ 945,823: 24x10')
/
insert into input (message) values ('#560 @ 675,40: 21x25')
/
insert into input (message) values ('#561 @ 313,926: 26x11')
/
insert into input (message) values ('#562 @ 393,953: 15x27')
/
insert into input (message) values ('#563 @ 394,280: 29x15')
/
insert into input (message) values ('#564 @ 224,304: 20x23')
/
insert into input (message) values ('#565 @ 571,481: 23x22')
/
insert into input (message) values ('#566 @ 419,301: 18x29')
/
insert into input (message) values ('#567 @ 316,244: 11x22')
/
insert into input (message) values ('#568 @ 6,388: 14x27')
/
insert into input (message) values ('#569 @ 349,201: 12x10')
/
insert into input (message) values ('#570 @ 681,42: 18x12')
/
insert into input (message) values ('#571 @ 721,54: 15x27')
/
insert into input (message) values ('#572 @ 40,193: 6x9')
/
insert into input (message) values ('#573 @ 3,626: 10x20')
/
insert into input (message) values ('#574 @ 57,575: 24x19')
/
insert into input (message) values ('#575 @ 423,75: 20x21')
/
insert into input (message) values ('#576 @ 539,311: 27x20')
/
insert into input (message) values ('#577 @ 853,638: 16x22')
/
insert into input (message) values ('#578 @ 227,544: 29x20')
/
insert into input (message) values ('#579 @ 401,74: 11x22')
/
insert into input (message) values ('#580 @ 672,452: 12x20')
/
insert into input (message) values ('#581 @ 873,826: 26x11')
/
insert into input (message) values ('#582 @ 175,227: 17x17')
/
insert into input (message) values ('#583 @ 291,10: 27x28')
/
insert into input (message) values ('#584 @ 141,316: 20x27')
/
insert into input (message) values ('#585 @ 269,19: 13x18')
/
insert into input (message) values ('#586 @ 53,622: 17x24')
/
insert into input (message) values ('#587 @ 569,50: 18x20')
/
insert into input (message) values ('#588 @ 786,698: 22x13')
/
insert into input (message) values ('#589 @ 973,584: 21x16')
/
insert into input (message) values ('#590 @ 743,251: 24x11')
/
insert into input (message) values ('#591 @ 300,558: 21x21')
/
insert into input (message) values ('#592 @ 679,682: 20x10')
/
insert into input (message) values ('#593 @ 883,201: 23x22')
/
insert into input (message) values ('#594 @ 734,381: 25x18')
/
insert into input (message) values ('#595 @ 51,864: 24x12')
/
insert into input (message) values ('#596 @ 831,380: 23x11')
/
insert into input (message) values ('#597 @ 113,602: 15x20')
/
insert into input (message) values ('#598 @ 718,435: 21x24')
/
insert into input (message) values ('#599 @ 839,78: 21x19')
/
insert into input (message) values ('#600 @ 820,838: 29x15')
/
insert into input (message) values ('#601 @ 93,304: 22x21')
/
insert into input (message) values ('#602 @ 959,640: 12x23')
/
insert into input (message) values ('#603 @ 653,442: 24x18')
/
insert into input (message) values ('#604 @ 401,184: 10x28')
/
insert into input (message) values ('#605 @ 790,878: 17x11')
/
insert into input (message) values ('#606 @ 616,181: 13x13')
/
insert into input (message) values ('#607 @ 260,258: 28x16')
/
insert into input (message) values ('#608 @ 126,792: 25x28')
/
insert into input (message) values ('#609 @ 120,802: 13x27')
/
insert into input (message) values ('#610 @ 804,187: 13x14')
/
insert into input (message) values ('#611 @ 559,739: 23x28')
/
insert into input (message) values ('#612 @ 815,757: 18x11')
/
insert into input (message) values ('#613 @ 502,569: 17x16')
/
insert into input (message) values ('#614 @ 807,390: 29x26')
/
insert into input (message) values ('#615 @ 626,191: 10x25')
/
insert into input (message) values ('#616 @ 66,493: 17x18')
/
insert into input (message) values ('#617 @ 925,488: 16x10')
/
insert into input (message) values ('#618 @ 910,645: 17x17')
/
insert into input (message) values ('#619 @ 185,423: 22x16')
/
insert into input (message) values ('#620 @ 796,109: 15x14')
/
insert into input (message) values ('#621 @ 801,975: 29x23')
/
insert into input (message) values ('#622 @ 266,25: 21x29')
/
insert into input (message) values ('#623 @ 821,185: 25x25')
/
insert into input (message) values ('#624 @ 257,832: 15x19')
/
insert into input (message) values ('#625 @ 219,108: 18x21')
/
insert into input (message) values ('#626 @ 119,40: 21x29')
/
insert into input (message) values ('#627 @ 451,891: 28x27')
/
insert into input (message) values ('#628 @ 810,432: 10x16')
/
insert into input (message) values ('#629 @ 5,382: 26x11')
/
insert into input (message) values ('#630 @ 494,538: 21x22')
/
insert into input (message) values ('#631 @ 620,203: 14x23')
/
insert into input (message) values ('#632 @ 250,289: 29x26')
/
insert into input (message) values ('#633 @ 761,967: 14x19')
/
insert into input (message) values ('#634 @ 928,661: 18x12')
/
insert into input (message) values ('#635 @ 547,170: 29x17')
/
insert into input (message) values ('#636 @ 244,597: 18x12')
/
insert into input (message) values ('#637 @ 415,394: 20x14')
/
insert into input (message) values ('#638 @ 320,108: 28x21')
/
insert into input (message) values ('#639 @ 337,696: 19x16')
/
insert into input (message) values ('#640 @ 36,831: 20x27')
/
insert into input (message) values ('#641 @ 891,514: 13x10')
/
insert into input (message) values ('#642 @ 862,592: 14x25')
/
insert into input (message) values ('#643 @ 101,176: 11x27')
/
insert into input (message) values ('#644 @ 287,693: 21x27')
/
insert into input (message) values ('#645 @ 251,368: 19x25')
/
insert into input (message) values ('#646 @ 268,307: 10x24')
/
insert into input (message) values ('#647 @ 902,1: 20x11')
/
insert into input (message) values ('#648 @ 166,902: 16x23')
/
insert into input (message) values ('#649 @ 830,451: 10x24')
/
insert into input (message) values ('#650 @ 227,220: 14x26')
/
insert into input (message) values ('#651 @ 205,108: 29x29')
/
insert into input (message) values ('#652 @ 406,375: 28x22')
/
insert into input (message) values ('#653 @ 824,270: 24x22')
/
insert into input (message) values ('#654 @ 197,332: 10x11')
/
insert into input (message) values ('#655 @ 576,730: 15x15')
/
insert into input (message) values ('#656 @ 617,770: 16x26')
/
insert into input (message) values ('#657 @ 246,728: 24x19')
/
insert into input (message) values ('#658 @ 965,848: 15x25')
/
insert into input (message) values ('#659 @ 809,933: 11x20')
/
insert into input (message) values ('#660 @ 198,897: 27x17')
/
insert into input (message) values ('#661 @ 186,489: 20x29')
/
insert into input (message) values ('#662 @ 239,651: 10x18')
/
insert into input (message) values ('#663 @ 884,295: 22x22')
/
insert into input (message) values ('#664 @ 147,226: 11x20')
/
insert into input (message) values ('#665 @ 10,946: 6x14')
/
insert into input (message) values ('#666 @ 493,544: 25x12')
/
insert into input (message) values ('#667 @ 164,176: 12x11')
/
insert into input (message) values ('#668 @ 141,531: 21x18')
/
insert into input (message) values ('#669 @ 689,416: 15x12')
/
insert into input (message) values ('#670 @ 605,78: 23x22')
/
insert into input (message) values ('#671 @ 544,408: 24x23')
/
insert into input (message) values ('#672 @ 560,387: 27x28')
/
insert into input (message) values ('#673 @ 903,760: 16x19')
/
insert into input (message) values ('#674 @ 108,420: 28x13')
/
insert into input (message) values ('#675 @ 398,787: 11x10')
/
insert into input (message) values ('#676 @ 872,702: 19x27')
/
insert into input (message) values ('#677 @ 661,260: 29x20')
/
insert into input (message) values ('#678 @ 529,581: 25x15')
/
insert into input (message) values ('#679 @ 252,599: 5x6')
/
insert into input (message) values ('#680 @ 173,363: 28x22')
/
insert into input (message) values ('#681 @ 469,446: 25x29')
/
insert into input (message) values ('#682 @ 883,613: 17x10')
/
insert into input (message) values ('#683 @ 97,608: 4x12')
/
insert into input (message) values ('#684 @ 917,587: 23x13')
/
insert into input (message) values ('#685 @ 888,522: 17x23')
/
insert into input (message) values ('#686 @ 663,241: 14x18')
/
insert into input (message) values ('#687 @ 93,272: 18x11')
/
insert into input (message) values ('#688 @ 174,357: 15x19')
/
insert into input (message) values ('#689 @ 247,984: 15x14')
/
insert into input (message) values ('#690 @ 817,846: 28x13')
/
insert into input (message) values ('#691 @ 587,292: 12x10')
/
insert into input (message) values ('#692 @ 350,130: 18x25')
/
insert into input (message) values ('#693 @ 330,175: 17x23')
/
insert into input (message) values ('#694 @ 126,828: 14x23')
/
insert into input (message) values ('#695 @ 606,481: 28x11')
/
insert into input (message) values ('#696 @ 887,781: 15x29')
/
insert into input (message) values ('#697 @ 121,569: 29x17')
/
insert into input (message) values ('#698 @ 167,524: 16x25')
/
insert into input (message) values ('#699 @ 663,71: 25x16')
/
insert into input (message) values ('#700 @ 372,281: 28x29')
/
insert into input (message) values ('#701 @ 103,191: 26x14')
/
insert into input (message) values ('#702 @ 864,373: 18x29')
/
insert into input (message) values ('#703 @ 91,267: 17x17')
/
insert into input (message) values ('#704 @ 195,408: 16x19')
/
insert into input (message) values ('#705 @ 107,953: 27x22')
/
insert into input (message) values ('#706 @ 173,917: 22x15')
/
insert into input (message) values ('#707 @ 13,968: 25x14')
/
insert into input (message) values ('#708 @ 356,599: 20x24')
/
insert into input (message) values ('#709 @ 813,426: 18x15')
/
insert into input (message) values ('#710 @ 793,700: 10x11')
/
insert into input (message) values ('#711 @ 766,834: 13x27')
/
insert into input (message) values ('#712 @ 58,150: 10x27')
/
insert into input (message) values ('#713 @ 54,233: 27x29')
/
insert into input (message) values ('#714 @ 130,62: 14x23')
/
insert into input (message) values ('#715 @ 885,122: 11x25')
/
insert into input (message) values ('#716 @ 543,280: 11x29')
/
insert into input (message) values ('#717 @ 650,896: 20x20')
/
insert into input (message) values ('#718 @ 228,601: 18x13')
/
insert into input (message) values ('#719 @ 726,311: 18x13')
/
insert into input (message) values ('#720 @ 352,898: 19x29')
/
insert into input (message) values ('#721 @ 55,173: 27x27')
/
insert into input (message) values ('#722 @ 731,530: 16x10')
/
insert into input (message) values ('#723 @ 662,20: 26x26')
/
insert into input (message) values ('#724 @ 951,648: 22x13')
/
insert into input (message) values ('#725 @ 353,484: 22x27')
/
insert into input (message) values ('#726 @ 802,863: 22x23')
/
insert into input (message) values ('#727 @ 984,237: 12x24')
/
insert into input (message) values ('#728 @ 541,739: 22x17')
/
insert into input (message) values ('#729 @ 588,274: 14x29')
/
insert into input (message) values ('#730 @ 854,718: 13x11')
/
insert into input (message) values ('#731 @ 562,777: 17x26')
/
insert into input (message) values ('#732 @ 789,493: 15x18')
/
insert into input (message) values ('#733 @ 475,50: 27x25')
/
insert into input (message) values ('#734 @ 794,394: 11x8')
/
insert into input (message) values ('#735 @ 179,735: 27x12')
/
insert into input (message) values ('#736 @ 27,375: 22x24')
/
insert into input (message) values ('#737 @ 403,868: 29x27')
/
insert into input (message) values ('#738 @ 5,166: 19x17')
/
insert into input (message) values ('#739 @ 572,979: 15x19')
/
insert into input (message) values ('#740 @ 400,707: 15x29')
/
insert into input (message) values ('#741 @ 968,916: 14x24')
/
insert into input (message) values ('#742 @ 404,184: 11x19')
/
insert into input (message) values ('#743 @ 42,953: 25x11')
/
insert into input (message) values ('#744 @ 955,779: 26x14')
/
insert into input (message) values ('#745 @ 940,339: 19x27')
/
insert into input (message) values ('#746 @ 868,644: 12x10')
/
insert into input (message) values ('#747 @ 808,756: 11x11')
/
insert into input (message) values ('#748 @ 462,20: 26x20')
/
insert into input (message) values ('#749 @ 112,453: 12x27')
/
insert into input (message) values ('#750 @ 839,883: 28x15')
/
insert into input (message) values ('#751 @ 179,310: 21x28')
/
insert into input (message) values ('#752 @ 809,425: 15x22')
/
insert into input (message) values ('#753 @ 25,174: 17x20')
/
insert into input (message) values ('#754 @ 248,915: 25x19')
/
insert into input (message) values ('#755 @ 881,647: 17x29')
/
insert into input (message) values ('#756 @ 339,312: 25x16')
/
insert into input (message) values ('#757 @ 905,929: 11x20')
/
insert into input (message) values ('#758 @ 22,508: 23x16')
/
insert into input (message) values ('#759 @ 846,771: 20x16')
/
insert into input (message) values ('#760 @ 119,608: 22x14')
/
insert into input (message) values ('#761 @ 180,332: 13x29')
/
insert into input (message) values ('#762 @ 443,42: 14x10')
/
insert into input (message) values ('#763 @ 564,794: 10x3')
/
insert into input (message) values ('#764 @ 845,74: 14x17')
/
insert into input (message) values ('#765 @ 635,304: 27x14')
/
insert into input (message) values ('#766 @ 297,965: 18x14')
/
insert into input (message) values ('#767 @ 414,880: 28x24')
/
insert into input (message) values ('#768 @ 135,13: 12x21')
/
insert into input (message) values ('#769 @ 732,567: 22x23')
/
insert into input (message) values ('#770 @ 616,582: 23x10')
/
insert into input (message) values ('#771 @ 322,11: 21x13')
/
insert into input (message) values ('#772 @ 540,806: 21x10')
/
insert into input (message) values ('#773 @ 441,407: 15x10')
/
insert into input (message) values ('#774 @ 729,773: 17x18')
/
insert into input (message) values ('#775 @ 14,355: 14x26')
/
insert into input (message) values ('#776 @ 624,122: 19x18')
/
insert into input (message) values ('#777 @ 811,839: 10x26')
/
insert into input (message) values ('#778 @ 316,182: 22x29')
/
insert into input (message) values ('#779 @ 741,270: 12x18')
/
insert into input (message) values ('#780 @ 931,842: 16x15')
/
insert into input (message) values ('#781 @ 396,263: 16x23')
/
insert into input (message) values ('#782 @ 868,207: 16x21')
/
insert into input (message) values ('#783 @ 931,827: 10x14')
/
insert into input (message) values ('#784 @ 600,804: 12x21')
/
insert into input (message) values ('#785 @ 878,578: 12x12')
/
insert into input (message) values ('#786 @ 398,120: 21x11')
/
insert into input (message) values ('#787 @ 385,369: 23x20')
/
insert into input (message) values ('#788 @ 671,162: 11x16')
/
insert into input (message) values ('#789 @ 956,130: 15x29')
/
insert into input (message) values ('#790 @ 564,735: 20x24')
/
insert into input (message) values ('#791 @ 721,811: 12x21')
/
insert into input (message) values ('#792 @ 895,187: 18x17')
/
insert into input (message) values ('#793 @ 657,276: 21x13')
/
insert into input (message) values ('#794 @ 1,444: 23x21')
/
insert into input (message) values ('#795 @ 641,70: 13x17')
/
insert into input (message) values ('#796 @ 33,979: 29x17')
/
insert into input (message) values ('#797 @ 827,456: 17x16')
/
insert into input (message) values ('#798 @ 809,906: 26x25')
/
insert into input (message) values ('#799 @ 179,100: 20x11')
/
insert into input (message) values ('#800 @ 164,909: 19x13')
/
insert into input (message) values ('#801 @ 302,579: 24x14')
/
insert into input (message) values ('#802 @ 963,692: 18x23')
/
insert into input (message) values ('#803 @ 557,477: 24x26')
/
insert into input (message) values ('#804 @ 609,937: 16x24')
/
insert into input (message) values ('#805 @ 294,643: 24x12')
/
insert into input (message) values ('#806 @ 707,295: 25x12')
/
insert into input (message) values ('#807 @ 143,88: 28x26')
/
insert into input (message) values ('#808 @ 120,701: 10x29')
/
insert into input (message) values ('#809 @ 405,868: 22x13')
/
insert into input (message) values ('#810 @ 438,706: 20x10')
/
insert into input (message) values ('#811 @ 174,736: 18x10')
/
insert into input (message) values ('#812 @ 553,652: 18x16')
/
insert into input (message) values ('#813 @ 876,53: 26x22')
/
insert into input (message) values ('#814 @ 756,271: 28x25')
/
insert into input (message) values ('#815 @ 684,845: 13x14')
/
insert into input (message) values ('#816 @ 29,527: 11x25')
/
insert into input (message) values ('#817 @ 266,223: 13x11')
/
insert into input (message) values ('#818 @ 48,460: 19x28')
/
insert into input (message) values ('#819 @ 383,769: 25x16')
/
insert into input (message) values ('#820 @ 807,339: 21x26')
/
insert into input (message) values ('#821 @ 857,596: 23x18')
/
insert into input (message) values ('#822 @ 270,914: 22x15')
/
insert into input (message) values ('#823 @ 378,968: 16x29')
/
insert into input (message) values ('#824 @ 179,831: 23x29')
/
insert into input (message) values ('#825 @ 49,227: 20x16')
/
insert into input (message) values ('#826 @ 879,720: 17x23')
/
insert into input (message) values ('#827 @ 564,17: 29x18')
/
insert into input (message) values ('#828 @ 754,144: 18x20')
/
insert into input (message) values ('#829 @ 206,920: 19x25')
/
insert into input (message) values ('#830 @ 455,501: 23x22')
/
insert into input (message) values ('#831 @ 779,112: 15x11')
/
insert into input (message) values ('#832 @ 40,476: 26x24')
/
insert into input (message) values ('#833 @ 771,225: 23x26')
/
insert into input (message) values ('#834 @ 358,682: 29x10')
/
insert into input (message) values ('#835 @ 8,974: 22x19')
/
insert into input (message) values ('#836 @ 18,744: 26x20')
/
insert into input (message) values ('#837 @ 196,239: 18x13')
/
insert into input (message) values ('#838 @ 895,312: 25x14')
/
insert into input (message) values ('#839 @ 285,666: 26x23')
/
insert into input (message) values ('#840 @ 14,868: 22x10')
/
insert into input (message) values ('#841 @ 695,297: 18x26')
/
insert into input (message) values ('#842 @ 949,341: 20x11')
/
insert into input (message) values ('#843 @ 440,91: 25x23')
/
insert into input (message) values ('#844 @ 887,688: 10x29')
/
insert into input (message) values ('#845 @ 320,710: 21x16')
/
insert into input (message) values ('#846 @ 598,156: 20x13')
/
insert into input (message) values ('#847 @ 860,212: 29x19')
/
insert into input (message) values ('#848 @ 728,316: 21x13')
/
insert into input (message) values ('#849 @ 404,553: 23x29')
/
insert into input (message) values ('#850 @ 556,424: 10x15')
/
insert into input (message) values ('#851 @ 532,491: 21x12')
/
insert into input (message) values ('#852 @ 735,29: 19x13')
/
insert into input (message) values ('#853 @ 890,612: 19x12')
/
insert into input (message) values ('#854 @ 338,834: 14x18')
/
insert into input (message) values ('#855 @ 762,264: 16x16')
/
insert into input (message) values ('#856 @ 961,496: 14x16')
/
insert into input (message) values ('#857 @ 616,756: 11x15')
/
insert into input (message) values ('#858 @ 40,948: 12x15')
/
insert into input (message) values ('#859 @ 492,139: 23x25')
/
insert into input (message) values ('#860 @ 866,70: 26x19')
/
insert into input (message) values ('#861 @ 909,652: 14x15')
/
insert into input (message) values ('#862 @ 68,740: 28x21')
/
insert into input (message) values ('#863 @ 765,736: 20x22')
/
insert into input (message) values ('#864 @ 687,856: 16x12')
/
insert into input (message) values ('#865 @ 187,76: 25x27')
/
insert into input (message) values ('#866 @ 104,162: 13x21')
/
insert into input (message) values ('#867 @ 377,479: 13x27')
/
insert into input (message) values ('#868 @ 768,553: 17x19')
/
insert into input (message) values ('#869 @ 914,744: 11x22')
/
insert into input (message) values ('#870 @ 338,152: 10x18')
/
insert into input (message) values ('#871 @ 665,305: 12x12')
/
insert into input (message) values ('#872 @ 401,702: 25x29')
/
insert into input (message) values ('#873 @ 78,285: 26x26')
/
insert into input (message) values ('#874 @ 931,369: 15x14')
/
insert into input (message) values ('#875 @ 710,483: 17x28')
/
insert into input (message) values ('#876 @ 228,717: 19x23')
/
insert into input (message) values ('#877 @ 819,390: 19x28')
/
insert into input (message) values ('#878 @ 797,673: 10x22')
/
insert into input (message) values ('#879 @ 528,282: 25x15')
/
insert into input (message) values ('#880 @ 887,703: 15x24')
/
insert into input (message) values ('#881 @ 591,295: 22x10')
/
insert into input (message) values ('#882 @ 892,777: 16x25')
/
insert into input (message) values ('#883 @ 893,148: 28x21')
/
insert into input (message) values ('#884 @ 477,897: 13x24')
/
insert into input (message) values ('#885 @ 932,299: 17x21')
/
insert into input (message) values ('#886 @ 190,120: 23x14')
/
insert into input (message) values ('#887 @ 203,598: 27x18')
/
insert into input (message) values ('#888 @ 803,934: 28x29')
/
insert into input (message) values ('#889 @ 126,204: 15x26')
/
insert into input (message) values ('#890 @ 388,379: 27x15')
/
insert into input (message) values ('#891 @ 159,730: 11x21')
/
insert into input (message) values ('#892 @ 185,638: 14x22')
/
insert into input (message) values ('#893 @ 943,572: 26x14')
/
insert into input (message) values ('#894 @ 585,740: 23x26')
/
insert into input (message) values ('#895 @ 258,606: 27x28')
/
insert into input (message) values ('#896 @ 329,66: 29x16')
/
insert into input (message) values ('#897 @ 613,935: 14x29')
/
insert into input (message) values ('#898 @ 947,156: 24x22')
/
insert into input (message) values ('#899 @ 753,295: 25x23')
/
insert into input (message) values ('#900 @ 763,591: 14x19')
/
insert into input (message) values ('#901 @ 662,409: 20x22')
/
insert into input (message) values ('#902 @ 627,213: 25x10')
/
insert into input (message) values ('#903 @ 324,813: 7x6')
/
insert into input (message) values ('#904 @ 275,695: 29x25')
/
insert into input (message) values ('#905 @ 362,485: 24x14')
/
insert into input (message) values ('#906 @ 965,761: 28x22')
/
insert into input (message) values ('#907 @ 982,553: 12x10')
/
insert into input (message) values ('#908 @ 335,787: 28x20')
/
insert into input (message) values ('#909 @ 349,315: 19x25')
/
insert into input (message) values ('#910 @ 226,62: 25x18')
/
insert into input (message) values ('#911 @ 173,489: 27x26')
/
insert into input (message) values ('#912 @ 213,204: 28x25')
/
insert into input (message) values ('#913 @ 57,708: 28x19')
/
insert into input (message) values ('#914 @ 623,612: 27x16')
/
insert into input (message) values ('#915 @ 579,861: 12x19')
/
insert into input (message) values ('#916 @ 895,0: 22x10')
/
insert into input (message) values ('#917 @ 14,164: 22x15')
/
insert into input (message) values ('#918 @ 68,284: 22x29')
/
insert into input (message) values ('#919 @ 61,869: 16x18')
/
insert into input (message) values ('#920 @ 318,539: 25x25')
/
insert into input (message) values ('#921 @ 803,699: 16x11')
/
insert into input (message) values ('#922 @ 620,713: 28x18')
/
insert into input (message) values ('#923 @ 123,789: 21x28')
/
insert into input (message) values ('#924 @ 171,279: 11x28')
/
insert into input (message) values ('#925 @ 725,516: 25x22')
/
insert into input (message) values ('#926 @ 202,570: 13x18')
/
insert into input (message) values ('#927 @ 575,801: 21x29')
/
insert into input (message) values ('#928 @ 715,485: 26x28')
/
insert into input (message) values ('#929 @ 348,906: 15x20')
/
insert into input (message) values ('#930 @ 683,680: 27x13')
/
insert into input (message) values ('#931 @ 375,767: 19x27')
/
insert into input (message) values ('#932 @ 347,915: 10x21')
/
insert into input (message) values ('#933 @ 723,556: 27x26')
/
insert into input (message) values ('#934 @ 737,291: 25x12')
/
insert into input (message) values ('#935 @ 850,110: 13x18')
/
insert into input (message) values ('#936 @ 132,326: 23x24')
/
insert into input (message) values ('#937 @ 523,757: 27x13')
/
insert into input (message) values ('#938 @ 747,720: 26x11')
/
insert into input (message) values ('#939 @ 12,520: 19x13')
/
insert into input (message) values ('#940 @ 868,576: 19x23')
/
insert into input (message) values ('#941 @ 552,792: 19x12')
/
insert into input (message) values ('#942 @ 18,620: 23x20')
/
insert into input (message) values ('#943 @ 689,569: 16x22')
/
insert into input (message) values ('#944 @ 518,316: 28x25')
/
insert into input (message) values ('#945 @ 364,927: 21x13')
/
insert into input (message) values ('#946 @ 119,116: 16x16')
/
insert into input (message) values ('#947 @ 374,790: 10x12')
/
insert into input (message) values ('#948 @ 53,165: 10x19')
/
insert into input (message) values ('#949 @ 361,493: 16x26')
/
insert into input (message) values ('#950 @ 400,102: 22x12')
/
insert into input (message) values ('#951 @ 544,700: 23x15')
/
insert into input (message) values ('#952 @ 461,339: 22x10')
/
insert into input (message) values ('#953 @ 307,842: 27x25')
/
insert into input (message) values ('#954 @ 299,77: 28x18')
/
insert into input (message) values ('#955 @ 208,153: 22x21')
/
insert into input (message) values ('#956 @ 206,710: 27x12')
/
insert into input (message) values ('#957 @ 554,963: 29x25')
/
insert into input (message) values ('#958 @ 160,786: 19x26')
/
insert into input (message) values ('#959 @ 982,751: 11x29')
/
insert into input (message) values ('#960 @ 121,29: 25x20')
/
insert into input (message) values ('#961 @ 341,788: 17x19')
/
insert into input (message) values ('#962 @ 288,537: 21x13')
/
insert into input (message) values ('#963 @ 878,230: 10x17')
/
insert into input (message) values ('#964 @ 704,222: 14x24')
/
insert into input (message) values ('#965 @ 904,783: 29x13')
/
insert into input (message) values ('#966 @ 270,357: 17x11')
/
insert into input (message) values ('#967 @ 760,567: 14x26')
/
insert into input (message) values ('#968 @ 368,585: 21x23')
/
insert into input (message) values ('#969 @ 754,609: 11x12')
/
insert into input (message) values ('#970 @ 16,372: 12x17')
/
insert into input (message) values ('#971 @ 822,975: 16x21')
/
insert into input (message) values ('#972 @ 738,125: 11x10')
/
insert into input (message) values ('#973 @ 158,796: 24x15')
/
insert into input (message) values ('#974 @ 151,814: 17x12')
/
insert into input (message) values ('#975 @ 250,0: 24x22')
/
insert into input (message) values ('#976 @ 430,78: 11x14')
/
insert into input (message) values ('#977 @ 422,565: 26x29')
/
insert into input (message) values ('#978 @ 328,573: 10x17')
/
insert into input (message) values ('#979 @ 733,338: 12x19')
/
insert into input (message) values ('#980 @ 759,44: 19x29')
/
insert into input (message) values ('#981 @ 170,175: 16x14')
/
insert into input (message) values ('#982 @ 766,135: 10x21')
/
insert into input (message) values ('#983 @ 389,96: 28x20')
/
insert into input (message) values ('#984 @ 972,848: 14x15')
/
insert into input (message) values ('#985 @ 950,899: 13x28')
/
insert into input (message) values ('#986 @ 391,980: 16x13')
/
insert into input (message) values ('#987 @ 878,327: 12x13')
/
insert into input (message) values ('#988 @ 812,922: 22x23')
/
insert into input (message) values ('#989 @ 349,475: 19x19')
/
insert into input (message) values ('#990 @ 498,152: 10x15')
/
insert into input (message) values ('#991 @ 12,428: 13x29')
/
insert into input (message) values ('#992 @ 801,799: 17x28')
/
insert into input (message) values ('#993 @ 564,877: 18x18')
/
insert into input (message) values ('#994 @ 553,733: 14x24')
/
insert into input (message) values ('#995 @ 520,325: 24x15')
/
insert into input (message) values ('#996 @ 73,267: 27x11')
/
insert into input (message) values ('#997 @ 45,186: 23x21')
/
insert into input (message) values ('#998 @ 514,531: 14x24')
/
insert into input (message) values ('#999 @ 732,235: 18x10')
/
insert into input (message) values ('#1000 @ 832,185: 15x22')
/
insert into input (message) values ('#1001 @ 734,942: 11x24')
/
insert into input (message) values ('#1002 @ 5,175: 10x24')
/
insert into input (message) values ('#1003 @ 554,498: 14x16')
/
insert into input (message) values ('#1004 @ 969,428: 22x19')
/
insert into input (message) values ('#1005 @ 330,227: 13x20')
/
insert into input (message) values ('#1006 @ 97,359: 28x27')
/
insert into input (message) values ('#1007 @ 907,151: 28x26')
/
insert into input (message) values ('#1008 @ 193,235: 10x16')
/
insert into input (message) values ('#1009 @ 870,309: 22x15')
/
insert into input (message) values ('#1010 @ 114,180: 11x11')
/
insert into input (message) values ('#1011 @ 713,489: 19x19')
/
insert into input (message) values ('#1012 @ 349,665: 26x11')
/
insert into input (message) values ('#1013 @ 138,723: 12x26')
/
insert into input (message) values ('#1014 @ 920,289: 11x19')
/
insert into input (message) values ('#1015 @ 331,208: 28x27')
/
insert into input (message) values ('#1016 @ 886,699: 19x12')
/
insert into input (message) values ('#1017 @ 246,961: 14x22')
/
insert into input (message) values ('#1018 @ 304,596: 12x12')
/
insert into input (message) values ('#1019 @ 436,770: 14x22')
/
insert into input (message) values ('#1020 @ 870,124: 19x13')
/
insert into input (message) values ('#1021 @ 23,946: 24x13')
/
insert into input (message) values ('#1022 @ 851,100: 13x16')
/
insert into input (message) values ('#1023 @ 262,208: 19x21')
/
insert into input (message) values ('#1024 @ 80,263: 25x19')
/
insert into input (message) values ('#1025 @ 463,540: 11x19')
/
insert into input (message) values ('#1026 @ 710,442: 29x25')
/
insert into input (message) values ('#1027 @ 635,678: 20x13')
/
insert into input (message) values ('#1028 @ 551,186: 17x21')
/
insert into input (message) values ('#1029 @ 307,910: 10x29')
/
insert into input (message) values ('#1030 @ 819,357: 11x16')
/
insert into input (message) values ('#1031 @ 630,121: 10x27')
/
insert into input (message) values ('#1032 @ 677,403: 23x27')
/
insert into input (message) values ('#1033 @ 8,944: 12x19')
/
insert into input (message) values ('#1034 @ 439,498: 21x10')
/
insert into input (message) values ('#1035 @ 122,293: 24x28')
/
insert into input (message) values ('#1036 @ 680,763: 15x11')
/
insert into input (message) values ('#1037 @ 906,655: 26x25')
/
insert into input (message) values ('#1038 @ 302,454: 23x10')
/
insert into input (message) values ('#1039 @ 596,297: 3x5')
/
insert into input (message) values ('#1040 @ 680,537: 28x10')
/
insert into input (message) values ('#1041 @ 347,730: 21x12')
/
insert into input (message) values ('#1042 @ 410,663: 17x24')
/
insert into input (message) values ('#1043 @ 898,584: 24x6')
/
insert into input (message) values ('#1044 @ 778,59: 24x10')
/
insert into input (message) values ('#1045 @ 601,91: 16x13')
/
insert into input (message) values ('#1046 @ 434,483: 24x14')
/
insert into input (message) values ('#1047 @ 771,963: 17x10')
/
insert into input (message) values ('#1048 @ 279,205: 26x11')
/
insert into input (message) values ('#1049 @ 591,775: 3x12')
/
insert into input (message) values ('#1050 @ 799,762: 23x27')
/
insert into input (message) values ('#1051 @ 760,653: 17x15')
/
insert into input (message) values ('#1052 @ 374,828: 24x26')
/
insert into input (message) values ('#1053 @ 52,606: 28x27')
/
insert into input (message) values ('#1054 @ 41,247: 27x26')
/
insert into input (message) values ('#1055 @ 551,483: 19x15')
/
insert into input (message) values ('#1056 @ 565,19: 14x11')
/
insert into input (message) values ('#1057 @ 207,676: 11x10')
/
insert into input (message) values ('#1058 @ 533,85: 11x24')
/
insert into input (message) values ('#1059 @ 490,532: 17x21')
/
insert into input (message) values ('#1060 @ 721,473: 12x17')
/
insert into input (message) values ('#1061 @ 757,587: 15x10')
/
insert into input (message) values ('#1062 @ 465,729: 23x18')
/
insert into input (message) values ('#1063 @ 279,698: 17x3')
/
insert into input (message) values ('#1064 @ 217,169: 14x18')
/
insert into input (message) values ('#1065 @ 183,680: 23x14')
/
insert into input (message) values ('#1066 @ 362,483: 26x19')
/
insert into input (message) values ('#1067 @ 680,762: 17x22')
/
insert into input (message) values ('#1068 @ 622,688: 23x20')
/
insert into input (message) values ('#1069 @ 899,760: 15x26')
/
insert into input (message) values ('#1070 @ 836,618: 11x28')
/
insert into input (message) values ('#1071 @ 241,175: 13x14')
/
insert into input (message) values ('#1072 @ 225,191: 16x19')
/
insert into input (message) values ('#1073 @ 488,131: 12x28')
/
insert into input (message) values ('#1074 @ 138,568: 17x27')
/
insert into input (message) values ('#1075 @ 42,943: 22x25')
/
insert into input (message) values ('#1076 @ 868,603: 24x27')
/
insert into input (message) values ('#1077 @ 519,409: 28x28')
/
insert into input (message) values ('#1078 @ 46,667: 16x20')
/
insert into input (message) values ('#1079 @ 616,483: 13x5')
/
insert into input (message) values ('#1080 @ 288,435: 20x22')
/
insert into input (message) values ('#1081 @ 324,197: 27x22')
/
insert into input (message) values ('#1082 @ 408,792: 12x18')
/
insert into input (message) values ('#1083 @ 69,618: 20x23')
/
insert into input (message) values ('#1084 @ 535,750: 28x12')
/
insert into input (message) values ('#1085 @ 679,457: 16x14')
/
insert into input (message) values ('#1086 @ 920,741: 24x15')
/
insert into input (message) values ('#1087 @ 244,626: 18x10')
/
insert into input (message) values ('#1088 @ 569,884: 11x15')
/
insert into input (message) values ('#1089 @ 919,294: 15x17')
/
insert into input (message) values ('#1090 @ 603,442: 22x16')
/
insert into input (message) values ('#1091 @ 417,468: 24x28')
/
insert into input (message) values ('#1092 @ 557,661: 10x14')
/
insert into input (message) values ('#1093 @ 403,853: 13x22')
/
insert into input (message) values ('#1094 @ 50,972: 12x16')
/
insert into input (message) values ('#1095 @ 371,794: 14x12')
/
insert into input (message) values ('#1096 @ 568,621: 24x14')
/
insert into input (message) values ('#1097 @ 152,786: 14x29')
/
insert into input (message) values ('#1098 @ 204,880: 14x22')
/
insert into input (message) values ('#1099 @ 819,848: 14x7')
/
insert into input (message) values ('#1100 @ 307,782: 22x21')
/
insert into input (message) values ('#1101 @ 289,708: 24x14')
/
insert into input (message) values ('#1102 @ 147,818: 24x21')
/
insert into input (message) values ('#1103 @ 715,545: 26x19')
/
insert into input (message) values ('#1104 @ 281,431: 17x11')
/
insert into input (message) values ('#1105 @ 806,367: 19x12')
/
insert into input (message) values ('#1106 @ 766,745: 18x23')
/
insert into input (message) values ('#1107 @ 766,815: 27x14')
/
insert into input (message) values ('#1108 @ 838,753: 22x28')
/
insert into input (message) values ('#1109 @ 144,293: 29x29')
/
insert into input (message) values ('#1110 @ 756,357: 14x25')
/
insert into input (message) values ('#1111 @ 876,605: 12x9')
/
insert into input (message) values ('#1112 @ 764,243: 12x26')
/
insert into input (message) values ('#1113 @ 82,758: 18x19')
/
insert into input (message) values ('#1114 @ 153,58: 11x20')
/
insert into input (message) values ('#1115 @ 403,91: 12x24')
/
insert into input (message) values ('#1116 @ 643,357: 27x28')
/
insert into input (message) values ('#1117 @ 198,352: 22x29')
/
insert into input (message) values ('#1118 @ 861,870: 16x15')
/
insert into input (message) values ('#1119 @ 110,281: 20x22')
/
insert into input (message) values ('#1120 @ 373,480: 13x16')
/
insert into input (message) values ('#1121 @ 778,270: 23x20')
/
insert into input (message) values ('#1122 @ 567,300: 23x19')
/
insert into input (message) values ('#1123 @ 349,668: 27x24')
/
insert into input (message) values ('#1124 @ 720,496: 29x28')
/
insert into input (message) values ('#1125 @ 330,119: 22x14')
/
insert into input (message) values ('#1126 @ 763,388: 22x16')
/
insert into input (message) values ('#1127 @ 833,592: 10x21')
/
insert into input (message) values ('#1128 @ 871,716: 26x27')
/
insert into input (message) values ('#1129 @ 611,144: 10x15')
/
insert into input (message) values ('#1130 @ 279,716: 17x12')
/
insert into input (message) values ('#1131 @ 824,588: 12x25')
/
insert into input (message) values ('#1132 @ 703,132: 21x11')
/
insert into input (message) values ('#1133 @ 569,402: 22x26')
/
insert into input (message) values ('#1134 @ 874,908: 28x24')
/
insert into input (message) values ('#1135 @ 33,776: 28x22')
/
insert into input (message) values ('#1136 @ 876,902: 14x19')
/
insert into input (message) values ('#1137 @ 84,712: 19x24')
/
insert into input (message) values ('#1138 @ 171,284: 10x14')
/
insert into input (message) values ('#1139 @ 659,865: 23x20')
/
insert into input (message) values ('#1140 @ 133,924: 26x17')
/
insert into input (message) values ('#1141 @ 261,41: 19x13')
/
insert into input (message) values ('#1142 @ 32,351: 11x17')
/
insert into input (message) values ('#1143 @ 777,337: 22x23')
/
insert into input (message) values ('#1144 @ 469,342: 16x12')
/
insert into input (message) values ('#1145 @ 258,611: 18x26')
/
insert into input (message) values ('#1146 @ 13,405: 10x10')
/
insert into input (message) values ('#1147 @ 185,653: 24x14')
/
insert into input (message) values ('#1148 @ 349,289: 29x28')
/
insert into input (message) values ('#1149 @ 771,653: 18x22')
/
insert into input (message) values ('#1150 @ 432,612: 11x12')
/
insert into input (message) values ('#1151 @ 421,525: 19x29')
/
insert into input (message) values ('#1152 @ 39,601: 12x20')
/
insert into input (message) values ('#1153 @ 315,230: 12x21')
/
insert into input (message) values ('#1154 @ 574,739: 11x10')
/
insert into input (message) values ('#1155 @ 335,779: 20x13')
/
insert into input (message) values ('#1156 @ 874,949: 19x28')
/
insert into input (message) values ('#1157 @ 0,823: 16x15')
/
insert into input (message) values ('#1158 @ 6,698: 29x18')
/
insert into input (message) values ('#1159 @ 351,514: 18x24')
/
insert into input (message) values ('#1160 @ 669,551: 21x14')
/
insert into input (message) values ('#1161 @ 893,799: 25x12')
/
insert into input (message) values ('#1162 @ 51,116: 11x20')
/
insert into input (message) values ('#1163 @ 34,184: 26x22')
/
insert into input (message) values ('#1164 @ 669,868: 12x11')
/
insert into input (message) values ('#1165 @ 10,601: 3x9')
/
insert into input (message) values ('#1166 @ 460,805: 20x13')
/
insert into input (message) values ('#1167 @ 357,750: 21x20')
/
insert into input (message) values ('#1168 @ 70,600: 10x16')
/
insert into input (message) values ('#1169 @ 397,188: 20x22')
/
insert into input (message) values ('#1170 @ 604,163: 14x27')
/
insert into input (message) values ('#1171 @ 696,299: 10x21')
/
insert into input (message) values ('#1172 @ 839,455: 29x28')
/
insert into input (message) values ('#1173 @ 808,781: 14x26')
/
insert into input (message) values ('#1174 @ 565,803: 10x10')
/
insert into input (message) values ('#1175 @ 131,153: 24x19')
/
insert into input (message) values ('#1176 @ 347,824: 22x26')
/
insert into input (message) values ('#1177 @ 583,296: 15x15')
/
insert into input (message) values ('#1178 @ 777,951: 15x24')
/
insert into input (message) values ('#1179 @ 211,122: 23x17')
/
insert into input (message) values ('#1180 @ 804,576: 23x14')
/
insert into input (message) values ('#1181 @ 401,766: 28x28')
/
insert into input (message) values ('#1182 @ 816,817: 17x28')
/
insert into input (message) values ('#1183 @ 499,278: 11x19')
/
insert into input (message) values ('#1184 @ 320,515: 18x26')
/
insert into input (message) values ('#1185 @ 458,711: 26x20')
/
insert into input (message) values ('#1186 @ 496,612: 15x28')
/
insert into input (message) values ('#1187 @ 783,795: 10x21')
/
insert into input (message) values ('#1188 @ 837,681: 14x26')
/
insert into input (message) values ('#1189 @ 310,762: 17x25')
/
insert into input (message) values ('#1190 @ 551,197: 12x10')
/
insert into input (message) values ('#1191 @ 889,898: 22x13')
/
insert into input (message) values ('#1192 @ 750,733: 29x19')
/
insert into input (message) values ('#1193 @ 596,925: 18x10')
/
insert into input (message) values ('#1194 @ 194,684: 6x4')
/
insert into input (message) values ('#1195 @ 202,344: 26x21')
/
insert into input (message) values ('#1196 @ 99,809: 15x10')
/
insert into input (message) values ('#1197 @ 606,256: 18x21')
/
insert into input (message) values ('#1198 @ 307,190: 25x15')
/
insert into input (message) values ('#1199 @ 442,34: 29x16')
/
insert into input (message) values ('#1200 @ 391,380: 14x20')
/
insert into input (message) values ('#1201 @ 853,158: 17x23')
/
insert into input (message) values ('#1202 @ 209,658: 22x24')
/
insert into input (message) values ('#1203 @ 182,691: 4x4')
/
insert into input (message) values ('#1204 @ 675,816: 18x26')
/
insert into input (message) values ('#1205 @ 923,379: 27x28')
/
insert into input (message) values ('#1206 @ 54,315: 9x8')
/
insert into input (message) values ('#1207 @ 230,892: 24x17')
/
insert into input (message) values ('#1208 @ 819,479: 24x24')
/
insert into input (message) values ('#1209 @ 516,580: 26x22')
/
insert into input (message) values ('#1210 @ 270,218: 25x17')
/
insert into input (message) values ('#1211 @ 981,377: 15x20')
/
insert into input (message) values ('#1212 @ 788,499: 11x10')
/
insert into input (message) values ('#1213 @ 623,343: 28x18')
/
insert into input (message) values ('#1214 @ 594,489: 23x15')
/
insert into input (message) values ('#1215 @ 434,678: 17x10')
/
insert into input (message) values ('#1216 @ 555,522: 13x16')
/
insert into input (message) values ('#1217 @ 707,534: 29x22')
/
insert into input (message) values ('#1218 @ 381,465: 22x20')
/
insert into input (message) values ('#1219 @ 93,173: 19x28')
/
insert into input (message) values ('#1220 @ 213,513: 17x11')
/
insert into input (message) values ('#1221 @ 234,61: 13x27')
/
insert into input (message) values ('#1222 @ 508,577: 18x27')
/
insert into input (message) values ('#1223 @ 718,122: 17x11')
/
insert into input (message) values ('#1224 @ 274,577: 29x12')
/
insert into input (message) values ('#1225 @ 372,829: 19x18')
/
insert into input (message) values ('#1226 @ 344,263: 14x20')
/
insert into input (message) values ('#1227 @ 749,280: 14x23')
/
insert into input (message) values ('#1228 @ 744,488: 13x18')
/
insert into input (message) values ('#1229 @ 777,204: 26x20')
/
insert into input (message) values ('#1230 @ 460,32: 29x12')
/
insert into input (message) values ('#1231 @ 253,151: 13x20')
/
insert into input (message) values ('#1232 @ 950,198: 27x11')
/
insert into input (message) values ('#1233 @ 307,137: 14x18')
/
insert into input (message) values ('#1234 @ 554,332: 29x11')
/
insert into input (message) values ('#1235 @ 770,318: 15x22')
/
insert into input (message) values ('#1236 @ 331,38: 29x20')
/
insert into input (message) values ('#1237 @ 779,493: 22x16')
/
insert into input (message) values ('#1238 @ 739,335: 21x23')
/
insert into input (message) values ('#1239 @ 396,570: 21x11')
/
insert into input (message) values ('#1240 @ 354,309: 11x10')
/
insert into input (message) values ('#1241 @ 809,320: 29x24')
/
insert into input (message) values ('#1242 @ 959,879: 20x20')
/
insert into input (message) values ('#1243 @ 489,346: 14x29')
/
insert into input (message) values ('#1244 @ 468,179: 22x27')
/
insert into input (message) values ('#1245 @ 794,21: 23x18')
/
insert into input (message) values ('#1246 @ 51,708: 14x28')
/
insert into input (message) values ('#1247 @ 913,448: 17x19')
/
insert into input (message) values ('#1248 @ 810,787: 20x12')
/
insert into input (message) values ('#1249 @ 896,578: 29x24')
/
insert into input (message) values ('#1250 @ 948,562: 12x19')
/
insert into input (message) values ('#1251 @ 756,410: 24x13')
/
insert into input (message) values ('#1252 @ 373,589: 12x7')
/
insert into input (message) values ('#1253 @ 474,224: 22x19')
/
insert into input (message) values ('#1254 @ 127,222: 23x14')
/
insert into input (message) values ('#1255 @ 422,317: 19x16')
/
insert into input (message) values ('#1256 @ 372,250: 20x28')
/
insert into input (message) values ('#1257 @ 932,598: 21x12')
/
insert into input (message) values ('#1258 @ 529,151: 20x15')
/
insert into input (message) values ('#1259 @ 422,441: 14x22')
/
insert into input (message) values ('#1260 @ 751,761: 21x11')
/
insert into input (message) values ('#1261 @ 956,817: 25x29')
/
insert into input (message) values ('#1262 @ 622,418: 19x27')
/
insert into input (message) values ('#1263 @ 490,38: 29x15')
/
insert into input (message) values ('#1264 @ 525,310: 23x17')
/
insert into input (message) values ('#1265 @ 2,610: 29x21')
/
insert into input (message) values ('#1266 @ 873,517: 12x23')
/
insert into input (message) values ('#1267 @ 565,594: 25x16')
/
insert into input (message) values ('#1268 @ 306,500: 17x18')
/
insert into input (message) values ('#1269 @ 616,480: 16x13')
/
insert into input (message) values ('#1270 @ 742,971: 20x21')
/
insert into input (message) values ('#1271 @ 633,573: 24x14')
/
insert into input (message) values ('#1272 @ 399,11: 25x15')
/
insert into input (message) values ('#1273 @ 334,186: 23x16')
/
insert into input (message) values ('#1274 @ 13,754: 26x22')
/
insert into input (message) values ('#1275 @ 500,784: 17x24')
/
insert into input (message) values ('#1276 @ 316,811: 20x17')
/
insert into input (message) values ('#1277 @ 128,842: 15x22')
/
insert into input (message) values ('#1278 @ 558,515: 11x12')
/
insert into input (message) values ('#1279 @ 610,262: 4x7')
/
insert into input (message) values ('#1280 @ 499,321: 26x29')
/
insert into input (message) values ('#1281 @ 178,367: 15x14')
/
insert into input (message) values ('#1282 @ 826,493: 19x27')
/
insert into input (message) values ('#1283 @ 462,871: 26x22')
/
insert into input (message) values ('#1284 @ 91,889: 14x29')
/
insert into input (message) values ('#1285 @ 157,746: 26x12')
/
insert into input (message) values ('#1286 @ 799,339: 21x23')
/
insert into input (message) values ('#1287 @ 221,661: 21x20')
/
insert into input (message) values ('#1288 @ 697,551: 28x10')
/
insert into input (message) values ('#1289 @ 241,530: 17x26')
/
insert into input (message) values ('#1290 @ 72,249: 18x15')
/
insert into input (message) values ('#1291 @ 948,549: 29x12')
/
insert into input (message) values ('#1292 @ 564,689: 29x14')
/
insert into input (message) values ('#1293 @ 714,265: 27x15')
/
insert into input (message) values ('#1294 @ 629,198: 3x8')
/
insert into input (message) values ('#1295 @ 766,155: 12x12')
/
insert into input (message) values ('#1296 @ 590,742: 13x21')
/
insert into input (message) values ('#1297 @ 125,605: 29x26')
/
insert into input (message) values ('#1298 @ 285,461: 21x26')
/
insert into input (message) values ('#1299 @ 416,16: 11x22')
/
insert into input (message) values ('#1300 @ 237,466: 18x11')
/
insert into input (message) values ('#1301 @ 784,394: 23x19')
/
insert into input (message) values ('#1302 @ 953,368: 27x25')
/
insert into input (message) values ('#1303 @ 375,976: 20x12')
/
insert into input (message) values ('#1304 @ 199,656: 25x11')
/
insert into input (message) values ('#1305 @ 582,121: 29x11')
/
insert into input (message) values ('#1306 @ 63,620: 29x17')
/
insert into input (message) values ('#1307 @ 846,612: 19x24')
/
insert into input (message) values ('#1308 @ 588,799: 11x16')
/
insert into input (message) values ('#1309 @ 209,633: 21x23')
/
insert into input (message) values ('#1310 @ 120,453: 20x13')
/
insert into input (message) values ('#1311 @ 566,915: 20x11')
/
insert into input (message) values ('#1312 @ 917,666: 23x27')
/
insert into input (message) values ('#1313 @ 439,905: 22x12')
/
insert into input (message) values ('#1314 @ 605,131: 19x25')
/
insert into input (message) values ('#1315 @ 603,30: 16x20')
/
insert into input (message) values ('#1316 @ 477,183: 26x25')
/
insert into input (message) values ('#1317 @ 638,66: 21x27')
/
insert into input (message) values ('#1318 @ 729,959: 12x15')
/
insert into input (message) values ('#1319 @ 119,379: 20x11')
/
insert into input (message) values ('#1320 @ 543,281: 25x27')
/
insert into input (message) values ('#1321 @ 82,728: 10x11')
/
insert into input (message) values ('#1322 @ 336,557: 29x27')
/
insert into input (message) values ('#1323 @ 277,314: 22x14')
/
insert into input (message) values ('#1324 @ 810,286: 28x21')
/
insert into input (message) values ('#1325 @ 164,397: 18x17')
/
insert into input (message) values ('#1326 @ 105,190: 15x17')
/
insert into input (message) values ('#1327 @ 250,357: 18x25')
/
insert into input (message) values ('#1328 @ 818,347: 12x18')
/
insert into input (message) values ('#1329 @ 525,299: 11x29')
/
insert into input (message) values ('#1330 @ 944,627: 12x23')
/
insert into input (message) values ('#1331 @ 599,609: 14x19')
/
insert into input (message) values ('#1332 @ 756,125: 26x18')
/
insert into input (message) values ('#1333 @ 454,881: 19x18')
/
insert into input (message) values ('#1334 @ 880,142: 18x20')
/
insert into input (message) values ('#1335 @ 310,590: 10x11')
/
insert into input (message) values ('#1336 @ 239,879: 27x19')
/
insert into input (message) values ('#1337 @ 280,327: 24x19')
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
