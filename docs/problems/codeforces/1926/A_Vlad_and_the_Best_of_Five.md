# Problem: A_Vlad_and_the_Best_of_Five.pas

```pascal
program A_Vlad_and_the_Best_of_Five;
var
    ntc, tci, i, c: int8;
    ch: char;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        c := 0;
        for i := 1 to 5 do begin
            read(ch);
            case ch of
                'A': inc(c);
                'B': dec(c);
            end;
        end;
        readln;

        if c > 0 then
            writeln('A')
        else
            writeln('B');

    end;
end.

```
