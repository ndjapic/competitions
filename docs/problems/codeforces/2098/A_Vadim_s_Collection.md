# Problem: A_Vadim_s_Collection.pas

```pascal
program A_Vadim_s_Collection;
{$MODE DELPHI}
var
    ntc, tci: int16;
    n, i, j, k: int8;
    s: string;
    ch: char;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(s);
        n := length(s);

        for i := 1 to n do begin

            k := i;
            for j := i+1 to n do
                if (ord(s[j]) - ord('0') >= 10-i) and (s[j] < s[k]) then
                    k := j;

            if k > i then begin
                ch := s[i];
                s[i] := s[k];
                s[k] := ch;
            end;

        end;

        writeln(s);

    end;
end.

```
