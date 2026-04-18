# Задатак: B_Puzzle_of_Lamps.pas

```pascal
program B_Puzzle_of_Lamps;
{$mode delphi}
const
    nn = 30;
var
    n, i, l, r: int8;
    m: int16;
    s, t: string;

begin
    readln(n);
    readln(s);
    setlength(t, 900);

    m := 0;
    r := 0;
    for l := n downto 0 do
        if (l > 0) and (s[l] = '1') then begin
            if r = 0 then r := l;
        end else if (r > 0) and ((l = 0) or (s[l] = '0')) then begin
            for i := 1 to r do begin
                inc(m);
                t[m] := 'A';
            end;
            for i := 1 to l do begin
                inc(m);
                t[m] := 'B';
            end;
            r := 0;
        end;

    writeln(m);
    setlength(t, m);
    writeln(t);
end.

```
