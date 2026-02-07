program B_cat;
{$MODE DELPHI}
const
    nn = 50;
var
    n, i, j: int8;
    s: array [1 .. nn] of string;
    t: string;

begin
    readln(n);

    for i := 1 to n do begin
        readln(t);
        j := i-1;
        while (j > 0) and (length(s[j]) > length(t)) do begin
            s[j+1] := s[j];
            dec(j);
        end;
        s[j+1] := t;
    end;

    for i := 1 to n do write(s[i]);
    writeln;
end.
