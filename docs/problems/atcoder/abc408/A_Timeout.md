# Problem: A_Timeout.pas

```pascal
program A_Timeout;
const
    nn = 100;
var
    n, s, i: int8;
    ans: boolean;
    t: array [0 .. nn] of int16;

begin
    readln(n, s);

    t[0] := 0;
    ans := true;

    for i := 1 to n do begin
        read(t[i]);
        if ans then ans := t[i] - t[i-1] <= s;
    end;
    readln;

    if ans then
        writeln('Yes')
    else
        writeln('No');
end.

```
