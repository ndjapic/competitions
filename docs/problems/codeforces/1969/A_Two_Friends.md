# Problem: A_Two_Friends.pas

```pascal
program A_Two_Friends;
const
    sz = 50;
var
    ntc, tci: int16;
    n, i: int32;
    p: array [0 .. sz] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(p[i]);
        readln;

        i := 1;
        while (i <= n) and (p[p[i]] <> i) do inc(i);

        if i <= n then
            writeln(2)
        else
            writeln(3);

    end;
end.

```
