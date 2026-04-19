# Problem: A_Deranged_Deletions.pas

```pascal
program A_Deranged_Deletions;
const
    nn = 100;
var
    ntc, tci, n, i: int8;
    a: array [1 .. nn] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(a[i]); readln;

        i := 1;
        while (i < n) and (a[i] <= a[i+1]) do inc(i);

        if i = n then
            writeln('NO')
        else begin
            writeln('YES');
            writeln(2);
            writeln(a[i], ' ', a[i+1]);
        end;

    end;
end.

```
