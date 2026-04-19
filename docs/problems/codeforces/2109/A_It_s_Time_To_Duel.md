# Problem: A_It_s_Time_To_Duel.pas

```pascal
program A_It_s_Time_To_Duel;
const
    nn = 100;
var
    ntc, tci: int8;
    n, i: int8;
    ans: boolean;
    a: array [1 .. nn] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]);
        readln;

        i := 1;
        while (i < n) and ((a[i] > 0) or (a[i+1] > 0)) do inc(i);
        ans := i < n;

        if not ans then begin

            i := 1;
            while (i <= n) and (a[i] = 1) do inc(i);
            ans := i > n;

        end;

        if ans then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
