# Задатак: C_Training_Before_the_Olympiad.pas

```pascal
program C_Training_Before_the_Olympiad;
const
    maxn = 100 * 1000;
var
    ntc, tci: int16;
    n, i: int32;
    s, ans: int64;
    a: array [1 .. maxn] of int32;
    c: array [0 .. 1] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);

        c[0] := 0;
        c[1] := 0;
        s := 0;

        for i := 1 to n do begin

            read(a[i]);
            inc(s, a[i]);
            inc(c[a[i] mod 2]);

            ans := s - c[1] div 3;
            if ((c[0] > 0) or (i > 3)) and (c[1] mod 3 = 1) then dec(ans);

            write(ans);
            if i < n then write(' ');

        end;
        readln;
        writeln;

    end;
end.

```
