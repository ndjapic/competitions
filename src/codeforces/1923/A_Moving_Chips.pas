program A_Moving_Chips;
uses
    math;
const
    maxn = 50;
var
    ntc, tci: int16;
    n, i, l, r, ans: int8;
    a: array [1 .. maxn] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]); readln;

        l := 1;
        r := n;
        while (l < r) and (a[l] = 0) do inc(l);
        while (l < r) and (a[r] = 0) do dec(r);

        ans := 0;
        for i := l to r do
            if a[i] = 0 then inc(ans);

        writeln(ans);

    end;

end.
