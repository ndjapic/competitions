# Problem: C_Choose_the_Different_Ones.pas

```pascal
program C_Choose_the_Different_Ones;
const
    maxk = 400 * 1000;
var
    ntc, tci: int16;
    n, m, i, j, k, h, c, ca, cb, cab: int32;
    ans: boolean;
    a, b: array [1 .. maxk] of boolean;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m , k);
        h := k div 2;

        for j := 1 to k do begin
            a[j] := false;
            b[j] := false;
        end;

        for i := 1 to n do begin
            read(j);
            if j <= k then a[j] := true;
        end;
        readln;

        for i := 1 to m do begin
            read(j);
            if j <= k then b[j] := true;
        end;
        readln;

        c := 0;
        ca := 0;
        cb := 0;
        cab := 0;

        for j := 1 to k do
            if a[j] and not b[j] then
                inc(ca)
            else if b[j] and not a[j] then
                inc(cb)
            else if a[j] and b[j] then
                inc(cab)
            else
                inc(c);

        ans := (c = 0) and (ca <= h) and (cb <= h) and (ca+cb+cab >= k);

        if ans then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
