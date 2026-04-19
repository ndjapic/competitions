# Problem: D1_Range_Sorting_Easy_Version.pas

```pascal
program D1_Range_Sorting_Easy_Version;
uses
    math;
const
    maxn = 200 * 1000;
    prime = 1000 * 1000 * 1000 + 7;
type
    tarray = array [1 .. maxn] of int32;
var
    notc: int16;
    n, i, j, ans: int32;
    a, b, c: tarray;

procedure msort(var a: tarray; l, r: int32);
var
    m, i, il, ir: int32;
begin
    if l < r then begin

        m := (l+r) div 2;
        msort(a, l, m);
        msort(a, m+1, r);

        il := l;
        ir := m+1;
        for i := l to r do
            if (ir > r) or (il <= m) and (a[il] < a[ir]) then begin
                c[i] := a[il];
                inc(il);
            end else begin
                c[i] := a[ir];
                inc(ir);
            end;

        for i := l to r do a[i] := c[i];

    end;
end;

begin
    readln(notc);
    repeat

        readln(n);
        for i := 1 to n do read(a[i]); readln;
        for j := 1 to n do read(b[j]); readln;
        msort(a, 1, n);
        msort(b, 1, n);

        ans := 1;
        i := 1;
        j := 1;
        while (ans > 0) and (i <= n) do begin
            while (j <= n) and (b[j] < a[i]) do inc(j);
            ans := int64(ans) * max(j-i, 0) mod prime;
            inc(i);
        end;

        writeln(ans);

        dec(notc);
    until notc = 0;
end.


```
