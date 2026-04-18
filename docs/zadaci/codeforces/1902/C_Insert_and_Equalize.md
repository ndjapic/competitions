# Задатак: C_Insert_and_Equalize.pas

```pascal
program C_Insert_and_Equalize;
const
    maxn = 200 * 1000 + 1;
var
    ntc, tci: int16;
    n, i, x: int32;
    ans: int64;
    a, merge: array [1 .. maxn] of int32;

function gcd(a, b: int32): int32;
begin
    if b = 0 then
        gcd := a
    else
        gcd := gcd(b, a mod b);
end;

procedure msort(l, r: int32);
var
    m, i, il, ir: int32;
begin
    if l < r then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m+1, r);

        il := l;
        ir := m+1;
        for i := l to r do
            if (ir > r) or (il <= m) and (a[il] <= a[ir]) then begin
                merge[i] := a[il];
                inc(il);
            end else begin
                merge[i] := a[ir];
                inc(ir);
            end;

        for i := l to r do a[i] := merge[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]);
        readln;
        msort(1, n);

        for i := 2 to n do dec(a[i], a[1]);
        a[1] := 0;

        x := 0;
        for i := 2 to n do x := gcd(x, a[i] - a[i-1]);

        if x = 0 then
            ans := 1
        else begin

            for i := 2 to n do a[i] := a[i] div x;

            a[n+1] := a[n];
            for i := n downto 1 do
                if a[n+1] = a[i] then dec(a[n+1]);

            ans := 0;
            for i := 1 to n+1 do
                inc(ans, a[n] - a[i]);

        end;

        writeln(ans);

    end;
end.

```
