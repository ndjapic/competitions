# Problem: D_Yarik_and_Musical_Notes.pas

```pascal
program D_Yarik_and_Musical_Notes;
uses
    math;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, l, r: int32;
    d, ans: int64;
    a, merge, last: array [1 .. maxn] of int32;
    c: array [1 .. 2] of int32;

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

        c[1] := 0;
        c[2] := 0;

        for i := 1 to n do begin
            read(a[i]);
            if a[i] < 3 then inc(c[a[i]]);
        end;
        readln;
        msort(1, n);

        last[n] := n;
        for i := n-1 downto 1 do
            if a[i] = a[i+1] then
                last[i] := last[i+1]
            else
                last[i] := i;

        ans := -n;

        l := 1;
        while l <= n do begin
            r := last[l];
            d := r-l+1;
            inc(ans, d*d);
            l := r+1;
        end;
        ans := ans div 2;

        inc(ans, int64(c[1]) * c[2]);

        writeln(ans);

    end;
end.

```
