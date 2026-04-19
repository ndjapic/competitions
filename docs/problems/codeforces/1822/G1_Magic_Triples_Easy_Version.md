# Problem: G1_Magic_Triples_Easy_Version.pas

```pascal
{%RunFlags MESSAGES+}
program G1_Magic_Triples_Easy_Version;
uses
    math;
const
    maxn = 200 * 1000;
var
    notc: int16;
    n, i, l, r, b, c0: int32;
    ans: int64;
    a, z: array [0 .. maxn] of int32;

procedure msort(l, r: int32);
var
    i, j, k, m: int32;
begin
    if r - l > 1 then begin
        m := (l + r) div 2;
        msort(l, m);
        msort(m, r);

        i := l;
        j := m;
        for k := l to r-1 do
            if (j = r) or (i < m) and (a[i] < a[j]) then begin
                z[k] := a[i];
                inc(i);
            end else begin
                z[k] := a[j];
                inc(j);
            end;
        for k := l to r-1 do a[k] := z[k];
    end;
end;

function bisect(x, l, r: int32): int32;
var
    m: int32;
begin
    if x < a[l] then
        bisect := l
    else if a[r-1] <= x then
        bisect := r
    else begin
        m := (l + r) div 2;
        if x < a[m] then
            bisect := bisect(x, l, m)
        else
            bisect := bisect(x, m, r);
    end;
end;

function cnt(x: int32): int32;
var
    i: int32;
begin
    i := bisect(x, 0, n);
    if (i = 0) or (a[i-1] < x) then
        cnt := 0
    else if (i = 1) or (a[i-2] < x) then
        cnt := 1
    else if (i = 2) or (a[i-3] < x) then
        cnt := 2
    else begin
        cnt := i - bisect(x-1, 0, i);
    end;
end;

begin
    readln(notc);
    repeat

        readln(n);
        for i := 0 to n-1 do read(a[i]);
        readln;
        msort(0, n);

        ans := 0;

        l := 0;
        for r := 0 to n do begin
            if (r = n) or (a[l] < a[r]) then begin
                c0 := r - l;
                inc(ans, int64(c0-2) * (c0-1) * c0);
                l := r;
            end;
        end;

        b := 2;
        while {(int64(a[0]) * b <= a[n-1]) and} (int64(a[0]) * b * b <= a[n-1]) do begin
            l := 0;
            r := 0;
            while (r < n) {and (int64(a[r]) * b <= a[n-1])} and (int64(a[r]) * b * b <= a[n-1]) do begin
                inc(r);
                if (r = n) or (a[l] < a[r]) then begin
                    inc(ans, int64(r-l) * cnt(a[l]*b) * cnt(a[l]*b*b));
                    l := r;
                end;
            end;
            inc(b);
        end;

        writeln(ans);

        dec(notc);
    until notc = 0;
end.


```
