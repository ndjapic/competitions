program D_Lonely_Mountain_Dungeons;
uses
    math;
const
    maxn = 200 * 1000;
var
    ntc, tci, n, b, x, i, j: int32;
    k, ans: int64;
    loop: boolean;
    c, merge: array [1 .. maxn] of int32;
    strength: array [1 .. maxn] of int64;
    a: record
        n: int32;
        x, c: array [1 .. maxn] of int32;
    end;

procedure msort(l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (c[j] <= c[k]) then begin
                merge[i] := c[j];
                inc(j);
            end else begin
                merge[i] := c[k];
                inc(k);
            end;

        for i := l to r-1 do c[i] := merge[i];

    end;
end;

function f(ci, k: int32): int64;
var
    d, m: int32;
begin
    d := ci div k;
    m := ci mod k;
    f := (int64(ci) * ci - int64(k-m) * d * d - int64(m) * (d+1) * (d+1)) div 2;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, b, x);

        for i := 1 to n do read(c[i]);
        readln;
        msort(1, n+1);

        a.n := 1;
        a.x[1] := c[1];
        a.c[1] := 1;

        for i := 2 to n do
            if c[i] = c[i-1] then
                inc(a.c[a.n])
            else begin
                inc(a.n);
                a.x[a.n] := c[i];
                a.c[a.n] := 1;
            end;

        k := 1;
        strength[1] := 0;
        ans := 0;
        loop := true;

        while (k < c[n]) and loop do begin
            inc(k);
            strength[k] := -(k-1) * x;
            for j := 1 to a.n do
                inc(strength[k], f(a.x[j], k) * b * a.c[j]);
            loop := strength[k] > strength[k-1];
            if loop then
                ans := strength[k];
        end;

        writeln(ans);

    end;
end.
