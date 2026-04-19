# Problem: D_Not_Intersect.pas

```pascal
program D_Not_Intersect;
uses
    math;
const
    maxn = 3000;
    maxm = 3000;
    prime = 1000 * 1000 * 1000 + 7;
var
    n: int32;
    m: int64;
    f, g: array [1 .. maxn, 0 .. maxm] of int64;

function get_g(n: int32; m: int64): int32;
var
    ans: int64;
begin
    if int64(n-1) * n div 2 - 1 < m then
        ans := 0
    else if m = 0 then
        ans := 1
    else if m = 1 then
        ans := int64(n-1) * n div 2 - 1
    else
        ans := g[n, m];
    g[n, m] := ans mod prime;
    get_g := g[n, m];
end;

function get_f(n: int32; m: int64): int32;
var
    ans: int64;
begin
    if int64(n-1) * n div 2 < m then
        ans := 0
    else if m = 0 then
        ans := 1
    else if m = 1 then
        ans := int64(n-1) * n div 2
    else
        ans := f[n, m];
    f[n, m] := ans mod prime;
    get_f := f[n, m];
end;

begin
    for m := 0 to maxm do begin
        g[1, m] := 0;
        g[2, m] := 0;
        f[1, m] := 0;
    end;

    for n := 2 to maxn do begin
        {write('n=', n);}
        for m := 0 to maxm do begin
            g[n, m] := 2 * (get_g(n-1, m) + get_g(n-1, m-1)) - get_g(n-2, m);
            g[n, m] := (g[n, m] + prime) mod prime;
            f[n, m] := get_g(n, m) + get_g(n, m-1);
            f[n, m] := f[n, m] mod prime;
            {write(' ', f[n, m]);}
        end;
        {writeln;}
    end;

    readln(n, m);
    writeln(get_f(n, m));
end.

```
