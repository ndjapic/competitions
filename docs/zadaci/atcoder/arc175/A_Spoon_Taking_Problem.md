# Задатак: A_Spoon_Taking_Problem.pas

```pascal
program A_Spoon_Taking_Problem;
{$H+}{$modeswitch result+}
uses
    math;
const
    maxn = 200 * 1000;
    prime = 998244353;
var
    n, i: int32;
    s: string;
    p: array [1 .. maxn] of int32;
    spoon: array [1 .. maxn] of boolean;

function dfs(i: int32): int32;
var
    x, y: int32;
begin
    if i > n then
        result := 1
    else begin

        result := 0;
        x := p[i];
        y := x mod n + 1;

        if spoon[x] and not spoon[y] then begin
            spoon[x] := false;
            inc(result, dfs(i+1));
            spoon[x] := true;
        end else if spoon[y] and not spoon[x] then begin
            spoon[y] := false;
            inc(result, dfs(i+1));
            spoon[y] := true;
        end else if spoon[x] and spoon[y] then begin
            if s[i] <> 'R' then begin
                spoon[x] := false;
                inc(result, dfs(i+1));
                spoon[x] := true;
            end;
            if s[i] <> 'L' then begin
                spoon[y] := false;
                inc(result, dfs(i+1));
                spoon[y] := true;
            end;
            if result >= prime then dec(result, prime);
        end;

    end;
end;

begin
    readln(n);
    for i := 1 to n do read(p[i]); readln;
    readln(s);

    for i := 1 to n do spoon[i] := true;
    writeln(dfs(1));
end.

```
