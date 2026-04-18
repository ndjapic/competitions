# Задатак: F_0_1_2_Tree.pas

```pascal
program F_0_1_2_Tree;
{$MODESWITCH RESULT+}
uses
    math;
const
    maxn = 300 * 1000;
var
    ntc, tci: int16;
    a, b, c, u, v: int32;
    l, r: array [1 .. maxn] of int32;

function dfs(u: int32): int32;
begin
    result := 0;
    if u <= a+b then result := max(result, dfs(l[u]));
    if u <= a then result := max(result, dfs(r[u]));
    inc(result);
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(a, b, c);

        v := 1;
        for u := 1 to a+b do begin
            inc(v);
            l[u] := v;
            if u <= a then begin
                inc(v);
                r[u] := v;
            end;
        end;

        if v <> a+b+c then
            writeln(-1)
        else
            writeln(dfs(1)-1);

    end;
end.

```
