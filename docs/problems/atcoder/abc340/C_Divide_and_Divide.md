# Problem: C_Divide_and_Divide.pas

```pascal
program C_Divide_and_Divide;
var
    n: int64;

function dfs(n: int64): int64;
var
    h, k: int64;
begin
    h := n div 2;
    k := n div 4;
    if n < 2 then
        dfs := 0
    else if n = 3 then
        dfs := 5
    else if not odd(n) then
        dfs := n + 2*dfs(h)
    else if odd(h) then
        dfs := 2*n + dfs(k) + 3*dfs(k+1)
    else
        dfs := 2*n + 3*dfs(k) + dfs(k+1);
end;

begin
    readln(n);
    writeln(dfs(n));
end.

```
