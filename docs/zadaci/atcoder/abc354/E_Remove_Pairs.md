# Задатак: E_Remove_Pairs.pas

```pascal
program E_Remove_Pairs;
{$MODE DELPHI}
const
    nn = 18;
    pownn = 256 * 1024;
var
    n, i: int8;
    seen: int32;
    a, b: array [1 .. nn] of int32;
    dp: array [0 .. pownn] of int8;

function dfs(seen: int32): boolean;
var
    l, r: int8;
begin
    if dp[seen] = -1 then begin
        result := false;
        l := 1;
        r := 2;
        while not result and (r <= n) do begin
            if not odd(seen shr (l-1)) and not odd(seen shr (r-1)) and ((a[l] = a[r]) or (b[l] = b[r])) then begin
                seen := seen xor (int32(1) shl (l-1));
                seen := seen xor (int32(1) shl (r-1));
                result := not dfs(seen);
                seen := seen xor (int32(1) shl (l-1));
                seen := seen xor (int32(1) shl (r-1));
            end;
            inc(l);
            if l = r then begin
                inc(r);
                l := 1;
            end;
        end;
        if result then
            dp[seen] := 1
        else
            dp[seen] := 0;
    end;
    result := dp[seen] = 1;
end;

begin
    readln(n);
    for i := 1 to n do readln(a[i], b[i]);
    for seen := 0 to pownn do dp[seen] := -1;

    if dfs(0) then
        writeln('Takahashi')
    else
        writeln('Aoki');
end.

```
