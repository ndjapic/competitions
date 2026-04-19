# Problem: C_Iroha_s_Obsession.pas

```pascal
program C_Iroha_s_Obsession;
var
    n: int32;
    k, d, i: int8;
    like: array [0 .. 9] of boolean;

function contain_disliked(n: int32): boolean;
begin
    while (n > 0) and (like[n mod 10]) do n := n div 10;
    contain_disliked := n > 0;
end;

begin
    readln(n, k);

    for d := 0 to 9 do like[d] := true;

    for i := 1 to k do begin
        read(d);
        like[d] := false;
    end;
    readln;

    while contain_disliked(n) do inc(n);
    writeln(n);
end.

```
