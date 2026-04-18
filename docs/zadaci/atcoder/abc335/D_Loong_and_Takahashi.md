# Задатак: D_Loong_and_Takahashi.pas

```pascal
program D_Loong_and_Takahashi;
const
    maxn = 45;
var
    n, h, i, j: int8;
    x: int16;
    a: array [1 .. maxn, 1 .. maxn] of int16;

procedure advance(i, j: int8);
begin
    inc(x);
    a[i, j] := x;
end;

begin
    readln(n);
    h := n div 2;
    a[h+1, h+1] := 0;

    x := 0;
    for i := 1 to h do begin
        for j := i to n-i do advance(i, j);
        for j := i to n-i do advance(j, n+1-i);
        for j := n+1-i downto i+1 do advance(n+1-i, j);
        for j := n+1-i downto i+1 do advance(j, i);
    end;

    for i := 1 to n do begin
        for j := 1 to n-1 do
            if a[i, j] > 0 then
                write(a[i, j], ' ')
            else
                write('T ');
        writeln(a[i, n]);
    end;
end.

```
