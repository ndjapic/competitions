# Задатак: D_Tiling.pas

```pascal
program D_Tiling;
var
    n, h, w, i, k: int8;
    a, b: array [1 .. 7] of int8;
    used_tile: int8;
    used_cell: array [1 .. 10] of int16;

function used_rectangle(i1, j1, i2, j2: int8): boolean;
var
    i: int8;
    mask: int16;
    ans: boolean;
begin
    ans := true;
    mask := (int16(1) shl j2) - (int16(1) shl (j1-1));
    i := i1;
    while ans and (i <= i2) do begin
        ans := used_cell[i] and mask = 0;
        inc(i);
    end;
    used_rectangle := ans;
end;

procedure swap_rectangle(i1, j1, i2, j2: int8);
var
    i: int8;
    mask: int16;
begin
    mask := (int16(1) shl j2) - (int16(1) shl (j1-1));
    for i := i1 to i2 do
        used_cell[i] := used_cell[i] xor mask;
end;

function dfs(i1, j1, remain: int8): boolean;
var
    i2, j2, k, m: int8;
    ans: boolean;
begin
    if j1 > w then begin
        j1 := 1;
        inc(i1);
    end;

    while (i1 <= h) and odd(used_cell[i1] shr (j1-1)) do begin
        inc(j1);
        if j1 > w then begin
            j1 := 1;
            inc(i1);
        end;
    end;

    ans := remain = 0;

    if not ans and (i1 <= h) then begin
        k := 1;
        m := 1;
        while not ans and (k <= n) do begin
            i2 := i1 + a[k] - 1;
            j2 := j1 + b[k] - 1;
            if (used_tile and m = 0) and (i2 <= h) and (j2 <= w) and used_rectangle(i1, j1, i2, j2) then begin
                swap_rectangle(i1, j1, i2, j2);
                inc(used_tile, m);
                ans := dfs(i1, j2+1, remain - a[k] * b[k]);
                swap_rectangle(i1, j1, i2, j2);
                dec(used_tile, m);
            end;
            inc(k);
            inc(m, m);
        end;
    end;

    if not ans and (i1 <= h) then begin
        k := 1;
        m := 1;
        while not ans and (k <= n) do begin
            i2 := i1 + b[k] - 1;
            j2 := j1 + a[k] - 1;
            if (used_tile and m = 0) and (i2 <= h) and (j2 <= w) and used_rectangle(i1, j1, i2, j2) then begin
                swap_rectangle(i1, j1, i2, j2);
                inc(used_tile, m);
                if j2 < w then
                    ans := dfs(i1, j2+1, remain - a[k] * b[k])
                else
                    ans := dfs(i1+1, 1, remain - a[k] * b[k]);
                swap_rectangle(i1, j1, i2, j2);
                dec(used_tile, m);
            end;
            inc(k);
            inc(m, m);
        end;
    end;

    dfs := ans;
end;

begin
    readln(n, h, w);

    for k := 1 to n do readln(a[k], b[k]);

    for i := 1 to h do used_cell[i] := 0;
    used_tile := 0;

    if dfs(1, 1, h*w) then
        writeln('Yes')
    else
        writeln('No');
end.

```
