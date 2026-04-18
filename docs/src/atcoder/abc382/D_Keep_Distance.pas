program D_Keep_Distance;
const
    nn = 12;
var
    n, m, i: int8;
    x, y: int32;
    a: array of array [1 .. nn] of int8;

procedure dfs(i, ai: int8);
begin
    if i <= n then begin
        a[x][i] := ai;
        while a[x][i] <= m do begin
            dfs(i+1, a[x][i]);
            inc(a[x][i]);
        end;
    end else begin
        inc(x);
        if x = length(a) then setlength(a, 2*x);
        a[x] := a[x-1];
    end;
end;

begin
    readln(n, m);
    dec(m, (n-1)*10);

    x := 0;
    setlength(a, 1);
    dfs(1, 1);

    writeln(x);
    for y := 0 to x-1 do begin
        for i := 1 to n-1 do write(a[y][i] + (i-1)*10, ' ');
        writeln(a[y][n] + (n-1)*10);
    end;
end.
