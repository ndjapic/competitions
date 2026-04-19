# Problem: D_Medicines_on_Grid.pas

```pascal
program D_Medicines_on_Grid;
{$H+}
const
    maxh = 200;
    maxn = 300;
var
    h, w, n, i, j, k, e, l, r: int32;
    a: array [1 .. maxh] of string;
    medicine, energy: array [1 .. maxh, 1 .. maxh] of int32;
    dp: array [1 .. maxh, 1 .. maxh] of boolean;
    queue: array of record
        i, j, e: int32;
    end;

procedure enqueue(i, j, e: int32);
begin
    if (0 < i) and (0 < j) and (i <= h) and (j <= w) and (a[i][j] <> '#') and (energy[i, j] < e) then begin

        inc(r);
        if length(queue) = r then setlength(queue, 2*r);
        energy[i, j] := e;
        queue[r].i := i;
        queue[r].j := j;
        queue[r].e := e;
        dp[i, j] := true;

    end;
end;

begin
    readln(h, w);

    for i := 1 to h do readln(a[i]);

    setlength(queue, 1);
    l := 0;
    r := -1;

    for i := 1 to h do
        for j := 1 to w do begin
            energy[i, j] := -1;
            medicine[i, j] := 0;
            dp[i, j] := false;
            if a[i][j] = 'S' then enqueue(i, j, 0);
        end;

    readln(n);
    for k := 1 to n do begin
        readln(i, j, e);
        medicine[i, j] := e;
    end;

    while l <= r do begin

        i := queue[l].i;
        j := queue[l].j;
        e := queue[l].e;
        inc(l);

        if e = energy[i, j] then begin

            if e < medicine[i, j] then begin
                e := medicine[i, j];
                energy[i, j] := e;
                medicine[i, j] := 0;
            end;

            dec(e);
            enqueue(i, j+1, e);
            enqueue(i, j-1, e);
            enqueue(i+1, j, e);
            enqueue(i-1, j, e);

        end;

    end;

    for i := 1 to h do
        for j := 1 to w do
            if a[i][j] <> 'T' then
            else if dp[i, j] then
                writeln('Yes')
            else
                writeln('No');
end.

```
