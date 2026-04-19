# Problem: B_Uniform_Sum.pas

```pascal
program B_Uniform_Sum;
var
    n, na, nb, i: int32;
    found: boolean;
    a, b, cp: array of int32;

procedure msort(var arr: array of int32; l, r: int32);
var
    m, i, il, ir: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(arr, l, m);
        msort(arr, m, r);

        il := l;
        ir := m;
        for i := l to r-1 do
            if (ir >= r) or (il < m) and (arr[il] <= arr[ir]) then begin
                cp[i] := arr[il];
                inc(il);
            end else begin
                cp[i] := arr[ir];
                inc(ir);
            end;

        for i := l to r-1 do arr[i] := cp[i];

    end;
end;

procedure readarr(var arr: array of int32; var j: int32);
begin
    j := 0;
    for i := 1 to n do begin
        read(arr[j]);
        if arr[j] > -1 then inc(j);
    end;
    readln;
    msort(arr, 0, j);
    for i := 0 to j-1 do write(' ', arr[i]); writeln;
end;

function check(i0: int32): boolean;
var
    s, i, j, ca, cb: int32;
begin
    s := a[i0] + b[nb - 1];
    ca := n - na;
    cb := n - nb;
    i := 0;
    j := nb - 1;

    while (i < na) and (j >= 0) and (ca >= 0) and (cb >= 0) do
        if a[i] + b[j] < s then begin
            inc(i);
            dec(ca);
        end else if a[i] + b[j] > s then begin
            dec(j);
            dec(cb);
        end else begin
            inc(i);
            dec(j);
        end;

    check := (ca >= 0) and (cb >= 0);
end;

begin
    readln(n);

    setlength(a, n);
    setlength(b, n);
    setlength(cp, n);

    readarr(a, na);
    readarr(b, nb);

    i := 0;
    found := false;
    while (i < na) and not found do begin
        found := check(i);
        inc(i);
    end;

    if found then
        writeln('Yes', i-1)
    else
        writeln('No');
end.

```
