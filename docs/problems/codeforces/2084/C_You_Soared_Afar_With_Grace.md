# Problem: C_You_Soared_Afar_With_Grace.pas

```pascal
program C_You_Soared_Afar_With_Grace;
{$INLINE ON}
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, j, k, m: int32;
    a, b, inv: array [1 .. nn] of int32;
    op: array [1 .. nn] of record
        i, j: int32;
    end;

procedure swp(i, j: int32); inline;
var
    x: int32;
begin
    if i <> j then begin
        x := a[i];
        a[i] := a[j];
        a[j] := x;

        x := b[i];
        b[i] := b[j];
        b[j] := x;

        inv[b[i]] := i;
        inv[b[j]] := j;

        inc(m);
        op[m].i := i;
        op[m].j := j;
    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]); readln;
        for i := 1 to n do begin
            read(b[i]);
            inv[b[i]] := i;
        end;
        readln;

        m := 0;
        for i := 1 to n do
            if m > -1 then begin
                j := inv[a[i]];
                if i = j then
                else if b[i] <> a[j] then
                    m := -1
                else {if j > i then}
                    swp(n+1-i, j);
            end;

        i := 1;
        while (i <= n) and (a[i] = b[n+1-i]) do inc(i);
        if i <= n then m := -1;

        writeln(m);
        for k := 1 to m do writeln(op[k].i, ' ', op[k].j);

    end;
end.

```
