# Задатак: A1_Balanced_Shuffle.pas

```pascal
program A1_Balanced_Shuffle;
const
    nn = 500 * 1000 + 1;
var
    n, i: int32;
    s, t: string;
    position, balance, merge: array [1 .. nn] of int32;

procedure msorti(l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msorti(l, m);
        msorti(m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (
                (balance[position[j]] < balance[position[k]]) or
                (balance[position[j]] = balance[position[k]]) and
                (position[j] > position[k])
            ) then begin
                merge[i] := position[j];
                inc(j);
            end else begin
                merge[i] := position[k];
                inc(k);
            end;

        for i := l to r-1 do position[i] := merge[i];

    end;
end;

begin
    readln(s);
    n := length(s);

    balance[1] := 0;
    for i := 1 to n do begin
        balance[i+1] := balance[i];
        if s[i] = '(' then
            inc(balance[i+1])
        else
            dec(balance[i+1]);
        position[i] := i;
    end;

    msorti(1, n+1);

    setlength(t, n);
    for i := 1 to n do t[i] := s[position[i]];
    writeln(t);
end.

```
