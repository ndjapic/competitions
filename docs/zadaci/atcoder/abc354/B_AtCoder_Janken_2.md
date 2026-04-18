# Задатак: B_AtCoder_Janken_2.pas

```pascal
program B_AtCoder_Janken_2;
{$H+}
const
    nn = 100;
type
    tarr8 = array [0 .. nn] of int8;
    tarr16 = array [0 .. nn] of int16;
    tarrstr = array [0 .. nn] of string;
var
    n, i, j: int8;
    t: int32;
    ch: char;
    s: tarrstr;
    c: tarr16;
    p, merge: tarr8;

procedure msorti(var indices: tarr8; priority: tarrstr; l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msorti(indices, priority, l, m);
        msorti(indices, priority, m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (
                priority[indices[j]] <= priority[indices[k]]
            ) then begin
                merge[i] := indices[j];
                inc(j);
            end else begin
                merge[i] := indices[k];
                inc(k);
            end;

        for i := l to r-1 do indices[i] := merge[i];

    end;
end;

begin
    readln(n);
    t := 0;

    for i := 0 to n-1 do begin

        setlength(s[i], 16);

        j := 0;
        read(ch);

        while ch <> ' ' do begin
            inc(j);
            s[i][j] := ch;
            read(ch);
        end;
        setlength(s[i], j);

        readln(c[i]);
        p[i] := i;
        inc(t, c[i]);

    end;

    msorti(p, s, 0, n);

    i := p[t mod n];
    writeln(s[i]);
end.

```
