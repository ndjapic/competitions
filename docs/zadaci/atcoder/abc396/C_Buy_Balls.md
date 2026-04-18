# Задатак: C_Buy_Balls.pas

```pascal
program C_Buy_Balls;
{$mode delphi}
uses
    Generics.Defaults, Generics.Collections;
const
    nn = 200 * 1000;

var
    n, m, i, j, x: int32;
    s: int64;
    b, w: TList<int32>;

begin
    readln(n, m);

    b := TList<int32>.Create();
    for i := 1 to n do begin
        read(x);
        b.Add(x);
    end;
    readln;
    b.Sort();

    w := TList<int32>.Create();
    for j := 1 to m do begin
        read(x);
        w.Add(x);
    end;
    readln;
    w.Sort();

    s := 0;

    j := m-1;
    for i := n-1 downto 0 do
        if b[i] >= 0 then begin
            inc(s, b[i]);
            if (j >= 0) and (w[j] >= 0) then begin
                inc(s, w[j]);
                dec(j);
            end;
        end else if (j >= 0) and (b[i] + w[j] >= 0) then begin
            inc(s, b[i] + w[j]);
            dec(j);
        end;

    writeln(s);

    b.Free();
    w.Free();
end.

```
