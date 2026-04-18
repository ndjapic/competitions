# Задатак: B_StORage_room.pas

```pascal
program B_StORage_room;
const
    maxn = 1000;
    maxm = 1024 * 1024 * 1024 - 1;
var
    ntc, tci: int16;
    n, i, j: int16;
    ans: boolean;
    m: array [1 .. maxn, 1 .. maxn] of int32;
    a: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do a[i] := maxm;

        for i := 1 to n do begin
            for j := 1 to n do begin
                read(m[i, j]);
                if i <> j then begin
                    a[i] := a[i] and m[i, j];
                    a[j] := a[j] and m[i, j];
                end;
            end;
            readln;
        end;

        ans := true;
        for i := 1 to n do
            for j := 1 to n do
                if i <> j then
                    ans := ans and (a[i] or a[j] = m[i, j]);

        if ans then begin
            writeln('YES');
            for i := 1 to n-1 do write(a[i], ' '); writeln(a[n]);
        end else
            writeln('NO');

    end;
end.

```
