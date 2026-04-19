# Problem: A_Max_and_Mod.pas

```pascal
program A_Max_and_Mod;
{$MODE DELPHI}
uses
    math;
const
    nn = 100;
var
    ntc, tci: int8;
    n, i, j, k, x: int8;
    ans: boolean;
    p: array [1 .. nn] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        ans := false;

        for j := 1 to 2 do
            for k := 1 to n do
                if not ans and (j <> k) then begin

                    p[j] := 1;
                    p[k] := n;
                    i := n;
                    for x := n-1 downto 2 do begin
                        if i = j then dec(i);
                        p[i] := x;
                        dec(i);
                    end;

                    ans := true;
                    for i := 2 to n do
                        ans := ans and (max(p[i-1], p[i]) mod i = i-1);

                end;

        if ans then begin
            for i := 1 to n-1 do write(p[i], ' ');
            writeln(p[n]);
        end else
            writeln(-1);

    end;
end.

```
