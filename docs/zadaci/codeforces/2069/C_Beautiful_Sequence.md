# Задатак: C_Beautiful_Sequence.pas

```pascal
program C_Beautiful_Sequence;
const
    nn = 200 * 1000;
    prime = 998244353;
var
    ntc, tci: int16;
    n, i, h, s, t: int32;
    ai: int8;
    c: array [1 .. 3] of int32;
    p2, ph: array [0 .. nn] of int32;

begin
    h := (prime + 1) div 2;
    p2[0] := 1;
    ph[0] := 1;
    for i := 1 to nn do begin
        p2[i] := p2[i-1] * 2 mod prime;
        ph[i] := int64(ph[i-1]) * h mod prime;
    end;

    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);

        s := 0;
        t := 0;

        for ai := 1 to 3 do c[ai] := 0;
        for i := 1 to n do begin
            read(ai);
            case ai of
                1: s := (s + ph[c[2]]) mod prime;
                3: t := (t + int64(p2[c[2]]) * s + prime - c[1]) mod prime;
            end;
            inc(c[ai]);
        end;
        readln;

        writeln(t);

    end;
end.

```
